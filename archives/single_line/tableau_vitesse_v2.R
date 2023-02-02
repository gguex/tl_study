############ Création d'un tableau comportant le temps de déplacement à pied d'un arrêt i à un arrêt j
# Possible aussi de d'avoir d'autres moyens de transport


# Database connexion
library("RPostgreSQL")
# Connexion à la base de données
conn <- dbConnect("PostgreSQL", dbname = 'tl_small', host = 'localhost', user = 'rloup', password = '')

library("sf")
# Réseau à cartographier
reseau <- st_read(conn, query="
select distinct code_arret_theo, libelle_arret_theo, lat_arret, lon_arret
from freq_theo
order by code_arret_theo;")

# To avoid to connect to the database
# write.csv(reseau,"reseau.csv", row.names = FALSE)
setwd("/Users/rloup/Desktop")
# reseau <- read.csv("reseau.csv")

# OTP connexion
# Set the working directory
setwd("/Users/rloup/Documents/otpServ") # choose the location of your server
# install.packages("curl")
# install.packages("remotes")
# remotes::install_github("ITSleeds/opentripplanner")
library("opentripplanner")
# https://github.com/ropensci/opentripplanner/blob/master/R/otp-plan.R

# Set the server
otp = paste0(getwd(), "/otp-1.4.0-shaded.jar")
graphdir <- paste0(getwd(), "/graphs/default")
# Allocate the RAM (in Mo) and set the name of the router, possible to allocate less memory but it will be very slow
# Be sure to have Java insalled
log <- otp_setup(otp, getwd(), memory = 20000, router = "default") # possible to change the port adding 'port = 8081' before the last bracket

# Connexion to OTP
otpcon <- otp_connect(hostname =  "localhost",
                      router = "default",
                      port = 8080)

# Function that give the walking time [s] from a point i to a point j
ok <- function(i, j){
  origin <- reseau$code_arret_theo[i]
  destination <- reseau$code_arret_theo[j]
  time <- otp_plan(otpcon, 
                   fromPlace = c(reseau$lon_arret[i], reseau$lat_arret[i]), 
                   toPlace = c(reseau$lon_arret[j], reseau$lat_arret[j]),
                   mode = "WALK",
                   get_geometry = F
  )$walkTime
  
  return(time)
}

# In case of error, a "time_error" message, NA is placed instead of the walking time
not_ok <- function(i, j){
  origin <- reseau$code_arret_theo[i]
  destination <- reseau$code_arret_theo[j]
  destination <<- destination
  if (origin != destination) {
    return(NA)
  }
  else{
    return(0)
  }
}

# Iterations (take a long time for all the network), stop i compared to the stop j
my_func = function(indices){
  i = indices[1]
  j = indices[2]
    tryCatch(
      {
        ok(i, j)
      },
      error = function(e){
        return(not_ok(i, j))
      },
      warning=function(w){
        return(not_ok(i, j))
      }
    )
}


n <- length(reseau$code_arret_theo)

# n<-100
indices_list = mapply(c, rep(1:n,n), rep(1:n, each=n), SIMPLIFY = F)
rep = mclapply(indices_list, my_func, mc.cores = 8)
# rep = lapply(indices_list, my_func)
time_mat <- matrix(unlist(rep),nrow = n, ncol = n)

colnames(time_mat) <- reseau$code_arret_theo

# time_mat : Matrice de vitesse de déplacement à pied (en secondes) entre arrêts. Axe x = destination et axe y = origine.
write.csv(time_mat, "time_mat.csv", row.names = FALSE)



######
# Tests with public transportation
test <- otp_plan(otpcon, 
         fromPlace = c(reseau$lon_arret[i], reseau$lat_arret[i]), 
         toPlace = c(reseau$lon_arret[820], reseau$lat_arret[820]),
         mode = c("TRANSIT", "WALK"),
         date_time = as.POSIXct(strptime("2019-06-03 13:30", "%Y-%m-%d %H:%M"))
)$duration[1]

otp_plan(otpcon, 
         fromPlace = c(reseau$lon_arret[3], reseau$lat_arret[3]), 
         toPlace = c(reseau$lon_arret[2], reseau$lat_arret[2]),
         mode = c("WALK"),
         date_time = as.POSIXct(strptime("2019-06-03 13:30", "%Y-%m-%d %H:%M"))
)$duration
