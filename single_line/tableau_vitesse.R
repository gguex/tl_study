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
from freq_theo;")

# To avoid to connect to the database
# write.csv(reseau,"reseau.csv", row.names = FALSE)
# setwd("/Users/rloup/Desktop")
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
ok <- function(){
  origin <- reseau$code_arret_theo[i]
  destination <- reseau$code_arret_theo[j]
  time <- otp_plan(otpcon, 
                   fromPlace = c(reseau$lon_arret[i], reseau$lat_arret[i]), 
                   toPlace = c(reseau$lon_arret[j], reseau$lat_arret[j]),
                   mode = "WALK"
  )$walkTime
  
  df <- data.frame(origin, destination, time)
  names(df) <- c("origin", "destination", "time")
  OD <- rbind(OD, df)
  OD <<- OD
}

# In case of error, a "time_error" message, NA is placed instead of the walking time
not_ok <- function(){
  origin <- reseau$code_arret_theo[i]
  destination <- reseau$code_arret_theo[j]
  if (origin != destination) {
    df <- data.frame(origin, destination, NA)
    names(df) <- c("origin", "destination", "time")
    OD <- rbind(OD, df)
    OD <<- OD
  }
  else{
    df <- data.frame(origin, destination, 0)
    names(df) <- c("origin", "destination", "time")
    OD <- rbind(OD, df)
    OD <<- OD
  }
}

# Initialisation of the origin-destination table
OD <- data.frame(matrix(ncol = 3, nrow = 0))
namesOD <- c("origin", "destination", "time")
colnames(OD) <- namesOD

# Iterations (take a long time for all the network), stop i compared to the stop j
i<-1
j<-2
while (i <= length(reseau$code_arret_theo)) {
  while (j <= length(reseau$code_arret_theo)) {
    tryCatch(
      {
        ok()
      },
      error = function(e){
        not_ok()
      },
      warning=function(w){
        print(paste("warning:", j, destination))
        not_ok()
      }
    )
    j<-j+1
  }
  j<-i
  i<-i+1
}

# write.csv(OD, "OD.csv", row.names = FALSE)


# Tests with public transportation
test <- otp_plan(otpcon, 
         fromPlace = c(reseau$lon_arret[i], reseau$lat_arret[i]), 
         toPlace = c(reseau$lon_arret[820], reseau$lat_arret[820]),
         mode = c("TRANSIT", "WALK"),
         date_time = as.POSIXct(strptime("2019-06-03 13:30", "%Y-%m-%d %H:%M"))
)$duration[1]

otp_plan(otpcon, 
         fromPlace = c(reseau$lon_arret[i], reseau$lat_arret[i]), 
         toPlace = c(reseau$lon_arret[820], reseau$lat_arret[820]),
         mode = c("WALK"),
         date_time = as.POSIXct(strptime("2019-06-03 13:30", "%Y-%m-%d %H:%M"))
)$duration
