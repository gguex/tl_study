############################################################### 
####### Setting paths

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# setwd("/Users/rloup/Desktop")

# Command lines to cut a pbf file -->
#   osmconvert /Users/rloup/Desktop/switzerland-latest.osm.pbf -b=6.48008,46.48580,6.89584,46.72089 --complete-ways -o=lausanne.pbf

####### Libraries
library("opentripplanner")
library(tidyverse) # Data filter


# path_data <- file.path(tempdir(), "OTP")
# dir.create(path_data)

path_data <- file.path("/otp")
# dir.create(path_data)

path_otp <- file.path("/otp/otp-2.0.0-shaded.jar")

# path_otp <- otp_dl_jar(path_data, cache = FALSE)


# Demo
otp_dl_demo(path_data)
log1 <- otp_build_graph(otp = path_otp, dir = path_data)
log2 <- otp_setup(otp = path_otp, dir = path_data)

log1 <- otp_build_graph(otp = "/Users/rloup/Documents/r_projects/tl_study/data_processing/otp/otp-2.0.0-shaded.jar", dir = "/Users/rloup/Documents/r_projects/tl_study/data_processing/otp", memory = 30000)


log2 <- otp_setup(otp = "/Users/rloup/Documents/r_projects/tl_study/data_processing/otp/otp-2.0.0-shaded.jar", dir = "/Users/rloup/Documents/r_projects/tl_study/data_processing/otp", memory = 16000, port = 8080)
# log2 <- otp_setup(otp = "otp-2.0.0-shaded.jar", dir = "/Users/rloup/otp2", memory = 30000, port = 8080, router = "default")


otpcon <- otp_connect(hostname =  "localhost",
                      router = "default",
                      port = 8080)

otp_plan(otpcon,
         fromPlace = c(6.62801,46.52025),
         toPlace = c(6.633971156608949, 46.522829744515576),
         mode = c("WALK"),
         # maxWalkDistance = 1000,
         # date_time = as.POSIXct(strptime("2019-11-14 13:02", "%Y-%m-%d %H:%M")),
         numItineraries = 1,
         # fromID = "hello",
         # toID = "ciao",
         # maxWalkDistance = 5
         get_geometry = FALSE
)$duration


###################
df <- read.csv2("stop_frequentation.csv")

# Select some lines
df <- filter(df,code_ligne_theo==6|code_ligne_theo==7|code_ligne_theo==8|code_ligne_theo==9)

# Create empty distance matrix nxn with the right n
a <- matrix(nrow = length(df$code_arret_theo), ncol = length(df$code_arret_theo))

# Create empty list
l <- list(c())
# Push all stop codes in the list
i <- 1
while(i <= length(df$code_arret_theo)) {
  l[[i]] <- df$code_arret_theo[i]
  i <- i + 1
}
colnames(a) <- l
rownames(a) <- l


# Fill the matrix with the transit walk speed between stops
i <- 1
j <- 1
while (j <= length(df$code_arret_theo)) {
  while (i <= length(df$code_arret_theo)) {
    # function that show the distance between two lon lat points
    if (c(df$lat[i], df$lon[i]) == c(df$lat[i], df$lon[i])) {
      a[i,j] <- 0
    }
    if (c(df$lat[i], df$lon[i]) != c(df$lat[j], df$lon[j])) {
      a[i,j] <- otp_plan(otpcon,
                         fromPlace = c(df$lon[i], df$lat[i]),
                         toPlace = c(df$lon[j], df$lat[j]),
                         mode = c("WALK"),
                         numItineraries = 1,
                         maxWalkDistance = 1000,
                         get_geometry = FALSE,
                         get_elevation = FALSE
      )$duration[1]
    }
    i <- i + 1
  }
  i <- 1
  j <- j + 1
}


1

otp_plan(otpcon, 
         fromPlace = c(reseau$lon_arret[i], reseau$lat_arret[i]), 
         toPlace = c(reseau$lon_arret[j], reseau$lat_arret[j]),
         mode = c("WALK"),
         date_time = as.POSIXct(strptime("2019-06-03 13:30", "%Y-%m-%d %H:%M")),
         numItineraries = 1,
         get_geometry = FALSE
)$duration[1]

otp_plan(otpcon,
         fromPlace = c(df$lon[i], df$lat[i]),
         toPlace = c(df$lon[j], df$lat[j]),
         mode = c("WALK"),
         date_time = as.POSIXct(strptime("2019-06-03 13:30", "%Y-%m-%d %H:%M")),
         
         # numItineraries = 1,
         # maxWalkDistance = 1000,
         get_geometry = FALSE,
         # get_elevation = FALSE
)$duration[1]







































otp_build_graph(
  otp = otp-2.1.0-SNAPSHOT-shaded.jar,
  dir = NULL,
  memory = 2048,
  router = "default",
  analyst = FALSE
)

log <- otp_build_graph(otp = "otp-2.1.0-SNAPSHOT-shaded.jar", dir = "./data",router = "default")
setwd("/Users/rloup/Desktop/otp")




setwd("/Users/rloup/Documents/otpServ")

# Set the server
otp = paste0(getwd(), "/otp-2.1.0-SNAPSHOT-shaded.jar")
graphdir <- paste0(getwd(), "/graphs/default")
# Allocate the RAM (in Mo) and set the name of the router, possible to allocate less memory but it will be very slow
# Be sure to have Java insalled
otp_setup(otp, getwd(), memory = 8000,port = 8080) # possible to change the port adding 'port = 8081' before the last bracket











setwd("/Users/rloup/Documents/otpServ")
# Set the server
otp = paste0(getwd(), "/otp-2.1.0-SNAPSHOT-shaded.jar")
graphdir <- paste0(getwd(), "/graphs/default")
# Allocate the RAM (in Mo) and set the name of the router, possible to allocate less memory but it will be very slow
# Be sure to have Java insalled
log <- otp_setup(otp, getwd(), memory = 20000, router = "default",port = 8081) # possible to change the port adding 'port = 8081' before the last bracket

# Connexion to OTP
otpcon <- otp_connect(hostname =  "localhost",
                      router = "default",
                      port = 8081)










##################################################################
##################################################################


# test 28.09.2021
library(opentripplanner)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
path_data <- file.path(dirname(rstudioapi::getActiveDocumentContext()$path), "otp")

path_otp <- file.path(dirname(rstudioapi::getActiveDocumentContext()$path), "otp/otp-2.1.0-SNAPSHOT-shaded.jar")


# log1 <- otp_build_graph(otp = path_otp, dir = path_data, memory = 20240) 


otpcon <- otp_connect(hostname =  "localhost",
                      router = "default",
                      port = 8080)

route3 <- otp_plan(otpcon, 
                  fromPlace = c(-1.17502, 50.64590), 
                  toPlace = c(-1.15339, 50.72266))



##################################################################

# test 28.09.2021 vraies données
library(opentripplanner)

# Access path
path_data <- file.path("/Users/rloup", "otp3")
path_otp <- "/Users/rloup/otp3/otp-2.0.0-shaded.jar"

# Building graph.obj
log1 <- otp_build_graph(otp = path_otp, dir = path_data, memory = 30000) 

# Start up OTP server
log2 <- otp_setup(otp = path_otp, dir = path_data)

# OTP connexion
otpcon <- otp_connect()
otpcon <- otp_connect(hostname =  "localhost",
                      # router = "default",
                      port = 8080)




# test duration
otp_plan(otpcon,
         fromPlace = c(6.62801,46.52025),
         toPlace = c(6.65529,46.53276))

test <- otp_plan(otpcon,
                 fromPlace = c(6.62801,46.52025),
                 toPlace = c(6.65529,46.53276),
                 mode = c("WALK","BUS"),
                 # maxWalkDistance = 1000,
                 date_time = as.POSIXct(strptime("2019-10-04 13:30", "%Y-%m-%d %H:%M"))
                 # numItineraries = 2,
                 # fromID = "hello",
                 # toID = "ciao",
                 # maxWalkDistance = 500
                 # get_geometry = FALSE
)


test <- otp_plan(otpcon,
         fromPlace = c(6.62801,46.52025),
         toPlace = c(6.65529,46.53276),
         mode = c("WALK"),
         # maxWalkDistance = 1000,
         date_time = as.POSIXct(strptime("2019-11-14 13:02", "%Y-%m-%d %H:%M")),
         numItineraries = 1,
         # fromID = "hello",
         # toID = "ciao",
         maxWalkDistance = 5
         # get_geometry = FALSE
)
# http://localhost:8080/otp/routers/default/plan?fromPlace=46.52025,6.62801&toPlace=46.53276,6.65529&time=1:02pm&date=11-14-2019&mode=TRANSIT,WALK&maxWalkDistance=500&arriveBy=false
plot(test$geometry)

# tmap
library(tmap)
tmap_mode("view")
qtm(sf::st_zm(test), lines.lwd = 3, 
    lines.col = "mode")   

# Database connexion
library("RPostgreSQL")
# Connexion à la base de données
conn <- dbConnect("PostgreSQL", dbname = 'tl_small', host = 'localhost', user = 'rloup', password = '')

library("sf")
# Network with stops
reseau <- st_read(conn, query="
select *
from stop_frequentation_tot")

# Extract geometries
library(tidyverse)
reseau_coord <- reseau %>%
  mutate(lat = unlist(map(reseau$geom,1)),
         long = unlist(map(reseau$geom,2)))

# Select some lines
reseau_coord_saved <- reseau_coord
reseau_coord <- filter(reseau_coord,code_ligne_theo==6|code_ligne_theo==7|code_ligne_theo==8|code_ligne_theo==9)


# Create empty distance matrix nxn with the right n
a <- matrix(nrow = length(reseau_coord$code_arret_theo), ncol = length(reseau_coord$code_arret_theo))

# Create empty list
l <- list(c())
# Push all stop codes in the list
i <- 1
while(i <= length(reseau_coord$code_arret_theo)) {
  l[[i]] <- reseau_coord$code_arret_theo[i]
  i <- i + 1
}
colnames(a) <- l
rownames(a) <- l


# Fill the matrix with the transit walk speed between stops
i <- 1
j <- 1
while (j <= length(reseau_coord$code_arret_theo)) {
  while (i <= length(reseau_coord$code_arret_theo)) {
    # function that show the distance between two lon lat points
    if (c(reseau_coord$lat[i], reseau_coord$lon[i]) == c(reseau_coord$lat[i], reseau_coord$lon[i])) {
      a[i,j] <- 0
    }
    if (c(reseau_coord$lat[i], reseau_coord$lon[i]) != c(reseau_coord$lat[j], reseau_coord$lon[j])) {
      a[i,j] <- otp_plan(otpcon,
                         fromPlace = c(reseau_coord$lat[i], reseau_coord$lon[i]),
                         toPlace = c(reseau_coord$lat[j], reseau_coord$lon[j]),
                         mode = c("TRANSIT", "WALK"),
                         # date_time = as.POSIXct(strptime("2019-06-03 13:30", "%Y-%m-%d %H:%M")),
                         numItineraries = 1,
                         get_geometry = FALSE
      )$duration[1]
      }
    i <- i + 1
  }
  i <- 1
  j <- j + 1
}


# Save data
# Origine = ligne, destination = colonne
setwd("/Users/rloup/Desktop")
# write.csv(a, "time_mat_s_6789_names.csv")


a <- read.csv("/Users/rloup/Desktop/time_mat_s_6789.csv", sep = ",")
tps_stop <- read.csv("/Users/rloup/switchdrive/theseRomain/data/tps_attente_arret2.csv", sep = ",")
tps_stop <- filter(tps_stop,code_ligne_theo==6|code_ligne_theo==7|code_ligne_theo==8|code_ligne_theo==9)

# Compute avg eaiting time at a stop
tps_stop$tps_attente <- round(tps_stop$avg.1 - tps_stop$avg)

# Put the waiting time instead of the 0 diagonal
i <- 1
while(i <= dim(a)[1]) {
  a[i,i] <- tps_stop$tps_attente[i]
  i <- i+1
}
write.csv(a, "/Users/rloup/switchdrive/theseRomain/data/time_mat_s_6789_diag_avg.csv")
















i <- 50
j <- 320


a <- otp_plan(otpcon,
             fromPlace = c(reseau_coord$lat[i], reseau_coord$lon[i]),
             toPlace = c(reseau_coord$lat[j], reseau_coord$lon[j]),
             mode = c('WALK','BUS','SUBWAY'),
             # date_time = as.POSIXct(strptime("2019-09-04 14:50", "%Y-%m-%d %H:%M")),
             date_time = as.POSIXct(strptime("2019-10-04 13:30", "%Y-%m-%d %H:%M")),
             # date_time = Sys.time()-80000000,
             numItineraries = 10,
             maxWalkDistance = 500
             # get_geometry = FALSE
)
a2 <- filter(a,route_option==4)

qtm(sf::st_zm(a2), lines.lwd = 3, 
    lines.col = "mode")  

a <- otp_plan(otpcon,
                 fromPlace = c(reseau_coord$lat[i], reseau_coord$lon[i]),
                 toPlace = c(reseau_coord$lat[j], reseau_coord$lon[j]),
                 mode = c("WALK","BUS"),
                 # maxWalkDistance = 1000,
                 date_time = as.POSIXct(strptime("2019-11-14 13:02", "%Y-%m-%d %H:%M")),
                 numItineraries = 1,
                 fromID = reseau_coord$libelle_arret_theo2[i],
                 toID = reseau_coord$libelle_arret_theo2[j],
                 maxWalkDistance = 5
                 # get_geometry = FALSE
)
# tmap_mode("view")
qtm(sf::st_zm(a), lines.lwd = 3, 
    lines.col = "mode")   

routeOptions <- otp_route_options
otp_routing_options()

http://localhost:8080/?module=planner&fromPlace=46.544221875806294%2C6.595916748046874&toPlace=46.53146906088904%2C6.650848388671875&time=2%3A22pm&date=10-04-2019&mode=TRANSIT%2CWALK&maxWalkDistance=4828.032&arriveBy=false&wheelchair=false&debugItineraryFilter=false&locale=en&itinIndex=0

http://localhost:8080/?module=planner&fromPlace=46.544221875806294%2C6.595916748046874&toPlace=46.53146906088904%2C6.650848388671875&time=2%3A22pm&date=10-04-2019&mode=TRANSIT%2CWALK&maxWalkDistance=4828.032&arriveBy=false&wheelchair=false&debugItineraryFilter=false&locale=en&itinIndex=0
