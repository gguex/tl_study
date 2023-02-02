###############################################################
##    Calcul of a O-D matrix from TL's stops coordinates     ##
###############################################################

####### Setting paths
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

####### Libraries
library("opentripplanner")

####### Paths to OTP
path_data <- file.path("../../../otp")
path_otp <- file.path("../../../otp/otp-2.0.0-shaded.jar")

####### Run OTP

# Build graph (only one time)
# log1 <- otp_build_graph(otp = path_otp, dir = path_data)

# Run the server
log2 <- otp_setup(otp = path_otp, dir = path_data, memory = 30000, wait = FALSE)

# Create a personal connexion to the router (optional)
otpcon <- otp_connect(hostname =  "localhost",
                      router = "default",
                      port = 8080)

# Read origin-destination file
df <- read.csv2("stop_frequentation.csv")

##### Apply function #####
# Fill the matrix with the transit walk speed between stops

# set function to calculate the time between stops
function_route <- function(j) {
  if (df$lat[i] == df$lat[j] & df$lon[i] == df$lon[j]) {
    res <- 0
  } else {
    res <- otp_plan(otp_connect(),
                    fromPlace = c(df$lon[i], df$lat[i]),
                    toPlace = c(df$lon[j], df$lat[j]),
                    mode = c("WALK"),
                    numItineraries = 1,
                    # ncores = 3,
                    maxWalkDistance = 50000,
                    get_geometry = FALSE,
                    get_elevation = FALSE
    )$duration[1]
  }
  return(res)
}

# create n*n stops matrix
x <- matrix(data = NA, nrow = dim(df)[1], ncol = dim(df)[1])

# read intermediate state file (i=390 here)
# x <- read.csv("/Users/rloup/Documents/tl_study_copy/data_processing/i390_walking_time.csv")

# fill the matrix per column !!take a very very long time!!
for (i in 1:dim(df)[1]){
  x[,i] <- mapply(function_route, 1:dim(df)[1])
  i <- i + 1
}

# Save data
write.csv(x, "stop_walking_time.csv", row.names = FALSE)