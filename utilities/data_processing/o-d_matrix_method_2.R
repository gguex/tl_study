###############################################################
##    Calcul of a O-D matrix from TL's stops coordinates     ##
###############################################################
#######            Source of th process                 #######
# https://cran.r-project.org/web/packages/opentripplanner/vignettes/advanced_features.html

####### Setting paths
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

####### Libraries
library("opentripplanner")
library("sf")

####### Paths to OTP
path_data <- file.path("otp")
path_otp <- file.path("otp/otp-2.0.0-shaded.jar")

####### Run OTP
# Run the server
log2 <- otp_setup(otp = path_otp, dir = path_data, memory = 32000, wait = FALSE)

####### Read spatial feature, TL stops
df <- st_read("sf_data/df.shp")
df$code_id <- paste0(df$cd_lgn_,"_",df$drctn__,"_",df$cd_rrt_)

####### Create matrix
toPlace   = df[rep(seq(1, nrow(df)), times = nrow(df)),]
fromPlace = df[rep(seq(1, nrow(df)), each  = nrow(df)),]

## Sample ton test
# toPlace <- head(toPlace, 1000)
# fromPlace <- head(fromPlace, 1000)


####### Routing (/!\ take a long time)
routes <- otp_plan(otpcon = otpcon,
                   fromPlace = fromPlace,
                   toPlace = toPlace,
                   fromID = fromPlace$code_id,
                   toID = toPlace$code_id,
                   get_geometry = FALSE,
                   mode = c("WALK"),
                   numItineraries = 1,
                   maxWalkDistance = 10000,
                   ncores = 3)

routes <- routes[,c("fromPlace","toPlace","duration")]
# Use the tidyr package to go from long to wide format

routes_matrix <- tidyr::pivot_wider(routes, 
                                    names_from = "toPlace", 
                                    values_from = "duration")
## Save matrix
write.csv(routes_matrix, "routes_matrix3.csv",row.names = FALSE)
