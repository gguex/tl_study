#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Transfer visualization
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# - Map important nodes

#--------------------------------
# Head
#--------------------------------


# Libraries
#--------------------------------
library(dplyr)
library(leaflet)
library(BAMMtools) # Jenks library

# The folder containing results
results_folder = "results/all_lines2"

#--------------------------------
# Process
#--------------------------------

# --- Reading files
transfer_df = read.csv(paste0(results_folder, "/transfer_df.csv"))
coord = read.csv2("utilities/data_processing/stop_frequentation2.csv")

# --- Adaptation of coord
coord$id = paste0("S", coord$code_ligne_theo,"_", coord$direction_voy_theo, "_",
                  coord$code_arret_theo)
coord = coord[,c(11, 9, 10)]

# Join i
transfer_df <- left_join(
  transfer_df,
  coord,
  by = c("tag_i" = "id")
  )
colnames(transfer_df)[12] = "lon_i"
colnames(transfer_df)[13] = "lat_i"

# Join j
transfer_df <- left_join(
  transfer_df,
  coord,
  by = c("tag_j" = "id")
)
colnames(transfer_df)[14] = "lon_j"
colnames(transfer_df)[15] = "lat_j"

transfer_df$mean_lon = (transfer_df$lon_i + transfer_df$lon_j)/2
transfer_df$mean_lat = (transfer_df$lat_i + transfer_df$lat_j)/2


# Mapping

# Color palette natural Jenks threshold
# colorPal = colorNumeric(palette = "Reds",
#                         domain = transfer_df$number_of_transfers)
colorPal = colorBin("Reds", transfer_df$number_of_transfers,
                    bins = getJenksBreaks(transfer_df$number_of_transfers, 7,
                                          subset = NULL))

# Map results
nodes_map = leaflet() %>%
  addProviderTiles(providers$CartoDB.DarkMatter) %>%
  setView(lat=46.54033, lng=6.63575 , zoom=13) %>%
  addCircles(lng = transfer_df$mean_lon, lat = transfer_df$mean_lat,
             radius = transfer_df$number_of_transfers/3000,
             color = colorPal(transfer_df$number_of_transfers),
             fillColor = "transparent",
             opacity = 1
  )

nodes_map

# # Flow map
# library(leaflet.minicharts)
# 
# leaflet() %>%
#   addProviderTiles(providers$CartoDB.DarkMatter) %>%
#   setView(lat=46.54033, lng=6.63575 , zoom=13) %>%
#   addFlows(
#     lng0 = transfer_df$lon_i,
#     lat0 = transfer_df$lat_i,
#     lng1 = transfer_df$lon_j,
#     lat1 = transfer_df$lat_j,
#     flow = transfer_df$number_of_transfers,
#     color = colorPal(transfer_df$number_of_transfers),
#     minThickness = 2,
#     maxThickness = 15
#   )
