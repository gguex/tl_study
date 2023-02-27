setwd("/Users/rloup/Documents/r_projects/tl_study")
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Transfer visualisation
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
aggregate_transfers_df = read.csv("results/all_lines2/aggregate_transfers_df.csv")
coord = read.csv2("utilities/data_processing/stop_frequentation2.csv")

# --- Adaptation of coord
coord$id = paste0("S", coord$code_ligne_theo,"_", coord$direction_voy_theo, "_",
                  coord$code_arret_theo)
coord$id <- gsub("\\s+", "", coord$id)
coord = coord[,c(11, 4, 9, 10)]
colnames(coord)[2] = "name"

# Mean of coord
coord_mean <- aggregate(cbind(lon, lat) ~ name, data = coord, FUN = mean)

# Join data
aggregate_transfers_df <- left_join(
  aggregate_transfers_df,
  coord_mean,
  by = c("stop_i" = "name")
)


# Color palette from white to red
colorPal = colorBin("Reds", aggregate_transfers_df$sum.number_of_transfers.,
                    bins = getJenksBreaks(
                      aggregate_transfers_df$sum.number_of_transfers., 
                      9, subset = NULL))
# Map results
nodes_map = leaflet() %>%
  addProviderTiles(providers$CartoDB.DarkMatter) %>%
  setView(lat=46.54033, lng=6.63575 , zoom=13) %>%
  addCircles(lng = aggregate_transfers_df$lon, lat = aggregate_transfers_df$lat,
             radius = aggregate_transfers_df$sum.number_of_transfers./15000,
             color = colorPal(aggregate_transfers_df$sum.number_of_transfers.),
             fillOpacity = 1,
             opacity = 1
  )
nodes_map
