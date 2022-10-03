#-------------------------------------------------------------------------------
#
# Display the results in leaflet
#
#-------------------------------------------------------------------------------

#--------------------------------
# Head
#--------------------------------

# Set working directory path
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load packages 
library("sf")
library("dplyr")
library("leaflet")

# Load functions
source("local_functions.R")

#--------------------------------
# Parameters
#--------------------------------

# The folder containing pre-processed data
data_folder = "multilines_data/preprocessed_data/test_6789"
# The folder containing results
results_folder = "results/test_6789"

#--------------------------------
# Process
#--------------------------------


# --- Reading files

# Reading results data 
line_res = read.csv(paste0(results_folder, "/line_res.csv")) 
n_mat = as.matrix(read.csv(paste0(results_folder, "/n_mat.csv"), header=F))
stop_names = line_res$stop_names
# Load edge_ref
edge_ref = as.matrix(read.csv(paste0(data_folder, "/edge_ref.csv")))
# Load sp_ref
sp_ref = as.matrix(read.csv(paste0(data_folder, "/sp_ref.csv")))
# Load p_mat
p_mat = as.matrix(read.csv(paste0(data_folder, "/p_mat.csv"), header=F))
# Reading shapefiles
lines_shp = st_read("multilines_data/shapefiles/frequentation_lignes_tot.shp")
stops_shp = st_read("multilines_data/shapefiles/frequentation_arrets_tot.shp")


# --- Selecting pertinent lines and stops

# Creating stop_names for current stops
current_stop_names = trimws(paste(paste0("S", lines_shp$code_ligne), 
                                  lines_shp$direction_, 
                                  lines_shp$code_arret, sep="_"))
# Corrected direction for line 7
direction_corrected = lines_shp$direction_
direction_corrected[(lines_shp$code_ligne == "7") & 
                      (lines_shp$direction_ == "R") &
                      (lines_shp$code_arret == "BESS_S")] = "A"
# Creating stop_names for next stops
next_stop_names = trimws(paste(paste0("S", lines_shp$code_ligne), 
                               direction_corrected, 
                               lines_shp$destinatio, sep="_"))
# Creating stop_names for stops
s_stop_names = trimws(paste(paste0("S", stops_shp$code_ligne), 
                            stops_shp$direction_, 
                            stops_shp$code_arret, sep="_"))

# Filtering shapes with known stops names 
lines_sel_shp = lines_shp %>%
  mutate(current_stop_names=current_stop_names, 
         next_stop_names=next_stop_names) %>%
  filter(current_stop_names %in% stop_names, 
         next_stop_names %in% stop_names)
stops_sel_shp = stops_shp %>%
  mutate(s_stop_names=s_stop_names) %>%
  filter(s_stop_names %in% stop_names) %>%
  arrange(match(s_stop_names, stop_names))


# --- Creating big shapefiles from results  

# Id for the starting stop
id_stop = 1

# Getting the ingoing and outgoing flow for each stops
init_stop = stop_names[id_stop]
passengers = n_mat[id_stop, ]
passengers[id_stop] = mean(passengers) * 2
stops_i_shp = stops_sel_shp %>%
  mutate(init_stop=init_stop, passengers=as.vector(passengers))
stops_i_shp$color[id_stop] = "red"

# Getting the flow on edges
n_i_mat = n_mat
n_i_mat[!(1:dim(n_mat)[1] %in% id_stop),] = 0
x_i_mat = compute_x_from_n(n_i_mat, edge_ref, sp_ref, p_mat)$x_mat
edge_flow = mapply(function(i, j) x_i_mat[which(stop_names == i), 
                                          which(stop_names == j)],
                   lines_sel_shp$current_stop_names, 
                   lines_sel_shp$next_stop_names)
lines_i_shp = lines_sel_shp %>%
  mutate(init_stop = init_stop, edge_flow=as.vector(edge_flow))

# --- Mapping
labels = paste0("Arrêt : ",stops_i_shp$libelle_ar, "<br />","Passagers : ", round(stops_i_shp$passengers))
line_map = leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView( lat=46.54033, lng=6.63575 , zoom=12) %>%
  addCircles(data = stops_i_shp,
             weight = 0.5,
             radius = (stops_i_shp$passengers/max(stops_i_shp$passengers)*60000)^0.45, # coefficient de correction avec un minimum de largeur de 1
             fillOpacity = 0.7,
             color = stops_i_shp$color, # couleur de chaque ligne tl
             highlight = highlightOptions(
               # radius = poids,
               weight = 1,
               color = "white",
               fillOpacity = 0.8,
               bringToFront = TRUE),
             label = paste0("Ligne n° ",stops_i_shp$code_ligne), # avec le hover
             popup = labels,
             labelOptions = labelOptions(
               style = list("font-weight" = "normal", padding = "3px 8px"),
               textsize = "15px",
               direction = "auto"),
             group = as.character(lines_i_shp$code_ligne) # groupes séparés par ligne
  ) %>%
  addPolylines(data=lines_i_shp,
               layerId = lines_i_shp$code_ligne,
               weight = (lines_i_shp$edge_flow/max(lines_i_shp$edge_flow)*50)^0.57+0.05, # coefficient de correction avec un minimum de largeur de 1
               opacity = 1,
               color = lines_i_shp$color, # couleur de chaque ligne tl
               highlight = highlightOptions(
                 weight = 3,
                 color = "black",
                 fillOpacity = 0.7,
                 bringToFront = TRUE),
               label = paste0("Ligne n° ",lines_i_shp$code_ligne), # avec le hover
               popup = labels,
               labelOptions = labelOptions(
                 style = list("font-weight" = "normal", padding = "3px 8px"),
                 textsize = "15px",
                 direction = "auto"),
               group = as.character(lines_i_shp$code_ligne) # groupes séparés par ligne
               
  ) %>%
  addLayersControl(
    overlayGroups = lines_i_shp$code_ligne, # groupes séparés par ligne
    options = layersControlOptions(collapsed = FALSE)
  )
line_map

# --- Transfert table
line_res_small = line_res[, c("line_nbr", "direction", "label", "transferts_in", "transferts_out")]

# Only not null transferts edges
line_res_small_NN = line_res_small[rowSums(line_res_small[,4:5]) > 0, ]
