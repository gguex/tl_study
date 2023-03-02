#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Correspondence Analysis (CA)
# Visualisation output
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# - Map CA

#--------------------------------
# Head
#--------------------------------

# Libraries
#--------------------------------
library(dplyr)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(FactoMineR)
library(sf)
library(RColorBrewer)
library(leaflet)

# The folder containing results
results_folder = "results/all_lines"

#--------------------------------
# Process
#--------------------------------

# Choose "up" or "down" to set visualisation
up_down = "up"

# --- Reading files

# Reading results data 
line_res = read.csv(paste0(results_folder, "/line_res.csv")) 
n_mat = as.matrix(read.csv(paste0(results_folder, "/n_mat.csv"), header=F))
ped_time = read.csv(as.matrix("multilines_data/formatted_data/all_lines/ped_time.csv"), header=F)
stop_names = line_res$stop_names

colnames(n_mat) = stop_names
rownames(n_mat) = stop_names

# Visu 2 (not used)
# access = function(x){
#   if (x>120) {
#     res = 0
#   }
#   else{
#     res = 1
#   }
# }
# ped_acc <- apply(ped_time, MARGIN = c(1, 2), FUN = access)
# ANA = ped_acc %*% n_mat %*% ped_acc

# Compute CA
res.ca <- CA(n_mat, graph = FALSE)

# Dim 1 and 2 according to up_down
if (up_down == "up") {
  CA_dim = res.ca$row$coord[, 1:2]
}
if (up_down == "down") {
  CA_dim = res.ca$col$coord[, 1:2]
}
colnames(CA_dim) <- c("dim1","dim2")
CA_dim = as.data.frame(CA_dim)
CA_dim$stop_names = rownames(CA_dim)

# Compute dim 1
q1 <- quantile(CA_dim$dim1, 0.10)
q2 <- quantile(CA_dim$dim1, 0.90)

for (i in 1:dim(CA_dim)[1]) {
  if (CA_dim$dim1[i] > q2) {
    CA_dim$dim1[i] = q2
  }
  if (CA_dim$dim1[i] < q1) {
    CA_dim$dim1[i] = q1
  }
}

# Compute dim 2
q1_2 <- quantile(CA_dim$dim2, 0.10)
q2_2 <- quantile(CA_dim$dim2, 0.90)

for (i in 1:dim(CA_dim)[1]) {
  if (CA_dim$dim2[i] > q2_2) {
    CA_dim$dim2[i] = q2_2
  }
  if (CA_dim$dim2[i] < q1_2) {
    CA_dim$dim2[i] = q1_2
  }
}


# Basic scatter plot
ggplot(CA_dim, aes(x=dim1, y=dim2)) + 
  geom_point() + 
  geom_text(label=rownames(CA_dim))


# Reading shapefiles
lines_shp = st_read("multilines_data/shapefiles/frequentation_lignes_tot.shp")
stops_shp = st_read("multilines_data/shapefiles/frequentation_arrets_tot.shp")

# Creating stop_names for current stops
lines_shp$stop_names = trimws(paste(paste0("S", lines_shp$code_ligne), 
                                    lines_shp$direction_, 
                                    lines_shp$code_arret, sep="_"))
stops_shp$stop_names = trimws(paste(paste0("S", stops_shp$code_ligne), 
                                    stops_shp$direction_, 
                                    stops_shp$code_arret, sep="_"))

# Join data and shapefiles
CA_shp = right_join(stops_shp, CA_dim, by = "stop_names")

lines_shp_sample = right_join(lines_shp, CA_dim, by = "stop_names")

# Radius of the proportional circles
if (up_down == "up") {
  CA_shp$CA_radius = CA_shp$montees
}

if (up_down == "down") {
  CA_shp$CA_radius = CA_shp$descentes
}


# --- Mapping results

# Color palette for 2 dimensions
colorPal1 = colorNumeric(palette = "RdBu", domain = CA_shp$dim1)
colorPal2 = colorNumeric(palette = "RdBu", domain = CA_shp$dim2)

CA_map2 = leaflet(CA_shp) %>%
  addProviderTiles(providers$CartoDB.DarkMatter) %>%
  setView( lat=46.54033, lng=6.63575 , zoom=13) %>%
  addPolylines(data=lines_shp_sample,
               weight = 1,
               color = "#eeeeee") %>%
  addCircles(
    group = "dim1",
    # radius = ((CA_shp$montees)/max(CA_shp$montees)*100000)^0.45,
    radius = ((CA_shp$CA_radius)/max(CA_shp$CA_radius)*100000)^0.45, # corr coef
    fillColor = "transparent",
    opacity = ((CA_shp$CA_radius)/max(CA_shp$CA_radius))^0.05,
    color = ~colorPal1(dim1),
    highlight = highlightOptions(
      color = "white",
      bringToFront = TRUE),
    label = paste0("Dim 1: ",CA_shp$dim1), # avec le hover
  ) %>%
  addCircles(
    group = "dim2",
    # radius = ((CA_shp$montees)/max(CA_shp$montees)*100000)^0.45,
    radius = ((CA_shp$CA_radius)/max(CA_shp$CA_radius)*100000)^0.45, # corr coef
    fillColor = "transparent",
    opacity = ((CA_shp$CA_radius)/max(CA_shp$CA_radius))^0.05,
    color = ~colorPal2(dim2),
    highlight = highlightOptions(
      color = "white",
      bringToFront = TRUE),
    label = paste0("Dim 2: ",CA_shp$dim2), # mouse hover
  ) %>%
  addLayersControl(
    baseGroups = c("dim1", "dim2"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addLegend(
    group = "dim1",
    pal = colorPal1,
    # title = "AFC dim.1",
    opacity = 1,
    values = ~dim1
  ) %>%
  addLegend(
    group = "dim2",
    pal = colorPal2,
    # title = "AFC dim.2",
    opacity = 1,
    values = ~dim2
  ) %>%
  htmlwidgets::onRender("
    function(el, x) {
      var updateLegend = function () {
          var selectedGroup = document.querySelectorAll('input:checked')[0].nextSibling.innerText.substr(1);

          document.querySelectorAll('.legend').forEach(a => a.hidden=true);
          document.querySelectorAll('.legend').forEach(l => {
            if (l.children[0].children[0].innerText == selectedGroup) l.hidden=false;
          });
      };
      updateLegend();
      this.on('baselayerchange', e => updateLegend());
    }")
CA_map2
