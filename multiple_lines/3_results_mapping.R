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

#--------------------------------
# Parameters
#--------------------------------

# The folder containing results
results_folder = "results/test_6789"

#--------------------------------
# Process
#--------------------------------

# Reading results matrices 
n_mat = 
# Reading shapefiles
lines_shp = st_read("multilines_data/shapefiles/frequentation_lignes_tot.shp")
stops_shp = st_read("multilines_data/shapefiles/frequentation_arrets_tot.shp")

# --- Pre-processing 

# Selecting only lines 6 to 9
lines_sel_shp = lines_shp %>%
  filter(code_ligne %in% 6:9)
stops_sel_shp = stops_shp %>%
  filter(code_ligne %in% 6:9)

# --- Mapping

line_map = leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView( lat=46.54033, lng=6.63575 , zoom=12) %>%
  addPolylines(data=lines_shp,
               layerId = lines_shp$code_ligne,
               weight = (lines_shp$montees/max(lines_shp$montees)*50)^0.57+0.75, # coefficient de correction avec un minimum de largeur de 1
               opacity = 1,
               color = lines_shp$color, # couleur de chaque ligne tl
               highlight = highlightOptions(
                 weight = 3,
                 color = "black",
                 fillOpacity = 0.7,
                 bringToFront = TRUE),
               label = paste0("Ligne n° ",lines_shp$code_ligne), # avec le hover
               popup = labels,
               labelOptions = labelOptions(
                 style = list("font-weight" = "normal", padding = "3px 8px"),
                 textsize = "15px",
                 direction = "auto"),
               group = as.character(lines_shp$code_ligne) # groupes séparés par ligne
               
  ) %>%
  addLayersControl(
    overlayGroups = lines_shp$code_ligne, # groupes séparés par ligne
    options = layersControlOptions(collapsed = FALSE)
  )
line_map


# Carte cercles (ouvrir dans un navigateur)
poids <- (freq_a$montees/max(freq_a$montees)*50000)^0.57

m3 <- leaflet(data = freq_a) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView( lat=46.54033, lng=6.63575 , zoom=12) %>%
  addCircles(layerId = freq_a$code_ligne,
             weight = 0.5,
             radius = poids, # coefficient de correction avec un minimum de largeur de 1
             fillOpacity = 0.7,
             color = freq_a$color, # couleur de chaque ligne tl
             
             highlight = highlightOptions(
               # radius = poids,
               weight = 1,
               color = "white",
               fillOpacity = 0.8,
               bringToFront = TRUE),
             label = paste0("Ligne n° ",freq_a$code_ligne), # avec le hover
             popup = labels,
             labelOptions = labelOptions(
               style = list("font-weight" = "normal", padding = "3px 8px"),
               textsize = "15px",
               direction = "auto"),
             group = as.character(freq_a$code_ligne) # groupes séparés par ligne
  ) %>%
  addLayersControl(
    overlayGroups = l, # groupes séparés par ligne
    options = layersControlOptions(collapsed = FALSE)
  )
m3

# Carte cercles + lignes (ouvrir dans un navigateur)
l2 <- as.character(paste(freq_a$direction_,freq_a$code_ligne)) # groupe
m4 <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView( lat=46.54033, lng=6.63575 , zoom=12) %>%
  addCircles(data = freq_a,
             weight = 0.5,
             radius = poids, # coefficient de correction avec un minimum de largeur de 1
             fillOpacity = 0.7,
             color = freq_a$color, # couleur de chaque ligne tl
             highlight = highlightOptions(
               # radius = poids,
               weight = 1,
               color = "white",
               fillOpacity = 0.8,
               bringToFront = TRUE),
             label = paste0("Ligne n° ",freq_a$code_ligne), # avec le hover
             popup = labels,
             labelOptions = labelOptions(
               style = list("font-weight" = "normal", padding = "3px 8px"),
               textsize = "15px",
               direction = "auto"),
             group = as.character(paste(freq_a$direction_,freq_a$code_ligne)) # groupes séparés par ligne
  ) %>%
  addPolylines(data = freq_l,
               weight = (freq_l$montees/max(freq_l$montees)*50)^0.57+0.75, # coefficient de correction avec un minimum de largeur de 1
               opacity = 1,
               color = freq_l$color, # couleur de chaque ligne tl
               highlight = highlightOptions(
                 weight = 3,
                 color = "black",
                 fillOpacity = 0.7,
                 bringToFront = TRUE),
               label = paste0("Ligne n° ",freq_l$code_ligne), # avec le hover
               popup = labels,
               labelOptions = labelOptions(
                 style = list("font-weight" = "normal", padding = "3px 8px"),
                 textsize = "15px",
                 direction = "auto"),
               group = as.character(paste(freq_l$direction_,freq_l$code_ligne)), # groupes séparés par ligne,
  ) %>%
  addLayersControl(
    overlayGroups = l2, # groupes séparés par ligne
    options = layersControlOptions(collapsed = TRUE)
  ) %>%
  hideGroup(l2) %>%
  showGroup(l2[24]) %>%
  showGroup(l2[510]) 
m4

# Carte avec une grille de quartiers

#Différentes échelles de couleur
pal <- colorNumeric("viridis", NULL)
pal <- colorQuantile("viridis", NULL)
pal <- colorFactor("viridis", NULL)

m5 <- leaflet(data = grille_freq) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView( lat=46.54033, lng=6.63575 , zoom=12) %>%
  addPolygons(data = grille_freq$geometry,
              color = pal(grille_freq$montees_su),
              weight = 1, # coefficient de correction avec un minimum de largeur de 1
              opacity = 1,
              fillOpacity = 0.4,
              # color = "green",
              # color = freq_l$color, # couleur de chaque ligne tl
              highlight = highlightOptions(
                 weight = 3,
                 color = "black",
                 fillOpacity = 0.7,
                 bringToFront = TRUE),
               label = paste("Nombre de montées :",round(grille_freq$montees_su)), # avec le hover
               # popup = labels,
               labelOptions = labelOptions(
                 style = list("font-weight" = "normal", padding = "3px 8px"),
                 textsize = "15px",
                 direction = "auto"),
               group = "montees" # groupes séparés par ligne
  ) %>%
  addPolygons(data = grille_freq$geometry,
              color = pal(grille_freq$descentes_),
              weight = 1, # coefficient de correction avec un minimum de largeur de 1
              opacity = 1,
              fillOpacity = 0.4,
              # color = "green",
              # color = freq_l$color, # couleur de chaque ligne tl
              highlight = highlightOptions(
                weight = 3,
                color = "black",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = paste("Nombre de descentes :",round(grille_freq$descentes_)), # avec le hover
              # popup = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"),
              group = "descentes" # groupes séparés par ligne
  ) %>%
  addPolylines(data = freq_l,
               weight = (freq_l$montees/max(freq_l$montees)*50)^0.57+0.75, # coefficient de correction avec un minimum de largeur de 1
               opacity = 1,
               color = freq_l$color, # couleur de chaque ligne tl
               highlight = highlightOptions(
                 weight = 3,
                 color = "black",
                 fillOpacity = 0.7,
                 bringToFront = TRUE),
               label = paste0("Ligne n° ",freq_l$code_ligne), # avec le hover
               popup = labels,
               labelOptions = labelOptions(
                 style = list("font-weight" = "normal", padding = "3px 8px"),
                 textsize = "15px",
                 direction = "auto"),
               group = as.character(paste(freq_l$direction_, freq_l$code_ligne)), # groupes séparés par ligne,
  # ) %>%
  # addLegend(pal = pal, values = ~montees_su, opacity = 0.7, title = NULL,
  #           position = "bottomright"
  ) %>%
  addLayersControl(
    baseGroups = c("montees","descentes"), # groupes séparés par ligne
    overlayGroups = as.character(paste(freq_l$direction_, freq_l$code_ligne)),
    options = layersControlOptions(collapsed = TRUE)
  ) %>%
  hideGroup(as.character(paste(freq_l$direction_, freq_l$code_ligne)))
m5

# Carte autre grille
m6 <- leaflet(data = grille_npa_freq) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView( lat=46.54033, lng=6.63575 , zoom=12) %>%
  addPolygons(data = grille_npa_freq$geometry,
              color = pal(grille_npa_freq$montees_su),
              weight = 1, # coefficient de correction avec un minimum de largeur de 1
              opacity = 1,
              fillOpacity = 0.4,
              # color = "green",
              # color = freq_l$color, # couleur de chaque ligne tl
              highlight = highlightOptions(
                weight = 3,
                color = "black",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = paste("Nombre de montées :",round(grille_npa_freq$montees_su)), # avec le hover
              # popup = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"),
              group = "montees" # groupes séparés par ligne
  ) %>%
  addPolygons(data = grille_npa_freq$geometry,
              color = pal(grille_npa_freq$descentes_),
              weight = 1, # coefficient de correction avec un minimum de largeur de 1
              opacity = 1,
              fillOpacity = 0.4,
              # color = "green",
              # color = freq_l$color, # couleur de chaque ligne tl
              highlight = highlightOptions(
                weight = 3,
                color = "black",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = paste("Nombre de descentes :",round(grille_npa_freq$descentes_)), # avec le hover
              # popup = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"),
              group = "descentes" # groupes séparés par ligne
  ) %>%
  addPolylines(data = freq_l,
               weight = (freq_l$montees/max(freq_l$montees)*50)^0.57+0.75, # coefficient de correction avec un minimum de largeur de 1
               opacity = 1,
               color = freq_l$color, # couleur de chaque ligne tl
               highlight = highlightOptions(
                 weight = 3,
                 color = "black",
                 fillOpacity = 0.7,
                 bringToFront = TRUE),
               label = paste0("Ligne n° ",freq_l$code_ligne), # avec le hover
               popup = labels,
               labelOptions = labelOptions(
                 style = list("font-weight" = "normal", padding = "3px 8px"),
                 textsize = "15px",
                 direction = "auto"),
               group = as.character(paste(freq_l$direction_, freq_l$code_ligne)), # groupes séparés par ligne,
               # ) %>%
               # addLegend(pal = pal, values = ~montees_su, opacity = 0.7, title = NULL,
               #           position = "bottomright"
  ) %>%
  addLayersControl(
    baseGroups = c("montees","descentes"), # groupes séparés par ligne
    overlayGroups = as.character(paste(freq_l$direction_, freq_l$code_ligne)),
    options = layersControlOptions(collapsed = TRUE)
  ) %>%
  hideGroup(as.character(paste(freq_l$direction_, freq_l$code_ligne)))
m6

### Carte statique ###
# Carte statique avec Lausanne en fond
library("ggplot2")
library("ggsn") # complément à ggplot pour l'échelle et la flèche du nord
ggplot() +
  geom_sf(data = lausanne, size = 0.2, fill = "#f1f1f1") +
  # geom_sf(data = leman, size = 0.2, fill = "#626278") +
  geom_sf(data = freq_l, color = freq_l$color, size = (freq_l$montees/max(freq_l$montees)*20)^0.57+0.75) +
  theme_void() +
  scale_colour_gradient2(
    low = "#d7191c",
    mid = "#ffffbf",
    high = "#2c7bb6",
    midpoint = 50,
    space = "Lab",
    na.value = "grey80",
    guide = guide_legend( keyheight = unit(4, units = "mm"), keywidth=unit(10, units = "mm"), label.position = "right", title.position = 'top', reverse=TRUE),
    aesthetics = "fill",
    name = "Oui (%)"
  ) +
  labs(
    title = "Réseau des TL",
    subtitle = "Toutes les lignes",
    caption = "Données : TL | Auteur : Romain Loup"
  ) +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    
    plot.title = element_text(size= 14, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 12, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text(size=9, color = "#4e4d47", margin = margin(b = 0.1, unit = "cm") ),
    legend.position = c(0.9, 0.8)
  ) +
  scalebar(freq_l, dist = 2, dist_unit = "km", st.size=3, transform = TRUE, model = "WGS84", border.size = 0.5, height = 0.01, box.fill = c("#4e4d47", "#f5f5f2"), st.color = "#4e4d47", box.color = "#4e4d47")


### Carte interactive ECTQG ###
zoom_to <- c(6.67, 46.55)
zoom_level <- 27.2
C <- 40075016.686   # ~ circumference of Earth in meters
x_span <- C / 2^zoom_level
y_span <- C / 2^(zoom_level+1)   # also sets aspect ratio
target_crs <- 4326
zoom_to_xy <- st_transform(st_sfc(st_point(zoom_to), crs = 4326), crs = target_crs)

disp_window <- st_sfc(st_point(st_coordinates(zoom_to_xy - c(x_span / 2, y_span / 2))),
                      st_point(st_coordinates(zoom_to_xy + c(x_span / 2, y_span / 2))),
                      crs = target_crs)


# Carte statique avec Lausanne en fond
library("ggplot2")
library("ggsn") # complément à ggplot pour l'échelle et la flèche du nord
ggplot() +
  geom_sf(data = leman, size = 0, fill = "#C6C6D6") +
  geom_sf(data = lausanne, size = 0, fill = "#f6f6f6") +
  geom_sf(data = freq_l, color = freq_l$color, size = 0.4) +
  theme_void() +
  scale_colour_gradient2(
    low = "#d7191c",
    mid = "#ffffbf",
    high = "#2c7bb6",
    midpoint = 50,
    space = "Lab",
    na.value = "grey80",
    guide = guide_legend( keyheight = unit(4, units = "mm"), keywidth=unit(10, units = "mm"), label.position = "right", title.position = 'top', reverse=TRUE),
    aesthetics = "fill",
    name = "Oui (%)"
  ) +
  labs(
    title = "Transports Lausannois network",
    subtitle = "All bus/metro lines",
    caption = "Data: TL | Author : Romain Loup",
    tag = "Léman"
  ) +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#e7e7e7", color = NA),
    panel.background = element_rect(fill = "#e7e7e7", color = NA),
    legend.background = element_rect(fill = "#e7e7e7", color = NA),
    
    plot.title = element_text(size= 14, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 12, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.tag = element_text(size= 9, hjust=0.01, color = "#4e4d47"),
    plot.caption = element_text(size= 6, color = "#4e4d47", margin = margin(t = -0.2, unit = "cm") ),
    legend.position = c(0.9, 0.8)
  ) +
  scalebar(freq_l, dist = 2, dist_unit = "km", st.size=3, transform = TRUE, model = "WGS84", border.size = 0.2, height = 0.01, box.fill = c("#4e4d47", "#f5f5f2"), st.color = "#4e4d47", box.color = "#4e4d47") + 
  coord_sf(xlim = st_coordinates(disp_window)[,'X'],
           ylim = st_coordinates(disp_window)[,'Y'],
           crs = target_crs)


### ECTQG 2021
# Ajouter charge à bord pour le segment (arrêt)
i <- 1
freq_a$charge[1] <- freq_a$montees[1] - freq_a$descentes[1]
while (i < dim(freq_a)[1]) {
  stock <- freq_a$charge[i]
  if (stock < 0) {
    stock <- 0
  }
  i <- i+1
  freq_a$charge[i] <- stock + freq_a$montees[i] - freq_a$descentes[i]
}

# Ajouter charge à bord pour le segment (ligne)
i <- 1
freq_l$charge[1] <- freq_l$montees[1] - freq_l$descentes[1]
while (i < dim(freq_l)[1]) {
  stock <- freq_l$charge[i]
  if (stock < 0) {
    stock <- 0
  }
  i <- i+1
  freq_l$charge[i] <- stock + freq_l$montees[i] - freq_l$descentes[i]
}

poids <- (freq_a$montees/max(freq_a$montees)*20000)^0.57+10
poids2 <- (freq_a$descentes/max(freq_a$descentes)*20000)^0.57+10
# Carte cercles + lignes
l2 <- as.character(paste(freq_a$direction_,freq_a$code_ligne))

l4 <- c("D", "e", "f","gkfo")
l5 <- "M"


l3 <- as.character(paste("M",freq_a$direction_,freq_a$code_ligne))
l4 <- as.character(paste("D",freq_a$direction_,freq_a$code_ligne))
l5 <- c(l3,l4)
l5[grep("^M",l5)]

lm <- l3[grep("^M",l3)]
test <- cbind(l4, l5)

l4 <- as.character(paste("D",freq_a$direction_,freq_a$code_ligne))
substring(as.character(paste("D",freq_a$direction_,freq_a$code_ligne)), 3,)
substring(l3, 3,)


m7 <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView( lat=46.54033, lng=6.63575 , zoom=12) %>%
  
  addPolylines(data = freq_l,
               weight = (freq_l$charge/max(freq_l$charge)*50)^0.57+0.75, # coefficient de correction avec un minimum de largeur de 1
               # weight = 1,
               opacity = 0.8,
               color = freq_l$color, # couleur de chaque ligne tl
               highlight = highlightOptions(
                 weight = 5,
                 color = "black",
                 fillOpacity = 0.7,
                 bringToFront = TRUE),
               label = paste0("Ligne n° ",freq_l$code_ligne), # avec le hover
               popup = labels,
               labelOptions = labelOptions(
                 style = list("font-weight" = "normal", padding = "3px 8px"),
                 textsize = "15px",
                 direction = "auto"),
               group = l2, # groupes séparés par ligne,
  ) %>%

  addCircles(data = freq_a,
             weight = 0.5,
             radius = poids, # coefficient de correction avec un minimum de largeur de 1
             fillOpacity = 1,
             color = freq_a$color, # couleur de chaque ligne tl
             
             highlight = highlightOptions(
               # radius = poids,
               weight = 1,
               color = "white",
               fillOpacity = 0.8,
               bringToFront = TRUE),
             label = paste0("Ligne n° ",freq_a$code_ligne), # avec le hover
             popup = labels,
             labelOptions = labelOptions(
               style = list("font-weight" = "normal", padding = "3px 8px"),
               textsize = "15px",
               direction = "auto"),
             # layerId = "M",
             group = l2 # groupes séparés par ligne
  ) %>%
  addCircles(data = freq_a,
             weight = 0.5,
             radius = poids2, # coefficient de correction avec un minimum de largeur de 1
             fillOpacity = 1,
             color = freq_a$color, # couleur de chaque ligne tl

             highlight = highlightOptions(
               # radius = poids,
               weight = 1,
               color = "white",
               fillOpacity = 0.8,
               bringToFront = TRUE),
             label = paste0("Ligne n° ",freq_a$code_ligne), # avec le hover
             popup = labels,
             labelOptions = labelOptions(
               style = list("font-weight" = "normal", padding = "3px 8px"),
               textsize = "15px",
               direction = "auto"),
             # layerId = l5[grep("D.",l5)],
             group = l2 # groupes séparés par ligne
  ) %>%
  addLayersControl(
    # layerId = l3,
    # baseGroups = c(l5[grep("M.",l5)],l5[grep("D.",l5)]),
    # baseGroups = l2,
    # if (2==2) {
    #   print("hello")
    # },
    overlayGroups = l2, # groupes séparés par ligne
    options = layersControlOptions(collapsed = TRUE)
  ) %>%
  hideGroup(l2) %>%
  showGroup(l2[14]) %>%
  showGroup(l2[510]) 
m7
print("hello")
# Ajout de l'échelle
addScaleBar(
  m7,
  position = c("bottomright"),
  options = scaleBarOptions(
    maxWidth = 100,
    metric = TRUE,
    imperial = FALSE,
    updateWhenIdle = TRUE
  )
)



df <- c("AndyBullxxx", "AlexPullxcx","AndyPamxvb", "RickRalldfg","AndyPantert","SamSaltedf")
df[grep("^Andy",df)]

df <- c("M A 45", "D R 45","M A 33", "M R 45","D A 45","M A 45")
df[grep("^M",df)]


l5[grep("^M",l5)]
l5[grep("D.",l5)]
l5[grep(".M",l5)]
l2
