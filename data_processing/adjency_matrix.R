############################################################### 
####### Setting paths

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("/Users/rloup/switchdrive/theseRomain/data")

##### Distance Matrix #####


# Database connexion
library("RPostgreSQL")
# Connexion à la base de données
conn <- dbConnect("PostgreSQL", dbname = 'tl_small', host = 'localhost', user = 'rloup', password = '')

library("sf")

# Réseau de tous les arrêts
df <- st_read(conn, query="
select distinct stop_order.code_ligne_theo, stop_order.code_arret_theo, stop_order.libelle_arret_theo, stop_order.sequence_theo, stop_order.distance_arret_prec_theo, stop_order.direction_voy_theo,
freq_theo.lat_arret, freq_theo.lon_arret, freq_theo.geom
from stop_order
inner join freq_theo
on stop_order.code_arret_theo = freq_theo.code_arret_theo
order by code_ligne_theo, direction_voy_theo, sequence_theo;")

df2 <- df
df <- df2
df$new_seq <- 1
i <- 1
j <- 1

while (i <= length(df$code_arret_theo)) {
  df$new_seq[i] <- j
  if(df$code_ligne_theo[i] == df$code_ligne_theo[i+1] && df$code_ligne_theo[i] == df$code_ligne_theo[i+1] && df$direction_voy_theo[i] == df$direction_voy_theo[i+1]) {
    j <- j + 1
  }
  else{
    j <- 1
  }
  i <- i + 1
}

# Lier dernier arrêt de l'aller avec le premier du retour
while (i <= length(df$code_arret_theo)) {
  df$new_seq[i] <- j
  if(df$code_ligne_theo[i] == df$code_ligne_theo[i+1] && df$code_ligne_theo[i] == df$code_ligne_theo[i+1]) {
    j <- j + 1
  }
  else{
    j <- 1
  }
  i <- i + 1
}
  
# Libraries
library(geosphere)
df2 <- df
#Database
df <- read.csv("arrets_geom_inner.csv", header = TRUE)

df <- filter(df,code_ligne_theo==6|code_ligne_theo==7|code_ligne_theo==8|code_ligne_theo==9)



# Create empty distance matrix nxn with the right n
x <- matrix(nrow = length(df$code_arret_theo), ncol = length(df$code_arret_theo))
a <- matrix(nrow = length(df$code_arret_theo), ncol = length(df$code_arret_theo))

# Create empty list
l <- list(c())
# Push all stop codes in the list
i <- 1
while(i <= length(df$code_arret_theo)) {
  l[[i]] <- paste0(df$code_ligne_theo[i],"_",df$direction_voy_theo[i],"_",df$code_arret_theo[i])
  i <- i + 1
}
colnames(x) <- l
rownames(x) <- l
colnames(a) <- l
rownames(a) <- l

# Fill the matrix with the distances between stops
i <- 1
j <- 1
while (j <= length(df$code_arret_theo)) {
  while (i <= length(df$code_arret_theo)) {
    # function that show the distance between two lon lat points
    x[i,j] <- (distm(c(df$lon_arret[i], df$lat_arret[i]), c(df$lon_arret[j], df$lat_arret[j]), fun = distHaversine))
    i <- i + 1
  }
  i <- 1
  j <- j + 1
}


##### Adjacency Matrix #####

# Chosen distance
d <- 100
# Fill the matrix with the distances between stops
i <- 1
j <- 1
while (j <= length(df$code_arret_theo)) {
  while (i <= length(df$code_arret_theo)) {
    # function that show the distance between two lon lat points
    if( (distm(c(df$lon_arret[i], df$lat_arret[i]), c(df$lon_arret[j], df$lat_arret[j]), fun = distHaversine)) < d) {
      a[i,j] <- 1
    } else {
      a[i,j] <- 0
    }
    i <- i + 1
  }
  i <- 1
  j <- j + 1
}


# Add the next and not previous stop to the A matrix
# Fill the matrix with the distances between stops
i <- 1
j <- 1
while (j <= length(df$code_arret_theo)) {
  while (i <= length(df$code_arret_theo)) {
    # function that show the distance between two lon lat points
    if(df$new_seq[i] == (df$new_seq[j]-1) && df$code_ligne_theo[i] == df$code_ligne_theo[j] && df$direction_voy_theo[i] == df$direction_voy_theo[j] ) {
      a[i,j] <- 0
    }
    if(df$new_seq[i] == (df$new_seq[j]-1) && df$code_ligne_theo[i] == df$code_ligne_theo[j] && df$direction_voy_theo[i] == df$direction_voy_theo[j] ) {
      a[i,j] <- 1
    }else {
      a[i,j] <- 0
    }
    i <- i + 1
  }
  i <- 1
  j <- j + 1
}


while (j <= length(df$code_arret_theo)) {
  while (i <= length(df$code_arret_theo)) {
    # function that show the distance between two lon lat points
    if(df$new_seq[i] == (df$new_seq[j]-1) && df$code_ligne_theo[i] == df$code_ligne_theo[j]) {
      a[i,j] <- 0
    }
    if(df$new_seq[i] == (df$new_seq[j]-1) && df$code_ligne_theo[i] == df$code_ligne_theo[j]) {
      a[i,j] <- 1
    }else {
      a[i,j] <- 0
    }
    i <- i + 1
  }
  i <- 1
  j <- j + 1
}


i <- 1
j <- 1
while (j <= length(df$code_arret_theo)) {
  while (i <= length(df$code_arret_theo)) {
    # function that show the distance between two lon lat points
    if(df$new_seq[i] == (df$new_seq[j]-1) && df$code_ligne_theo[i] == df$code_ligne_theo[j]) {
      a[i,j] <- 0
    }
    if(df$new_seq[i] == (df$new_seq[j]-1) && df$code_ligne_theo[i] == df$code_ligne_theo[j]) {
      a[i,j] <- 1
    }
    if(df$code_arret_theo[min(df$new_seq)] == (df$new_seq[i]+1) && df$code_ligne_theo[i] == df$code_ligne_theo[j]) {
      a[i,j] <- 1
    }else {
      a[i,j] <- 0
    }
    i <- i + 1
  }
  i <- 1
  j <- j + 1
}



df$indice <- 1:dim(df)[1]


a = matrix(0, dim(df)[1], dim(df)[1]) 
colnames(a) <- l
rownames(a) <- l
for(code_ligne in df$code_ligne_theo){
  df_ligne = df[df$code_ligne_theo == code_ligne, ]
  indice_ligne = df_ligne$indice
  for(i in 1:(length(indice_ligne)-1)){
    a[indice_ligne[i], indice_ligne[i+1]] = 1
  }
  a[indice_ligne[length(indice_ligne)], indice_ligne[1]] = 1
}

write.csv(a, file = "adj_next_bind.csv", row.names = TRUE)
# write.csv(df, file = "arrets_geom_inner.csv", row.names = F)
# write.csv(x, file = "mat_dist_Haversine.csv", row.names = TRUE)
