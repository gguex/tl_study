# Set working directory 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load data
data = read.csv("data/Ptr_marges.csv")

nom_arrets = names(data)
nij = as.matrix(data)
n_arrets = dim(nij)[1]
l = n_arrets - 1

n_trajets = c()
red_nij = nij
for(i in 1:l){
  red_nij = red_nij[-dim(red_nij)[2],-1]
  if(!is.null(dim(red_nij))) n_trajets = c(n_trajets, sum(diag(red_nij)))
  else n_trajets = c(n_trajets, red_nij)
}

names(n_trajets) = 1:l
barplot(n_trajets)

for(id_arret in 1:(length(nom_arrets)-1)){
  rep_ligne = nij[id_arret,]
  names(rep_ligne) = nom_arrets
  rep_ligne[-(1:id_arret)]
  barplot(rep_ligne, main=nom_arrets[id_arret][])
}

n_montees = rowSums(nij)
n_descentes = colSums(nij)

# Calcul du transit (doit correspondre dans les données) 
transit_ij = n_montees[1] 
for (i in 2:(n_arrets-1)){
  transit_ij = c(transit_ij, transit_ij[i-1] + n_montees[i] - n_descentes[i])
}

p_in = n_montees / sum(n_montees) # Probabilité de montées 
p_out = n_descentes / c(1, transit_ij) # Probabilité de descente
p_ij = 1 - p_out # Probabilité de continuer

Pij = nij / sum(nij)
j = 3
n_descentes[j] / sum(n_descentes[-(1:(j-1))])
Pij[j-1, j] / sum(Pij[j-1, ])




