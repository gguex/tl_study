# Set working directory 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

############# Connexion à la base de données

library('RPostgreSQL')
library('config')

# Connexion à la base de données
conn <- dbConnect("PostgreSQL", dbname = 'tl_small', host = 'localhost', user = 'rloup', password = '')

# Fréquentation annuelle : somme des montées/descentes par an sur les ligne, direction et arrêt
stops_frequentation <- st_read(conn, query="select stop_order.code_arret_theo, freq_theo.libelle_arret_theo2, stop_order.code_ligne_theo, freq_theo.direction_voy_theo, stop_order.sequence_theo, sum(freq_theo.monte) AS montees, sum(freq_theo.descendu) AS descentes
from freq_theo

inner join stop_order
on freq_theo.code_ligne_theo = stop_order.code_ligne_theo
and freq_theo.code_arret_theo = stop_order.code_arret_theo
and freq_theo.direction_voy_theo = stop_order.direction_voy_theo

group by stop_order.code_arret_theo, freq_theo.libelle_arret_theo2, stop_order.code_ligne_theo, freq_theo.direction_voy_theo, stop_order.sequence_theo
order by stop_order.code_ligne_theo, freq_theo.direction_voy_theo, stop_order.sequence_theo;")

# Without database
stops_frequentation <- read.csv2("../data_processing/stop_frequentation.csv")


############# Petit script de calcul des trajets via une chaîne de Markov

# Fonction chaîne de Markov, à itérer sur chaque ligne TL
markov_Ptr <- function(n_montees, n_descentes, l){
  # Calcul du transit (doit correspondre dans les données) 
  transit_ij = n_montees[1] 
  for (i in 2:(l-1)){
    transit_ij = c(transit_ij, transit_ij[i-1] + n_montees[i] - n_descentes[i])
  }
  
  p_in = n_montees / sum(n_montees) # Probabilité de montées 
  p_out = n_descentes / c(1, transit_ij) # Probabilité de descente
  p_ij = 1 - p_out # Probabilité de continuer
  
  ########## Calcul avec chaîne de Markov 
  
  # On construit la matrice W
  W_tr = rbind(cbind(rep(0,l-1), diag(p_ij[-l])), rep(0, l))
  W = rbind(cbind(rep(0, l), rbind(p_in, cbind(W_tr[-1,-1], p_out[-1]))), rep(0, l+1))
  
  # On trouve la matrice fondamentale
  Z = solve(diag(dim(W)[1]) - W)
  
  # On construit la matrice de prob
  Ptr = outer(p_in[-l], rep(1,l-1)) * Z[-c(1, l+1), -c(1, l+1)] * outer(rep(1,l-1), p_out[-1])
  Ptr = rbind(cbind(rep(0, l-1), Ptr), rep(0, l))
  Ptr <<- Ptr
}


###### Itérations sur les différentes lignes TL
i<-1
j<-2
freq <- c()

# Table  qui regroupe le n° de ligne, la direction, le nombre d'arrêts attendu, l'origine type et la destination type d'une ligne TL
exp_long_trajet_table <- data.frame(ligne = character(0), direction = character(0), exp_long_trajet = numeric(0), entree_type = character(0), sortie_type = character(0))

# itération pour garder en i le premier arrêt de la séquence et en j le dernier
while (i <= length(stops_frequentation$code_ligne_theo)) {
  #row1 <- i
  while (stops_frequentation$sequence_theo[j] <= stops_frequentation$sequence_theo[j+1]) {
    if (stops_frequentation$sequence_theo[j] <= stops_frequentation$sequence_theo[j+1] &&
        stops_frequentation$direction_voy_theo[j] <= stops_frequentation$direction_voy_theo[j+1] &&
        stops_frequentation$code_ligne_theo[j] <= stops_frequentation$code_ligne_theo[j+1]
    ) {
      j<-j+1
    }
    else{}
  }
  # ici fonction i = row1 et j = row2
  freq <- stops_frequentation[c(i:j),] # séquence de la ligne par n° de ligne, direction et ordre, i premier arrêt et j terminus
  n_montees <- freq$montees # Nombre de gens qui montent
  n_descentes <-freq$descentes # Nombre de gens qui descendent
  l = length(n_montees) # Longueur du trajet
  markov_Ptr(n_montees,n_descentes, l)
  
  # Nom des arrêts pour les marges
  rownames(Ptr) <- freq$libelle_arret_theo2
  colnames(Ptr) <- freq$libelle_arret_theo2
  
  # Enregistrement du tableau
  write.csv(Ptr, paste0(freq$code_ligne_theo[1], freq$direction_voy_theo[1],"_Ptr.csv"), row.names = FALSE)
  
  # Espérance du trajet
  exp_long_trajet = 0
  for(a in 1:dim(Ptr)[1]){
    for(b in 1:dim(Ptr)[2]){
      exp_long_trajet = exp_long_trajet + abs(a - b) * Ptr[a, b]
    }
  }
  
  # Enregister les données dans la variable "exp_long_trajet_table"
  df <- data.frame(freq$code_ligne_theo[1], freq$direction_voy_theo[1], exp_long_trajet, freq$libelle_arret_theo2[which(Ptr == max(Ptr), arr.ind = TRUE)[1]], freq$libelle_arret_theo2[which(Ptr == max(Ptr), arr.ind = TRUE)[2]]) # Choisir la valeur maximale et ressortir l'arrêt de départ et d'arrivée
  names(df) <- c("ligne", "direction", "exp_long_trajet", "entree_type", "sortie_type")
  exp_long_trajet_table <- rbind(exp_long_trajet_table, df)
  exp_long_trajet_table <<- exp_long_trajet_table
  
  # Passage au n° de ligne suivant
  i <- j+1
  j <- j+1
}
# Enregistrer la fréquenation pour éviter d'avoir beseoin de la DB
write.csv(stops_frequentation, "stops_frequentation.csv")
