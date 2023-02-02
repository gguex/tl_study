############ Graphique de fréquentation annuelle des lignes TL

# Set working directory 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### Connexion à la base de données
library('RPostgreSQL')
library('sf')
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

# Sans passer par la base de données
stop_frequentation <- read.csv2("../data_processing/stop_frequentation.csv")

### Initialisation of the table
table <- data.frame(matrix(ncol = 7, nrow = 0))
namesTable <- c("code_arret", "libelle_arret", "code_ligne", "sequence", "direction", "montees", "descentes")
colnames(table) <- namesTable

# Itérations sur i, row 1 et j row 2
i <- 1 # row number
j <- 1

while (i <= length(stops_frequentation$code_ligne_theo)) {
  #row1 <- i
  while (stops_frequentation$sequence_theo[j] < stops_frequentation$sequence_theo[j+1]) {
    if (stops_frequentation$sequence_theo[j]<stops_frequentation$sequence_theo[j+1] &&
        stops_frequentation$direction_voy_theo[j]<=stops_frequentation$direction_voy_theo[j+1] &&
        stops_frequentation$code_ligne_theo[j]<=stops_frequentation$code_ligne_theo[j+1]
    ) {
      j<-j+1
    }
    else{}
  }
  row1 <- i
  row2 <- j
  # Stocker row1 et row2 et créer les graphiques
  png(file=paste0("graphs_tl/ligne_",stops_frequentation$code_ligne_theo[i],stops_frequentation$direction_voy_theo[i],".png"))
  barplot(as.matrix(stops_frequentation[i:j,6:7]), main = paste("Ligne", stops_frequentation$code_ligne_theo[i], stops_frequentation$direction_voy_theo[i]), beside=T)
  dev.off()
  
  i <- row2+1
  j <- row2+1
}
