############################################################### 
####### Setting paths

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


####### Libraries
library(RPostgreSQL) # Database connection
library(sf) # Read database

# Database connexion
conn <- dbConnect("PostgreSQL", dbname = 'tl_small', host = 'localhost', user = 'rloup', password = '')

# Mean stopping time
df <- st_read(conn, query="
    SELECT code_ligne_theo, direction_voy_theo, code_arret_theo, avg(heure_sortie_fenetre_arret_real_secondes - heure_entree_fenetre_arret_real_secondes) as mean_waiting_time
    FROM c1c2
    WHERE code_ligne_theo = 6 or code_ligne_theo = 7 or code_ligne_theo = 8 or code_ligne_theo = 9
    GROUP BY code_ligne_theo, direction_voy_theo, code_arret_theo;")

# Save data
write.csv2(df, "stopping_time.csv", row.names = FALSE)
