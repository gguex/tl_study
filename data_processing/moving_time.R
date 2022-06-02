############################################################### 
####### Setting paths

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

####### Libraries
library(RPostgreSQL) # Database connection
library(sf) # Read database
library(tidyverse) # Data filter
library(dplyr)
library(stringr) # Remove spaces

# Database connexion
conn <- dbConnect("PostgreSQL", dbname = 'tl_small', host = 'localhost', user = 'rloup', password = '')

# Mean travel time
travel_time <- st_read(conn, query="
-- temps moyen déplacement sur l'année de i à j
select code_ligne_theo, direction_voy_theo, code_arret_prec_theo, avg(temps_parcours_troncon_prec_real) as mean_travel_time
from c1c2
-- where code_ligne_theo = 6 or code_ligne_theo = 7 or code_ligne_theo = 8 or code_ligne_theo = 9
group by code_ligne_theo, direction_voy_theo, code_arret_prec_theo;")

# Save/load data
# write_csv(travel_time, "travel_time.csv")
# travel_time <- read_csv("travel_time.csv")

# Take mean waiting time in a data frame
waiting_time <- st_read(conn, query="select code_ligne_theo, direction_voy_theo, code_arret_theo, avg(heure_sortie_fenetre_arret_real_secondes - heure_entree_fenetre_arret_real_secondes) as mean_waiting_time
from c1c2
-- where code_ligne_theo = 6 or code_ligne_theo = 7 or code_ligne_theo = 8 or code_ligne_theo = 9
group by code_ligne_theo, direction_voy_theo, code_arret_theo;")

# Save/load data
# write_csv(waiting_time, "waiting_time.csv")
# waiting_time <- read_csv("waiting_time.csv")

# Read stop freq
df <- read_csv2("stop_frequentation.csv")

###### LINES FILTERS #####
# Select some lines
travel_time <- filter(travel_time,code_ligne_theo==6|code_ligne_theo==7|code_ligne_theo==8|code_ligne_theo==9)
waiting_time <- filter(waiting_time,code_ligne_theo==6|code_ligne_theo==7|code_ligne_theo==8|code_ligne_theo==9)
df <- filter(df,code_ligne_theo==6|code_ligne_theo==7|code_ligne_theo==8|code_ligne_theo==9)

# Rename columns with good name
names(travel_time)[names(travel_time) == "code_arret_theo"] <- "code_arret_suiv_theo"
names(travel_time)[names(travel_time) == "code_arret_prec_theo"] <- "code_arret_theo"

# Remove spaces in data frame
df$code_arret_theo <- gsub('\\s+', '', df$code_arret_theo)
travel_time$code_arret_theo <- gsub('\\s+', '', travel_time$code_arret_theo)
waiting_time <- as.data.frame(apply(waiting_time,2,function(x)gsub('\\s+', '',x)))

# Join travel time to the freq dataframe
df_time <- left_join(
  df,
  travel_time,
  by = c("code_ligne_theo","direction_voy_theo","code_arret_theo")
)

# Join waiting time
waiting_time$code_ligne_theo <- as.double(waiting_time$code_ligne_theo)
# Join waiting time to the dataframe
df_time <- left_join(
  df_time,
  waiting_time,
  by = c("code_ligne_theo","direction_voy_theo","code_arret_theo")
)

# Save data
write_csv(df_time, "stop_travel_waiting_time_all.csv")

