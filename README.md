# Memoire__M1

# Charger le package CASdatasets
library(CASdatasets)
library(dplyr)
library(lubridate)
library(MASS)
# Charger les donn?es
data("fremotor1prem0304a")
data("fremotor1freq0304a")
data("fremotor1sev0304a")
#fremotor1prem0304a

# Supprimer les doublonsde la dataframe fremotor1fre0304a:
duplicated(fremotor1freq0304a)
#pas de doublons !
#fusionner les 3 bases de données

#on va rajouter une colonne year dans la data frame fremotor1sev0304a pour pouvoir bien fusionner avec les deux autres dataframes

fremotor1sev0304a$OccurDate <- as.Date(fremotor1sev0304a$OccurDate)

# Créer une nouvelle colonne 'Year' avec seulement l'année
fremotor1sev0304a <- mutate(fremotor1sev0304a, Year = year(OccurDate))

#fusion des data frame pour les deux années
# Fusionner les dataframes pour l'année 2003
merged_data_2003 <- merge(merge(fremotor1freq0304a[fremotor1freq0304a$Year == 2003,], fremotor1sev0304a[fremotor1sev0304a$Year == 2003,], by = 'IDpol', all = TRUE), fremotor1prem0304a[fremotor1prem0304a$Year == 2003,], by = 'IDpol', all = TRUE)

# Fusionner les dataframes pour l'année 2004
merged_data_2004 <- merge(merge(fremotor1freq0304a[fremotor1freq0304a$Year == 2004,], fremotor1sev0304a[fremotor1sev0304a$Year == 2004,], by = 'IDpol', all = TRUE), fremotor1prem0304a[fremotor1prem0304a$Year == 2004,], by = 'IDpol', all = TRUE)

# Supprimer les lignes où 'Payment' est égal à 0
merged_data_2003 <- subset(merged_data_2003, Payment != 0)
merged_data_2004<- subset(merged_data_2004, Payment != 0)




#on va approcher la loi de fréquence garantie par garantie ! 
#année 2003 loi de fréquence
frequence_damage_2003 <- merged_data_2003$Damage
frequence_fire_2003 <- merged_data_2003$Fire
frequence_other_2003 <- merged_data_2003$Other
frequence_theft_2003 <- merged_data_2003$Theft
frequence_TPL_2003 <- merged_data_2003$TPL
frequence_windscreen_2003 <- merged_data_2003$Windscreen


# Ajuster une distribution de Poisson à nos données et estimer les paramètres
#La fonction fitdistr de R fait partie du package MASS et permet d'ajuster une distribution statistique à des données observées en estimant les paramètres de la distribution. Plus précisément, elle estime les paramètres de la distribution de Poisson qui correspondent le mieux aux données observées dans les donnée
#on estime les paramètre de la loi de poisson des différentes garanties (Fire, damage, other, theft...):
param_poiss_damage_2003 <- fitdistr(frequence_damage_2003, "Poisson")
param_poiss_fire_2003 <- fitdistr(frequence_fire_2003, "Poisson")
param_poiss_other_2003 <- fitdistr(frequence_other_2003, "Poisson")
param_poiss_theft_2003 <- fitdistr(frequence_theft_2003, "Poisson")
param_poiss_TPL_2003 <- fitdistr(frequence_TPL_2003, "Poisson")
param_poiss_windscreen_2003 <- fitdistr(frequence_windscreen_2003, "Poisson")

# Afficher les résultats
cat("L'estimateur du paramètre de la loi de poisson pour la guarantie damage est le suivant : ","\n")
param_poiss_damage_2003
cat("L'estimateur du paramètre de la loi de poisson pour la guarantie fire est le suivant : ", "\n")
param_poiss_fire_2003
cat("L'estimateur du paramètre de la loi de poisson pour la guarantie other est le suivant : ","\n")
param_poiss_other_2003
cat("L'estimateur du paramètre de la loi de poisson pour la guarantie theft est le suivant : ","\n")
param_poiss_theft_2003
cat("L'estimateur du paramètre de la loi de poisson pour la guarantie TPL est le suivant : ","\n")
param_poiss_TPL_2003
cat("L'estimateur du paramètre de la loi de poisson pour la guarantie windscreen est le suivant : ","\n")
param_poiss_windscreen_2003


#on a donc effectue une estimation du paramètre de la distribution de Poisson pour la lloi de fréquence 2003 pour les données contenues dans la variable frequence à l'aide de la fonction fitdistr.


#année 2004 loi de fréquence


# Année 2004
frequence_damage_2004 <- merged_data_2004$Damage
frequence_fire_2004 <- merged_data_2004$Fire
frequence_other_2004 <- merged_data_2004$Other
frequence_theft_2004 <- merged_data_2004$Theft
frequence_TPL_2004 <- merged_data_2004$TPL
frequence_windscreen_2004 <- merged_data_2004$Windscreen

# Ajuster une distribution de Poisson à nos données et estimer les paramètres
param_poiss_damage_2004 <- fitdistr(frequence_damage_2004, "Poisson")
param_poiss_fire_2004 <- fitdistr(frequence_fire_2004, "Poisson")
param_poiss_other_2004 <- fitdistr(frequence_other_2004, "Poisson")
param_poiss_theft_2004 <- fitdistr(frequence_theft_2004, "Poisson")
param_poiss_TPL_2004 <- fitdistr(frequence_TPL_2004, "Poisson")
param_poiss_windscreen_2004 <- fitdistr(frequence_windscreen_2004, "Poisson")

# Afficher les résultats pour l'année 2004
cat("L'estimateur du paramètre de la loi de Poisson pour la garantie damage en 2004 est le suivant : ","\n")
param_poiss_damage_2004
cat("L'estimateur du paramètre de la loi de Poisson pour la garantie fire en 2004 est le suivant : ", "\n")
param_poiss_fire_2004
cat("L'estimateur du paramètre de la loi de Poisson pour la garantie other en 2004 est le suivant : ","\n")
param_poiss_other_2004
cat("L'estimateur du paramètre de la loi de Poisson pour la garantie theft en 2004 est le suivant : ","\n")
param_poiss_theft_2004
cat("L'estimateur du paramètre de la loi de Poisson pour la garantie TPL en 2004 est le suivant : ","\n")
param_poiss_TPL_2004
cat("L'estimateur du paramètre de la loi de Poisson pour la garantie windscreen en 2004 est le suivant : ","\n")
param_poiss_windscreen_2004


library(ggplot2)
library(MASS)
library(cowplot)  

variables <- c("Damage", "Fire", "Other", "Theft", "TPL", "Windscreen")

# Créer une liste pour stocker les ggplots
ggplot_list <- list()

for (variable in variables) {
  # Paramètres Poisson déjà calculés
  params_poisson <- switch(variable,
                           "Damage" = param_poiss_damage_2004,
                           "Fire" = param_poiss_fire_2004,
                           "Other" = param_poiss_other_2004,
                           "Theft" = param_poiss_theft_2004,
                           "TPL" = param_poiss_TPL_2004,
                           "Windscreen" = param_poiss_windscreen_2004
  )
  
  # Histogramme pour la distribution empirique en 2004
  plot1 <- ggplot(merged_data_2004, aes(x = get(variable))) +
    geom_histogram(binwidth = 1, fill = "red", color = "black", alpha = 0.7) +
    labs(title = paste("Histogramme de la distribution empirique de", variable, "en 2004"), x = variable, y = "Fréquence")
  
  # Distribution théorique (Poisson) en 2004 (Histogramme en barre)
  plot2 <- ggplot(data.frame(x = seq(0, max(merged_data_2004[[variable]]) + 1)), aes(x)) +
    geom_bar(stat = "identity", fill = "blue", color = "black", alpha = 0.7, aes(y = dpois(x, params_poisson$estimate))) +
    labs(title = paste(" Histgramme de la distribution théorique (Poisson) de", variable, "en 2004"), x = variable, y = "Fréquence")
  
  # Stocker les ggplots dans la liste
  ggplot_list[[paste(variable, "_plot1", sep = "")]] <- plot1
  ggplot_list[[paste(variable, "_plot2", sep = "")]] <- plot2
}

# Créer une grille de ggplots avec plot_grid
grid <- plot_grid(plotlist = ggplot_list, ncol = 2, align = "h", hjust = -0.5)

# Afficher la grille
grid


library(ggplot2)
library(MASS)
library(cowplot)

variables <- c("Damage", "Fire", "Other", "Theft", "TPL", "Windscreen")

# Créer une liste pour stocker les ggplots
ggplot_list <- list()

for (variable in variables) {
  # Paramètres Poisson pour 2003
  params_poisson <- switch(variable,
                           "Damage" = param_poiss_damage_2003,
                           "Fire" = param_poiss_fire_2003,
                           "Other" = param_poiss_other_2003,
                           "Theft" = param_poiss_theft_2003,
                           "TPL" = param_poiss_TPL_2003,
                           "Windscreen" = param_poiss_windscreen_2003
  )
  
  # Histogramme pour la distribution empirique en 2003
  plot1 <- ggplot(merged_data_2003, aes(x = get(variable))) +
    geom_histogram(binwidth = 1, fill = "red", color = "black", alpha = 0.7) +
    labs(title = paste("Histogramme de la distribution empirique de", variable, "en 2003"), x = variable, y = "Fréquence")
  
  # Distribution théorique (Poisson) en 2003 (Histogramme en barre)
  plot2 <- ggplot(data.frame(x = seq(0, max(merged_data_2003[[variable]]) + 1)), aes(x)) +
    geom_bar(stat = "identity", fill = "blue", color = "black", alpha = 0.7, aes(y = dpois(x, params_poisson$estimate))) +
    labs(title = paste("Histogramme de la distribution théorique (Poisson) de", variable, "en 2003"), x = variable, y = "Fréquence")
  
  # Stocker les ggplots dans la liste
  ggplot_list[[paste(variable, "_plot1", sep = "")]] <- plot1
  ggplot_list[[paste(variable, "_plot2", sep = "")]] <- plot2
}

# Créer une grille de ggplots avec plot_grid
grid2 <- plot_grid(plotlist = ggplot_list, ncol = 2, align = "h", hjust = -0.5)

# Afficher la grille
grid2


library(ggplot2)
library(MASS)
library(cowplot)

variables <- c("Damage", "Fire", "Other", "Theft", "TPL", "Windscreen")

# Créer une liste pour stocker les ggplots
ggplot_list <- list()

for (variable in variables) {
  # Paramètres Poisson pour 2003
  params_poisson_2003 <- switch(variable,
                                "Damage" = param_poiss_damage_2003,
                                "Fire" = param_poiss_fire_2003,
                                "Other" = param_poiss_other_2003,
                                "Theft" = param_poiss_theft_2003,
                                "TPL" = param_poiss_TPL_2003,
                                "Windscreen" = param_poiss_windscreen_2003
  )
  
  # Paramètres Poisson pour 2004
  params_poisson_2004 <- switch(variable,
                                "Damage" = param_poiss_damage_2004,
                                "Fire" = param_poiss_fire_2004,
                                "Other" = param_poiss_other_2004,
                                "Theft" = param_poiss_theft_2004,
                                "TPL" = param_poiss_TPL_2004,
                                "Windscreen" = param_poiss_windscreen_2004
  )
  
  # Histogramme pour la distribution empirique en 2003
  plot1 <- ggplot(merged_data_2003, aes(x = get(variable))) +
    geom_histogram(binwidth = 1, fill = "red", color = "black", alpha = 0.7) +
    labs(title = paste("Distribution empirique de", variable, "en 2003"), x = variable, y = "Fréquence")
  
  # Distribution théorique (Poisson) en 2003 (Histogramme en barre)
  plot2 <- ggplot(data.frame(x = seq(0, max(merged_data_2003[[variable]]) + 1)), aes(x)) +
    geom_bar(stat = "identity", fill = "blue", color = "black", alpha = 0.7, aes(y = dpois(x, params_poisson_2003$estimate))) +
    labs(title = paste("Distribution théorique Poisson de", variable, "en 2003"), x = variable, y = "Fréquence")
  
  # QQ-plot pour la distribution empirique en 2003
  plot3 <- ggplot(merged_data_2003, aes(sample = get(variable))) +
    stat_qq(distribution = qpois, dparams = list(lambda = params_poisson_2003$estimate)) +
    labs(title = paste("QQ-plot de la distribution empirique de", variable, "en 2003"), x = "Théorique (Poisson)", y = "Empirique") +
    theme_minimal() +
    theme(legend.position = "none")
  
  # Stocker les ggplots dans la liste
  ggplot_list[[paste(variable, "_plot1", sep = "")]] <- plot1
  ggplot_list[[paste(variable, "_plot2", sep = "")]] <- plot2
  ggplot_list[[paste(variable, "_plot3", sep = "")]] <- plot3
}

# Créer une grille de ggplots avec plot_grid
grid3 <- plot_grid(plotlist = ggplot_list, ncol = 3, align = "h", hjust = -0.5)

# Afficher la grille
grid3

