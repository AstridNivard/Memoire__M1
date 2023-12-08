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
#duplicated(fremotor1freq0304a)
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

#on teste pour chaque garantie si la loi de fréquence suit une loi de poisson pour l'année 2003
H0="la loi de fréquence pour la garantie Damage suit une loi de poisson"
H1="la loi de fréquence pour la garantie Damage ne suit pas une loi de poisson"
# Compter le nombre de sinistres pour chaque niveau de dommage en 2003
count_damage_2003 <- table(merged_data_2003$Damage)

# Extraire les niveaux de dommage (0, 1, 2)
damage_levels <- as.numeric(names(count_damage_2003))

# Convertir le résultat en vecteur
vec_count_damage_2003 <- as.vector(count_damage_2003)

# Afficher les niveaux de dommage et le vecteur
print("Niveaux de dommage en 2003:")
print(damage_levels)
print("Vecteur du nombre de sinistres par niveau de dommage en 2003:")
print(vec_count_damage_2003)
# Créer une data frame avec les niveaux de dommage et le vecteur de compte en 2003
df_damage_2003 <- data.frame(Damage_Levels = damage_levels, Count = vec_count_damage_2003)

# Afficher la data frame
print("Data frame pour les sinistres en 2003:")
print(df_damage_2003)
x <- with(df_damage_2003, rep(damage_levels,vec_count_damage_2003 ))
p <- dpois(x, lambda = param_poiss_damage_2003$estimate)
p <- unique(p)
data <- cbind(df_damage_2003, p)
last.x <- max(data[ , 'Damage_Levels' ])
last.p <- data[data['Damage_Levels'] == last.x, 'p']
remain.p <- ppois(q = last.x - 1, lambda = param_poiss_damage_2003$estimate, lower.tail = FALSE)
data[data['Damage_Levels'] == last.x, 'p'] <- remain.p
data <- data[ , c('Count', 'p')]
invalid_Np <- with(data, sum(Count) * p)
invalid_Np <- invalid_Np > 5
invalid_Np <- sum(invalid_Np = FALSE)
if(invalid_Np == 0){
  
  result <- with(data, chisq.test(Count, p = p)) # Avant correction
  
  x2obs <- result[['statistic']] # Après correction
  df <- result[['parameter']] - 1 # -1 degré de liberté car il y a eu 1 paramètre estimé.
  p.value <- pchisq(x2obs, df = 5, lower.tail = FALSE)
  
} else {
  cat("Au moins 1 Np n'est pas supérieur à 5")
}

print(result)
print(p.value)

if(p.value < 0.05){
  answer <- paste('Il y a rejet de H0, car', p.value, 'est inférieur à', 0.05, '. Donc,', H1)
} else {
  answer <- paste('Il y a non rejet de H0, car', p.value, 'est supérieur à', 0.05, '. Donc,', H0)
}
answer


#garantie Fire

H0="la loi de fréquence pour la garantie Fire suit une loi de poisson pour l'année 2003"
H1="la loi de fréquence pour la garantie Fire ne suit pas une loi de poisson pour l'année 2003"
# Compter le nombre de sinistres pour chaque niveau de dommage en 2003
count_fire_2003 <- table(merged_data_2003$Fire)

# Extraire les niveaux de dommage (0, 1, 2)
fire_levels <- as.numeric(names(count_fire_2003))

# Convertir le résultat en vecteur
vec_count_fire_2003 <- as.vector(count_fire_2003)

# Afficher les niveaux de dommage et le vecteur
print("Niveaux de fire en 2003:")
print(fire_levels)
print("Vecteur du nombre de sinistres par niveau de fire en 2003:")
print(vec_count_fire_2003)
# Créer une data frame avec les niveaux de dommage et le vecteur de compte en 2003
df_fire_2003 <- data.frame(Fire_Levels = fire_levels, Count = vec_count_fire_2003)

# Afficher la data frame
print("Data frame pour les sinistres en 2003:")
print(df_fire_2003)
x <- with(df_fire_2003, rep(fire_levels,vec_count_fire_2003 ))
p <- dpois(x, lambda = param_poiss_fire_2003$estimate)
p <- unique(p)
data <- cbind(df_fire_2003, p)
last.x <- max(data[ , 'Fire_Levels' ])
last.p <- data[data['Fire_Levels'] == last.x, 'p']
remain.p <- ppois(q = last.x - 1, lambda = param_poiss_fire_2003$estimate, lower.tail = FALSE)
data[data['Fire_Levels'] == last.x, 'p'] <- remain.p
data <- data[ , c('Count', 'p')]
invalid_Np <- with(data, sum(Count) * p)
invalid_Np <- invalid_Np > 5
invalid_Np <- sum(invalid_Np = FALSE)
if(invalid_Np == 0){
  
  result <- with(data, chisq.test(Count, p = p)) # Avant correction
  
  x2obs <- result[['statistic']] # Après correction
  df <- result[['parameter']] - 1 # -1 degré de liberté car il y a eu 1 paramètre estimé.
  p.value <- pchisq(x2obs, df = 5, lower.tail = FALSE)
  
} else {
  cat("Au moins 1 Np n'est pas supérieur à 5")
}

print(result)
print(p.value)

if(p.value < 0.05){
  answer <- paste('Il y a rejet de H0, car', p.value, 'est inférieur à', 0.05, '. Donc,', H1)
} else {
  answer <- paste('Il y a non rejet de H0, car', p.value, 'est supérieur à', 0.05, '. Donc,', H0)
}
answer



#garantie other pour 2003
#on teste pour chaque garantie si la loi de fréquence suit une loi de poisson pour l'année 2003
H0="la loi de fréquence pour la garantie Other suit une loi de poisson"
H1="la loi de fréquence pour la garantie Other ne suit pas une loi de poisson"
# Compter le nombre de sinistres pour chaque niveau de dommage en 2003
count_other_2003 <- table(merged_data_2003$Other)

# Extraire les niveaux de dommage (0, 1, 2)
other_levels <- as.numeric(names(count_other_2003))

# Convertir le résultat en vecteur
vec_count_other_2003 <- as.vector(count_other_2003)

# Afficher les niveaux de dommage et le vecteur
print("Niveaux de dommage en 2003:")
print(other_levels)
print("Vecteur du nombre de sinistres par niveau de dommage en 2003:")
print(vec_count_other_2003)
# Créer une data frame avec les niveaux de dommage et le vecteur de compte en 2003
df_other_2003 <- data.frame(Other_Levels = other_levels, Count = vec_count_other_2003)

# Afficher la data frame
print("Data frame pour les sinistres en 2003:")
print(df_other_2003)
x <- with(df_other_2003, rep(other_levels,vec_count_other_2003 ))
p <- dpois(x, lambda = param_poiss_other_2003$estimate)
p <- unique(p)
data <- cbind(df_other_2003, p)
last.x <- max(data[ , 'Other_Levels' ])
last.p <- data[data['Other_Levels'] == last.x, 'p']
remain.p <- ppois(q = last.x - 1, lambda = param_poiss_other_2003$estimate, lower.tail = FALSE)
data[data['Other_Levels'] == last.x, 'p'] <- remain.p
data <- data[ , c('Count', 'p')]
invalid_Np <- with(data, sum(Count) * p)
invalid_Np <- invalid_Np > 5
invalid_Np <- sum(invalid_Np = FALSE)
if(invalid_Np == 0){
  
  result <- with(data, chisq.test(Count, p = p)) # Avant correction
  
  x2obs <- result[['statistic']] # Après correction
  df <- result[['parameter']] - 1 # -1 degré de liberté car il y a eu 1 paramètre estimé.
  p.value <- pchisq(x2obs, df = 5, lower.tail = FALSE)
  
} else {
  cat("Au moins 1 Np n'est pas supérieur à 5")
}

print(result)
print(p.value)

if(p.value < 0.05){
  answer <- paste('Il y a rejet de H0, car', p.value, 'est inférieur à', 0.05, '. Donc,', H1)
} else {
  answer <- paste('Il y a non rejet de H0, car', p.value, 'est supérieur à', 0.05, '. Donc,', H0)
}
answer


#loi de fréquence pour la garantie theft pour l'année 2003
#on teste pour chaque garantie si la loi de fréquence suit une loi de poisson pour l'année 2003
H0="la loi de fréquence pour la garantie Theft suit une loi de poisson"
H1="la loi de fréquence pour la garantie Theft ne suit pas une loi de poisson"
# Compter le nombre de sinistres pour chaque niveau de dommage en 2003
count_theft_2003 <- table(merged_data_2003$Theft)

# Extraire les niveaux de dommage (0, 1, 2)
theft_levels <- as.numeric(names(count_theft_2003))

# Convertir le résultat en vecteur
vec_count_theft_2003 <- as.vector(count_theft_2003)

# Afficher les niveaux de dommage et le vecteur
print("Niveaux de dommage en 2003:")
print(theft_levels)
print("Vecteur du nombre de sinistres par niveau de dommage en 2003:")
print(vec_count_theft_2003)
# Créer une data frame avec les niveaux de dommage et le vecteur de compte en 2003
df_theft_2003 <- data.frame(Theft_Levels = theft_levels, Count = vec_count_theft_2003)

# Afficher la data frame
print("Data frame pour les sinistres en 2003:")
print(df_theft_2003)
x <- with(df_theft_2003, rep(theft_levels,vec_count_theft_2003 ))
p <- dpois(x, lambda = param_poiss_theft_2003$estimate)
p <- unique(p)
data <- cbind(df_theft_2003, p)
last.x <- max(data[ , 'Theft_Levels' ])
last.p <- data[data['Theft_Levels'] == last.x, 'p']
remain.p <- ppois(q = last.x - 1, lambda = param_poiss_theft_2003$estimate, lower.tail = FALSE)
data[data['Theft_Levels'] == last.x, 'p'] <- remain.p
data <- data[ , c('Count', 'p')]
invalid_Np <- with(data, sum(Count) * p)
invalid_Np <- invalid_Np > 5
invalid_Np <- sum(invalid_Np = FALSE)
if(invalid_Np == 0){
  
  result <- with(data, chisq.test(Count, p = p)) # Avant correction
  
  x2obs <- result[['statistic']] # Après correction
  df <- result[['parameter']] - 1 # -1 degré de liberté car il y a eu 1 paramètre estimé.
  p.value <- pchisq(x2obs, df = 5, lower.tail = FALSE)
  
} else {
  cat("Au moins 1 Np n'est pas supérieur à 5")
}

print(result)
print(p.value)

if(p.value < 0.05){
  answer <- paste('Il y a rejet de H0, car', p.value, 'est inférieur à', 0.05, '. Donc,', H1)
} else {
  answer <- paste('Il y a non rejet de H0, car', p.value, 'est supérieur à', 0.05, '. Donc,', H0)
}
answer

#GARANTIE TPL
#on teste pour chaque garantie si la loi de fréquence suit une loi de poisson pour l'année 2003
H0="la loi de fréquence pour la garantie TPL suit une loi de poisson"
H1="la loi de fréquence pour la garantie TPL ne suit pas une loi de poisson"
# Compter le nombre de sinistres pour chaque niveau de dommage en 2003
count_TPL_2003 <- table(merged_data_2003$TPL)

# Extraire les niveaux de dommage (0, 1, 2)
TPL_levels <- as.numeric(names(count_TPL_2003))

# Convertir le résultat en vecteur
vec_count_TPL_2003 <- as.vector(count_TPL_2003)

# Afficher les niveaux de dommage et le vecteur
print("Niveaux de dommage en 2003:")
print(TPL_levels)
print("Vecteur du nombre de sinistres par niveau de dommage en 2003:")
print(vec_count_TPL_2003)
# Créer une data frame avec les niveaux de dommage et le vecteur de compte en 2003
df_TPL_2003 <- data.frame(TPL_Levels = TPL_levels, Count = vec_count_TPL_2003)

# Afficher la data frame
print("Data frame pour les sinistres en 2003:")
print(df_TPL_2003)
x <- with(df_TPL_2003, rep(TPL_levels, vec_count_TPL_2003))
p <- dpois(x, lambda = param_poiss_TPL_2003$estimate)
p <- unique(p)
data <- cbind(df_TPL_2003, p)
last.x <- max(data[, 'TPL_Levels'])
last.p <- data[data['TPL_Levels'] == last.x, 'p']
remain.p <- ppois(q = last.x - 1, lambda = param_poiss_TPL_2003$estimate, lower.tail = FALSE)
data[data['TPL_Levels'] == last.x, 'p'] <- remain.p
data <- data[, c('Count', 'p')]
invalid_Np <- with(data, sum(Count) * p)
invalid_Np <- invalid_Np > 5
invalid_Np <- sum(invalid_Np = FALSE)
if (invalid_Np == 0) {
  
  result <- with(data, chisq.test(Count, p = p)) # Avant correction
  
  x2obs <- result[['statistic']] # Après correction
  df <- result[['parameter']] - 1 # -1 degré de liberté car il y a eu 1 paramètre estimé.
  p.value <- pchisq(x2obs, df = 5, lower.tail = FALSE)
  
} else {
  cat("Au moins 1 Np n'est pas supérieur à 5")
}

print(result)
print(p.value)

if (p.value < 0.05) {
  answer <- paste('Il y a rejet de H0, car', p.value, 'est inférieur à', 0.05, '. Donc,', H1)
} else {
  answer <- paste('Il y a non rejet de H0, car', p.value, 'est supérieur à', 0.05, '. Donc,', H0)
}
answer


#Garantie windscreen
#on teste pour chaque garantie si la loi de fréquence suit une loi de poisson pour l'année 2003
H0="la loi de fréquence pour la garantie windscreen suit une loi de poisson"
H1="la loi de fréquence pour la garantie windscreen ne suit pas une loi de poisson"
# Compter le nombre de sinistres pour chaque niveau de dommage en 2003
count_windscreen_2003 <- table(merged_data_2003$Windscreen)

# Extraire les niveaux de dommage (0, 1, 2)
windscreen_levels <- as.numeric(names(count_windscreen_2003))

# Convertir le résultat en vecteur
vec_count_windscreen_2003 <- as.vector(count_windscreen_2003)

# Afficher les niveaux de dommage et le vecteur
print("Niveaux de dommage en 2003:")
print(windscreen_levels)
print("Vecteur du nombre de sinistres par niveau de dommage en 2003:")
print(vec_count_windscreen_2003)
# Créer une data frame avec les niveaux de dommage et le vecteur de compte en 2003
df_windscreen_2003 <- data.frame(windscreen_Levels = windscreen_levels, Count = vec_count_windscreen_2003)

# Afficher la data frame
print("Data frame pour les sinistres en 2003:")
print(df_windscreen_2003)
x <- with(df_windscreen_2003, rep(windscreen_levels, vec_count_windscreen_2003))
p <- dpois(x, lambda = param_poiss_windscreen_2003$estimate)
p <- unique(p)
data <- cbind(df_windscreen_2003, p)
last.x <- max(data[, 'windscreen_Levels'])
last.p <- data[data['windscreen_Levels'] == last.x, 'p']
remain.p <- ppois(q = last.x - 1, lambda = param_poiss_windscreen_2003$estimate, lower.tail = FALSE)
data[data['windscreen_Levels'] == last.x, 'p'] <- remain.p
data <- data[, c('Count', 'p')]
invalid_Np <- with(data, sum(Count) * p)
invalid_Np <- invalid_Np > 5
invalid_Np <- sum(invalid_Np = FALSE)
if (invalid_Np == 0) {
  
  result <- with(data, chisq.test(Count, p = p)) # Avant correction
  
  x2obs <- result[['statistic']] # Après correction
  df <- result[['parameter']] - 1 # -1 degré de liberté car il y a eu 1 paramètre estimé.
  p.value <- pchisq(x2obs, df = 5, lower.tail = FALSE)
  
} else {
  cat("Au moins 1 Np n'est pas supérieur à 5")
}

print(result)
print(p.value)

if (p.value < 0.05) {
  answer <- paste('Il y a rejet de H0, car', p.value, 'est inférieur à', 0.05, '. Donc,', H1)
} else {
  answer <- paste('Il y a non rejet de H0, car', p.value, 'est supérieur à', 0.05, '. Donc,', H0)
}
answer







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
