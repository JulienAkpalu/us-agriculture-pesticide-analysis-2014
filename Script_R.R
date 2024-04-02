# Set the working directory

setwd("C:/Users/julien/Desktop/KAGGLE/Pesticide_Use_Agriculture")

#####installer les packages nécessaires

install.packages("readr")
library(readr)

## Importation des données

mydata <- read.csv2("mydata.csv",header=TRUE, sep=",", dec=".")
summary(mydata)

# Afficher les premières lignes des données
head(mydata)

# Afficher la liste des variabes
names(mydata)

# Afficher la liste des variabes

View(mydata)


# Calculer le nombre de différents états
num_states <- length(unique(mydata$STATE_CODE))

# Afficher le résultat
print(num_states)


# Calculer la somme totale des estimations basses et hautes en ignorant les valeurs NA
total_low_estimate <- sum(mydata$LOW_ESTIMATE, na.rm = TRUE)
total_high_estimate <- sum(mydata$HIGH_ESTIMATE, na.rm = TRUE)

# Afficher les résultats
print(paste("Total Low Estimate: ", total_low_estimate))
print(paste("Total High Estimate: ", total_high_estimate))


# Utilisation des pesticides dans le "state 1"
specific_state_code <- 1 # Remplacez X par le code de l'état réel 

# Filtrer les données pour cet état spécifique
state_data <- subset(mydata, STATE_CODE == specific_state_code)

# Calculer la somme totale des estimations basses et hautes pour cet état
total_low_estimate_state <- sum(state_data$LOW_ESTIMATE, na.rm = TRUE)
total_high_estimate_state <- sum(state_data$HIGH_ESTIMATE, na.rm = TRUE)

# Afficher les résultats
print(paste("Total Low Estimate for State", specific_state_code, ": ", total_low_estimate_state))
print(paste("Total High Estimate for State", specific_state_code, ": ", total_high_estimate_state))

# Number of different pesticides used in 2014

# Filtrer les données pour l'année 2014
data_2014 <- subset(mydata, YEAR == 2014)

# Compter le nombre de pesticides uniques utilisés en 2014
num_pesticides_2014 <- length(unique(data_2014$COMPOUND))

# Afficher le résultat
print(paste("Number of different pesticides used in 2014: ", num_pesticides_2014))

# Top 10 pesticides and the states where they are most commonly found

# Aggréger l'utilisation des pesticides (choisir soit LOW_ESTIMATE soit HIGH_ESTIMATE)
pesticide_usage <- aggregate(cbind(LOW_ESTIMATE, HIGH_ESTIMATE) ~ COMPOUND, mydata, sum)

# Trier pour obtenir les 10 pesticides les plus utilisés
top_pesticides <- head(pesticide_usage[order(-pesticide_usage$LOW_ESTIMATE), ], 10)

# Trouver les états où ces pesticides sont les plus utilisés
most_common_states <- list()
for (pesticide in top_pesticides$COMPOUND) {
  state_usage <- aggregate(cbind(LOW_ESTIMATE, HIGH_ESTIMATE) ~ STATE_CODE, subset(mydata, COMPOUND == pesticide), sum)
  most_common_state <- state_usage[which.max(state_usage$LOW_ESTIMATE), "STATE_CODE"]
  most_common_states[[pesticide]] <- most_common_state
}

# Afficher les résultats
print("Top 10 pesticides and the states where they are most commonly found:")
print(most_common_states)

# Bottom 10 pesticides and the states where they are least commonly found

# Aggréger l'utilisation des pesticides (choisir soit LOW_ESTIMATE soit HIGH_ESTIMATE)
pesticide_usage <- aggregate(cbind(LOW_ESTIMATE, HIGH_ESTIMATE) ~ COMPOUND, mydata, sum)

# Trier pour obtenir les 10 pesticides les moins utilisés
bottom_pesticides <- head(pesticide_usage[order(pesticide_usage$LOW_ESTIMATE), ], 10)

# Trouver les états où ces pesticides sont les moins utilisés
least_common_states <- list()
for (pesticide in bottom_pesticides$COMPOUND) {
  state_usage <- aggregate(cbind(LOW_ESTIMATE, HIGH_ESTIMATE) ~ STATE_CODE, subset(mydata, COMPOUND == pesticide), sum)
  least_common_state <- state_usage[which.min(state_usage$LOW_ESTIMATE), "STATE_CODE"]
  least_common_states[[pesticide]] <- least_common_state
}

# Afficher les résultats
print("Bottom 10 pesticides and the states where they are least commonly found:")
print(least_common_states)

# Install and load necessary libraries
if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)

# Prepare data for top and bottom pesticides



# Liste des 10 pesticides les plus utilisés (remplacez par vos résultats réels)
top_pesticides_list <- c("Glyphosate", "Atrazine", "Sulfur", "Petroleum Oil", "Dichloropropene", 
                         "Metolachlor-S", "Acetochlor", "2,4-D", "Metam", "Metam Potassium")

# Liste des 10 pesticides les moins utilisés (remplacez par vos résultats réels)
bottom_pesticides_list <- c("Resmethrin", "Folpet", "Gallex", "Nosema Locustae Cann", "Uniconazole", 
                            "Diphenamid", "Triforine", "Fenoxycarb", "Silicates", "Parathion")

# Créer top_data avec la colonne Pesticide_Type
top_data <- aggregate(LOW_ESTIMATE ~ STATE_CODE, mydata[mydata$COMPOUND %in% top_pesticides_list, ], sum)
top_data$Pesticide_Type <- 'Top'

# Créer bottom_data avec la colonne Pesticide_Type
bottom_data <- aggregate(LOW_ESTIMATE ~ STATE_CODE, mydata[mydata$COMPOUND %in% bottom_pesticides_list, ], sum)
bottom_data$Pesticide_Type <- 'Bottom'

# Combiner les données en conservant la colonne Pesticide_Type
top_bottom_data <- rbind(top_data, bottom_data)




# Maintenant, vous pouvez exécuter le code ggplot pour la visualisation


# Ensuite, vous pouvez exécuter votre code ggplot

library(ggplot2)

# Utiliser ggplot pour visualiser les données
library(ggplot2)

# Utiliser ggplot pour visualiser les données
ggplot(top_bottom_data, aes(x = STATE_CODE, y = LOW_ESTIMATE, fill = Pesticide_Type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("Top" = "blue", "Bottom" = "red")) +
  labs(title = "Top and Bottom Pesticides Use by State",
       x = "State Code",
       y = "Total Pesticide Use (Low Estimate)",
       fill = "Pesticide Type") +
  theme_minimal()
names(top_bottom_data)