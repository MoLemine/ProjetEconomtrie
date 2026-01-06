
# PROJET D'ÉCONOMÉTRIE AVANCÉE 
# Master SSD – Université de Nouakchott

# I. Préparation de l'environnement de travail

# Installation et chargement des bibliothèques nécessaires
packages <- c("readxl", "plm", "dplyr", "psych", "stargazer")
install.packages(packages)
lapply(packages, library, character.only = TRUE)


# II. Chargement et inspection des données

# Vérification du répertoire de travail
getwd()

# Importation du fichier Excel
base_donnees <- read_excel("Panal data.xlsx")

# Aperçu général des données
head(base_donnees)
str(base_donnees)


# III. Structuration en données de panel

# Définition de la structure individuelle et temporelle (Question 1)
panel_data <- pdata.frame(base_donnees, index = c("firm", "year"))


# IV. Analyse descriptive initiale

# Statistiques globales des variables principales
summary(panel_data)

# Statistiques descriptives détaillées (Question 2)
describe(panel_data[, c("Y", "X1", "X2")]) 

# V. Moyennes temporelles par entreprise (Question 3)
moyennes_par_firme <- panel_data %>%
  group_by(firm) %>%
  summarise(
    Y_moy  = mean(Y, na.rm = TRUE),
    X1_moy = mean(X1, na.rm = TRUE),
    X2_moy = mean(X2, na.rm = TRUE)
  )
movennes_par_firme <- as.data.frame(moyennes_par_firme)
movennes_par_firme


# VI. Estimation du modèle poolé (OLS classique) (Question 4)
modele_pooling <- plm(
  Y ~ X1 + X2,
  data = panel_data,
  model = "pooling"
)

summary(modele_pooling)


# VII. Estimation du modèle à effets fixes (Within) (Question 6)
modele_within <- plm(
  Y ~ X1 + X2,
  data = panel_data,
  model = "within"
)

summary(modele_within)


# VIII. Analyse des effets fixes individuels (Question 7)
effets_individuels <- fixef(modele_within)
print(effets_individuels)

# IX. Test de significativité des effets fixes (Fisher) (Question 8)
test_F <- pFtest(modele_within, modele_pooling)
print(test_F)


# X. Estimation du modèle à effets aléatoires (Random) (Question 11)
modele_random <- plm(
  Y ~ X1 + X2,
  data = panel_data,
  model = "random"
)

summary(modele_random)


# XI. Extraction du paramètre theta (Question 12)
theta_random <- ercomp(modele_random)$theta
print(theta_random)


# XII. Test de Breusch-Pagan pour les effets individuels (Question 13)
test_BP <- plmtest(modele_pooling, type = "bp")
print(test_BP)


# XIII. Test de Hausman : effets fixes vs aléatoires (Question 14)
test_H <- phtest(modele_within, modele_random)
print(test_H)


# XIV. TABLEAU COMPARATIF (OPTIONNEL)
stargazer(
  modele_pooling,
  modele_within,
  modele_random,
  type = "text",
  title = "Comparaison des estimations sur données de panel",
  column.labels = c("Pooling", "Effets fixes", "Effets aléatoires"),
  dep.var.labels = "Variable dépendante : Y"
)

# save .RData file (Optionnel)
save.image("econometrie_avancee_panel_data.RData")

# Fin du script



