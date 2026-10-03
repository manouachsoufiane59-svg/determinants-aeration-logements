## Sujet 2 ##
# 1. Definissons le chemin d'accès

setwd("/Users/richardlarri/Desktop/Rendu Operationnel_Econometrie ")

# Installons et chargons les packages 

install.packages("readxl")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("stargazer")
install.packages("pROC")
install.packages("margins")
install.packages("lmtest")
install.packages("car")

library(readr)
library(tidyverse)    
library(ggplot2)      
library(stargazer) 
library(pROC)         
library(margins)      
library(lmtest)       
library(car)      

# Importons la base de donnée 
data <- read.csv("data_air.csv", stringsAsFactors = FALSE, na.strings = "NA")

# Vérification de la structure de la base

str(data)
dim(data)     # On a dans notre base de donnée 200 observations et 10 variables
head(data, 5)


# 2. NETTOYAGE ET PRÉPARATION DES DONNÉES

#  2.1 Diagnostic des valeurs manquantes 

Val_manq <- colSums(is.na(data))
print(Val_manq)   # On a 3 valeurs manquantes dans nuisance_sonore et une valeur dans aere_matin

# 2.2 Traitement des valeurs aberrantes

# on remarque que la variable sensib_froid est codée de 0 à 10 et observation n°17 présente une valeur de 12 ce qui est hors de l'échelle (0 à 10).

data$sensib_froid[data$sensib_froid > 10] <- NA

# Vérification
summary(data$sensib_froid)

# 2.3 Encodage des variables A

data$type_logement <- factor(data$type_logement,
                             levels = c("Appartement", "Maison"))

# Variables binaires explicatives

data$presence_enfant <- as.factor(data$presence_enfant)
data$membre_asthme   <- as.factor(data$membre_asthme)

# Variable dépendante : conservée numérique pour le glm()
data$aere_matin <- as.numeric(data$aere_matin)

# 2.4 Supprimons les observations incomplètes 

Data_clean <- na.omit(data)
nrow(Data_clean)         # on obtient comme éffectif final : 193 observations

# Vérification finale de notre nouvelle base de donée néttoyée (data_clean)
colSums(is.na(Data_clean))

# Vérification de la structure
str(Data_clean)
summary(Data_clean)


# 3. ANALYSE STATISTIQUE DESCRIPTIVE

#  3.1 Variable dépendante 

# Taux d'aération quotidienne dans l'échantillon

prop.table(table(Data_clean$aere_matin)) * 100   # on constate que 52.85% de notre échantillon aèrent leurs logement tandis que 47.15 % n’aèrent pas.

# 3.2 Variables quantitatives

Vari_quanti <- c("nuisance_sonore", "revenu_foyer", "sensib_froid",
                "surface_m2", "connaissance_cov")

summary(Data_clean[, Vari_quanti])

# 3.3 Analyse bivariée : aération selon les groupes

# Taux d'aération moyen selon la présence d'enfants
tapply(Data_clean$aere_matin, Data_clean$presence_enfant, mean) * 100 # 23% des ménages sans enfants aérent quotidienement leurs logements, Parmi les ménages avec enfant, 95 % aèrent.

# Taux d'aération moyen selon la présence d'asthmatiques
tapply(Data_clean$aere_matin, Data_clean$membre_asthme, mean) * 100  # 45,1 % des ménages sans membre asthmatique aèrent quotidiennement leur logement. 96,6 % des ménages avec au moins un membre asthmatique aèrent quotidiennement leur logement.

# Taux d'aération moyen selon le type de logement
tapply(Data_clean$aere_matin, Data_clean$type_logement, mean) * 100. # 22,6 % des ménages vivant en appartement aèrent quotidiennement leur logement. 97,4 % des ménages vivant en maison aèrent quotidiennement leur logement.


#  3.4 Corrélations entre variables quantitatives 

Cor_matrix <- cor(Data_clean[, Vari_quanti], use = "complete.obs")
round(Cor_matrix, 2)


# 4. VISUALISATIONS GRAPHIQUES

#  4.1 Distribution de nuisance_sonore selon l'aération 

ggplot(Data_clean, aes(x = nuisance_sonore, fill = factor(aere_matin))) +
  geom_histogram(position = "dodge", binwidth = 1, color = "white") +
  scale_fill_manual(values = c("#D73027", "#4575B4"),
                    labels = c("Non (0)", "Oui (1)"),
                    name = "Aère quotidiennement") +
  labs(title = "Distribution du bruit extérieur selon l'aération quotidienne",
       x = "Niveau de nuisance sonore (0-10)", y = "Effectif") +
  theme_minimal()

# 4.2 Revenu du foyer et probabilité d'aération 

ggplot(Data_clean, aes(x = revenu_foyer, y = aere_matin)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"),
              se = TRUE, color = "#4575B4") +
  labs(title = "Probabilité d'aération selon le revenu du foyer",
       x = "Revenu mensuel net (€)", y = "P(aérer quotidiennement)") +
  theme_minimal()

# 4.3 Sensibilité au froid et probabilité d'aération 

ggplot(Data_clean, aes(x = sensib_froid, y = aere_matin)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"),
              se = TRUE, color = "#D73027") +
  labs(title = "Probabilité d'aération selon la sensibilité au froid",
       x = "Score de sensibilité au froid (0-10)", y = "P(aérer quotidiennement)") +
  theme_minimal()

# 4.4 Connaissance des COV selon le type de logement 

ggplot(Data_clean, aes(x = type_logement, y = connaissance_cov, fill = type_logement)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("#74ADD1", "#ABD9E9")) +
  labs(title = "Connaissance des COV selon le type de logement",
       x = "Type de logement", y = "Score de connaissance COV (0-5)") +
  theme_minimal() + theme(legend.position = "none")



# 5. CONSTRUCTION DU MODÈLE LOGIT

# La variable dépendante aere_matin est binaire (0/1).
# Le modèle Logit est approprié : il modélise la probabilité qu'un ménage aère quotidiennement P(Y=1|X) via la fonction logistique.

# Sélection des variables explicatives fondée sur le corpus documentaire :
#
#  - nuisance_sonore  : Frein environnemental direct.
#                       Signe attendu : (-) — plus le bruit est fort, moins on ouvre.
#  - revenu_foyer     : Contrainte budgétaire.
#                       Signe attendu : (+) — les ménages aisés aèrent davantage.
#  - presence_enfant  : Facteur de motivation sanitaire.
#                       Signe attendu : (+) — présence d'enfants → attention à la santé.
#  - sensib_froid     : Mesure directe de l'arbitrage thermique.
#                       Signe attendu : (-) — forte réticence → moins d'aération.
#  - surface_m2       : Proxy du confort du logement.
#                       Signe attendu : (+) — surface plus grande → ventilation facilitée.
#  - membre_asthme    : Motivation sanitaire spécifique.
#                       Signe attendu : (+) — problème respiratoire → comportement préventif.
#  - connaissance_cov : Information sur les risques — levier clé de politique publique.
#                       Signe attendu : (+) — mieux informé → plus susceptible d'aérer.
#  - type_logement    : Maison vs Appartement — structure différente d'exposition.
#                       Signe attendu : Maison (+) par rapport à Appartement.

# 5.1 Estimation du modèle Logit 

Modele_logit <- glm(aere_matin ~ nuisance_sonore + revenu_foyer + presence_enfant +
                      sensib_froid + surface_m2 + membre_asthme +
                      connaissance_cov + type_logement,
                    data   = Data_clean,
                    family = binomial(link = "logit"))

summary(Modele_logit)
#presence_enfant1 est significative (p < 0.05)  au risque de première espèce (pvalue) = 0.037 ,La présence d'au moins un enfant de moins de 10 ans augmente significativement la probabilité d'aérer. 
#sensib_froid  est significative (p < 0.05)  au risque de première espèce (pvalue) = 0.025 .Chaque point supplémentaire sur l'échelle de sensibilité au froid réduit significativement la probabilité d'aérer.
#connaissance_cov  est significative (p < 0.05)  au risque de première espèce (pvalue) = 0.027. Chaque point de connaissance des COV augmente fortement la probabilité d'aérer. 
#type_logementMaison est significative (p < 0.05)  au risque de première espèce (pvalue) = 0.030. Vivre en maison plutôt qu'en appartement est associé à une très forte augmentation de la probabilité d'aérer. 


# 5.2 Affichage tableau propre avec stargazer 

stargazer(Modele_logit,
          type  = "text",
          title = "Déterminants de l'aération quotidienne - Modèle Logit",
          dep.var.labels = "Aération quotidienne (oui = 1)",
          covariate.labels = c("Nuisance sonore", "Revenu du foyer (€)",
                               "Présence d'enfant (réf. : Non)",
                               "Sensibilité au froid",
                               "Surface habitable (m²)",
                               "Membre asthmatique (réf. : Non)",
                               "Connaissance des COV",
                               "Type logement : Maison (réf. : Appartement)"),
          digits = 3,
          no.space = TRUE)


# 6. INTERPRÉTATION DES RÉSULTATS

# 6.1 Odds ratios (exponentielle des coefficients) 

OR <- exp(coef(Modele_logit))
IC <- exp(confint(Modele_logit))
round(cbind(OR = OR, IC), 3)

# Interprétation des résultats obtenus :
# - sensib_froid       : OR = 0.368 : chaque point supplémentaire sur l'échelle de
#                        sensibilité au froid réduit les chances d'aérer de 63 %.
# - connaissance_cov   : OR = 8.184 :chaque point de connaissance des COV multiplie
#                        par 8 les chances d'aérer.
# - presence_enfant1   : OR = 6.857 : la présence d'enfants multiplie par 7
#                        les chances d'aérer.
# - type_logementMaison: OR = 25.825 : vivre en maison vs appartement multiplie
#                        par 26 les chances d'aérer.

# 6.2 Effets marginaux moyens (AME) 

Ame <- margins(Modele_logit)
summary(Ame)

# Interprétation des résultats des résultats obtenus :

# - connaissance_cov   : AME = +0.128 : 1 point de connaissance COV augmente
#                        la probabilité d'aérer de 12.8 points de pourcentage.
# - sensib_froid       : AME = -0.061 : 1 point de sensibilité au froid diminue
#                        la probabilité d'aérer de 6.1 points de pourcentage.
# - type_logementMaison: AME = +0.405 : vivre en maison augmente la probabilité
#                        d'aérer de 40.5 points de pourcentage (significatif à 10%).



# 7. QUALITÉ ET VALIDATION DU MODÈLE

#  7.1 Pseudo R² de McFadden 

Log_vrais_modele  <- logLik(Modele_logit)
Modele_nul <- glm(aere_matin ~ 1, data = Data_clean, family = binomial)
Log_vrai_mod_nul     <- logLik(Modele_nul)

Pseudo_r2_mcfadden <- 1 - (as.numeric(Log_vrais_modele) / as.numeric(Log_vrai_mod_nul))
cat("Pseudo R² de McFadden :", round(Pseudo_r2_mcfadden, 4), "\n")

# Résultat obtenu : 0.6827, notre modèle est bien ajusté.

# 7.2 Critères AIC et BIC 

AIC(Modele_logit) # 103.6545
BIC(Modele_logit) # 133.0187

# 7.3 Test du rapport de vraisemblance (LR test) 

lrtest(Modele_logit, Modele_nul)

#  7.4 Matrice de confusion et taux de bonne classification 

# On classe chaque observation selon que la probabilité prédite dépasse 0.5.
Probabilites_predites <- predict(Modele_logit, type = "response")
Predictions_binaires  <- ifelse(Probabilites_predites >= 0.5, 1, 0)

Matrice_confusion <- table(Observé = Data_clean$aere_matin,
                           Prédit   = Predictions_binaires)
print(Matrice_confusion)
# 86 vrais négatifs : ménages qui n'aèrent pas et que le modèle prédit correctement comme non-aérants.
# 94 vrais positifs : ménages qui aèrent et que le modèle prédit correctement.
# 5 faux positifs : ménages qui n'aèrent pas mais que le modèle prédit à tort comme aérants.
# 8 faux négatifs : ménages qui aèrent mais que le modèle prédit à tort comme non-aérants.
# Le modèle se trompe sur 13 observations seulement sur 193.

# Taux de bonne classification globale
Taux_classif <- sum(diag(Matrice_confusion)) / sum(Matrice_confusion)
cat("Taux de bonne classification :", round(Taux_classif * 100, 1), "%\n")

#Taux de bonne classification = 93.3 % : (86 + 94) / 193 = 0.933. Le modèle prédit correctement le comportement de 9 ménages sur 10. C'est cohérent avec les autres indicateurs de qualité.

#  7.5 Courbe ROC et AUC 

# L'AUC (Area Under the Curve) mesure la capacité discriminante du modèle.
# Valeurs de référence : 0.5 = aléatoire, 0.7-0.8 = acceptable, > 0.8 = bon.
Courbe_roc <- roc(Data_clean$aere_matin, Probabilites_predites, quiet = TRUE)
plot(Courbe_roc, main = "Courbe ROC - Modèle Logit ANQAI",
     col = "#4575B4", lwd = 2)
abline(a = 0, b = 1, lty = 2, col = "gray")
cat("AUC :", round(auc(Courbe_roc), 4), "\n")

# AUC = 0.955, très proche de 1. Cela confirme les corrélations très fortes entre les variables et aere_matin.

#  7.6 Diagnostic de multicolinéarité (VIF) 

Modele_lm_vif <- lm(aere_matin ~ nuisance_sonore + revenu_foyer + presence_enfant +
                      sensib_froid + surface_m2 + membre_asthme +
                      connaissance_cov + type_logement,
                    data = Data_clean)
vif(Modele_lm_vif)


# 8. MODÈLE RÉDUIT

Modele_reduit <- glm(aere_matin ~ sensib_froid + connaissance_cov +
                       presence_enfant + membre_asthme + type_logement,
                     data   = Data_clean,
                     family = binomial(link = "logit"))

summary(Modele_reduit)

# Comparaison des deux modèles par LR test

lrtest(Modele_reduit, Modele_logit)

# Comparaison AIC : le modèle avec l'AIC le plus bas est préféré.
cat("AIC - Modèle complet :", AIC(Modele_logit), "\n") #  103.6545 

cat("AIC - Modèle réduit  :", AIC(Modele_reduit), "\n") # 103.3004 

# L'AIC du modèle réduit est légèrement inférieur (103.30 < 103.65). Combiné au LR test non significatif, cela confirme que le modèle réduit est préférable : il est plus simple, plus parcimonieux, et offre un meilleur compromis ajustement/complexité. 


