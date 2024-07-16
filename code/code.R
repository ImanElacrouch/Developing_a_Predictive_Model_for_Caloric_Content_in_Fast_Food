# Remove all variables in the global environment
rm(list = ls())

library(carData)

library(ggplot2)

calories <- read.csv("C:/Users/User/Downloads/Calories.csv")

names(calories)=c(
  "Fast Food Restaurant",
  "Type",
  "ServingSize",
  "Calories",
  "TotalFat",
  "SaturatedFat",
  "TransFat",
  "Sodium",
  "Carbs",
  "Sugars",
  "Protein"
)

str(calories)

calories<-calories[,3:11]

str(calories)

any(is.na(calories))==TRUE

which(is.na(calories))
is.na(calories)

calories$TransFat[which(is.na(calories$TransFat))]<-mean(calories$TransFat,na.rm = TRUE)

any(is.na(calories$TransFat))==TRUE

any(is.na(calories))==TRUE

nrow(calories)

head(calories, 5)

summary(calories)

calories$Sodiumg=calories$Sodium/1000

head(calories)

#représentations unidimensionnelles des données
options(repr.plot.width=1, repr.plot.height=2)
boxplot(calories[,3:4]) # boîtes par groupe
boxplot(calories[,5:6])
boxplot(calories[,7:9])

#corrélations entre les variables
pairs(calories)


model1<-lm(Calories~ServingSize+TotalFat+SaturatedFat+TransFat+Sodiumg+Carbs+Sugars+Protein, data=calories)

summary(model1)

#On retire les variables non significatives
#On enlève Sugars car elle a une p-valeur de 0.89

model2<-lm(Calories~ServingSize+TotalFat+SaturatedFat+TransFat+Sodiumg+Carbs+Protein, data=calories)

summary(model2)

#On enlève SaturatedFat car elle a une p-valeur de 0.91

model3<-lm(Calories~ServingSize+TotalFat+TransFat+Sodiumg+Carbs+Protein, data=calories)

summary(model3)

#On enlève ServingSize car elle a une p-valeur de 0.06

model4<-lm(Calories~TotalFat+TransFat+Sodiumg+Carbs+Protein, data=calories)

summary(model4)

#On enlève Sodiumg car elle a une p-valeur de 0.08

model5<-lm(Calories~TotalFat+TransFat+Carbs+Protein, data=calories)

summary(model5)

model6<-lm(formula = Calories ~ TotalFat + TransFat + Carbs + Protein - 1, data = calories)
summary(model6)

# Nouvelles données pour la prédiction
nouvelles_donnees <- data.frame(
  TotalFat = 13,  
  TransFat = 0.5,     
  Carbs = 32,       
  Protein = 15      
)

# Prédiction avec le nouveau modèle (model5 dans cet exemple)
prediction <- predict(model6, nouvelles_donnees)

# Affichage de la prédiction
cat("La prédiction de Calories pour les nouvelles données est :", round(prediction, digits = 2), "\n")

####################################

alpha <- 0.05
n <- dim(calories)[1]
p <- 5

# Calcul du levier avec le modèle6
leviers <- hat(model.matrix(model6))
seuil_levier <- 2 * p / n

# Visualisation des leviers
ggplot(data = data.frame(obs = 1:n, levier = leviers), aes(x = obs, y = levier)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_hline(yintercept = seuil_levier, col = "red") +
  theme_minimal() +
  xlab("Observation") +
  ylab("Leviers") +
  scale_x_continuous(breaks = seq(0, n, by = 5))

# Calcul des résidus studentisés
analyses <- data.frame(obs = 1:n)
analyses$rstudent <- rstudent(model6)
seuil_rstudent <- qt(1 - alpha / 2, n - length(coef(model6)))


# Visualisation des résidus studentisés
ggplot(data = analyses, aes(x = obs, y = rstudent)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_hline(yintercept = c(-seuil_rstudent, seuil_rstudent), col = "red", linetype = "dashed") +
  theme_minimal() +
  xlab("Observation") +
  ylab("Résidus Studentisés") +
  scale_x_continuous(breaks = seq(0, n, by = 5))

# Calcul des distances de Cook
influence <- influence.measures(model6)
analyses$dcook <- influence$infmat[, "cook.d"]
seuil_dcook <- 4 / (n - length(coef(model5)))

# Visualisation des distances de Cook
ggplot(data = analyses, aes(x = obs, y = dcook)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_hline(yintercept = seuil_dcook, col = "red") +
  theme_minimal() +
  xlab("Observation") +
  ylab("Distance de Cook") +
  scale_x_continuous(breaks = seq(0, n, by = 5))

#Vérification de la colinéarité des variables
library(car)
vif(model5)



#Teste d’homoscédasticité
library(lmtest)
bptest(model5)


#Test de la normalité des résidus
shapiro.test(model5$residuals)
qqnorm(model5$residuals)
qqline(model5$residuals)


## Ellipse de confiance
library(ellipse)
i=0
j=1
resume <- summary(model5)
plot(ellipse(model5,c(i+1,j+1),level=0.95,type="l",xlab=paste("beta",i,sep=""), ylab=paste("beta",j,sep=""), ylim=c(9,18), xlim=c(9,18))) # 
points(coef(resume)[i+1],coef(resume)[j+1],pch=3)

## Sélectionner automatiquement un modèle avec l'ensemble des variables
reg_null <- lm(Calories ~ 1, data = calories)
reg_tot <- lm(Calories ~ ServingSize + TotalFat + SaturatedFat + TransFat + Sodiumg + Carbs + Sugars + Protein, data = calories)
reg_backward <- step(reg_tot, direction = "backward")
