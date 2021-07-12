setwd("C:/Users/enerc/OneDrive/Desktop/data science/sessions/r_training")
getwd()

library(dplyr)
library(caTools)
library(rpart)


pokemon <- read.csv("Pokemon.csv", stringsAsFactors = T)

pokemon %>% select(-1) -> pokemon

#Renaming columns
colnames(pokemon)[2] <- "Primary_Type"
colnames(pokemon)[3] <- "Secondary_Type"
colnames(pokemon)[5] <- "Health_Points"
colnames(pokemon)[8] <- "Special_Attack"
colnames(pokemon)[9] <- "Special_Defence"


# A) Selecting a Pokémon 
# a. Select a Pokémon whose primary type is "Grass", secondary type is "Poison" 
# with the maximum speed 
# b. Select a Pokémon whose primary type is "Water", secondary type is "Flying" 
# with the maximum speed 
# c. Select a Pokémon whose primary type is "Fire", secondary type is "Psychic" 
# with the maximum speed 


#Selecting a grass pokemon
pokemon %>% filter(Primary_Type == "Grass") -> grass_pokemon
grass_pokemon %>% filter(Secondary_Type == "Poison") -> grass_poison_pokemon
range(grass_poison_pokemon$Speed)
grass_poison_pokemon %>% filter(Speed == 90) -> my_grass_pokemon

#Selecting a Water pokemon
pokemon %>% filter(Primary_Type == "Water") -> water_pokemon
water_pokemon %>% filter(Secondary_Type == "Flying") -> water_flying_pokemon
range(water_flying_pokemon$Speed)
water_flying_pokemon %>% filter(Speed == 98) -> my_water_pokemon

#selecting a Fire pokemon

pokemon %>% filter(Primary_Type == "Fire") -> Fire_pokemon
Fire_pokemon %>% filter(Secondary_Type == "Psychic") -> fire_psychic_pokemon
range(fire_psychic_pokemon$Speed)
fire_psychic_pokemon %>% filter(Speed == 104) -> my_fire_pokemon

rbind(my_fire_pokemon,my_water_pokemon,my_grass_pokemon) -> my_pokemon

# B) Attack vs. Defence 
# a. Divide the data-set into train & test sets 
# b. Build a linear model on train set where independent variable is 'Defence' & 
#   Dependent variable is 'Attack'
# c. Predict the values on the 'test' set
# d. Find the root mean square error

sample.split(pokemon$Attack,SplitRatio = 0.70) -> split_pokemon
subset(pokemon,split_pokemon==T) -> train
subset(pokemon,split_pokemon==F) -> test

#Building the model
lm(Attack~Defense,data=train) -> pokemon_mod

#predicting the values
predict(pokemon_mod,newdata = test) -> predicting_pokemon

data.frame(Acutal=test$Attack,Predicted=predicting_pokemon) -> pokemon_final_data
View(pokemon_final_data)

#root mean square error
(pokemon_final_data$Acutal-pokemon_final_data$Predicted) -> pokemon_final_data$error
sqrt(mean(pokemon_final_data$error^2))



# C) Legendary or not 
# a. Divide the data-set into train & test sets 
# b. Build a decision tree on train set where dependent variable is 'Legendary' & 
#   all other columns are independent variables 
# c. Predict the values on the 'test' set
# d. Find the accuracy by making a confusion matrix

sample.split(pokemon$Legendary, SplitRatio = 0.65) -> split_legendary
subset(pokemon,split_legendary==T) -> train_legendary
subset(pokemon,split_legendary==F) -> test_legendary

rpart(Legendary~., data = train_legendary) -> legendary_mod
predict(legendary_mod, newdata = test_legendary, type = "class") -> predicted_legendary

table(test_legendary$Legendary,predicted_legendary)

(253+9)/(253+4+14+9) -> acc

acc

