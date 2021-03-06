library("caret")
library("class")
library("e1071")
library("randomForest")
library(tidyverse)

cuisine <- read.csv('../data/chefmozcuisine.csv')
rating <- read.csv('../data/rating_final.csv')
userprofile <- read.csv('../data/userprofile.csv', na.strings = "?")

## Transforming Levels
cuisine$Rcuisine = revalue(cuisine$Rcuisine, c("Japanese"="Asian", "Chinese"="Asian",
                                               "Sushi"="Asian", "Korean"="Asian",
                                               "Mongolian"="Asian", "Thai"="Asian",
                                               "Asia"="Asian", "Vietnamese"="Asian",
                                               "Deli-Sandwiches"="Asian"))

cuisine$Rcuisine = revalue(cuisine$Rcuisine, c("Dutch-Belgian"="European",
                                               "Continental-European"="European",
                                               "Eastern_European"="European",
                                               "Greek"="European",
                                               "Spanish"="European", "French"="European",
                                               "German"="European", "Italian"="European",
                                               "Polish"="European", "Pizzeria"="European",
                                               "Dessert-Ice_Cream"="European", 
                                               "Seafood"="European"))

cuisine$Rcuisine = revalue(cuisine$Rcuisine, c("Ethiopian"="African",
                                               "African"="African"))

cuisine$Rcuisine = revalue(cuisine$Rcuisine, c("Barbecue"="American",
                                               "Hot_Dogs"="American",
                                               "Steaks"="American",
                                               "American"="American",
                                               "Fast_Food"="American",
                                               "Burgers"="American",
                                               "California"="American",
                                               "Southwestern"="American",
                                               "Game"="American",
                                               "Diner"="American"))

cuisine$Rcuisine = revalue(cuisine$Rcuisine, c("Persian"="Persian",
                                               "Mediterranean"="Persian",
                                               "Turkish"="Persian",
                                               "Afghan"="Persian",
                                               "Armenian"="Persian"))

cuisine$Rcuisine = revalue(cuisine$Rcuisine, c("Brazilian"="South_American",
                                               "Caribbean"="South_American",
                                               "Southern"="South_American",
                                               "Mexican"="South_American",
                                               "Latin_American"="South_American"))

cuisine$Rcuisine = revalue(cuisine$Rcuisine, c("Bar"="International",
                                               "Contemporary"="International",
                                               "Fine_Dining"="International",
                                               "Vegetarian"="International",
                                               "Bakery"="International",
                                               "Cafe-Coffee_Shop"="International",
                                               "Organic-Healthy"="International",
                                               "Juice"="International",
                                               "Soup"="International",
                                               "Bagels"="International",
                                               "Bar_Pub_Brewery"="International",
                                               "Breakfast-Brunch"="International",
                                               "Cafeteria"="International",
                                               "Family"="International",
                                               "Regional"="International"))


rating_user_detail <- rating %>% 
  join(userprofile) %>% 
  join(cuisine)

data_rating <- rating_user_detail %>% 
  mutate(rating = as.factor(rating)) %>% 
  select(rating , smoker, drink_level, budget, birth_year, Rcuisine) # without food_rating, service_rating

N = nrow(data_rating)
train_ind = sample(1: N, size = N * 2 / 3)
train_rating = data_rating[train_ind,]
scaler = preProcess(train_rating)
train_rating_scaled = predict(scaler, train_rating)

train_rating_scaled_na_free <- na.omit(train_rating_scaled)

m = randomForest(rating ~ ., data = train_rating_scaled_na_free, ntree = 50)
save(m, file = "model.rda")

m
