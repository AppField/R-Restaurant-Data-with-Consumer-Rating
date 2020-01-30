## Expose prediction model as web service
library(plyr)
library("caret")
library("class")
library("e1071")
library("randomForest")
library(tidyverse)
library(rjson)
load("model.rda")

cuisine <- read.csv('Data Science Spezialisierung/Project/R-Restaurant-Data-with-Consumer-Rating/data/chefmozcuisine.csv')
rating <- read.csv('./data/rating_final.csv')
userprofile <- read.csv('./data/userprofile.csv', na.strings = "?")

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

decode <- function(s)
{
    df <- as.data.frame(fromJSON(s$postBody))
    df$smoker <- factor(df$smoker, levels = levels(data_rating$smoker))
    df$budget <- factor(df$budget, levels = levels(data_rating$budget))
    df$drink_level <- factor(df$drink_level, levels = levels(data_rating$drink_level))
    df$Rcuisine <- factor(df$Rcuisine, levels = levels(data_rating$Rcuisine))
    
    df
}

#* @post /regression
#* @json
function(req)
{
    as.character(try(predict(m, decode(req))))
}

## add more interfaces, if needed -- e.g., for other models.
