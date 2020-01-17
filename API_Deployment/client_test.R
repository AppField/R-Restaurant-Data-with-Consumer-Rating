library(httr)
library(rjson)
test = data.frame(smoker = c("true", "false", "false"), 
                  drink_level = c("casual drinker", "abstemious", "social drinker"),
                  budget = c("medium", "low", "medium"),
                  birth_year = c(1990, 1990, 1985),
                  Rcuisine = c("Asian", "Persian", "European"))

str = toJSON(test)
content(POST("http://127.0.0.1:8080/regression", body = str, encode = "json"))


