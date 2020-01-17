library(httr)
library(rjson)
test = data.frame(smoker = c(TRUE, FALSE, FALSE), 
                  drink_level = c("casual drinker", "abstemious", "social drinker"),
                  budget = c("medium", "low", "medium"),
                  birth_year = c(1950, 1990, 1985),
                  Rcuisin = c("Asian", "Persian", "European"))

str = toJSON(test)
content(POST("http://127.0.0.1:8080/rf", body = str, encode = "json"))


