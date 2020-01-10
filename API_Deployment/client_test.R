library(httr)
library(rjson)
test = data.frame(x = c(1, 99, 500))

str = toJSON(test)
content(POST("http://127.0.0.1:8080/regression", body = str, encode = "json"))

content(POST("http://127.0.0.1:8080/regression", body = "bla", encode = "json"))


