library(plumber)
ws = plumb("server.R")
ws$run(host="127.0.0.1", port = 8080)
