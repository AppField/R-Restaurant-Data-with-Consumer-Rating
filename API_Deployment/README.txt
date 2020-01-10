This example demonstrates how to deploy a simple forecasting model as a
RESTful web service.

* model.R:

small script to create a simple regression model, stored in file
"model.rda".

* server.R:

This defines the services (actually: resources) as anonymous R functions
and their URLs (along with the http method).

* server_start.R:

commands to be executed in an R session to start the plumber web server.

This exposes the service at endpoint: http://127.0.0.1:8080/regression

* client_test.R:

simple test commands to test the API. Basically, creates JSON objects from the
x values and sends them to the endpoints.

Instructions for testing:

- put all files in one directory
- start an R session, execute the commands in server_start.R
- start a second R session, try the commands in client_test.R

