# load the required packages
library(shiny)
require(shinydashboard)
library(plyr)
library(gplots)
library(RColorBrewer)
library(ggplot2)
library(ggmosaic)
library(ggrepel)
library(leaflet)
library("caret")
library("class")
library("e1071")
library("rpart")
library("randomForest")
library(nnet)
library(tidyverse)
library(corrplot)

# DATA
accept <- read.csv('../data/chefmozaccepts.csv')
cuisine <- read.csv('../data/chefmozcuisine.csv')
hours <- read.csv('../data/chefmozhours4.csv')
parking <- read.csv('../data/chefmozparking.csv')
geoplaces <- read.csv('../data/geoplaces2.csv', na.strings = "?")
rating <- read.csv('../data/rating_final.csv')
usercuisine <- read.csv('../data/usercuisine.csv')
userpayment <- read.csv('../data/userpayment.csv')
userprofile <- read.csv('../data/userprofile.csv', na.strings = "?")

usercuisine$Rcuisine = revalue(usercuisine$Rcuisine, c("Japanese"="Asian", "Chinese"="Asian",
                                                       "Sushi"="Asian", "Korean"="Asian",
                                                       "Mongolian"="Asian", "Thai"="Asian",
                                                       "Asia"="Asian", "Vietnamese"="Asian",
                                                       "Deli-Sandwiches"="Asian",
                                                       "Southeast_Asian"="Asian",
                                                       "Burmese"="Asian", "Cambodian"="Asian", 
                                                       "Malaysian"="Asian", "Dim_Sum"="Asian", "Indonesian"="Asian"))

usercuisine$Rcuisine = revalue(usercuisine$Rcuisine, c("Dutch-Belgian"="European",
                                                       "Continental-European"="European",
                                                       "Eastern_European"="European",
                                                       "Greek"="European",
                                                       "Spanish"="European", "French"="European",
                                                       "German"="European", "Italian"="European",
                                                       "Polish"="European", "Pizzeria"="European",
                                                       "Dessert-Ice_Cream"="European", 
                                                       "Seafood"="European",
                                                       "British"="European",
                                                       "Irish"="European",
                                                       "Swiss"="European",
                                                       "Filipino"="European", "Austrian"="European", "Hungarian"="European",
                                                       "Portuguese"="European", "Romanian"="European", "Basque"="European",
                                                       "Scandinavian"="European", "Russian-Ukrainian"="European"))

usercuisine$Rcuisine = revalue(usercuisine$Rcuisine, c("Ethiopian"="African",
                                                       "African"="African",
                                                       "North_African"="African",
                                                       "Israeli"="African",
                                                       "Jamaican"="African", "Lebanese"="African", "Tibetan"="African", "Tunisian"="African", 
                                                       "Middle_Eastern"="African", "Moroccan"="African"))

usercuisine$Rcuisine = revalue(usercuisine$Rcuisine, c("Barbecue"="American",
                                                       "Hot_Dogs"="American",
                                                       "Steaks"="American",
                                                       "American"="American",
                                                       "Fast_Food"="American",
                                                       "Burgers"="American",
                                                       "California"="American",
                                                       "Southwestern"="American",
                                                       "Game"="American",
                                                       "Diner"="American",
                                                       "Doughnuts"="American",
                                                       "Pacific_Northwest"="American",
                                                       "Cajun-Creole"="American",
                                                       "Pacific_Rim"="American",
                                                       "Canadian"="American",
                                                       "Hawaiian"="American", "Indigenous"="American"))

usercuisine$Rcuisine = revalue(usercuisine$Rcuisine, c("Persian"="Persian",
                                                       "Mediterranean"="Persian",
                                                       "Turkish"="Persian",
                                                       "Afghan"="Persian",
                                                       "Armenian"="Persian",
                                                       "Indian-Pakistani"="Persian"))

usercuisine$Rcuisine = revalue(usercuisine$Rcuisine, c("Brazilian"="South_American",
                                                       "Caribbean"="South_American",
                                                       "Southern"="South_American",
                                                       "Mexican"="South_American",
                                                       "Latin_American"="South_American", "Peruvian"="South_American",
                                                       "Tapas"="South_American", "Tex-Mex"="South_American",
                                                       "Chilean"="South_American", "Cuban"="South_American"))

usercuisine$Rcuisine = revalue(usercuisine$Rcuisine, c("Bar"="International",
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
                                                       "Regional"="International",
                                                       "Eclectic"="International", "Fusion"="International",
                                                       "Tea_House"="International", 
                                                       "Australian"="International",
                                                       "Kosher"="International", "Polynesian"="International"))

#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Customer-Rating Dashboard")  

#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem( "Daten", tabName = "data", icon = icon("list-alt",lib='glyphicon'),
                  menuSubItem("Overview", tabName = "data_overview", icon = icon("blackboard",lib='glyphicon')),
                  menuSubItem("Restaurant", tabName = "restaurant", icon = icon("cutlery",lib='glyphicon')),
                  menuSubItem("User", tabName = "user", icon = icon("user",lib='glyphicon')),
                  menuSubItem("Rating", tabName = "rating", icon = icon("equalizer",lib='glyphicon'))),
        menuItem( "Machine Learning", tabName = "ml", icon = icon("random",lib='glyphicon'),
                  menuSubItem("Overview", tabName = "pred_overview", icon = icon("blackboard",lib='glyphicon')),
                  menuSubItem("Predicting Rating", tabName = "pred_rating", icon = icon("equalizer",lib='glyphicon')),
                  menuSubItem("Predicting Cuisine", tabName = "pred_cuisine", icon = icon("cutlery",lib='glyphicon')))
    )
)

#BODY
body <- dashboardBody(
    tabItems(
        tabItem(tabName = "dashboard",
                h2("Dashboard Customer - Rating"),
                fluidRow(
                    valueBoxOutput("datasets"),
                    valueBoxOutput("dataset_rows"),
                    valueBoxOutput("ml_method_rating"),
                ),
                fluidRow(
                    box(
                        title = "Beschreibung Daten",
                        status = "success",
                        collapsible = TRUE, 
                        "In dem Projekt wurden Daten von Restaurant Bewertungen ausgewerten.
                        Die herangezogenen Daten stammen von KundInnen und Restaurants in Mexiko.
                        Die verschiedenen Datensätze lassen sich unterteilen in:", br(),
                        " - Restaurant-Daten", br(),
                        " - Kunden - Daten", br(),
                        " - Rating - Daten", br(), br(),
                        "(Für einen detailierteren Überblick über die Daten zu der Daten-Ansicht wechseln)"
                    ),
                    box(
                        title = "Beschreibung Machine Learning Modelle",
                        status = "success",
                        collapsible = TRUE, 
                        "Für diese Daten wurden zwei Modelle entwickelt. Eines sagt das Rating von einem Kunden vorher
                        aufgrund von verschienen Attributen des Kunden.", br(),
                        "Das zweite Modell sagt das die bevorzugte Küche eines Kunden vorher.", br(),
                        "Des Weiteren wurde versucht das Rating aufgrund von Restaurant Attributen vorherzusagen, dies ergab jedoch
                        kein aussagekräftiges Modell und wurde somit nicht weiter verfolgt.", br(), br(),
                        "(Für einen detailierteren Überblick über die Modelle zu der Machine Learning-Ansicht wechseln)"
                    )
                ),
                fluidRow(
                    box(
                        title = "Service Rating ~ Food Rating",
                        status = "primary",
                        collapsible = TRUE, 
                        plotOutput("rating_lm", height = "300px")
                    ),
                    box(
                        title = "Anzahl der Restaurants je Küche",
                        status = "primary", 
                        collapsible = TRUE, 
                        plotOutput("count_cuisine", height = "300px")
                    ) 
                ),
        ),
        tabItem(tabName = "data_overview",
                 h2("Data tab content"),
                 box(
                      title = "Inputs", 
                      status = "primary",
                      sliderInput("slider", "Slider input:", 1, 100, 50),
                      textInput("text", "Text input:")
                  ),
                tabBox(
                      title = "Daten",
                      # The id lets us use input$tabset1 on the server to find the current tab
                      id = "tab_overview_data", height = "300px",
                      tabPanel("Tab2", "Tab content 2"),
                      tabPanel("Tab2", "Tab content 2")
                  ),
                tabBox(
                    title = "Machine Learning",
                    # The id lets us use input$tabset1 on the server to find the current tab
                    id = "tab_overview_data", height = "300px",
                    tabPanel("Tab1", "First tab content"),
                    tabPanel("Tab2", "Tab content 2")
                )
        ),
        tabItem(tabName = "restaurant",
                h2("Data tab content"),
                box(
                    title = "Inputs", 
                    status = "primary",
                    sliderInput("slider", "Slider input:", 1, 100, 50),
                    textInput("text", "Text input:")
                )
        ),
        tabItem(tabName = "user",
                h2("user tab content")
        ),
        tabItem(tabName = "rating",
                h2("rating tab content")
        ),
        tabItem(tabName = "pred_overview",
                h2("rating pred tab content")
        ),
        tabItem(tabName = "pred_rating",
                h2("rating pred tab content")
        ),
        tabItem(tabName = "pred_cuisine",
                h2("predcuisine tab content")
        )
    )
)

#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'Customer - Rating Dashboard', header, sidebar, body, skin='blue')

# create the server functions for the dashboard  
server <- function(input, output) { 
    
    #some data manipulation to derive the values of KPI boxes
    #total.revenue <- sum(recommendation$Revenue)
    #sales.account <- recommendation %>% group_by(Account) %>% summarise(value = sum(Revenue)) %>% filter(value==max(value))
    #prof.prod <- recommendation %>% group_by(Product) %>% summarise(value = sum(Revenue)) %>% filter(value==max(value))
    
    
    # Dashboard Tab
    output$datasets <- renderValueBox({
        valueBox(
            formatC("9", format="d", big.mark=','),
            paste('Datensets'),
            icon = icon("list-alt",lib='glyphicon'),
            color = "purple")
    })
    
    output$dataset_rows <- renderValueBox({
        valueBox(
            formatC(nrow(rating), format="d", big.mark=','),
            'Datensätze (Rating)',
            icon = icon("list",lib='glyphicon'),
            color = "green")
    })
    
    output$ml_method_rating <- renderValueBox({
        valueBox(
            formatC(nrow(userprofile), format="d", big.mark=','),
            paste('Datensätze (User)'),
            # paste('Datensätze (User):',rating$rating),
            icon = icon("list",lib='glyphicon'),
            color = "yellow")
    })
    
    output$rating_lm <- renderPlot({
        ggplot(rating, aes(food_rating, service_rating)) +  
            geom_smooth(method = "lm")
    })
    
    output$count_cuisine <- renderPlot({
        ggplot(usercuisine, aes(x = Rcuisine, fill=Rcuisine)) + geom_bar()
    })
}


shinyApp(ui, server)