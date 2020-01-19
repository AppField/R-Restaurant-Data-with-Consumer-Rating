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

cuisine$Rcuisine = revalue(cuisine$Rcuisine, c("Japanese"="Asian", "Chinese"="Asian","Sushi"="Asian", "Korean"="Asian","Mongolian"="Asian", "Thai"="Asian","Asia"="Asian", "Vietnamese"="Asian", "Deli-Sandwiches"="Asian","Southeast_Asian"="Asian","Burmese"="Asian", "Cambodian"="Asian", "Malaysian"="Asian", "Dim_Sum"="Asian", "Indonesian"="Asian"))
cuisine$Rcuisine = revalue(cuisine$Rcuisine, c("Dutch-Belgian"="European","Continental-European"="European","Greek"="European","Spanish"="European", "French"="European","German"="European", "Italian"="European","Polish"="European", "Pizzeria"="European","Dessert-Ice_Cream"="European", "Seafood"="European","British"="European","Irish"="European","Swiss"="European","Filipino"="European", "Austrian"="European", "Hungarian"="European","Portuguese"="European", "Romanian"="European", "Basque"="European", "Scandinavian"="European", "Russian-Ukrainian"="European"))
cuisine$Rcuisine = revalue(cuisine$Rcuisine, c("Ethiopian"="African","African"="African","North_African"="African","Israeli"="African", "Jamaican"="African", "Lebanese"="African", "Tibetan"="African", "Tunisian"="African", "Middle_Eastern"="African", "Moroccan"="African"))
cuisine$Rcuisine = revalue(cuisine$Rcuisine, c("Barbecue"="American","Hot_Dogs"="American","Steaks"="American", "American"="American","Fast_Food"="American","Burgers"="American","California"="American", "Southwestern"="American","Game"="American","Diner"="American","Doughnuts"="American","Pacific_Northwest"="American","Cajun-Creole"="American","Pacific_Rim"="American","Canadian"="American","Hawaiian"="American", "Indigenous"="American"))
cuisine$Rcuisine = revalue(cuisine$Rcuisine, c("Persian"="Persian", "Mediterranean"="Persian","Turkish"="Persian","Afghan"="Persian","Armenian"="Persian","Indian-Pakistani"="Persian"))
cuisine$Rcuisine = revalue(cuisine$Rcuisine, c("Brazilian"="South_American","Caribbean"="South_American", "Southern"="South_American","Mexican"="South_American","Latin_American"="South_American", "Peruvian"="South_American","Tapas"="South_American", "Tex-Mex"="South_American","Chilean"="South_American", "Cuban"="South_American"))
cuisine$Rcuisine = revalue(cuisine$Rcuisine, c("Bar"="International","Contemporary"="International", "Fine_Dining"="International","Vegetarian"="International", "Bakery"="International","Cafe-Coffee_Shop"="International","Organic-Healthy"="International","Juice"="International","Soup"="International","Bagels"="International", "Bar_Pub_Brewery"="International","Breakfast-Brunch"="International", "Cafeteria"="International","Family"="International","Regional"="International","Eclectic"="International", "Fusion"="International","Tea_House"="International", "Australian"="International","Kosher"="International", "Polynesian"="International"))

usercuisine$Rcuisine = revalue(usercuisine$Rcuisine, c("Japanese"="Asian", "Chinese"="Asian","Sushi"="Asian", "Korean"="Asian","Mongolian"="Asian", "Thai"="Asian","Asia"="Asian", "Vietnamese"="Asian", "Deli-Sandwiches"="Asian","Southeast_Asian"="Asian","Burmese"="Asian", "Cambodian"="Asian", "Malaysian"="Asian", "Dim_Sum"="Asian", "Indonesian"="Asian"))
usercuisine$Rcuisine = revalue(usercuisine$Rcuisine, c("Dutch-Belgian"="European","Continental-European"="European","Greek"="European","Spanish"="European", "French"="European","German"="European", "Italian"="European","Polish"="European", "Pizzeria"="European","Dessert-Ice_Cream"="European", "Seafood"="European","British"="European","Irish"="European","Swiss"="European","Filipino"="European", "Austrian"="European", "Hungarian"="European","Portuguese"="European", "Romanian"="European", "Basque"="European", "Scandinavian"="European", "Russian-Ukrainian"="European"))
usercuisine$Rcuisine = revalue(usercuisine$Rcuisine, c("Ethiopian"="African","African"="African","North_African"="African","Israeli"="African", "Jamaican"="African", "Lebanese"="African", "Tibetan"="African", "Tunisian"="African", "Middle_Eastern"="African", "Moroccan"="African"))
usercuisine$Rcuisine = revalue(usercuisine$Rcuisine, c("Barbecue"="American","Hot_Dogs"="American","Steaks"="American", "American"="American","Fast_Food"="American","Burgers"="American","California"="American", "Southwestern"="American","Game"="American","Diner"="American","Doughnuts"="American","Pacific_Northwest"="American","Cajun-Creole"="American","Pacific_Rim"="American","Canadian"="American","Hawaiian"="American", "Indigenous"="American"))
usercuisine$Rcuisine = revalue(usercuisine$Rcuisine, c("Persian"="Persian", "Mediterranean"="Persian","Turkish"="Persian","Afghan"="Persian","Armenian"="Persian","Indian-Pakistani"="Persian"))
usercuisine$Rcuisine = revalue(usercuisine$Rcuisine, c("Brazilian"="South_American","Caribbean"="South_American", "Southern"="South_American","Mexican"="South_American","Latin_American"="South_American", "Peruvian"="South_American","Tapas"="South_American", "Tex-Mex"="South_American","Chilean"="South_American", "Cuban"="South_American"))
usercuisine$Rcuisine = revalue(usercuisine$Rcuisine, c("Bar"="International","Contemporary"="International", "Fine_Dining"="International","Vegetarian"="International", "Bakery"="International","Cafe-Coffee_Shop"="International","Organic-Healthy"="International","Juice"="International","Soup"="International","Bagels"="International", "Bar_Pub_Brewery"="International","Breakfast-Brunch"="International", "Cafeteria"="International","Family"="International","Regional"="International","Eclectic"="International", "Fusion"="International","Tea_House"="International", "Australian"="International","Kosher"="International", "Polynesian"="International"))

#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Customer-Rating Dashboard")  

# https://rstudio.github.io/shinydashboard/appearance.html#statuses-and-colors

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
                h2("Data Overview"),
                fluidRow(
                    column(width = 12,
                           box(title = "Beschreibung", 
                               status = "success", 
                               solidHeader = FALSE,
                               width = 2.4,
                               "In den untenstehenden Grafiken befinden sich ausgewählte Überblicke über die jeweiligen Datengruppen.", br(), br(),
                               "Unter 'Geo Locations' befindet sich eine Map in der alle Kunden und Restaurants eingezeichnet sind. In dem Tab 'Restaurant'
                               wird veranschaulicht welche Angebote die einzelnen Küchen der Bereiche Alkohol, Rauchen und Preis haben.
                               Unter 'Kunden' befindet sich eine Diagramm, welches die das Budget mit dem Trinkverhalten in Verbindung setzt.
                               In dem Tab 'Rating' befindet sich eine Veranschaulichung der Beziehung von Food-Rating und Service-Rating in anbetracht der verschiedenen Küchen."),
                           tabBox(
                               title = "Daten",
                               id = "tab_overview_data", height = "500px",
                               tabPanel("Geo Locations", leafletOutput("shared_location_leaflet"),),
                               tabPanel("Restaurant", plotOutput("restaurant_ballon")),
                               tabPanel("Kunden", plotOutput("kunden_drinking_budget")),
                               tabPanel("Rating", plotOutput("rating_overview_grouped_cuisine")),
                               width = 2.4
                            ),
                    )
                ),
        ),
        tabItem(tabName = "restaurant",
                h2("Restaurant Daten"),
                fluidRow(
                    valueBoxOutput("datasets_restaurant"),
                    valueBoxOutput("count_restaurants"),
                    valueBoxOutput("count_cuisines"),
                ),
                fluidRow(
                    box(
                        title = "Beschreibung", 
                        status = "success",
                        "Die Restaurant Daten setzen sich zusammen aus:", br(),
                        " - Geodata", br(),
                        " - Payment Acceptance", br(),
                        " - Parking Possibilities", br(),
                        " - Cuisine Variations", br(),
                        " - Working Hours"
                    ),
                    box(
                        title = "Filter Optionen", 
                        status = "success",
                        selectInput("select_dia_restaurant", "Diagramm Möglichkeiten", c("Verteilung-Cuisines", "Parking-Optionen", "Preisklassen"), selected = "Verteilung-Cuisines", multiple = FALSE,
                                    selectize = TRUE, width = NULL, size = NULL),
                        selectInput("select_cuisine_restaurant", "Cuisine (Außer bei Verteilung-Cuisines)", c("Alle", "Persian","American", "Asian","International", "South_American European", "European", "African"), selected = "Alle", multiple = FALSE,
                                    selectize = TRUE, width = NULL, size = NULL)
                    ),
                ),
                fluidRow(
                    column(width = 12,
                           box(title = "Diagramm", 
                               status = "primary", 
                               solidHeader = FALSE,
                               width = 2.4,
                               plotOutput("restaurant_detail_data")
                        )
                    ),
                ),
        ),
        tabItem(tabName = "user",
                h2("User Daten"),
                fluidRow(
                    valueBoxOutput("datasets_user"),
                    valueBoxOutput("count_users"),
                    valueBoxOutput("count_cuisines_user"),
                ),
                fluidRow(
                    box(
                        title = "Beschreibung", 
                        status = "success",
                        "Die User Daten setzen sich zusammen aus:", br(),
                        " - usercuisine", br(),
                        " - Payment userpayment", br(),
                        " - userprofile"
                    ),
                    box(
                        title = "Filter Optionen", 
                        status = "success",
                        selectInput("select_dia_user", "Diagramm Möglichkeiten", c("Verteilung-Cuisines", "Parking-Optionen"), selected = "Verteilung-Cuisines", multiple = FALSE,
                                    selectize = TRUE, width = NULL, size = NULL),
                        selectInput("select_cuisine_user", "Cuisine", c("Alle", "Persian","American", "Asian","International", "South_American European", "European", "African"), selected = "Alle", multiple = FALSE,
                                    selectize = TRUE, width = NULL, size = NULL)
                    ),
                ),
                
        ),
        tabItem(tabName = "rating",
                h2("Rating Daten"),
                ""
        ),
        tabItem(tabName = "pred_overview",
                h2("Overview: Predictions"),
                fluidRow(
                    column(width = 12,
                           box(title = "Beschreibung", 
                               status = "success", 
                               solidHeader = FALSE,
                               width = 2.4,
                               "In dem Projekt wurden zwei Modelle entwickelt um das Rating und die Cuisine von einem Kunden vorherzusagen.", br(),
                               "Für diese Modelle wurden verschiedene Methoden herangezogen. Diese daraus entstandenen Modelle wurden getunen und im folgenden gefittet mit Trainingsdaten.
                               Die verwendeten Datensätze wurden jeweils in Trainings- und Testdaten aufgeteilt im Verhältnis 1:2. Die Testdaten wurden daraufhin herangezogen, um die Performance
                               der Modelle miteinander zu vergleichen."
                           )
                    ),
                ),
                fluidRow(
                    box(
                        title = "Predicting Rating",
                        status = "primary",
                        collapsible = TRUE, 
                        "ASdasd"
                    ),
                    box(
                        title = "Predicting Cuisine",
                        status = "primary", 
                        collapsible = TRUE,
                        "sad"
                    ) 
                ),
        ),
        tabItem(tabName = "pred_rating",
                h2("Predicting Rating"),
                fluidRow(
                    valueBoxOutput("methods_rating"),
                    valueBoxOutput("best_acc_rating"),
                    valueBoxOutput("selected_method_rating"),
                ),
                fluidRow(
                    column(width = 12,
                           box(title = "Beschreibung", 
                               status = "success", 
                               solidHeader = FALSE,
                               width = 2.4,
                               "In dem Projekt wurden zwei Modelle entwickelt um das Rating und die Cuisine von einem Kunden vorherzusagen.", br(),
                               "Für diese Modelle wurden verschiedene Methoden herangezogen. Diese daraus entstandenen Modelle wurden getunen und im folgenden gefittet mit Trainingsdaten.
                               Die verwendeten Datensätze wurden jeweils in Trainings- und Testdaten aufgeteilt im Verhältnis 1:2. Die Testdaten wurden daraufhin herangezogen, um die Performance
                               der Modelle miteinander zu vergleichen."
                           )
                    ),
                ),
                fluidRow(
                    box(
                        title = "Service Rating ~ Food Rating",
                        status = "primary",
                        collapsible = TRUE,
                    ),
                    box(
                        title = "Anzahl der Restaurants je Küche",
                        status = "primary", 
                        collapsible = TRUE,
                    ) 
                ),
        ),
        tabItem(tabName = "pred_cuisine",
                h2("Predicting Cuisine"),
                fluidRow(
                    valueBoxOutput("methods_cuisine"),
                    valueBoxOutput("best_acc_cuisine"),
                    valueBoxOutput("selected_method_cuisine"),
                ),
                fluidRow(
                    column(width = 12,
                           box(title = "Beschreibung", 
                               status = "success", 
                               solidHeader = FALSE,
                               width = 2.4,
                               "In dem Projekt wurden zwei Modelle entwickelt um das Rating und die Cuisine von einem Kunden vorherzusagen.", br(),
                               "Für diese Modelle wurden verschiedene Methoden herangezogen. Diese daraus entstandenen Modelle wurden getunen und im folgenden gefittet mit Trainingsdaten.
                               Die verwendeten Datensätze wurden jeweils in Trainings- und Testdaten aufgeteilt im Verhältnis 1:2. Die Testdaten wurden daraufhin herangezogen, um die Performance
                               der Modelle miteinander zu vergleichen."
                           )
                    ),
                ),
                fluidRow(
                    box(
                        title = "Service Rating ~ Food Rating",
                        status = "primary",
                        collapsible = TRUE,
                    ),
                    box(
                        title = "Anzahl der Restaurants je Küche",
                        status = "primary", 
                        collapsible = TRUE,
                    ) 
                ),
        )
    )
)

#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'Customer - Rating Dashboard', header, sidebar, body, skin='blue')

# create the server functions for the dashboard  
server <- function(input, output) { 
    # Leaf let Icons
    customers = makeIcon("../user_icon.png", 50, 50)
    restaurants = makeIcon("../restaurant_icon.png", 50, 50)
    
    dt <- data_frame(Rcuisine = c("Persian","American", "Asian","International", "South_American European", "European"),
                     Alc_Full_Bar = c(0,0,0,1,1,0),
                     Alc_No_Alcohol_Served = c(1,1,1,1,1,1),
                     Alc_Wine_Beer = c(0,0,1,1,1,1),
                     smoking_none = c(1,1,1,1,1,1),
                     smoking_not_permitted = c(0,1,0,1,1,1),
                     smoking_only_at_bar = c(0,0,0,0,1,0),
                     smoking_permitted = c(0,0,1,1,1,0),
                     smoking_section = c(0,1,1,1,1,1),
                     price_high = c(0,1,1,1,1,1),
                     price_low = c(1,1,0,1,1,1),
                     price_medium = c(0,1,1,1,1,1))
    dt <- column_to_rownames(dt_dist, 'Rcuisine')
    balloonplot_data_restaurant <- as.table(as.matrix(dt))
    
    user_detail <- userprofile %>% 
        join(usercuisine) %>% 
        join(userpayment)
    
    rating_detailed <- rating %>% 
        inner_join(cuisine) %>% 
        inner_join(geoplaces) %>%
        arrange(placeID) %>% 
        select(placeID, rating, food_rating, service_rating, name, Rcuisine)
    
    rating_detailed %>% 
        group_by(placeID, name, Rcuisine) %>% 
        summarize(rating = mean(rating),
                  food_rating = mean(food_rating),
                  service_rating = mean(food_rating))
    
    detailed_price <- geoplaces %>% 
        join(cuisine)
    
    # Dashboard Tab
    output$datasets <- renderValueBox({
        valueBox(
            formatC("9", format="d", big.mark=','),
            paste('Datasets'),
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
    
    # Daten Overview Tab
    output$shared_location_leaflet <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%  # Add default OpenStreetMap map tiles
            addMarkers(lng=userprofile$longitude, lat=userprofile$latitude, popup=userprofile$userID, icon = customers) %>% 
            addMarkers(lng = geoplaces$longitude, lat = geoplaces$latitude, icon = restaurants, popup = geoplaces$name)
    })
    
    output$restaurant_ballon <- renderPlot({
        balloonplot(t(balloonplot_data_restaurant), main ="Distribution Smoking, Alcohol, Pricing Grouped By the Cuisines", xlab ="", ylab="",
                    label = FALSE, show.margins = FALSE, colsrt=90, rowmar=5, colmar=10)
    })
    
    output$kunden_drinking_budget <- renderPlot({
        ggplot(user_detail) + geom_mosaic(aes(product(drink_level,budget), fill = drink_level)) +
            ggtitle("Distribution Drinking Level ~ Budget")
    })
    
    output$rating_overview_grouped_cuisine <- renderPlot({
        ggplot(rating_detailed) + aes(food_rating, service_rating, col = Rcuisine) +  
            geom_smooth(method = "lm", se=F) + ggtitle("Relation Food Rating and Service Rating Grouped By Cuisine")
    })
    
    # Daten Restaurant Tab
    output$datasets_restaurant <- renderValueBox({
        valueBox(
            formatC("5", format="d", big.mark=','),
            paste('Datasets'),
            icon = icon("list-alt",lib='glyphicon'),
            color = "purple")
    })
    
    output$count_restaurants <- renderValueBox({
        if(input$select_cuisine_restaurant != "Alle"){
            count_restaurants_val <- geoplaces %>% 
                join(cuisine) %>% 
                filter(Rcuisine == input$select_cuisine_restaurant)
        }else{
            count_restaurants_val <- geoplaces
        }
        valueBox(
            formatC(nrow(count_restaurants_val), format="d", big.mark=','),
            paste('Restaurants: ', input$select_cuisine_restaurant),
            icon = icon("home",lib='glyphicon'),
            color = "green")
    })
    
    output$count_cuisines <- renderValueBox({
        valueBox(
            formatC(8, format="d", big.mark=','),
            paste('Küchen Ausprägungen'),
            # paste('Datensätze (User):',rating$rating),
            icon = icon("cutlery",lib='glyphicon'),
            color = "yellow")
    })
    
    output$restaurant_detail_data <- renderPlot({
        
        if (input$select_dia_restaurant == "Verteilung-Cuisines")  {print(
                ggplot(cuisine, aes(x = Rcuisine, fill=Rcuisine)) + geom_bar()
        )}   
        if (input$select_dia_restaurant == "Parking-Optionen")  {
            if(req(input$select_cuisine_restaurant) != "Alle") {
                parking_filtered <- parking %>% 
                    join(geoplaces) %>% 
                    join(cuisine) %>% 
                    filter(Rcuisine == input$select_cuisine_restaurant)
                
                print(
                    print(ggplot(parking_filtered, aes(x = parking_lot, fill=parking_lot)) + geom_bar())
                )
            }
            if(req(input$select_cuisine_restaurant) == "Alle") {print(
                print(ggplot(parking, aes(x = parking_lot, fill=parking_lot)) + geom_bar())
            )}
        }  
        if (input$select_dia_restaurant == "Preisklassen")  {
            if(req(input$select_cuisine_restaurant) != "Alle") {
                detailed_price_filtered <- detailed_price %>% 
                    filter(Rcuisine == input$select_cuisine_restaurant)
                
                print(
                    ggplot(detailed_price_filtered, aes(x = price, fill=price)) + geom_bar()
                )
            }
            if(req(input$select_cuisine_restaurant) == "Alle") {print(
                ggplot(detailed_price, aes(x = price, fill=price)) + geom_bar() + facet_wrap(.~Rcuisine)
            )}
        }  
    })
    
    # Prediction Rating
    output$methods_rating <- renderValueBox({
        valueBox(
            formatC("5", format="d", big.mark=','),
            paste('verwendete Methoden'),
            icon = icon("tasks",lib='glyphicon'),
            color = "purple")
    })
    
    output$best_acc_rating <- renderValueBox({
        valueBox(
            formatC("52%"),
            paste('Overall Accuracy'),
            icon = icon("stats",lib='glyphicon'),
            color = "green")
    })
    
    output$selected_method_rating <- renderValueBox({
        valueBox(
            formatC("50%", format="d", big.mark=','),
            paste('Overall Accuracy Test'),
            icon = icon("stats",lib='glyphicon'),
            color = "yellow")
    })

    # Predicting Cuisine
    output$methods_cuisine <- renderValueBox({
        valueBox(
            formatC("4", format="d", big.mark=','),
            paste('verwendete Methoden'),
            icon = icon("tasks",lib='glyphicon'),
            color = "purple")
    })
    
    output$best_acc_cuisine <- renderValueBox({
        valueBox(
            formatC("50%", format="d", big.mark=','),
            paste('Overall Accuracy Training'),
            icon = icon("stats",lib='glyphicon'),
            color = "green")
    })
    
    output$selected_method_cuisine <- renderValueBox({
        valueBox(
            formatC("50%", format="d", big.mark=','),
            paste('Overall Accuracy Test'),
            # paste('Datensätze (User):',rating$rating),
            icon = icon("stats",lib='glyphicon'),
            color = "yellow")
    })
}


shinyApp(ui, server)