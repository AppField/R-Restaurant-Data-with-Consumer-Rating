# load the required packages
library(shiny)
require(shinydashboard)
library(ggplot2)
library(dplyr)

# DATA
recommendation <- read.csv('recommendation.csv', stringsAsFactors = F,header=T)
accept <- read.csv('../data/chefmozaccepts.csv')
cuisine <- read.csv('../data/chefmozcuisine.csv')
hours <- read.csv('../data/chefmozhours4.csv')
parking <- read.csv('../data/chefmozparking.csv')
geoplaces <- read.csv('../data/geoplaces2.csv', na.strings = "?")
rating <- read.csv('../data/rating_final.csv')
usercuisine <- read.csv('../data/usercuisine.csv')
userpayment <- read.csv('../data/userpayment.csv')
userprofile <- read.csv('../data/userprofile.csv', na.strings = "?")

#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Customer-Rating Dashboard")  

#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem( "Daten", tabName = "data", icon = icon("list-alt",lib='glyphicon'),
                  menuSubItem("Restaurant", tabName = "restaurant", icon = icon("cutlery",lib='glyphicon')),
                  menuSubItem("User", tabName = "user", icon = icon("user",lib='glyphicon')),
                  menuSubItem("Rating", tabName = "rating", icon = icon("equalizer",lib='glyphicon'))),
        menuItem( "Machine Learning", tabName = "ml", icon = icon("random",lib='glyphicon'),
                  menuSubItem("Predicting Rating", tabName = "pred_rating", icon = icon("equalizer",lib='glyphicon')),
                  menuSubItem("Predicting Cuisine", tabName = "pred_cuisine", icon = icon("cutlery",lib='glyphicon')))
    )
)


frow1 <- fluidRow(
    valueBoxOutput("value1")
    ,valueBoxOutput("value2")
    ,valueBoxOutput("value3")
)

frow2 <- fluidRow(
    
    box(
        title = "Revenue per Account"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,plotOutput("revenuebyPrd", height = "300px")
    )
    
    ,box(
        title = "Revenue per Product"
        ,status = "primary"
        ,solidHeader = TRUE 
        ,collapsible = TRUE 
        ,plotOutput("revenuebyRegion", height = "300px")
    ) 
    
)

#BODY
body <- dashboardBody(
    tabItems(
        tabItem(tabName = "dashboard",
                h2("Dashboard tab content")
        ),
        tabItem(tabName = "data",
                h2("Data tab content")
        ),
        tabItem(tabName = "restaurant",
                h2("restaurant tab content")
        ),
        tabItem(tabName = "user",
                h2("user tab content")
        ),
        tabItem(tabName = "rating",
                h2("rating tab content")
        ),
        tabItem(tabName = "ml",
                h2("ML tab content")
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
    total.revenue <- sum(recommendation$Revenue)
    sales.account <- recommendation %>% group_by(Account) %>% summarise(value = sum(Revenue)) %>% filter(value==max(value))
    prof.prod <- recommendation %>% group_by(Product) %>% summarise(value = sum(Revenue)) %>% filter(value==max(value))
    
    
    #creating the valueBoxOutput content
    output$value1 <- renderValueBox({
        valueBox(
            formatC(sales.account$value, format="d", big.mark=',')
            ,paste('Top Account:',sales.account$Account)
            ,icon = icon("stats",lib='glyphicon')
            ,color = "purple")
        
        
    })
    
    
    
    output$value2 <- renderValueBox({
        
        valueBox(
            formatC(total.revenue, format="d", big.mark=',')
            ,'Total Expected Revenue'
            ,icon = icon("gbp",lib='glyphicon')
            ,color = "green")
        
    })
    
    
    
    output$value3 <- renderValueBox({
        
        valueBox(
            formatC(prof.prod$value, format="d", big.mark=',')
            ,paste('Top Product:',prof.prod$Product)
            ,icon = icon("menu-hamburger",lib='glyphicon')
            ,color = "yellow")
        
    })
    
    #creating the plotOutput content
    
    output$revenuebyPrd <- renderPlot({
        ggplot(data = recommendation, 
               aes(x=Product, y=Revenue, fill=factor(Region))) + 
            geom_bar(position = "dodge", stat = "identity") + ylab("Revenue (in Euros)") + 
            xlab("Product") + theme(legend.position="bottom" 
                                    ,plot.title = element_text(size=15, face="bold")) + 
            ggtitle("Revenue by Product") + labs(fill = "Region")
    })
    
    
    output$revenuebyRegion <- renderPlot({
        ggplot(data = recommendation, 
               aes(x=Account, y=Revenue, fill=factor(Region))) + 
            geom_bar(position = "dodge", stat = "identity") + ylab("Revenue (in Euros)") + 
            xlab("Account") + theme(legend.position="bottom" 
                                    ,plot.title = element_text(size=15, face="bold")) + 
            ggtitle("Revenue by Region") + labs(fill = "Region")
    })
}


shinyApp(ui, server)