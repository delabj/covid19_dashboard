library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(gitlink)
library(tidyverse)
library(leaflet)
library(viridis)
library(htmltools)
library(lubridate)
library(ggtext)


## ui.R ##
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")), 
    box(
      a("Data: Johns Hopkins CSSE", href="https://github.com/CSSEGISandData/COVID-19"), 
      br(),
      a("Code: @delabj", href="http://twitter.com/delabj")
    )
   
  )
    
)


body <- dashboardBody(
  ribbon_css("https://github.com/delabj/covid19_dashboard", color = "black", font_color = "white", border_color = "white", parent_css =  list(top = "50px", "z-index" = "10")), 
  
  shinyDashboardThemes(
    theme = "grey_dark"
  ),
  tabItems(
    tabItem(tabName = "dashboard",
            
            tags$style(type="text/css", "#active_cases_map.recalculating { opacity: 1.0; }"),
            tags$style(type="text/css", "#cum_cases_map.recalculating { opacity: 1.0; }"),
            tags$style(type="text/css", "#daily_change_plot.recalculating { opacity: 1.0; }"),
            tags$style(type="text/css", "#total_trends_plot.recalculating { opacity: 1.0; }"),
            
            
            
            tags$style(type = "text/css", "#active_cases_map {height: calc(70vh - 80px) !important;}"),
            tags$style(type = "text/css", "#cum_cases_map {height: calc(70vh - 80px) !important;}"),
            tags$style(type = "text/css", "#daily_change_plot {height: calc(32vh - 40px) !important;}"),
            tags$style(type = "text/css", "#total_trends_plot {height: calc(32vh - 40px) !important;}"),
            
            tags$head(tags$style(HTML('.irs-from, .irs-to, .irs-min, .irs-max {
            visibility: hidden !important;
    }'))),
            
            
            column(
              width = 7,
              tabBox(width=12,
                     title = "Map of COVID-19 Outbreaks",
                     tabPanel(
                       title = "Cumulative Cases",
                       leafletOutput("cum_cases_map")
                     ),
                     tabPanel(
                       title = "Active Cases",
                       leafletOutput("active_cases_map")
                     )
                     
              ),
              uiOutput("date_selection")
              
             
            ), 
            column( 
             width = 5, 
             uiOutput("value_total_cases"),
             valueBoxOutput("value_active_cases"),
             valueBoxOutput("value_recovered"),
             valueBoxOutput("value_deaths"),
             box(
               width =12,
               plotOutput("daily_change_plot")
             ), 
             box(
               width = 12,
               plotOutput("total_trends_plot")
             ), 
             
              
             
             
             
             )
            
    ),
    
    tabItem(tabName = "widgets",
            h2("Widgets tab content")
    )
  )
)

# Put them together into a dashboardPage
dashboardPage(
  dashboardHeader(title = "COVID-19"),
  sidebar,
  body
)

