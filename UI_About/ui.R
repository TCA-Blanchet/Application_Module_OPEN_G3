# https://shiny.posit.co/

library(shiny)
library(leaflet)

# Define UI for application that draws a histogram
fluidPage(
  titlePanel("Carte des AOP Françaises"), 
  
  tabsetPanel(
    
    tabPanel("About",
             titlePanel("L'Equipe"),
             
             # Section des membres du groupe
             fluidRow(
               column(width = 12,
                      tags$ul(
                        tags$li(strong("Louis")),
                        tags$li(strong("Kevine")),
                        tags$li(strong("Tibault")),
                        tags$li(strong("Glory"))
                      )
               )
             ),
             
             fluidRow(column(width = 4),
                      column(width = 2, offset = 3)),
             fluidRow(column(width = 12)),
             ),
             
             sidebarLayout(
               sidebarPanel(),
               mainPanel(leafletOutput("map")
               )
             
             )
  )# fermeture tabsetPanel
)# fermeture Fluidpage