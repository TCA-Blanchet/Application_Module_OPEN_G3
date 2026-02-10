#    https://shiny.posit.co/

library(shiny)
library(leaflet)

# Define UI for application that draws a histogram
fluidPage(
  titlePanel("Carte des AOP Françaises"), 
  
  tabsetPanel(
    
    tabPanel("About",
             titlePanel("L'Equipe"),
             
             fluidRow(column(width = 4),
                      column(width = 2, offset = 3)),
             fluidRow(column(width = 12)),
             checkboxGroupInput("icons", "Choose icons:",
                                choiceNames =
                                  list(icon("calendar"), icon("bed"),
                                       icon("cog"), icon("bug")),
                                choiceValues =
                                  list("calendar", "bed", "cog", "bug")
             ),
             sidebarLayout(
               sidebarPanel(checkboxGroupInput("variable", "Variables to show:",
                                               c("Cylinders" = "cyl",
                                                 "Transmission" = "am",
                                                 "Gears" = "gear"))
               ),
               mainPanel(leafletOutput("map")
               )
             ))
    
  )# fermeture tabsetPanel
)# fermeture Fluidpage