# https://shiny.posit.co/

library(shiny)
library(leaflet)

# Define UI for application
fluidPage(
  titlePanel("Carte des AOP Françaises"), 
  
  tabsetPanel(
    
    tabPanel("About",
             titlePanel("L'Equipe"),
             
             # Zones d'informations modifiables pour chaque membre
             fluidRow(
               column(width = 3,
                      wellPanel(
                        h4("Julien"),
                        p("Infos sur Julien : [Saisir ici]") 
                      )
               ),
               column(width = 3,
                      wellPanel(
                        h4("Kevine"),
                        p("Infos sur Kevine : [Saisir ici]") 
                      )
               ),
               column(width = 3,
                      wellPanel(
                        h4("Tibault"),
                        p("Infos sur Tibault : [Saisir ici]") 
                      )
               ),
               column(width = 3,
                      wellPanel(
                        h4("Glory"),
                        p("Infos sur Glory : [Saisir ici]") 
                      )
               )
             ),
             
             # Espacement visuel
             fluidRow(column(width = 12, hr()))
             
             # Vos sélections d'icônes
             
    ),
    
    # Onglet pour la carte
    tabPanel("Carte",
             sidebarLayout(
               sidebarPanel(
                 helpText("Utilisez les options ci-dessous pour filtrer la carte."),
                 checkboxGroupInput("variable", "Variables à afficher :",
                                    c("Cylinders" = "cyl",
                                      "Transmission" = "am",
                                      "Gears" = "gear"))
               ),
               mainPanel(
                 leafletOutput("map", height = "600px")
               )
             )
    )
    
  )# fermeture tabsetPanel
)# fermeture Fluidpage