# ui.R
library(shiny)
library(leaflet)
library(bslib)

fluidPage(
  theme = bs_theme(
    bg = "white",
    fg = "blue",
    primary = "#0d6efd",
    secondary = "#6c757d",
    base_font = font_google("Roboto")
  ),
  
  titlePanel("Carte des AOP Françaises"), 
  
  tabsetPanel(
    tabPanel("Accueil",
             titlePanel(icon("home"), "Accueil"),
             passwordInput("password", "Password:"),
             actionButton("go", "Go"),
             verbatimTextOutput("value"), 
             navlistPanel(
               "Liste",
               tabPanel("Catégorie produits"),
               tabPanel("Nom de la région")
             ),
             imageOutput("aop_image")
    ),
    
    tabPanel("Carte AOP",
             titlePanel("Carte Interactive des AOP"),
             
             sidebarLayout(
               sidebarPanel(
                 width = 4,
                 
                 selectInput("departement", "Département :", 
                             choices = NULL),
                 
                 selectInput("aop_selectionnee", "AOP :", 
                             choices = NULL), 
                 
                 div(style = "display: flex; gap: 5px; margin-bottom: 10px;",
                     actionButton("reset_dept", "Réinit. Dept", icon = icon("map"), 
                                  class = "btn-secondary btn-sm", style = "flex: 1;"),
                     actionButton("reset_aop", "Réinit. AOP", icon = icon("times"), 
                                  class = "btn-secondary btn-sm", style = "flex: 1;")),
                 
                 hr(),
                 checkboxGroupInput("categories", "Catégories d'AOP :",
                                    choices = NULL),
                 hr(),
                 verbatimTextOutput("info_selection")
               ),
               
               mainPanel(width = 8, leafletOutput("carte", height = "700px"))
             )
    ),
    
    tabPanel("About",
             titlePanel("L'Equipe"),
             checkboxGroupInput("variable_about", "Variables to show:",
                                c("Cylinders" = "cyl",
                                  "Transmission" = "am",
                                  "Gears" = "gear")),
             fluidRow(
               column(width = 4),
               column(width = 2, offset = 3)
             ),
             fluidRow(column(width = 12)),
             checkboxGroupInput("variable_about2", "Variables to show:",
                                c("Cylinders" = "cyl",
                                  "Transmission" = "am",
                                  "Gears" = "gear"))
    )
  )
)