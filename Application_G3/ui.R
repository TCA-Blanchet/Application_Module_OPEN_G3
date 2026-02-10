library(shiny)
library(leaflet)

# Define UI for application that draws a histogram
fluidPage(
  titlePanel("Carte des AOP Françaises"), 
  
  tabsetPanel(
    tabPanel("Accueil",
             titlePanel(icon("home"),"Accueil"),
             
             passwordInput("password", "Password:"),
             actionButton("go", "Go"),
             verbatimTextOutput("value"), 
             
             selectInput("variable", "Type d'AOP:",
                         c("Cylinders" = "cyl",
                           "Transmission" = "am",
                           "Gears" = "gear")),
             tableOutput("data_AOP")),
    
    selectInput("variable", "Nom du département:",
                c("Cylinders" = "cyl",
                  "Transmission" = "am",
                  "Gears" = "gear")),
    tableOutput("data_département")
  ),
  
  imageOutput("aop_image.pjp"),
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  ) # fermeture sidebarLayout
  ,# fermeture tabPanel1
  
  tabPanel("Carte AOP",
           titlePanel("Carte Interactive des AOP"),
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
             
           )), 
  
  tabPanel("About",
           titlePanel("L'Equipe"),
           
           checkboxGroupInput("variable", "Variables to show:",
                              c("Cylinders" = "cyl",
                                "Transmission" = "am",
                                "Gears" = "gear")),
           fluidRow(column(width = 4),
                    column(width = 2, offset = 3)),
           fluidRow(column(width = 12)),
           checkboxGroupInput("variable", "Variables to show:",
                              c("Cylinders" = "cyl",
                                "Transmission" = "am",
                                "Gears" = "gear"))
  )# fermeture tabsetPanel
  
) # fermeture Fluidpage