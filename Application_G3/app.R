library(shiny)
library(sf)
library(leaflet)
library(dplyr)
library(bslib)
library(shinyjs)

if (!dir.exists("data/processed")) {
  stop("ERREUR : Le dossier 'data/processed' introuvable.\n",
       "Veuillez exécuter 'preparation_donnees_pour_app.R' et 'optimisation_donnees.r' d'abord.")
}

communes_light <- readRDS("data/processed/communes_light.rds")
departements <- readRDS("data/processed/departements.rds")
aop_communes <- readRDS("data/processed/aop_communes.rds")
aop_centroides <- readRDS("data/processed/aop_centroides.rds")

categories_uniques <- sort(unique(aop_centroides$categorie))
liste_aop <- sort(unique(aop_centroides$AOP))
liste_departements <- c("Tous les départements" = "", sort(unique(departements$NOM_M)))

# ===== UI =====
ui <- fluidPage(
  theme = bs_theme(
    bg = "#FFFFFF",
    fg = "#333333",
    primary = "#b06680",
    secondary = "#d3d3d3",
    base_font = font_google("Inter"), 
    heading_font = font_google("Playfair Display")
  ),
  
  tags$head(
    tags$style(HTML("
      .tab-content {
        background-color: #FFFFFF !important;
      }
      
      body {
        background-color: #FFFFFF !important;
      }
      
      /* Onglets propres */
      .nav-tabs {
        background-color: #2D1B1F;
        margin: 0;
        padding: 0 30px;
        border-bottom: none;
      }
      
      .nav-tabs > li > a {
        background-color: transparent;
        color: #F5E6D3;
        border: none;
        margin-right: 5px;
      }
      
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus {
        background-color: #FFFFFF;
        color: #9A2A56;
        border: none;
        border-radius: 5px 5px 0 0;
      }
      
      .nav-tabs > li > a:hover {
        background-color: rgba(255,255,255,0.1);
        color: #F5E6D3;
      }
    .container-fluid h2 {
      color: #2D1B1F !important;
    }
    
    /* Labels des inputs */
    .control-label {
      color: #333333 !important;
      font-weight: 500;
    }
    
    .form-control {
      background-color: #FFFFFF !important;
      color: #333333 !important;
      border: 1px solid #CCCCCC !important;
    }
    
    .form-control:focus {
      border-color: #b06680 !important;
      box-shadow: 0 0 0 0.2rem rgba(176, 102, 128, 0.25) !important;
    }
    
    .btn-secondary {
      background-color: #6C757D !important;
      color: #FFFFFF !important;
      border-color: #6C757D !important;
    }
    
    .btn-secondary:hover {
      background-color: #5A6268 !important;
      border-color: #545B62 !important;
    }
    
    .checkbox label {
      color: #333333 !important;
    }
    
    pre {
      background-color: #F8F9FA !important;
      color: #333333 !important;
      border: 1px solid #DEE2E6 !important;
    }
    
    .well {
      background-color: #F8F9FA !important;
      border: 1px solid #DEE2E6 !important;
    }
    
    hr {
      border-top-color: #DEE2E6 !important;
    }
    /* Texte noir pour la page About */
h2, h4 {
  color: #2D1B1F !important;
}

.well p {
  color: #333333 !important;
}
  "))
  ),
  
  tags$div(style = "background-color: #2D1B1F;
                    padding: 20px 30px;
                    margin-bottom: 0;",
           tags$div(style = "display: flex; 
                             justify-content: space-between; 
                             align-items: center;",
                    tags$h1("Carte des AOP Françaises", 
                            style = "font-weight: 500; 
                                     color: #F5E6D3;
                                     margin: 0;
                                     font-family: 'Playfair Display', serif;"),
                    tags$img(src = "logo_aop.png", 
                             style = "height: 70px; margin-right: 25px; margin-top: 10px")
           )
  ),
  
  tabsetPanel(
    id = "tabs",
    tabPanel("Accueil",
             fluidRow(
               column(12, align = "center",
                      tags$div(style = "margin-top: 20px; margin-bottom: 15px;",
                               tags$h2("Bienvenue sur la Carte des AOP Françaises", 
                                       style = "color: #9A2A56; font-weight: 600;"),
                               tags$p(style = "font-size: 18px; color: #666; margin-top: 5px;",
                                      "Explorez le patrimoine gastronomique français")
                      )
               )
             ),
             
             fluidRow(style = "margin: 0; padding: 0;",
                      
                      column(3, style = "padding: 0;",
                             tags$div(style = "position: relative; height: 500px; overflow: hidden;",
                                      tags$img(src = "boissons.png", 
                                               style = "width: 100%; height: 100%; object-fit: cover;"),
                                      tags$div(style = "position: absolute; bottom: 0; left: 0; right: 0; 
                                            background: linear-gradient(to top, rgba(0,0,0,0.7), transparent); 
                                            padding: 40px 20px 30px; text-align: center;",
                                               tags$h3("Boissons", 
                                                       style = "color: white; margin-bottom: 15px; font-weight: 600;"),
                                               actionButton("go_boissons", "Découvrir", 
                                                            style = "background-color: #b06680; 
                                                         color: white; 
                                                         padding: 12px 35px;
                                                         border: none;
                                                         border-radius: 5px;
                                                         font-size: 16px;
                                                         cursor: pointer;
                                                         transition: all 0.3s;")
                                      )
                             )
                      ),
                      
                      column(3, style = "padding: 0;",
                             tags$div(style = "position: relative; height: 500px; overflow: hidden;",
                                      tags$img(src = "produits_laitiers.png", 
                                               style = "width: 100%; height: 100%; object-fit: cover;"),
                                      tags$div(style = "position: absolute; bottom: 0; left: 0; right: 0; 
                                            background: linear-gradient(to top, rgba(0,0,0,0.7), transparent); 
                                            padding: 40px 20px 30px; text-align: center;",
                                               tags$h3("Produits Laitiers", 
                                                       style = "color: white; margin-bottom: 15px; font-weight: 600;"),
                                               actionButton("go_laitiers", "Découvrir", 
                                                            style = "background-color: #b06680; 
                                                         color: white; 
                                                         padding: 12px 35px;
                                                         border: none;
                                                         border-radius: 5px;
                                                         font-size: 16px;
                                                         cursor: pointer;
                                                         transition: all 0.3s;")
                                      )
                             )
                      ),
                      
                      column(3, style = "padding: 0;",
                             tags$div(style = "position: relative; height: 500px; overflow: hidden;",
                                      tags$img(src = "produits_carnes.png", 
                                               style = "width: 100%; height: 100%; object-fit: cover;"),
                                      tags$div(style = "position: absolute; bottom: 0; left: 0; right: 0; 
                                            background: linear-gradient(to top, rgba(0,0,0,0.7), transparent); 
                                            padding: 40px 20px 30px; text-align: center;",
                                               tags$h3("Produits Carnés", 
                                                       style = "color: white; margin-bottom: 15px; font-weight: 600;"),
                                               actionButton("go_carnes", "Découvrir", 
                                                            style = "background-color: #b06680; 
                                                         color: white; 
                                                         padding: 12px 35px;
                                                         border: none;
                                                         border-radius: 5px;
                                                         font-size: 16px;
                                                         cursor: pointer;
                                                         transition: all 0.3s;")
                                      )
                             )
                      ),
                      
                      column(3, style = "padding: 0;",
                             tags$div(style = "position: relative; height: 500px; overflow: hidden;",
                                      tags$img(src = "autres.png", 
                                               style = "width: 100%; height: 100%; object-fit: cover;"),
                                      tags$div(style = "position: absolute; bottom: 0; left: 0; right: 0; 
                                            background: linear-gradient(to top, rgba(0,0,0,0.7), transparent); 
                                            padding: 40px 20px 30px; text-align: center;",
                                               tags$h3("Autres", 
                                                       style = "color: white; margin-bottom: 15px; font-weight: 600;"),
                                               actionButton("go_autres", "Découvrir", 
                                                            style = "background-color: #b06680; 
                                                         color: white; 
                                                         padding: 12px 35px;
                                                         border: none;
                                                         border-radius: 5px;
                                                         font-size: 16px;
                                                         cursor: pointer;
                                                         transition: all 0.3s;")
                                      )
                             )
                      )
             ),
             
             fluidRow(
               column(12, align = "center", style = "margin-top: 15px; margin-bottom: 20px;",
                      actionButton("go", "Voir toutes les AOP", 
                                   style = "background-color: #9A2A56; 
                                       color: white; 
                                       padding: 15px 40px;
                                       font-size: 18px;
                                       border: none;
                                       border-radius: 5px;")
               )
             )
    ),
    
    tabPanel("Carte AOP",
             titlePanel("Carte Interactive des AOP"),
             
             sidebarLayout(
               sidebarPanel(
                 width = 4,
                 
                 selectInput("departement", "Département :", 
                             choices = liste_departements,
                             selected = ""),
                 
                 selectInput("aop_selectionnee", "AOP :", 
                             choices = c("Aucune" = "", liste_aop),
                             selected = ""), 
                 
                 div(style = "display: flex; gap: 5px; margin-bottom: 10px;",
                     actionButton("reset_dept", "Réinit. Dept", icon = icon("map"), 
                                  class = "btn-secondary btn-sm", style = "flex: 1;"),
                     actionButton("reset_aop", "Réinit. AOP", icon = icon("times"), 
                                  class = "btn-secondary btn-sm", style = "flex: 1;")),
                 
                 hr(),
                 checkboxGroupInput("categories", "Catégories d'AOP :",
                                    choices = categories_uniques,
                                    selected = categories_uniques),
                 hr(),
                 verbatimTextOutput("info_selection")
               ),
               
               mainPanel(width = 8, leafletOutput("carte", height = "700px"))
             )
    ),
    useShinyjs(),
    tabPanel("About",
             tags$head(
               tags$style(HTML("
      .btn-float {
        position: fixed;
        bottom: 10px;
        right: 10px;
        z-index: 9999;
        background-color: transparent;
        color: #DAA520;
        border-radius: 50%;
        padding: 5px;
      }
      #to_top:hover {
        background-color: #D1477D;
      }
    "))
             ),
             titlePanel("L'Equipe"),
             fluidRow(
               column(width = 3, align = "center",
                      wellPanel(
                        img(src = "Profil_Julian.png", width = "35%", 
                            style = "border-radius: 50%; object-fit: cover; margin-bottom: 15px"),
                        h4("Julian", style = "border-bottom: 3px solid #D1477D; background: white"),
                        p("Server Développeur") 
                      )
               ),
               column(width = 3, align = "center",
                      wellPanel(
                        img(src = "Profil_Kevine.png", width = "35%", 
                            style = "border-radius: 50%; object-fit: cover; margin-bottom: 15px"),
                        h4("Kevine", style = "border-bottom: 3px solid #D1477D; background: white"),
                        p("UI Développeuse") 
                      )
               ),
               column(width = 3, align = "center",
                      wellPanel(
                        img(src = "Profil_Tibault.png", width = "35%", 
                            style = "border-radius: 50%; object-fit: cover; margin-bottom: 15px"),
                        h4("Tibault", style = "border-bottom: 3px solid #D1477D; background: white"),
                        p("Rapport Ecrivain") 
                      )
               ),
               column(width = 3, align = "center",
                      wellPanel(
                        img(src = "Profil_Glory.png", width = "35%", 
                            style = "border-radius: 50%; object-fit: cover; margin-bottom: 15px"),
                        h4("Glory", style = "border-bottom: 3px solid #D1477D; background: white"),
                        p("UI Développeuse") 
                      )
               )
             ),
             
             fluidRow(column(width = 12, hr())),
             fluidRow(
               column(width = 12,
                      wellPanel(
                        h2("Association Of People", style = "color: #D1477D; font-weight: 600"),
                        hr(style = "border-top: 2px solid #D1477D; width: 50px; margin-left: 0;"),
                        p("Description du projet :", style = "color: #9A2A56; letter-spacing: 2px; font-size: 0.8em; font-weight: bold"),
                        p(
                        "Notre projet consiste en la présentation des AOP françaises sur une carte interactive accessible en ligne.
Nous avons effectué ce travail sur 5 jours, nous nous sommes réparti le travail au fur et à mesure.
Ce projet a été un succès de notre côté ; en effet, nous sommes parvenus à obtenir une application complète et fonctionnelle.
Et ce projet nous a permis de nous replonger dans le langage R ainsi que de découvrir le module Shiny.
Cette expérience de travail collective nous a permis de nous entraider et de surmonter nos problèmes, ce qui a fait de nous une vraie mini-équipe de développement."
                        , style = "text-align: justify; color: #444; font-size: 1.1em"),
                        style = "background-color: #FFF9FB; border: none; border-left: 5px solid #D1477D; border-radius: 4px;"
                      )
               )
             ),
             actionButton("to_top", 
                          icon = icon("arrow-up"),
                          label = NULL,
                          class = "btn-float")
             
    )
  )
)

# ===== SERVER =====
server <- function(input, output, session) {
  
  output$carte <- renderLeaflet({
    leaflet() %>%
      addTiles(group = "OpenStreetMap") %>%
      addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") %>%
      addPolygons(data = departements, fillColor = "#95d5a6", fillOpacity = 0.3,
                  color = "#1fc919", weight = 1,
                  popup = ~paste0("<strong>", NOM_M, "</strong>"), 
                  layerId = ~NOM_M,
                  group = "departements") %>%
      addCircleMarkers(data = aop_centroides, radius = 6, color = "#FFE41D",
                       fillColor = "#F13E3D", fillOpacity = 0.8, weight = 2,
                       layerId = ~AOP,
                       popup = ~paste0("<strong>", AOP, "</strong><br>Catégorie: ", 
                                       categorie, "<br>Communes: ", n_communes),
                       clusterOptions = markerClusterOptions(),
                       group = "markers") %>%
      setView(lng = 2.5, lat = 46.5, zoom = 6) %>%
      addLayersControl(baseGroups = c("OpenStreetMap", "CartoDB"),
                       options = layersControlOptions(collapsed = FALSE))
  })
  
  observeEvent(input$go_boissons, {
    updateTabsetPanel(session = session, inputId = "tabs", selected = "Carte AOP")
    updateCheckboxGroupInput(session, "categories", selected = "Boissons")
  })
  
  observeEvent(input$go_laitiers, {
    updateTabsetPanel(session = session, inputId = "tabs", selected = "Carte AOP")
    updateCheckboxGroupInput(session, "categories", selected = "Produits Laitiers")
  })
  
  observeEvent(input$go_carnes, {
    updateTabsetPanel(session = session, inputId = "tabs", selected = "Carte AOP")
    updateCheckboxGroupInput(session, "categories", selected = "Produits Carnés")
  })
  
  observeEvent(input$go_autres, {
    updateTabsetPanel(session = session, inputId = "tabs", selected = "Carte AOP")
    updateCheckboxGroupInput(session, "categories", selected = "Autres")
  })
  
  observeEvent(input$reset_dept, {
    updateSelectInput(session, "departement", selected = "")
  })
  
  observeEvent(input$reset_aop, {
    updateSelectInput(session, "aop_selectionnee", selected = "")
  })
  
  observe({
    aop_disponibles <- aop_centroides %>% 
      filter(categorie %in% input$categories)
    
    if (!is.null(input$departement) && input$departement != "") {
      dept_selectionne <- departements %>% filter(NOM_M == input$departement)
      aop_dept_ids <- aop_communes %>%
        filter(INSEE_DEP %in% dept_selectionne$INSEE_DEP) %>%
        pull(AOP) %>%
        unique()
      aop_disponibles <- aop_disponibles %>% filter(AOP %in% aop_dept_ids)
    }
    
    choix_aop <- c("Aucune" = "", sort(unique(aop_disponibles$AOP)))
    
    updateSelectInput(session, "aop_selectionnee", 
                      choices = choix_aop,
                      selected = input$aop_selectionnee)
  })
  
  aop_filtrees <- reactive({
    if (is.null(input$categories) || length(input$categories) == 0) {
      return(aop_centroides[0, ]) 
    }
    aop_centroides %>% filter(categorie %in% input$categories)
  })
  
  observe({
    if (is.null(input$categories) || length(input$categories) == 0) {
      leafletProxy("carte") %>%
        clearGroup("departements") %>%
        clearGroup("markers") %>%
        clearGroup("communes") %>%
        clearShapes() %>%
        clearMarkers() %>%
        setView(lng = 2.5, lat = 46.5, zoom = 6)
      return()
    }
    
    aop_points <- aop_filtrees()
    
    if (!is.null(input$departement) && input$departement != "" && 
        (is.null(input$aop_selectionnee) || input$aop_selectionnee == "")) {
      dept_selectionne <- departements %>% filter(NOM_M == input$departement)
      aop_dept_ids <- aop_communes %>%
        filter(INSEE_DEP %in% dept_selectionne$INSEE_DEP) %>%
        pull(AOP) %>%
        unique()
      aop_points <- aop_points %>% filter(AOP %in% aop_dept_ids)
    }
    
    # CAS 1 : Une AOP est sélectionnée
    if (!is.null(input$aop_selectionnee) && input$aop_selectionnee != "") {
      communes_aop <- aop_communes %>% filter(AOP == input$aop_selectionnee)
      if (nrow(communes_aop) == 0) return()
      
      dept_aop <- departements %>% filter(NOM_M %in% unique(communes_aop$Departement))
      point_aop <- aop_filtrees() %>% filter(AOP == input$aop_selectionnee)
      if (nrow(point_aop) == 0) return()
      
      bbox <- st_bbox(communes_aop)
      
      xmin <- as.numeric(bbox["xmin"])
      xmax <- as.numeric(bbox["xmax"])
      ymin <- as.numeric(bbox["ymin"])
      ymax <- as.numeric(bbox["ymax"])
      
      largeur <- xmax - xmin
      hauteur <- ymax - ymin
      taille_max <- max(largeur, hauteur)
      
      if (taille_max < 0.5) {
        marge <- 0.5
      } else if (taille_max < 1) {
        marge <- 0.3
      } else if (taille_max < 2) {
        marge <- 0.15
      } else {
        marge <- 0.1
      }
      
      leafletProxy("carte") %>%
        clearShapes() %>%
        clearMarkers() %>%
        clearGroup("departements") %>%
        clearGroup("markers") %>%
        clearGroup("communes") %>%
        addPolylines(data = dept_aop, color = "#3f3f3f", weight = 3, opacity = 0.8,
                     popup = ~paste0("<strong>", NOM_M, "</strong>"),
                     group = "departements") %>%
        addPolygons(data = communes_aop, fillColor = "#F13E3D", fillOpacity = 0.6,
                    color = "#FFE41D", weight = 1,
                    popup = ~paste0("<strong>", Commune, "</strong><br>Département: ", 
                                    Departement, "<br>AOP: ", AOP),
                    highlightOptions = highlightOptions(weight = 2, color = "#F13E3D",
                                                        fillOpacity = 0.8, bringToFront = TRUE),
                    group = "communes") %>%
        addCircleMarkers(data = point_aop, radius = 12, color = "#FFE41D",
                         fillColor = "#F13E3D", fillOpacity = 1, weight = 3,
                         popup = ~paste0("<strong>", AOP, "</strong><br>Catégorie: ", 
                                         categorie, "<br>Communes: ", n_communes),
                         group = "markers") %>%
        fitBounds(xmin - largeur * marge, 
                  ymin - hauteur * marge,
                  xmax + largeur * marge, 
                  ymax + hauteur * marge)
      return()
    }
    
    # CAS 2 : Un département est sélectionné
    if (!is.null(input$departement) && input$departement != "") {
      dept_selectionne <- departements %>% filter(NOM_M == input$departement)
      bbox <- st_bbox(dept_selectionne)
      
      proxy <- leafletProxy("carte") %>%
        clearShapes() %>%
        clearMarkers() %>%
        clearGroup("departements") %>%
        clearGroup("markers") %>%
        addPolylines(data = dept_selectionne, color = "#F13E3D", weight = 4, opacity = 1,
                     popup = ~paste0("<strong>", NOM_M, "</strong>"),
                     group = "departements") %>%
        addCircleMarkers(data = aop_points, radius = 8, color = "#FFE41D",
                         fillColor = "#F13E3D", fillOpacity = 0.9, weight = 2,
                         layerId = ~AOP,
                         popup = ~paste0("<strong>", AOP, "</strong><br>Catégorie: ", 
                                         categorie, "<br>Communes: ", n_communes,
                                         "<br>Cliquez pour sélectionner"),
                         group = "markers")
      
      proxy %>% flyToBounds(
        lng1 = as.numeric(bbox["xmin"]),
        lat1 = as.numeric(bbox["ymin"]),
        lng2 = as.numeric(bbox["xmax"]),
        lat2 = as.numeric(bbox["ymax"]),
        options = list(padding = c(150, 100))
      )
      
      return()
    }
    
    # CAS 3 : Vue de toute la France
    leafletProxy("carte") %>%
      clearShapes() %>%
      clearMarkers() %>%
      clearGroup("departements") %>%
      clearGroup("markers") %>%
      addPolygons(data = departements, fillColor = "#95d5a6", fillOpacity = 0.3,
                  color = "#1fc919", weight = 1,
                  popup = ~paste0("<strong>", NOM_M, "</strong>"), layerId = ~NOM_M,
                  group = "departements") %>%
      addCircleMarkers(data = aop_points, radius = 6, color = "#FFE41D",
                       fillColor = "#F13E3D", fillOpacity = 0.8, weight = 2,
                       layerId = ~AOP,
                       popup = ~paste0("<strong>", AOP, "</strong><br>Catégorie: ", 
                                       categorie, "<br>Communes: ", n_communes),
                       clusterOptions = markerClusterOptions(),
                       group = "markers") %>%
      setView(lng = 2.5, lat = 46.5, zoom = 6)
  })
  
  observeEvent(input$carte_marker_click, {
    click <- input$carte_marker_click
    if (!is.null(click$id)) {
      updateSelectInput(session, "aop_selectionnee", selected = click$id)
    }
  })
  
  observeEvent(input$carte_shape_click, {
    click <- input$carte_shape_click
    if (!is.null(click$id)) {
      updateSelectInput(session, "departement", selected = click$id)
    }
  })
  
  output$info_selection <- renderText({
    if (is.null(input$categories) || length(input$categories) == 0) {
      return("Aucune catégorie sélectionnée")
    }
    
    n_aop_total <- nrow(aop_filtrees())
    
    if (!is.null(input$aop_selectionnee) && input$aop_selectionnee != "") {
      aop_info <- aop_communes %>% filter(AOP == input$aop_selectionnee)
      categorie_aop <- aop_centroides %>% 
        filter(AOP == input$aop_selectionnee) %>% 
        pull(categorie) %>% 
        unique()
      
      return(paste0("AOP sélectionnée : ", input$aop_selectionnee, "\n",
                    "Catégorie : ", categorie_aop, "\n",
                    nrow(aop_info), " communes dans ", 
                    length(unique(aop_info$Departement)), " département"))
    }
    
    if (!is.null(input$departement) && input$departement != "") {
      dept_sel <- departements %>% filter(NOM_M == input$departement)
      aop_dept_ids <- aop_communes %>%
        filter(INSEE_DEP %in% dept_sel$INSEE_DEP) %>%
        pull(AOP) %>%
        unique()
      n_aop_dept <- sum(aop_filtrees()$AOP %in% aop_dept_ids)
      
      return(paste0("Département : ", input$departement, "\n",
                    n_aop_dept, " AOP dans ce département"))
    }
    
    paste0("France entière : ", n_aop_total, " AOP affichées")
  })
  observeEvent(input$go, {
    updateTabsetPanel(session = session, inputId = "tabs", selected = "Carte AOP")
    updateCheckboxGroupInput(session, "categories", selected = categories_uniques)
  })
  observeEvent(input$to_top, {
    runjs("window.scrollTo({top: 0, behavior: 'smooth'});")
  })
}

shinyApp(ui = ui, server = server)