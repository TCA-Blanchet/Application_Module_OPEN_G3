library(shiny)
library(sf)
library(leaflet)
library(dplyr)
library(bslib)

if (!dir.exists("data/processed")) {
  stop("ERREUR : Le dossier 'data/processed' introuvable.\n",
       "Veuillez exécuter 'prepare-data.R' d'abord.")
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
    
    tabPanel("About",
             titlePanel("L'Equipe"),
             fluidRow(
               column(width = 3, align = "center",
                      wellPanel(
                        img(src = "Profil_Julian.png", width = "35%", 
                            style = "border-radius: 50%; object-fit: cover; margin-bottom: 15px"),
                        h4("Julien"),
                        p("Server Développeur") 
                      )
               ),
               column(width = 3, align = "center",
                      wellPanel(
                        img(src = "Profil_Kevine.png", width = "35%", 
                            style = "border-radius: 50%; object-fit: cover; margin-bottom: 15px"),
                        h4("Kevine"),
                        p("UI Développeuse") 
                      )
               ),
               column(width = 3, align = "center",
                      wellPanel(
                        img(src = "Profil_Tibault.png", width = "35%", 
                            style = "border-radius: 50%; object-fit: cover; margin-bottom: 15px"),
                        h4("Tibault"),
                        p("Rapport Ecrivain") 
                      )
               ),
               column(width = 3, align = "center",
                      wellPanel(
                        img(src = "Profil_Glory.png", width = "35%", 
                            style = "border-radius: 50%; object-fit: cover; margin-bottom: 15px"),
                        h4("Glory"),
                        p("UI Développeuse") 
                      )
               )
             ),
             
             # Espacement visuel
             fluidRow(column(width = 12, hr())),
             titlePanel("Histoire du projet"),
             fluidRow(
               column(12,
                      p("Nous nous sommes séparé les tâches : Glory sur l'UI pour la page d'accueil et l'onglet de la carte, Kevine sur l'UI de la page About, Julian sur le traitement des données et Tibault en support d'explication pour l'UI et la rédaction/synthétisation du traitement des données. 
Concernant le traitement des données, nous sommes partie de nos données .shp, nous avons dû avons les transformer géométriquement afin de les passer en WGS 84. Ensuite Julian a fait une jointure gauche entre notre catégorisation des AOPs et notre fichier commune.shp.
Afin de faciliter la lecture de la carte et de ne pas la surcharger la carte avec les zones géographiques des AOPs, Julian a calculer les centroides de chaque AOPs afin de ne faire apparaître qu'un unique point par AOP. Suite à cela nous avons enregistrer le fruit de nos calculs sur différents fichiers afin ne pas refaire faire les calcules à l'ouverture de l'application mais uniquement de la lecture des résultats.

Glory: sur la partie Ui, j'ai fait une barre déroulante des produits regroupés en 4 catégories: boissons, produits laitiers, produits carnés et autres. J'ai assi fait une déroulante afin de permettre à l'utilisateur de trier par région. J'ai également fait des recherches afin apporter plus de couleurs dans l'interface, mais je n'ai pas encore obtenu des résultats qui marchent.",
                        style = "text-align: justify; padding: 10px")
               )
             )
             
             
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
      addCircleMarkers(data = aop_centroides, radius = 6, color = "#0000ff",
                       fillColor = "#0080ff", fillOpacity = 0.8, weight = 2,
                       layerId = ~AOP,
                       popup = ~paste0("<strong>", AOP, "</strong><br>Catégorie: ", 
                                       categorie, "<br>Communes: ", n_communes),
                       clusterOptions = markerClusterOptions(),
                       group = "markers") %>%
      setView(lng = 2.5, lat = 46.5, zoom = 6) %>%
      addLayersControl(baseGroups = c("OpenStreetMap", "CartoDB"),
                       options = layersControlOptions(collapsed = FALSE))
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
        addPolygons(data = communes_aop, fillColor = "#0080ff", fillOpacity = 0.6,
                    color = "#0000ff", weight = 1,
                    popup = ~paste0("<strong>", Commune, "</strong><br>Département: ", 
                                    Departement, "<br>AOP: ", AOP),
                    highlightOptions = highlightOptions(weight = 2, color = "#0080ff",
                                                        fillOpacity = 0.8, bringToFront = TRUE),
                    group = "communes") %>%
        addCircleMarkers(data = point_aop, radius = 12, color = "#0000ff",
                         fillColor = "#0080ff", fillOpacity = 1, weight = 3,
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
        addPolylines(data = dept_selectionne, color = "#0080ff", weight = 4, opacity = 1,
                     popup = ~paste0("<strong>", NOM_M, "</strong>"),
                     group = "departements") %>%
        addCircleMarkers(data = aop_points, radius = 8, color = "#0000ff",
                         fillColor = "#0080ff", fillOpacity = 0.9, weight = 2,
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
      addCircleMarkers(data = aop_points, radius = 6, color = "#0000ff",
                       fillColor = "#0080ff", fillOpacity = 0.8, weight = 2,
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
                    length(unique(aop_info$Departement)), " département(s)"))
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
}

shinyApp(ui = ui, server = server)