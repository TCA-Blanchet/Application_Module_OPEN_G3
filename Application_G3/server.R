library(shiny)
library(leaflet)
library(sf)
library(dplyr)

function(input, output, session) {
  
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
  
  observe({
    updateSelectInput(session, "departement",
                      choices = c("Toute la France" = "", sort(unique(departements$NOM_M))))
    
    updateSelectInput(session, "aop_selectionnee",
                      choices = c("Aucune" = "", liste_aop))
    
    updateCheckboxGroupInput(session, "categories",
                             choices = categories_uniques,
                             selected = categories_uniques)
  }) %>% bindEvent(TRUE, ignoreInit = FALSE, once = TRUE)
  
  output$carte <- renderLeaflet({
    leaflet() %>%
      addTiles(group = "OpenStreetMap") %>%
      addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") %>%
      setView(lng = 2.5, lat = 46.5, zoom = 6) %>%
      addLayersControl(baseGroups = c("OpenStreetMap", "CartoDB"),
                       options = layersControlOptions(collapsed = FALSE))
  })
  
  observeEvent(input$reset_dept, updateSelectInput(session, "departement", selected = ""))
  observeEvent(input$reset_aop, updateSelectInput(session, "aop_selectionnee", selected = ""))
  
  observe({
    aop_disponibles <- aop_centroides %>% 
      filter(categorie %in% input$categories)
    
    if (input$departement != "") {
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
    if (length(input$categories) == 0) {
      return(aop_centroides[0, ])
    }
    aop_centroides %>% filter(categorie %in% input$categories)
  })
  
  observe({
    if (length(input$categories) == 0) {
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
    
    if (input$departement != "" && input$aop_selectionnee == "") {
      dept_selectionne <- departements %>% filter(NOM_M == input$departement)
      aop_dept_ids <- aop_communes %>%
        filter(INSEE_DEP %in% dept_selectionne$INSEE_DEP) %>%
        pull(AOP) %>%
        unique()
      aop_points <- aop_points %>% filter(AOP %in% aop_dept_ids)
    }
    
    if (input$aop_selectionnee != "") {
      communes_aop <- aop_communes %>% filter(AOP == input$aop_selectionnee)
      if (nrow(communes_aop) == 0) return()
      
      dept_aop <- departements %>% filter(NOM_M %in% unique(communes_aop$Departement))
      point_aop <- aop_filtrees() %>% filter(AOP == input$aop_selectionnee)
      if (nrow(point_aop) == 0) return()
      
      bbox <- st_bbox(communes_aop)
      marge <- 0.1
      
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
                    color = "#000fff", weight = 1,
                    popup = ~paste0("<strong>", Commune, "</strong><br>Département: ", 
                                    Departement, "<br>AOP: ", AOP),
                    highlightOptions = highlightOptions(weight = 2, color = "#0080ff",
                                                        fillOpacity = 0.8, bringToFront = TRUE),
                    group = "communes") %>%
        addCircleMarkers(data = point_aop, radius = 12, color = "#000fff",
                         fillColor = "#0080ff", fillOpacity = 1, weight = 3,
                         popup = ~paste0("<strong>", AOP, "</strong><br>Catégorie: ", 
                                         categorie, "<br>Communes: ", n_communes),
                         group = "markers") %>%
        fitBounds(bbox["xmin"] * (1 - marge), bbox["ymin"] * (1 - marge),
                  bbox["xmax"] * (1 + marge), bbox["ymax"] * (1 + marge))
      return()
    }
    
    if (input$departement != "") {
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
        addCircleMarkers(data = aop_points, radius = 8, color = "#000fff",
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
    
    leafletProxy("carte") %>%
      clearShapes() %>%
      clearMarkers() %>%
      clearGroup("departements") %>%
      clearGroup("markers") %>%
      addPolygons(data = departements, fillColor = "#95d5a6", fillOpacity = 0.3,
                  color = "#1fc919", weight = 1,
                  popup = ~paste0("<strong>", NOM_M, "</strong>"), layerId = ~NOM_M,
                  group = "departements") %>%
      addCircleMarkers(data = aop_points, radius = 6, color = "#000fff",
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
    if (!is.null(click$id)) updateSelectInput(session, "aop_selectionnee", selected = click$id)
  })
  
  observeEvent(input$carte_shape_click, {
    click <- input$carte_shape_click
    if (!is.null(click$id)) updateSelectInput(session, "departement", selected = click$id)
  })
  
  output$info_selection <- renderText({
    if (length(input$categories) == 0) {
      return("Aucune catégorie sélectionnée")
    }
    
    n_aop_total <- nrow(aop_filtrees())
    
    if (input$aop_selectionnee != "") {
      aop_info <- aop_communes %>% filter(AOP == input$aop_selectionnee)
      return(paste0("AOP sélectionnée : ", input$aop_selectionnee, "\n",
                    nrow(aop_info), " communes dans ", 
                    length(unique(aop_info$Departement)), " département(s)"))
    }
    
    if (input$departement != "") {
      dept_sel <- departements %>% filter(NOM_M == input$departement)
      aop_dept_ids <- aop_communes %>%
        filter(INSEE_DEP %in% dept_sel$INSEE_DEP) %>%
        pull(AOP) %>%
        unique()
      n_aop_dept <- sum(aop_filtrees()$AOP %in% aop_dept_ids)
      
      return(paste0("Département : ", input$departement, "\n",
                    n_aop_dept, " AOP(s) dans ce département"))
    }
    
    paste0("France entière : ", n_aop_total, " AOP(s) affichées")
  })
}

#ANCIEN CODE
# output$map <- renderLeaflet({ 
#   leaflet() |> 
#     addTiles() |> 
#     setView(4.00000000000000, 47.0000000000000, zoom = 5) 
# })
# 
# output$distPlot <- renderPlot({
#   x    <- faithful[, 2]
#   bins <- seq(min(x), max(x), length.out = input$bins + 1)
#   hist(x, breaks = bins, col = 'darkgray', border = 'white',
#        xlab = 'Waiting time to next eruption (in mins)',
#        main = 'Histogram of waiting times')
# })
#}