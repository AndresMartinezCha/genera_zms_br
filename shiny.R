
library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(htmlwidgets)
library(foreign)
sf_use_s2(F)

sectores <- readRDS('msectores.rds')
zms <- read.csv('zm_sectores.csv')
ciudades <- read.dbf('GEOFT_CIDADE_2016.dbf') %>%
  st_as_sf(coords = c('long_cid', 'lat_cid')) %>%
  st_set_crs(st_crs(sectores)) %>%
  slice(-5493)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput('hor','Horizontal',-1,1,0,0.1),
      sliderInput('ver','Vertical',-1,1,0,0.1),
      selectInput('zm_clv', 'Selecciona la zona metropolitana', unique(zms$zm_clv)),
      actionButton('restaura', 'Restaurar original'),
      actionButton('original', 'Cargar'),br(),br(),
      sliderInput('mbuffer', 'Buffer', 0.1, 1, 0.2, 0.1), br(),
      # numericInput('maximo', 'Agregar poligonos con area maxima', 100),
      # actionButton('agFiltro', 'Agregar'),br(),br(),
      actionButton('agencirculo', 'Agregar en circulo'),
      actionButton('elencirculo', 'Eliminar en circulo'),
      actionButton('borrarcirculo', 'Borrar circulo'),br(),br(),br(),
      textOutput('grupo'),
      actionButton('agregarGrupo','Agregar grupo'),
      actionButton('eliminarGrupo','Eliminar grupo'),br(),
      actionButton('borrarTodo', 'Borrar todo'),br(),br(),br(),
      actionButton('guardar', 'Guardar'),
      width = 2
    ),
    mainPanel(
      tags$style(type = "text/css", "#mapa {height: calc(100vh - 1px) !important;}"),
      leafletOutput('mapa', height = '50%'),
      width = 10
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues()
  
  observeEvent(input$original, {
    showModal(
      modalDialog(
        title="Cargar ZM?",
        footer = tagList(actionButton("sioriginal", "Confirmar"),modalButton("Cancel"))
      )
    )
  })
  observeEvent(input$restaura, {
    showModal(
      modalDialog(
        title="Restaurar original?",
        footer = tagList(actionButton("sirestaura", "Confirmar"),modalButton("Cancel"))
      )
    )
  })
  observeEvent(input$guardar, {
    showModal(
      modalDialog(
        title="Sobreescribir?",
        footer = tagList(actionButton("siguardar", "Confirmar"),modalButton("Cancel"))
      )
    )
  })
  observeEvent(input$borrarTodo, {
    showModal(
      modalDialog(
        title="Borrar todo?",
        footer = tagList(actionButton("siborrar", "Confirmar"),modalButton("Cancel"))
      )
    )
  })
  observeEvent(input$sirestaura, {
    removeModal()
    mzm <- zms %>% filter(zm_clv==input$zm_clv) %>% select(zm_clv, zm_nom, Cd_geocodi)
    saveRDS(mzm, paste0('arreglado/zm_',input$zm_clv,'.rds'))
  })
  observeEvent(input$siborrar, {
    removeModal()
    mzm <- rv$mzm
    rv$mzm <- mzm %>% filter(Cd_geocodi=='aaaaa')
  })
  observeEvent(input$siguardar, {
    removeModal()
    mzm <- rv$mzm
    mzm <- sectores %>%
      st_drop_geometry() %>% 
      filter(Cd_geocodi%in%mzm$Cd_geocodi) %>% 
      mutate(zm_clv=input$zm_clv, zm_nom = paste0('Zona Metropolitana ', input$zm_clv)) %>% 
      select(zm_clv, zm_nom, Cd_geocodi)
    saveRDS(mzm, paste0('arreglado/zm_',input$zm_clv,'.rds'))
    showNotification("Guardado")
  })
  observeEvent(input$sioriginal, {
    removeModal()
    withProgress(message = 'Cargando...', value = 0, {
      mzm <- readRDS(paste0('arreglado/zm_',input$zm_clv,'.rds'))
      mzm <- sectores %>% filter(Cd_geocodi%in%mzm$Cd_geocodi)
      cerca <- mzm
      if(nrow(cerca)==0) cerca <- sectores %>% filter(Cd_geocodi%in%zms$Cd_geocodi[zms$zm_clv==input$zm_clv])
      cerca <- suppressWarnings({
        cerca %>% 
          st_bbox() %>% 
          st_as_sfc() %>% 
          st_centroid()
      })
      # cerca <- cerca %>% 
        # st_bbox() %>% 
        # st_as_sfc() %>% 
        # st_centroid()
      cerca <- st_point(c(st_coordinates(cerca)[,1]+input$hor,st_coordinates(cerca)[,2])+input$ver)
      cerca <- cerca %>% 
        st_buffer(input$mbuffer)
      mciudades <- suppressMessages(cerca %>% st_intersects(ciudades))
      cerca <- suppressMessages(cerca %>% st_intersects(sectores))
      mciudades <- ciudades %>% slice(mciudades[[1]])
      cerca <- sectores %>% slice(cerca[[1]])
      rv$mzm <- mzm
      rv$cerca <- cerca
      rv$mciudades <- mciudades
      rv$circulo <- NULL
      rv$grupo <- NULL
      incProgress(1)
    })
  })
  output$mapa <- renderLeaflet({
    if(is.null(rv$mzm)) return()
    mzm <- rv$mzm
    mzm <- suppressMessages(st_union(mzm))
    cerca <- rv$cerca
    mciudades <- rv$mciudades
    mapa <- leaflet() %>% 
      addMapPane("ZM", zIndex = 410) %>%
      addMapPane("circulo", zIndex = 415) %>%
      addMapPane("cerca", zIndex = 420) %>%
      addMapPane("ciudades", zIndex = 430) %>% 
      addProviderTiles('OpenStreetMap', group = 'OpenStreetMap') %>% 
      addProviderTiles('OneMapSG.Grey', group = 'OneMapSG.Grey') %>%
      addTiles('http://mt0.google.com/vt/lyrs=s&hl=en&x={x}&y={y}&z={z}&s=Ga', group = 'Esri.WorldImagery') %>% 
      addPolygons(data = mzm, fillColor = '#2ddc9c', fillOpacity = 0.5, stroke = F, group = 'ZM', options = pathOptions(pane = "ZM")) %>% 
      addPolygons(data=cerca, color = 'black',weight = 0.5, opacity = 1, fillColor = 'grey', fillOpacity = 0.3,layerId = cerca$Cd_geocodi, 
                  group = 'Cercanos', options = pathOptions(pane = "cerca"), highlightOptions = highlightOptions(color = 'red', weight = 1),
                  popup= ~paste(actionLink(inputId = "modal", label = "Seleccionar", onclick = 'Shiny.setInputValue(\"button_click\", this.id, {priority: \"event\"})'))) %>% 
      addCircleMarkers(data = mciudades, color = 'black', fillColor = 'red', weight = 0.5, opacity = 1, fillOpacity = 1, radius = 4, 
                       group = 'Ciudades', options = pathOptions(pane = "ciudades")) %>%
      addLayersControl(
        baseGroups = c('Esri.WorldImagery','OpenStreetMap','OneMapSG.Grey'),
        overlayGroups = c("ZM", "Cercanos", 'Ciudades'),
        options = layersControlOptions(collapsed = FALSE)
      )
    if(length(mzm)!=0){
      bounds <- mzm %>% 
        st_bbox() %>% 
        as.character()
      mapa <- mapa %>%
        fitBounds(bounds[1], bounds[2], bounds[3], bounds[4])
    }
    if(!is.null(rv$circulo)) {
      mapa <- mapa %>% 
        addPolygons(data = rv$circulo, color = 'black', weight = 2, fillColor = 'red', fillOpacity = 0.4, options = pathOptions(pane = "circulo"))
    }
    mapa
  })
  
  observeEvent(input$button_click, {
    showModal(
      modalDialog(
        sliderInput('agbuffer','Agregar circulo',0.01,0.3,0.01,0.005),
        actionButton('crearcir','Crear circulo'),
        footer = tagList(
          actionButton("agrupo", "Agregar a grupo"),
          actionButton("agregar", "Agregar"),
          actionButton("eliminar", "Eliminar"),
          modalButton("Cancel")
        )
      )
    )
  })
  observeEvent(input$crearcir, {
    removeModal()
    punto <- st_point(c(input$mapa_shape_click$lng,input$mapa_shape_click$lat))
    circulo <- st_buffer(punto, input$agbuffer)
    circulo <- st_sf(id = 1, geometry = list(circulo), crs = st_crs(sectores))
    rv$circulo <- circulo
  })
  observeEvent(input$agrupo, {
    removeModal()
    mgrupo <- rv$grupo
    mgrupo <- c(mgrupo, input$mapa_shape_click$id)
    rv$grupo <- unique(mgrupo)
  })
  observeEvent(input$agregar, {
    removeModal()
    nuevo <- input$mapa_shape_click$id
    mzm <- rv$mzm
    if(!nuevo%in%mzm$Cd_geocodi) rv$mzm <- bind_rows(mzm, rv$cerca %>% filter(Cd_geocodi==nuevo))
  })
  observeEvent(input$eliminar, {
    removeModal()
    nuevo <- input$mapa_shape_click$id
    mzm <- rv$mzm
    if(nuevo%in%mzm$Cd_geocodi) rv$mzm <- mzm %>% filter(Cd_geocodi!=nuevo)
  })
  observeEvent(input$agFiltro, {
    nuevos <- rv$cerca
    nuevos <- nuevos %>% filter(marea<=input$maximo)
    mzm <- rv$mzm
    nuevos <- unique(c(mzm$Cd_geocodi,nuevos$Cd_geocodi))
    mzm <- sectores %>% filter(Cd_geocodi%in%nuevos)
    rv$mzm <- mzm
  })
  output$grupo <- renderText({
    if(is.null(rv$grupo)) return()
    mgrupo <- rv$grupo
    mgrupo
  })
  observeEvent(input$agregarGrupo, {
    if(is.null(rv$grupo)) return()
    mgrupo <- rv$grupo
    mzm <- rv$mzm
    if(any(!mgrupo%in%mzm$Cd_geocodi)){
      mgrupo <- mgrupo[!mgrupo%in%mzm$Cd_geocodi]
      rv$mzm <- bind_rows(mzm, rv$cerca %>% filter(Cd_geocodi%in%mgrupo))
    }
    rv$grupo <- NULL
  })
  observeEvent(input$eliminarGrupo, {
    if(is.null(rv$grupo)) return()
    mgrupo <- rv$grupo
    mzm <- rv$mzm
    if(any(mgrupo%in%mzm$Cd_geocodi)){
      mgrupo <- mgrupo[mgrupo%in%mzm$Cd_geocodi]
      rv$mzm <- mzm %>% filter(!Cd_geocodi%in%mgrupo)
    }
    rv$grupo <- NULL
  })
  observeEvent(input$borrarcirculo, {
    rv$circulo <- NULL
  })
  observeEvent(input$agencirculo, {
    if(is.null(rv$circulo)) return()
    circulo <- rv$circulo
    cerca <- rv$cerca
    mzm <- rv$mzm
    dentro <- suppressMessages(st_intersects(circulo, cerca))
    cuales <- cerca$Cd_geocodi[dentro[[1]]]
    if(any(!cuales%in%mzm$Cd_geocodi)){
      cuales <- cuales[!cuales%in%mzm$Cd_geocodi]
      rv$mzm <- bind_rows(mzm, rv$cerca %>% filter(Cd_geocodi%in%cuales))
    }
    rv$circulo <- NULL
  })
  observeEvent(input$elencirculo, {
    if(is.null(rv$circulo)) return()
    circulo <- rv$circulo
    cerca <- rv$cerca
    mzm <- rv$mzm
    dentro <- suppressMessages(st_intersects(circulo, cerca))
    cuales <- cerca$Cd_geocodi[dentro[[1]]]
    if(any(cuales%in%mzm$Cd_geocodi)){
      cuales <- cuales[cuales%in%mzm$Cd_geocodi]
      rv$mzm <- mzm %>% filter(!Cd_geocodi%in%cuales)
    }
    rv$circulo <- NULL
  })
}

shinyApp(ui, server)

