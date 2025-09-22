function(input, output, session) {
  
  # =============================================================================
  # 1. VALORES REACTIVOS Y PROCESAMIENTO DE DATOS
  # =============================================================================
  
  # Reactive values para control de estado
  valores_reactivos <- reactiveValues(
    datos_actualizados = Sys.time(),
    filtros_aplicados = FALSE,
    marca_seleccionada = NULL,
    alertas_nuevas = 0,
    logs_sistema = c(
      paste("[", Sys.time(), "] Sistema RLT iniciado correctamente"),
      paste("[", Sys.time(), "] Datos del parque vehicular cargados:", nrow(metricas_global), "marcas"),
      paste("[", Sys.time(), "] Dashboard ejecutivo activado")
    )
  )
  
  # Datos filtrados reactivos
  datos_filtrados <- reactive({
    cat("🔄 Aplicando filtros al dataset...\n")
    
    datos <- metricas_global
    
    # Filtro por categoría de volumen
    if (!is.null(input$filtro_categoria_volumen) && input$filtro_categoria_volumen != "todas") {
      categoria_map <- list(
        "alto" = "Alto Volumen (100K+)",
        "medio" = "Volumen Medio (10K-100K)",
        "bajo" = "Volumen Bajo (1K-10K)",
        "minimo" = "Volumen Mínimo (<1K)"
      )
      datos <- datos %>% 
        filter(Categoria_Volumen == categoria_map[[input$filtro_categoria_volumen]])
    }
    
    # Filtro por potencial
    if (!is.null(input$filtro_potencial) && input$filtro_potencial != "todos") {
      potencial_map <- list(
        "alta" = "🔴 ALTA PRIORIDAD",
        "media" = "🟠 MEDIA PRIORIDAD",
        "emergente" = "🔵 EMERGENTE",
        "baja" = "⚫ BAJA PRIORIDAD"
      )
      datos <- datos %>% 
        filter(Potencial_Radiadores == potencial_map[[input$filtro_potencial]])
    }
    
    # Filtro por volumen mínimo
    if (!is.null(input$min_vehiculos) && input$min_vehiculos > 0) {
      datos <- datos %>% filter(Volumen_Final >= input$min_vehiculos)
    }
    
    cat("✅ Filtros aplicados, registros resultantes:", nrow(datos), "\n")
    return(datos)
  })
  
  # =============================================================================
  # 2. DASHBOARD EJECUTIVO - VALUE BOXES
  # =============================================================================
  
  # Value Box: Total Vehículos 
  
  output$vb_total_vehiculos <- renderValueBox({
    
    valor <- prettyNum(kpis_global$total_vehiculos, big.mark = ',')
    valueBox(
      value = valor, 
      subtitle = "Total Vehículos Calculados", 
      icon = icon("car"),
      color = "red"
    )
  })
  
  # Value Box: Marcas Alta Prioridad 
  output$vb_marcas_alta_prioridad <- renderValueBox({
    
      valor <-  kpis_global$marcas_alta_prioridad
      valueBox(
        value = valor,
        subtitle = "Marcas Alta Prioridad",
        icon = icon("bullseye"),
        color = "orange"
      )
  })
  
  # Value Box: Crecimiento Promedio del Mercado 
  output$vb_crecimiento_mercado <- renderValueBox({
    
  valor <- kpis_global$crecimiento_promedio
      
  valueBox(
        value = paste0(round(valor, 1), "%"),
        subtitle = "Crecimiento Prom. Marcas Veh.",
        icon = icon("chart-line"),
        color = "green"
      )
  })
  
  # Value Box: Score Promedio de Oportunidad 
  output$vb_score_oportunidad <- renderValueBox({
    
      valor <- kpis_global$score_promedio
    
        valueBox(
        value = round(valor, 1),
        subtitle = "Score Promedio (0-100)",
        icon = icon("star"),
        color = "blue"
      )
  })
  
  
  # =============================================================================
  # 3. GRÁFICOS PRINCIPALES - DASHBOARD EJECUTIVO
  # =============================================================================
  
  # Mapa Estratégico Principal
  output$mapa_estrategico_principal <- renderPlotly({
      
    metricas_global %>% 
      filter(Volumen_Final >= 1000) %>%
      mutate(
        # Crear una escala de tamaño controlada entre 8 y 25
        tamano_punto = pmax(8, pmin(25, sqrt(Volumen_Final/5000) * 10))
      ) %>%
      plot_ly(
        x = ~log10(Volumen_Final), 
        y = ~Crecimiento_Relativo,
        size = ~tamano_punto,
        color = ~Potencial_Radiadores,
        colors = c("ALTA PRIORIDAD" = "#E31A1C", 
                   "MEDIA PRIORIDAD" = "#FF7F00",
                   "EMERGENTE" = "#1F78B4",
                   "BAJA PRIORIDAD" = "#666666"),
        text = ~paste0("Marca: ", Marca_Vehiculo,
                       "<br>Volumen: ", format(Volumen_Final, big.mark = ","),
                       "<br>Crecimiento: ", round(Crecimiento_Relativo, 1), "%",
                       "<br>Potencial: ", Potencial_Radiadores),
        hovertemplate = "%{text}<extra></extra>",
        type = "scatter",
        mode = "markers",
        marker = list(
          opacity = 1,
          line = list(width = 2, color = "#000000"),
          sizemode = "diameter"
        ),
        sizes = c(8, 25)
      ) %>%
      add_annotations(
        data = metricas_marcas %>% 
          filter(Potencial_Radiadores %in% c("ALTA PRIORIDAD", "MEDIA PRIORIDAD"),
                 Volumen_Final >= 1000),
        x = ~log10(Volumen_Final),
        y = ~Crecimiento_Relativo + 3,
        text = ~Marca_Vehiculo,
        showarrow = FALSE,
        font = list(size = 11, color = "#000000", family = "Arial")
      ) %>%
      layout(
        title = list(
          text = "<b>MAPA ESTRATÉGICO: Volumen vs Crecimiento</b><br><sub>Identificación de oportunidades para radiadores</sub>",
          font = list(size = 16, color = "#000000", family = "Arial")
        ),
        xaxis = list(
          title = "Volumen de Vehículos (Log10)",
          showgrid = TRUE,
          gridcolor = "#E0E0E0",
          titlefont = list(color = "#000000", size = 14),
          tickfont = list(color = "#000000", size = 12)
        ),
        yaxis = list(
          title = "Crecimiento Relativo (%)",
          showgrid = TRUE,
          gridcolor = "#E0E0E0",
          titlefont = list(color = "#000000", size = 14),
          tickfont = list(color = "#000000", size = 12)
        ),
        legend = list(
          title = list(text = "<b>Potencial para Radiadores</b>"),
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = -0.15,
          font = list(color = "#000000", size = 11)
        ),
        plot_bgcolor = "#FAFAFA",
        paper_bgcolor = "#FFFFFF",
        hovermode = "closest"
      )
    
    
    
  })
  
  
  

}