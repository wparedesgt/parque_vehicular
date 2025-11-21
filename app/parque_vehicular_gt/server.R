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
      paste("[", Sys.time(), "] Sistema DS Conexión iniciado correctamente"),
      paste("[", Sys.time(), "] Datos del parque vehicular cargados:", nrow(metricas_global), "marcas"),
      paste("[", Sys.time(), "] Dashboard ejecutivo activado")
    )
  )
  
  # Datos filtrados reactivos
  datos_filtrados <- reactive({
    cat("ðŸ”„ Aplicando filtros al dataset...\n")
    
    datos <- metricas_global
    
    # Filtro por categorÃ­a de volumen
    if (!is.null(input$filtro_categoria_volumen) && input$filtro_categoria_volumen != "todas") {
      categoria_map <- list(
        "alto" = "Alto Volumen (100K+)",
        "medio" = "Volumen Medio (10K-100K)",
        "bajo" = "Volumen Bajo (1K-10K)",
        "minimo" = "Volumen MÃ­nimo (<1K)"
      )
      datos <- datos %>% 
        filter(Categoria_Volumen == categoria_map[[input$filtro_categoria_volumen]])
    }
    
    # Filtro por potencial
    if (!is.null(input$filtro_potencial) && input$filtro_potencial != "todos") {
      potencial_map <- list(
        "alta" = "ðŸ”´ ALTA PRIORIDAD",
        "media" = "ðŸŸ  MEDIA PRIORIDAD",
        "emergente" = "ðŸ”µ EMERGENTE",
        "baja" = "âš« BAJA PRIORIDAD"
      )
      datos <- datos %>% 
        filter(Potencial_Analytics == potencial_map[[input$filtro_potencial]])
    }
    
    # Filtro por volumen mÃ­nimo
    if (!is.null(input$min_vehiculos) && input$min_vehiculos > 0) {
      datos <- datos %>% filter(Volumen_Final >= input$min_vehiculos)
    }
    
    cat("âœ… Filtros aplicados, registros resultantes:", nrow(datos), "\n")
    return(datos)
  })
  
  # =============================================================================
  # 2. DASHBOARD EJECUTIVO - VALUE BOXES
  # =============================================================================
  
  # Value Box: Total VehÃ­culos 
  
  output$vb_total_vehiculos <- renderValueBox({
    
    valor <- prettyNum(kpis_global$total_vehiculos, big.mark = ',')
    valueBox(
      value = valor, 
      subtitle = "Total VehÃ­culos Calculados", 
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
  # 3. GRÃFICOS PRINCIPALES - DASHBOARD EJECUTIVO
  # =============================================================================
  
  # Mapa EstratÃ©gico Principal - CORRECCIÃ“N
  output$mapa_estrategico_principal <- renderPlotly({
    
    metricas_global %>% 
      filter(Volumen_Final >= 1000) %>%
      mutate(
        # Crear una escala de tamaÃ±o controlada entre 8 y 25
        tamano_punto = pmax(8, pmin(25, sqrt(Volumen_Final/5000) * 10))
      ) %>%
      plot_ly(
        x = ~log10(Volumen_Final), 
        y = ~Crecimiento_Relativo,
        size = ~tamano_punto,
        color = ~Potencial_Analytics,
        # CORRECCIÃ“N: Los colores deben coincidir exactamente con los valores de global.R
        colors = c("ðŸ”´ ALTA PRIORIDAD" = "#ef4444", 
                   "ðŸŸ  MEDIA PRIORIDAD" = "#f59e0b",
                   "ðŸ”µ EMERGENTE" = "#3b82f6",
                   "âš« BAJA PRIORIDAD" = "#64748b"),
        text = ~paste0("Marca: ", Marca_Vehiculo,
                       "<br>Volumen: ", format(Volumen_Final, big.mark = ","),
                       "<br>Crecimiento: ", round(Crecimiento_Relativo, 1), "%",
                       "<br>Potencial: ", Potencial_Analytics),
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
        data = metricas_global %>% 
          filter(Potencial_Analytics %in% c("ðŸ”´ ALTA PRIORIDAD", "ðŸŸ  MEDIA PRIORIDAD"),
                 Volumen_Final >= 1000),
        x = ~log10(Volumen_Final),
        y = ~Crecimiento_Relativo + 3,
        text = ~Marca_Vehiculo,
        showarrow = FALSE,
        font = list(size = 11, color = "#000000", family = "Arial")
      ) %>%
      layout(
        title = list(
          text = "<b>MAPA ESTRATÃ‰GICO: Volumen vs Crecimiento</b><br><sub>IdentificaciÃ³n de oportunidades para análisis predictivo</sub>",
          font = list(size = 16, color = "#000000", family = "Arial")
        ),
        xaxis = list(
          title = "Volumen de VehÃ­culos (Log10)",
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
          title = list(text = "<b>Potencial para Analytics</b>"),
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
  
  
  # Panel de Alertas de Oportunidades
  output$panel_alertas_oportunidades <- renderUI({
    
    # Generar alertas actualizadas
    alertas <- generar_alertas_mercado(metricas_global)
    
    # Lista para almacenar los elementos de alerta
    elementos_alertas <- list()
    
    # Contador para las alertas
    contador <- 1
    
    # Procesar cada tipo de alerta
    if(!is.null(alertas) && length(alertas) > 0) {
      
      for(nombre_alerta in names(alertas)) {
        texto_alerta <- alertas[[nombre_alerta]]
        
        if(!is.null(texto_alerta) && texto_alerta != "") {
          
          # Determinar el estilo segÃºn el tipo de alerta
          if(nombre_alerta == "oportunidad") {
            clase_alerta <- "alert-success"
            icono <- "rocket"
            color_borde <- "#10b981"
          } else if(nombre_alerta == "riesgo") {
            clase_alerta <- "alert-danger" 
            icono <- "exclamation-triangle"
            color_borde <- "#06b6d4"
          } else {
            clase_alerta <- "alert-info"
            icono <- "info-circle"
            color_borde <- "#3498db"
          }
          
          # Crear el elemento de alerta
          elementos_alertas[[contador]] <- div(
            class = paste("alert", clase_alerta),
            style = paste0(
              "margin-bottom: 15px; ",
              "border-left: 4px solid ", color_borde, "; ",
              "border-radius: 8px; ",
              "box-shadow: 0 2px 8px rgba(0,0,0,0.1); ",
              "padding: 15px; ",
              "background: white; ",
              "position: relative;"
            ),
            
            # Icono y contenido
            div(
              style = "display: flex; align-items: flex-start; gap: 12px;",
              
              # Icono
              div(
                style = paste0("color: ", color_borde, "; font-size: 20px; margin-top: 2px;"),
                icon(icono)
              ),
              
              # Texto de la alerta
              div(
                style = "flex: 1; font-size: 13px; line-height: 1.4;",
                HTML(texto_alerta)
              )
            ),
            
            # Timestamp pequeÃ±o
            div(
              style = "text-align: right; margin-top: 8px; font-size: 10px; color: #6c757d;",
              format(Sys.time(), "%H:%M")
            )
          )
          
          contador <- contador + 1
        }
      }
    }
    
    # Si no hay alertas especÃ­ficas, mostrar mÃ©tricas importantes
    if(length(elementos_alertas) == 0) {
      
      # Calcular algunas mÃ©tricas para mostrar
      if(!is.null(metricas_global) && nrow(metricas_global) > 0) {
        
        # Top 3 marcas por score
        top_scores <- metricas_global %>%
          arrange(desc(Score_Oportunidad)) %>%
          head(3)
        
        # Marcas con crecimiento alto
        alto_crecimiento <- metricas_global %>%
          filter(Crecimiento_Relativo > 20, Volumen_Final > 1000) %>%
          nrow()
        
        # Total de marcas analizadas
        total_marcas <- nrow(metricas_global)
        
        elementos_alertas <- list(
          # Resumen de oportunidades
          div(
            class = "alert alert-info",
            style = "margin-bottom: 15px; border-left: 4px solid #3498db; border-radius: 8px; padding: 15px; background: white;",
            
            div(
              style = "display: flex; align-items: center; gap: 10px; margin-bottom: 10px;",
              icon("chart-line", style = "color: #3498db; font-size: 18px;"),
              strong("Resumen del Mercado", style = "color: #2c3e50;")
            ),
            
            p(paste0("ðŸ“Š ", total_marcas, " marcas analizadas"), 
              style = "margin: 5px 0; font-size: 12px;"),
            p(paste0("ðŸš€ ", alto_crecimiento, " marcas con alto crecimiento"), 
              style = "margin: 5px 0; font-size: 12px;"),
            p(paste0("â­ Score promedio: ", round(mean(metricas_global$Score_Oportunidad, na.rm = TRUE), 1)), 
              style = "margin: 5px 0; font-size: 12px;")
          ),
          
          # Top performers
          div(
            class = "alert alert-success",
            style = "margin-bottom: 15px; border-left: 4px solid #10b981; border-radius: 8px; padding: 15px; background: white;",
            
            div(
              style = "display: flex; align-items: center; gap: 10px; margin-bottom: 10px;",
              icon("trophy", style = "color: #10b981; font-size: 18px;"),
              strong("Top 3 Oportunidades", style = "color: #2c3e50;")
            ),
            
            lapply(1:min(3, nrow(top_scores)), function(i) {
              marca <- top_scores[i, ]
              p(
                paste0(i, ". ", marca$Marca_Vehiculo, " (Score: ", round(marca$Score_Oportunidad, 1), ")"),
                style = "margin: 3px 0; font-size: 12px; padding-left: 10px;"
              )
            })
          ),
          
          # ActualizaciÃ³n
          div(
            class = "alert alert-warning",
            style = "margin-bottom: 15px; border-left: 4px solid #f59e0b; border-radius: 8px; padding: 15px; background: white;",
            
            div(
              style = "display: flex; align-items: center; gap: 10px;",
              icon("clock", style = "color: #f59e0b; font-size: 16px;"),
              div(
                strong("Ãšltima actualizaciÃ³n", style = "color: #2c3e50; font-size: 12px;"),
                br(),
                span(format(Sys.time(), "%d/%m/%Y %H:%M"), style = "font-size: 11px; color: #6c757d;")
              )
            )
          )
        )
      }
    }
    
    # Devolver todos los elementos
    do.call(tagList, elementos_alertas)
  })
  
  # =============================================================================
  # RESUMEN EJECUTIVO DEL MERCADO - TABLAS
  # =============================================================================
  
  # Tabla de MÃ©tricas de Crecimiento
  output$tabla_metricas_crecimiento <- renderTable({
    
    if(is.null(metricas_global) || nrow(metricas_global) == 0) {
      return(data.frame(
        MÃ©trica = "Sin datos disponibles",
        Valor = "N/A"
      ))
    }
    
    # Calcular mÃ©tricas de crecimiento
    marcas_creciendo <- sum(metricas_global$Crecimiento_Relativo > 0, na.rm = TRUE)
    marcas_declinando <- sum(metricas_global$Crecimiento_Relativo < 0, na.rm = TRUE)
    marcas_estables <- sum(metricas_global$Crecimiento_Relativo == 0, na.rm = TRUE)
    
    crecimiento_promedio <- mean(metricas_global$Crecimiento_Relativo, na.rm = TRUE)
    crecimiento_mediano <- median(metricas_global$Crecimiento_Relativo, na.rm = TRUE)
    
    # Encontrar extremos
    marca_mayor_crecimiento <- metricas_global %>%
      filter(Crecimiento_Relativo == max(Crecimiento_Relativo, na.rm = TRUE)) %>%
      slice(1) %>%
      pull(Marca_Vehiculo)
    
    max_crecimiento <- max(metricas_global$Crecimiento_Relativo, na.rm = TRUE)
    
    # Volatilidad promedio
    volatilidad_promedio <- mean(metricas_global$Coef_Variacion, na.rm = TRUE) * 100
    
    # Crear tabla
    data.frame(
      MÃ©trica = c(
        "ðŸ“ˆ Crecimiento Promedio",
        "ðŸ“Š Crecimiento Mediano", 
        "ðŸš€ Marcas en Crecimiento",
        "ðŸ“‰ Marcas en Declive",
        "âš–ï¸ Marcas Estables",
        "ðŸ† Mayor Crecimiento",
        "ðŸ“Š Volatilidad Promedio"
      ),
      Valor = c(
        paste0(round(crecimiento_promedio, 1), "%"),
        paste0(round(crecimiento_mediano, 1), "%"),
        paste0(marcas_creciendo, " marcas"),
        paste0(marcas_declinando, " marcas"), 
        paste0(marcas_estables, " marcas"),
        paste0(marca_mayor_crecimiento, " (", round(max_crecimiento, 1), "%)"),
        paste0(round(volatilidad_promedio, 1), "%")
      )
    )
    
  }, bordered = TRUE, striped = TRUE, hover = TRUE, spacing = "s",
  width = "100%", align = "lr")
  
  # Tabla de ConcentraciÃ³n del Mercado  
  output$tabla_concentracion_mercado <- renderTable({
    
    if(is.null(metricas_global) || nrow(metricas_global) == 0) {
      return(data.frame(
        MÃ©trica = "Sin datos disponibles",
        Valor = "N/A"
      ))
    }
    
    # Calcular mÃ©tricas de concentraciÃ³n
    total_vehiculos <- sum(metricas_global$Volumen_Final, na.rm = TRUE)
    
    # Top 5 marcas por volumen
    top_5_vol <- sum(head(metricas_global$Participacion_Mercado, 5), na.rm = TRUE)
    
    # Top 10 marcas
    top_10_vol <- sum(head(metricas_global$Participacion_Mercado, 10), na.rm = TRUE)
    
    # Ãndice Herfindahl-Hirschman (HHI)
    hhi <- sum((metricas_global$Participacion_Mercado)^2, na.rm = TRUE)
    
    # InterpretaciÃ³n del HHI
    if(hhi < 1500) {
      interpretacion_hhi <- "Baja concentraciÃ³n"
    } else if(hhi < 2500) {
      interpretacion_hhi <- "ConcentraciÃ³n moderada"  
    } else {
      interpretacion_hhi <- "Alta concentraciÃ³n"
    }
    
    # LÃ­der del mercado
    lider_mercado <- metricas_global %>%
      arrange(desc(Volumen_Final)) %>%
      slice(1)
    
    # DistribuciÃ³n por categorÃ­as
    dist_categorias <- metricas_global %>%
      count(Categoria_Volumen, name = "cantidad") %>%
      arrange(desc(cantidad))
    
    categoria_dominante <- dist_categorias$Categoria_Volumen[1]
    
    # Crear tabla
    data.frame(
      MÃ©trica = c(
        "ðŸ¥‡ LÃ­der del Mercado",
        "ðŸ† Top 5 ConcentraciÃ³n", 
        "ðŸ“Š Top 10 ConcentraciÃ³n",
        "ðŸ“ˆ Ãndice HHI",
        "ðŸŽ¯ Nivel de Competencia",
        "ðŸ“‹ Total de Marcas",
        "ðŸ·ï¸ CategorÃ­a Dominante"
      ),
      Valor = c(
        paste0(lider_mercado$Marca_Vehiculo, " (", round(lider_mercado$Participacion_Mercado, 1), "%)"),
        paste0(round(top_5_vol, 1), "%"),
        paste0(round(top_10_vol, 1), "%"), 
        paste0(round(hhi, 0), " puntos"),
        interpretacion_hhi,
        paste0(nrow(metricas_global), " marcas activas"),
        categoria_dominante
      )
    )
    
  }, bordered = TRUE, striped = TRUE, hover = TRUE, spacing = "s", 
  width = "100%", align = "lr")
  
  
  # =============================================================================
  # TOP PERFORMERS - TABLAS DINÃMICAS
  # =============================================================================
  
  # Top por Volumen
  output$tabla_top_volumen <- renderDataTable({
    
    if(is.null(metricas_global) || nrow(metricas_global) == 0) {
      return(data.frame(
        Ranking = "Sin datos",
        Marca = "N/A", 
        VehÃ­culos = "N/A",
        ParticipaciÃ³n = "N/A"
      ))
    }
    
    tabla_volumen <- metricas_global %>%
      arrange(desc(Volumen_Final)) %>%
      head(10) %>%
      mutate(
        Ranking = 1:n(),
        Marca = Marca_Vehiculo,
        VehÃ­culos = format(Volumen_Final, big.mark = ","),
        `ParticipaciÃ³n %` = paste0(round(Participacion_Mercado, 1), "%"),
        Potencial = case_when(
          str_detect(Potencial_Analytics, "ALTA") ~ "ðŸ”´ Alta",
          str_detect(Potencial_Analytics, "MEDIA") ~ "ðŸŸ  Media", 
          str_detect(Potencial_Analytics, "EMERGENTE") ~ "ðŸ”µ Emergente",
          TRUE ~ "âš« Baja"
        )
      ) %>%
      select(Ranking, Marca, VehÃ­culos, `ParticipaciÃ³n %`, Potencial)
    
    datatable(
      tabla_volumen,
      options = list(
        pageLength = 10,
        scrollY = "300px",
        scrollCollapse = TRUE,
        dom = 't',
        ordering = FALSE,
        columnDefs = list(
          list(targets = 0, width = "15%", className = "dt-center"),
          list(targets = 1, width = "35%"),
          list(targets = 2, width = "20%", className = "dt-right"),
          list(targets = 3, width = "15%", className = "dt-center"),
          list(targets = 4, width = "15%", className = "dt-center")
        )
      ),
      rownames = FALSE,
      class = "compact stripe hover"
    ) %>%
      formatStyle(
        "Ranking",
        backgroundColor = styleInterval(c(1, 2, 3), c("#FFD700", "#C0C0C0", "#CD7F32", "#f8f9fa")),
        fontWeight = "bold"
      )
    
  }, server = FALSE)
  
  # Top por Crecimiento
  output$tabla_top_crecimiento <- renderDataTable({
    
    if(is.null(metricas_global) || nrow(metricas_global) == 0) {
      return(data.frame(
        Ranking = "Sin datos",
        Marca = "N/A",
        Crecimiento = "N/A", 
        Volumen = "N/A"
      ))
    }
    
    tabla_crecimiento <- metricas_global %>%
      filter(Volumen_Final >= 100) %>%  # Solo marcas con volumen mÃ­nimo
      arrange(desc(Crecimiento_Relativo)) %>%
      head(10) %>%
      mutate(
        Ranking = 1:n(),
        Marca = Marca_Vehiculo,
        `Crecimiento %` = paste0(round(Crecimiento_Relativo, 1), "%"),
        Volumen = format(Volumen_Final, big.mark = ","),
        Tendencia = case_when(
          Crecimiento_Relativo >= 50 ~ "ðŸš€ Explosivo",
          Crecimiento_Relativo >= 20 ~ "ðŸ“ˆ Alto",
          Crecimiento_Relativo >= 10 ~ "ðŸ“Š Moderado", 
          Crecimiento_Relativo >= 0 ~ "ðŸ”„ Estable",
          TRUE ~ "ðŸ“‰ Declive"
        )
      ) %>%
      select(Ranking, Marca, `Crecimiento %`, Volumen, Tendencia)
    
    datatable(
      tabla_crecimiento,
      options = list(
        pageLength = 10,
        scrollY = "300px", 
        scrollCollapse = TRUE,
        dom = 't',
        ordering = FALSE,
        columnDefs = list(
          list(targets = 0, width = "15%", className = "dt-center"),
          list(targets = 1, width = "35%"),
          list(targets = 2, width = "20%", className = "dt-right"),
          list(targets = 3, width = "15%", className = "dt-right"),
          list(targets = 4, width = "15%", className = "dt-center")
        )
      ),
      rownames = FALSE,
      class = "compact stripe hover"
    ) %>%
      formatStyle(
        "Crecimiento %",
        backgroundColor = styleInterval(c(10, 20, 50), c("#f8f9fa", "#d4edda", "#c3e6cb", "#b5dfbc")),
        fontWeight = "bold"
      )
    
  }, server = FALSE)
  
  # Top por Score
  output$tabla_top_score <- renderDataTable({
    
    if(is.null(metricas_global) || nrow(metricas_global) == 0) {
      return(data.frame(
        Ranking = "Sin datos",
        Marca = "N/A",
        Score = "N/A",
        ClasificaciÃ³n = "N/A"
      ))
    }
    
    tabla_score <- metricas_global %>%
      arrange(desc(Score_Oportunidad)) %>%
      head(10) %>%
      mutate(
        Ranking = 1:n(),
        Marca = Marca_Vehiculo,
        Score = round(Score_Oportunidad, 1),
        Volumen = format(Volumen_Final, big.mark = ","),
        ClasificaciÃ³n = case_when(
          Score_Oportunidad >= 80 ~ "â­â­â­ Excelente",
          Score_Oportunidad >= 60 ~ "â­â­ Muy Buena",
          Score_Oportunidad >= 40 ~ "â­ Buena",
          TRUE ~ "ðŸ’¡ Potencial"
        ),
        Potencial = case_when(
          str_detect(Potencial_Analytics, "ALTA") ~ "ðŸ”´",
          str_detect(Potencial_Analytics, "MEDIA") ~ "ðŸŸ ",
          str_detect(Potencial_Analytics, "EMERGENTE") ~ "ðŸ”µ", 
          TRUE ~ "âš«"
        )
      ) %>%
      select(Ranking, Marca, Score, Volumen, ClasificaciÃ³n, Potencial)
    
    datatable(
      tabla_score,
      caption = htmltools::tags$caption(
        style = "caption-side: bottom; text-align: left; padding-top: 15px; font-size: 11px; color: #6c757d; line-height: 1.4;",
        htmltools::HTML(
          "<strong>ðŸ“Š CÃ¡lculo del Score de Oportunidad (0-100 puntos):</strong><br>
        â€¢ <strong>Componente Volumen (0-45 pts):</strong> Logâ‚â‚€(Volumen + 1) Ã— 15<br>
        â€¢ <strong>Componente Crecimiento (0-40 pts):</strong> % Crecimiento Ã— 0.8 (mÃ¡x 50%)<br>
        â€¢ <strong>Componente Estabilidad (0-15 pts):</strong> (25 - Coef. VariaciÃ³n Ã— 100) Ã— 0.6<br>
        <em>Score = Volumen + Crecimiento + Estabilidad | Valores normalizados entre 0-100</em>"
        )
      ),
      options = list(
        pageLength = 10,
        scrollY = "300px",
        scrollCollapse = TRUE, 
        dom = 't',
        ordering = FALSE,
        columnDefs = list(
          list(targets = 0, width = "12%", className = "dt-center"),
          list(targets = 1, width = "30%"),
          list(targets = 2, width = "12%", className = "dt-center"),
          list(targets = 3, width = "18%", className = "dt-right"),
          list(targets = 4, width = "20%", className = "dt-center"),
          list(targets = 5, width = "8%", className = "dt-center")
        )
      ),
      rownames = FALSE,
      class = "compact stripe hover"
    ) %>%
      formatStyle(
        "Score",
        backgroundColor = styleInterval(c(40, 60, 80), c("#f8f9fa", "#fff3cd", "#d1ecf1", "#d4edda")),
        fontWeight = "bold",
        color = styleInterval(c(40, 60, 80), c("#6c757d", "#856404", "#0c5460", "#155724"))
      ) %>%
      formatStyle(
        "Ranking", 
        backgroundColor = styleInterval(c(1, 2, 3), c("#FFD700", "#C0C0C0", "#CD7F32", "#f8f9fa")),
        fontWeight = "bold"
      )
    
  }, server = FALSE)
  
  
  # =============================================================================
  # TENDENCIAS TEMPORALES
  # =============================================================================
  
  # Actualizar opciones de marcas para tendencias
  observe({
    if(!is.null(metricas_global) && nrow(metricas_global) > 0) {
      opciones_marcas <- setNames(
        metricas_global$Marca_Vehiculo,
        metricas_global$Marca_Vehiculo
      )
      
      # Top 10 por defecto
      seleccion_default <- head(metricas_global$Marca_Vehiculo, 10)
      
      updateSelectInput(
        session,
        "marcas_seleccionadas",
        choices = opciones_marcas,
        selected = seleccion_default
      )
    }
  })
  
  # GrÃ¡fico de tendencias temporales
  output$grafico_tendencias_temporal <- renderPlotly({
    
    req(input$marcas_seleccionadas)
    
    if(is.null(datos_long_global) || nrow(datos_long_global) == 0) {
      return(plot_ly() %>% layout(title = "Sin datos temporales disponibles"))
    }
    
    # Filtrar datos por marcas seleccionadas
    datos_tendencias <- datos_long_global %>%
      filter(Marca_Vehiculo %in% input$marcas_seleccionadas) %>%
      arrange(Fecha)
    
    if(nrow(datos_tendencias) == 0) {
      return(plot_ly() %>% layout(title = "No hay datos para las marcas seleccionadas"))
    }
    
    # Crear grÃ¡fico base
    fig <- plot_ly()
    
    if(input$tipo_visualizacion == "lineas") {
      
      for(marca in input$marcas_seleccionadas) {
        datos_marca <- datos_tendencias %>% filter(Marca_Vehiculo == marca)
        
        fig <- fig %>%
          add_trace(
            data = datos_marca,
            x = ~Fecha,
            y = ~Vehiculos_Registrados,
            type = "scatter",
            mode = "lines+markers",
            name = marca,
            line = list(width = 3),
            marker = list(size = 6),
            hovertemplate = paste0(
              "<b>%{fullData.name}</b><br>",
              "Fecha: %{x}<br>",
              "VehÃ­culos: %{y:,.0f}<br>",
              "<extra></extra>"
            )
          )
      }
      
      # Agregar lÃ­neas de tendencia si se solicita
      if(input$mostrar_tendencia) {
        for(marca in input$marcas_seleccionadas) {
          datos_marca <- datos_tendencias %>% 
            filter(Marca_Vehiculo == marca) %>%
            mutate(x_num = as.numeric(Fecha))
          
          if(nrow(datos_marca) > 2) {
            modelo <- lm(Vehiculos_Registrados ~ x_num, data = datos_marca)
            predicciones <- predict(modelo, newdata = datos_marca)
            
            fig <- fig %>%
              add_trace(
                data = datos_marca,
                x = ~Fecha,
                y = predicciones,
                type = "scatter",
                mode = "lines",
                name = paste0(marca, " (tendencia)"),
                line = list(dash = "dash", width = 2),
                showlegend = FALSE,
                hoverinfo = "skip"
              )
          }
        }
      }
      
    } else if(input$tipo_visualizacion == "area") {
      
      # GrÃ¡fico de Ã¡rea apilada
      for(marca in input$marcas_seleccionadas) {
        datos_marca <- datos_tendencias %>% filter(Marca_Vehiculo == marca)
        
        fig <- fig %>%
          add_trace(
            data = datos_marca,
            x = ~Fecha,
            y = ~Vehiculos_Registrados,
            type = "scatter",
            mode = "none",
            fill = "tonexty",
            name = marca,
            fillcolor = rainbow(length(input$marcas_seleccionadas))[match(marca, input$marcas_seleccionadas)],
            hovertemplate = paste0(
              "<b>%{fullData.name}</b><br>",
              "Fecha: %{x}<br>",
              "VehÃ­culos: %{y:,.0f}<br>",
              "<extra></extra>"
            )
          )
      }
      
    } else if(input$tipo_visualizacion == "barras") {
      
      # Transformar datos para barras agrupadas
      datos_barras <- datos_tendencias %>%
        select(Fecha, Marca_Vehiculo, Vehiculos_Registrados) %>%
        pivot_wider(names_from = Marca_Vehiculo, values_from = Vehiculos_Registrados, values_fill = 0)
      
      for(marca in input$marcas_seleccionadas) {
        if(marca %in% names(datos_barras)) {
          fig <- fig %>%
            add_trace(
              data = datos_barras,
              x = ~Fecha,
              y = as.formula(paste0("~`", marca, "`")),
              type = "bar",
              name = marca
            )
        }
      }
    }
    
    fig %>%
      layout(
        title = "EvoluciÃ³n Temporal del Parque Vehicular",
        xaxis = list(title = "PerÃ­odo"),
        yaxis = list(title = "VehÃ­culos Registrados"),
        hovermode = "x unified"
      )
  })
  
  # EstadÃ­sticas de tendencias
  output$tabla_estadisticas_tendencias <- renderDataTable({
    
    req(input$marcas_seleccionadas)
    
    if(is.null(datos_long_global)) {
      return(data.frame(Mensaje = "Sin datos temporales"))
    }
    
    estadisticas <- datos_long_global %>%
      filter(Marca_Vehiculo %in% input$marcas_seleccionadas) %>%
      group_by(Marca_Vehiculo) %>%
      summarise(
        PerÃ­odos = n(),
        Promedio = round(mean(Vehiculos_Registrados, na.rm = TRUE), 0),
        Mediana = round(median(Vehiculos_Registrados, na.rm = TRUE), 0),
        MÃ­nimo = min(Vehiculos_Registrados, na.rm = TRUE),
        MÃ¡ximo = max(Vehiculos_Registrados, na.rm = TRUE),
        `Desv. Est.` = round(sd(Vehiculos_Registrados, na.rm = TRUE), 0),
        `Coef. Var.` = round(sd(Vehiculos_Registrados, na.rm = TRUE) / mean(Vehiculos_Registrados, na.rm = TRUE) * 100, 1),
        .groups = "drop"
      ) %>%
      arrange(desc(Promedio))
    
    datatable(
      estadisticas,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 't'
      ),
      rownames = FALSE,
      class = "compact stripe hover"
    ) %>%
      formatCurrency(c("Promedio", "Mediana", "MÃ­nimo", "MÃ¡ximo", "Desv. Est."), "", digits = 0)
    
  }, server = FALSE)
  
  # Panel de mÃ©tricas de tendencia
  output$panel_metricas_tendencia <- renderUI({
    
    req(input$marcas_seleccionadas)
    
    if(is.null(datos_long_global)) {
      return(p("Sin datos disponibles", style = "color: #6c757d;"))
    }
    
    datos_filtrados_trend <- datos_long_global %>%
      filter(Marca_Vehiculo %in% input$marcas_seleccionadas)
    
    if(nrow(datos_filtrados_trend) == 0) {
      return(p("No hay datos para mostrar", style = "color: #6c757d;"))
    }
    
    # Calcular mÃ©tricas
    total_periodos <- length(unique(datos_filtrados_trend$Fecha))
    promedio_general <- mean(datos_filtrados_trend$Vehiculos_Registrados, na.rm = TRUE)
    tendencia_general <- ifelse(
      nrow(datos_filtrados_trend) > 1,
      ifelse(
        last(datos_filtrados_trend$Vehiculos_Registrados) > first(datos_filtrados_trend$Vehiculos_Registrados),
        "Creciente", "Decreciente"
      ),
      "Estable"
    )
    
    tagList(
      div(
        style = "background: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
        h5("ðŸ“Š Resumen General", style = "color: #2c3e50; margin-bottom: 10px;"),
        p(paste("PerÃ­odos analizados:", total_periodos), style = "margin: 5px 0;"),
        p(paste("Promedio general:", format(round(promedio_general), big.mark = ",")), style = "margin: 5px 0;"),
        p(paste("Tendencia:", tendencia_general), style = "margin: 5px 0;")
      ),
      
      div(
        style = "background: #e3f2fd; padding: 15px; border-radius: 8px;",
        h5("ðŸŽ¯ Recomendaciones", style = "color: #1565c0; margin-bottom: 10px;"),
        if(tendencia_general == "Creciente") {
          p("ðŸ“ˆ Mercado en expansiÃ³n - Oportunidad de crecimiento", style = "color: #10b981; margin: 0;")
        } else if(tendencia_general == "Decreciente") {
          p("ðŸ“‰ Mercado en contracciÃ³n - Revisar estrategia", style = "color: #06b6d4; margin: 0;")
        } else {
          p("âš–ï¸ Mercado estable - Monitorear cambios", style = "color: #f59e0b; margin: 0;")
        }
      )
    )
  })
  
  # =============================================================================
  # ANÃLISIS DETALLADO
  # =============================================================================
  
  # Actualizar opciones para anÃ¡lisis detallado
  observe({
    if(!is.null(metricas_global) && nrow(metricas_global) > 0) {
      opciones_detalle <- setNames(
        metricas_global$Marca_Vehiculo,
        paste0(metricas_global$Marca_Vehiculo, " (", format(metricas_global$Volumen_Final, big.mark = ","), " veh.)")
      )
      
      updateSelectInput(
        session,
        "marca_detalle",
        choices = opciones_detalle,
        selected = metricas_global$Marca_Vehiculo[1]
      )
    }
  })
  
  # Panel de mÃ©tricas de marca individual
  output$panel_metricas_marca <- renderUI({
    
    req(input$marca_detalle)
    
    if(is.null(metricas_global)) {
      return(p("Sin datos disponibles"))
    }
    
    marca_data <- metricas_global %>%
      filter(Marca_Vehiculo == input$marca_detalle)
    
    if(nrow(marca_data) == 0) {
      return(p("Marca no encontrada"))
    }
    
    marca_info <- marca_data[1, ]
    
    tagList(
      h4(marca_info$Marca_Vehiculo, style = "color: #1a365d; text-align: center; margin-bottom: 20px;"),
      
      div(
        style = "background: linear-gradient(135deg, #f8f9fa, #e9ecef); padding: 15px; border-radius: 10px; margin-bottom: 15px;",
        
        div(style = "text-align: center; margin-bottom: 15px;",
            h3(format(marca_info$Volumen_Final, big.mark = ","), style = "color: #2c3e50; margin: 0;"),
            p("VehÃ­culos Totales", style = "color: #6c757d; margin: 0; font-size: 12px;")
        ),
        
        hr(style = "margin: 15px 0;"),
        
        div(style = "display: flex; justify-content: space-between; margin-bottom: 10px;",
            span("ðŸ† Ranking Score:", style = "color: #495057; font-size: 13px;"),
            strong(paste("#", which(metricas_global$Marca_Vehiculo[order(-metricas_global$Score_Oportunidad)] == input$marca_detalle)), 
                   style = "color: #1a365d; font-size: 13px;")
        ),
        
        div(style = "display: flex; justify-content: space-between; margin-bottom: 10px;",
            span("ðŸ“ˆ Crecimiento:", style = "color: #495057; font-size: 13px;"),
            strong(paste0(round(marca_info$Crecimiento_Relativo, 1), "%"), 
                   style = paste0("color: ", ifelse(marca_info$Crecimiento_Relativo >= 0, "#10b981", "#06b6d4"), "; font-size: 13px;"))
        ),
        
        div(style = "display: flex; justify-content: space-between; margin-bottom: 10px;",
            span("â­ Score:", style = "color: #495057; font-size: 13px;"),
            strong(paste0(round(marca_info$Score_Oportunidad, 1), "/100"), style = "color: #3498db; font-size: 13px;")
        ),
        
        div(style = "display: flex; justify-content: space-between; margin-bottom: 10px;",
            span("ðŸ“Š ParticipaciÃ³n:", style = "color: #495057; font-size: 13px;"),
            strong(paste0(round(marca_info$Participacion_Mercado, 2), "%"), style = "color: #f59e0b; font-size: 13px;")
        ),
        
        hr(style = "margin: 15px 0;"),
        
        div(
          style = "text-align: center; padding: 10px; background: white; border-radius: 8px;",
          strong(marca_info$Potencial_Analytics, style = "font-size: 11px;")
        )
      ),
      
      div(
        style = "background: #fff3cd; padding: 15px; border-radius: 8px; border-left: 4px solid #f59e0b;",
        h6("ðŸ’¡ AnÃ¡lisis RÃ¡pido", style = "color: #856404; margin-bottom: 8px;"),
        p(
          if(marca_info$Score_Oportunidad >= 70) {
            "Excelente oportunidad para análisis predictivo. Alta prioridad comercial."
          } else if(marca_info$Score_Oportunidad >= 50) {
            "Buena oportunidad. Considerar para estrategia de mediano plazo."
          } else {
            "Oportunidad emergente. Monitorear evoluciÃ³n del mercado."
          },
          style = "color: #856404; font-size: 12px; margin: 0; line-height: 1.4;"
        )
      )
    )
  })
  
  # GrÃ¡fico de evoluciÃ³n individual
  output$grafico_evolucion_individual <- renderPlotly({
    
    req(input$marca_detalle)
    
    if(is.null(datos_long_global)) {
      return(plot_ly() %>% layout(title = "Sin datos temporales"))
    }
    
    datos_marca <- datos_long_global %>%
      filter(Marca_Vehiculo == input$marca_detalle) %>%
      arrange(Fecha)
    
    if(nrow(datos_marca) == 0) {
      return(plot_ly() %>% layout(title = "Sin datos para la marca seleccionada"))
    }
    
    # Crear grÃ¡fico con lÃ­nea y Ã¡rea
    fig <- plot_ly() %>%
      add_trace(
        data = datos_marca,
        x = ~Fecha,
        y = ~Vehiculos_Registrados,
        type = "scatter",
        mode = "lines+markers",
        fill = "tozeroy",
        fillcolor = "rgba(231, 76, 60, 0.2)",
        line = list(color = "#06b6d4", width = 3),
        marker = list(color = "#1a365d", size = 8),
        name = input$marca_detalle,
        hovertemplate = paste0(
          "<b>", input$marca_detalle, "</b><br>",
          "Fecha: %{x}<br>",
          "VehÃ­culos: %{y:,.0f}<br>",
          "<extra></extra>"
        )
      )
    
    # Agregar lÃ­nea de tendencia
    if(nrow(datos_marca) > 2) {
      datos_modelo <- datos_marca %>%
        mutate(x_num = as.numeric(Fecha))
      
      modelo <- lm(Vehiculos_Registrados ~ x_num, data = datos_modelo)
      predicciones <- predict(modelo, newdata = datos_modelo)
      
      fig <- fig %>%
        add_trace(
          data = datos_marca,
          x = ~Fecha,
          y = predicciones,
          type = "scatter",
          mode = "lines",
          line = list(color = "#2c3e50", width = 2, dash = "dash"),
          name = "Tendencia",
          hoverinfo = "skip"
        )
    }
    
    fig %>%
      layout(
        title = paste("EvoluciÃ³n de", input$marca_detalle),
        xaxis = list(title = "PerÃ­odo"),
        yaxis = list(title = "VehÃ­culos Registrados"),
        showlegend = TRUE
      )
  })
  
  # =============================================================================
  # CONFIGURACIÃ“N - INFORMACIÃ“N DEL SISTEMA
  # =============================================================================
  
  # InformaciÃ³n del sistema
  output$info_sistema_ds <- renderText({
    
    info_texto <- paste0(
      "Sistema: Estrategia DS Conexión v2.1.0\n",
      "Estado: Activo âœ“\n",
      "Datos cargados: ", ifelse(is.null(metricas_global), 0, nrow(metricas_global)), " marcas\n",
      "Ãšltima actualizaciÃ³n: ", format(Sys.time(), "%d/%m/%Y %H:%M"), "\n",
      "Memoria utilizada: ", round(object.size(metricas_global) / 1024^2, 2), " MB\n",
      "SesiÃ³n iniciada: ", format(timestamp_app, "%H:%M")
    )
    
    return(info_texto)
  })
  
  # EstadÃ­sticas de uso
  output$tabla_estadisticas_uso_ds <- renderTable({
    
    data.frame(
      MÃ©trica = c(
        "Total Marcas Analizadas",
        "PerÃ­odos Temporales", 
        "Marcas Alta Prioridad",
        "Score Promedio",
        "Tiempo de Procesamiento"
      ),
      Valor = c(
        ifelse(is.null(metricas_global), 0, nrow(metricas_global)),
        ifelse(is.null(datos_long_global), 0, length(unique(datos_long_global$Fecha))),
        ifelse(is.null(kpis_global), 0, kpis_global$marcas_alta_prioridad),
        paste0(ifelse(is.null(kpis_global), 0, round(kpis_global$score_promedio, 1)), " pts"),
        "< 2 seg"
      )
    )
    
  }, bordered = TRUE, striped = TRUE, spacing = "s")
  
  # Log del sistema
  output$log_sistema_ds <- renderText({
    
    log_entries <- c(
      paste("[", format(timestamp_app, "%H:%M:%S"), "] âœ“ Sistema DS Conexión iniciado correctamente"),
      paste("[", format(timestamp_app + 1, "%H:%M:%S"), "] âœ“ Datos del parque vehicular cargados:", ifelse(is.null(metricas_global), 0, nrow(metricas_global)), "marcas"),
      paste("[", format(timestamp_app + 2, "%H:%M:%S"), "] âœ“ MÃ©tricas calculadas exitosamente"),
      paste("[", format(timestamp_app + 3, "%H:%M:%S"), "] âœ“ Dashboard ejecutivo activado"),
      paste("[", format(Sys.time(), "%H:%M:%S"), "] âœ“ Interface de usuario lista"),
      "",
      "â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•",
      "SISTEMA FUNCIONANDO CORRECTAMENTE",
      "â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•"
    )
    
    paste(log_entries, collapse = "\n")
  })
  
  
  
  # =============================================================================
  # ELEMENTOS REACTIVOS Y OBSERVADORES
  # =============================================================================
  
  # Observador para actualizaciÃ³n automÃ¡tica de datos
  observeEvent(input$btn_actualizar_datos, {
    
    # Mostrar mensaje de carga
    showModal(modalDialog(
      title = "ðŸ”„ Actualizando Datos del Parque Vehicular",
      "Procesando informaciÃ³n mÃ¡s reciente del SAT...",
      easyClose = FALSE,
      footer = NULL
    ))
    
    # Simular actualizaciÃ³n (en producciÃ³n aquÃ­ se cargarÃ­an datos reales)
    Sys.sleep(2)
    
    # Actualizar timestamp
    valores_reactivos$datos_actualizados <- Sys.time()
    
    # Agregar entrada al log
    nuevos_logs <- c(
      valores_reactivos$logs_sistema,
      paste("[", format(Sys.time(), "%H:%M:%S"), "] ðŸ”„ Datos actualizados manualmente por el usuario")
    )
    valores_reactivos$logs_sistema <- tail(nuevos_logs, 20)  # Mantener Ãºltimas 20 entradas
    
    removeModal()
    
    # Mostrar notificaciÃ³n de Ã©xito
    showNotification(
      "âœ… Datos del parque vehicular actualizados correctamente",
      type = "success",
      duration = 3
    )
  })
  
  # Observador para aplicar filtros
  observeEvent(input$btn_aplicar_filtros, {
    valores_reactivos$filtros_aplicados <- !valores_reactivos$filtros_aplicados
    
    showNotification(
      "ðŸ” Filtros aplicados correctamente",
      type = "default",
      duration = 2
    )
  })
  
  # Observador para generar anÃ¡lisis detallado
  observeEvent(input$btn_generar_detalle, {
    
    req(input$marca_detalle)
    
    valores_reactivos$marca_seleccionada <- input$marca_detalle
    
    showNotification(
      paste("ðŸ“Š AnÃ¡lisis detallado generado para", input$marca_detalle),
      type = "success",
      duration = 3
    )
  })
  
  # Observador para botones de configuraciÃ³n
  observeEvent(input$limpiar_cache_ds, {
    
    showModal(modalDialog(
      title = "âš ï¸ Confirmar AcciÃ³n",
      "Â¿EstÃ¡ seguro de que desea limpiar el cache del sistema?",
      footer = tagList(
        actionButton("confirmar_limpiar", "SÃ­, Limpiar", class = "btn-warning"),
        modalButton("Cancelar")
      )
    ))
  })
  
  observeEvent(input$confirmar_limpiar, {
    
    removeModal()
    
    # Simular limpieza de cache
    Sys.sleep(1)
    
    # Agregar al log
    valores_reactivos$logs_sistema <- c(
      valores_reactivos$logs_sistema,
      paste("[", format(Sys.time(), "%H:%M:%S"), "] ðŸ—‘ï¸ Cache del sistema limpiado")
    )
    
    showNotification(
      "ðŸ—‘ï¸ Cache limpiado correctamente",
      type = "warning",
      duration = 3
    )
  })
  
  observeEvent(input$exportar_datos_ds, {
    
    showNotification(
      "ðŸ“¤ ExportaciÃ³n iniciada. Preparando archivo Excel...",
      type = "default",
      duration = 3
    )
    
    # En producciÃ³n aquÃ­ se ejecutarÃ­a la exportaciÃ³n real
    valores_reactivos$logs_sistema <- c(
      valores_reactivos$logs_sistema,
      paste("[", format(Sys.time(), "%H:%M:%S"), "] ðŸ“¤ Datos exportados a Excel")
    )
  })
  
  observeEvent(input$reiniciar_app_ds, {
    
    showModal(modalDialog(
      title = "ðŸ”„ Reiniciar AplicaciÃ³n",
      "âš ï¸ Esta acciÃ³n reiniciarÃ¡ completamente la aplicaciÃ³n. Â¿Continuar?",
      footer = tagList(
        actionButton("confirmar_reiniciar", "Reiniciar", class = "btn-danger"),
        modalButton("Cancelar")
      )
    ))
  })
  
  observeEvent(input$confirmar_reiniciar, {
    
    removeModal()
    
    showNotification(
      "ðŸ”„ Reiniciando aplicaciÃ³n...",
      type = "error",
      duration = 3
    )
    
    # En producciÃ³n esto podrÃ­a reiniciar la sesiÃ³n de Shiny
    session$reload()
  })
  
  # Observador para prueba de carga de datos
  observeEvent(input$test_carga_datos, {
    
    showModal(modalDialog(
      title = "ðŸ§ª Probando Carga de Datos",
      div(
        id = "test-progress",
        "Verificando integridad de datos...",
        br(), br(),
        progressBar(id = "test_progress_bar", value = 0, striped = TRUE, animated = TRUE)
      ),
      easyClose = FALSE,
      footer = NULL
    ))
    
    # Simular prueba de carga
    for(i in 1:5) {
      Sys.sleep(0.5)
      updateProgressBar(session, "test_progress_bar", value = i * 20)
    }
    
    Sys.sleep(1)
    removeModal()
    
    resultado_test <- ifelse(
      !is.null(metricas_global) && nrow(metricas_global) > 0,
      "âœ… Datos cargados correctamente",
      "âŒ Error en la carga de datos"
    )
    
    showNotification(
      resultado_test,
      type = ifelse(str_detect(resultado_test, "âœ…"), "success", "error"),
      duration = 4
    )
  })
  
  # Observador para auto-refresh
  observe({
    
    # Auto-refresh cada 30 minutos si estÃ¡ activado
    if(!is.null(input$auto_refresh_ds) && input$auto_refresh_ds) {
      
      invalidateLater(1800000)  # 30 minutos en milisegundos
      
      # Agregar al log
      valores_reactivos$logs_sistema <- c(
        valores_reactivos$logs_sistema,
        paste("[", format(Sys.time(), "%H:%M:%S"), "] ðŸ”„ Auto-actualizaciÃ³n ejecutada")
      )
      
      valores_reactivos$datos_actualizados <- Sys.time()
    }
  })
  
  # Observador para modo debug
  observe({
    
    if(!is.null(input$modo_debug_ds) && input$modo_debug_ds) {
      
      # En modo debug, mostrar informaciÃ³n adicional
      valores_reactivos$logs_sistema <- c(
        valores_reactivos$logs_sistema,
        paste("[", format(Sys.time(), "%H:%M:%S"), "] ðŸ› Modo debug activado")
      )
      
      showNotification(
        "ðŸ› Modo debug activado - InformaciÃ³n detallada disponible",
        type = "default",
        duration = 5
      )
    }
  })
  
  # =============================================================================
  # FUNCIONES AUXILIARES Y OUTPUTS ADICIONALES  
  # =============================================================================
  
  # GrÃ¡fico de comparaciÃ³n con competencia (anÃ¡lisis detallado)
  output$grafico_comparacion_competencia <- renderPlotly({
    
    req(input$marca_detalle)
    
    if(is.null(metricas_global)) {
      return(plot_ly() %>% layout(title = "Sin datos disponibles"))
    }
    
    marca_seleccionada <- metricas_global %>%
      filter(Marca_Vehiculo == input$marca_detalle)
    
    if(nrow(marca_seleccionada) == 0) {
      return(plot_ly() %>% layout(title = "Marca no encontrada"))
    }
    
    # Encontrar marcas similares (por volumen)
    volumen_ref <- marca_seleccionada$Volumen_Final
    
    competencia <- metricas_global %>%
      filter(Marca_Vehiculo != input$marca_detalle) %>%
      mutate(diferencia_volumen = abs(Volumen_Final - volumen_ref)) %>%
      arrange(diferencia_volumen) %>%
      head(5) %>%
      bind_rows(marca_seleccionada) %>%
      mutate(
        es_seleccionada = Marca_Vehiculo == input$marca_detalle,
        color_barra = ifelse(es_seleccionada, "#06b6d4", "#95a5a6")
      )
    
    plot_ly(
      competencia,
      x = ~reorder(Marca_Vehiculo, Score_Oportunidad),
      y = ~Score_Oportunidad,
      type = "bar",
      marker = list(color = ~color_barra, opacity = 0.8),
      text = ~paste0(round(Score_Oportunidad, 1), " pts"),
      textposition = "outside",
      hovertemplate = paste0(
        "<b>%{x}</b><br>",
        "Score: %{y}<br>",
        "Volumen: ", format(competencia$Volumen_Final, big.mark = ","), "<br>",
        "<extra></extra>"
      )
    ) %>%
      layout(
        title = paste("ComparaciÃ³n con Competencia Directa -", input$marca_detalle),
        xaxis = list(title = ""),
        yaxis = list(title = "Score de Oportunidad"),
        showlegend = FALSE
      )
  })
  
  # Tabla de reporte detallado
  output$tabla_reporte_detallado <- renderDataTable({
    
    req(input$marca_detalle)
    
    if(is.null(metricas_global)) {
      return(data.frame(Mensaje = "Sin datos disponibles"))
    }
    
    # Buscar la marca y su competencia
    marca_data <- metricas_global %>%
      filter(Marca_Vehiculo == input$marca_detalle)
    
    if(nrow(marca_data) == 0) {
      return(data.frame(Mensaje = "Marca no encontrada"))
    }
    
    volumen_ref <- marca_data$Volumen_Final
    
    reporte_detallado <- metricas_global %>%
      filter(abs(Volumen_Final - volumen_ref) <= volumen_ref * 0.5) %>%  # Â±50% del volumen
      arrange(desc(Score_Oportunidad)) %>%
      select(
        Marca = Marca_Vehiculo,
        Volumen = Volumen_Final,
        `Crecimiento %` = Crecimiento_Relativo,
        Score = Score_Oportunidad,
        `ParticipaciÃ³n %` = Participacion_Mercado,
        CategorÃ­a = Categoria_Volumen,
        Potencial = Potencial_Analytics
      ) %>%
      mutate(
        Volumen = format(Volumen, big.mark = ","),
        `Crecimiento %` = round(`Crecimiento %`, 1),
        Score = round(Score, 1),
        `ParticipaciÃ³n %` = round(`ParticipaciÃ³n %`, 2),
        `Es Seleccionada` = Marca == input$marca_detalle
      )
    
    datatable(
      reporte_detallado %>% select(-`Es Seleccionada`),
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      rownames = FALSE,
      class = "compact stripe hover",
      extensions = 'Buttons'
    ) %>%
      formatStyle(
        "Marca",
        backgroundColor = styleEqual(input$marca_detalle, "#fff3cd"),
        fontWeight = styleEqual(input$marca_detalle, "bold")
      ) %>%
      formatStyle(
        "Score",
        backgroundColor = styleInterval(c(40, 60, 80), c("#f8f9fa", "#fff3cd", "#d1ecf1", "#d4edda"))
      )
    
  }, server = FALSE)
  
  # =============================================================================
  # OUTPUTS FINALES DE TEXTO
  # =============================================================================
  
  # Actualizar timestamp en tiempo real
  observe({
    invalidateLater(60000)  # Actualizar cada minuto
    output$ultima_actualizacion_ds <- renderText({
      paste("Actualizado:", format(Sys.time(), "%H:%M"))
    })
  })
  
  # Output para el log en tiempo real
  observe({
    output$log_sistema_ds <- renderText({
      if(length(valores_reactivos$logs_sistema) > 0) {
        paste(tail(valores_reactivos$logs_sistema, 15), collapse = "\n")
      } else {
        paste(
          paste("[", format(Sys.time(), "%H:%M:%S"), "] âœ“ Sistema iniciado correctamente"),
          paste("[", format(Sys.time(), "%H:%M:%S"), "] âœ“ Datos cargados exitosamente"),
          "â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•â•",
          "SISTEMA FUNCIONANDO CORRECTAMENTE",
          sep = "\n"
        )
      }
    })
  })
  
  
  # =============================================================================
  # OUTPUTS FALTANTES PARA PANORAMA GENERAL
  # =============================================================================
  
  # GrÃ¡fico de distribuciÃ³n por categorÃ­as
  output$grafico_distribucion_categorias <- renderPlotly({
    
    if(is.null(metricas_global) || nrow(metricas_global) == 0) {
      return(plot_ly() %>% layout(title = "Sin datos disponibles"))
    }
    
    datos_categoria <- metricas_global %>%
      count(Categoria_Volumen, name = "cantidad") %>%
      mutate(
        porcentaje = round(cantidad / sum(cantidad) * 100, 1),
        Categoria_Volumen = fct_reorder(Categoria_Volumen, cantidad)
      )
    
    plot_ly(
      datos_categoria,
      x = ~cantidad,
      y = ~Categoria_Volumen,
      type = "bar",
      orientation = "h",
      marker = list(color = "#1a365d", opacity = 0.8),
      text = ~paste0(cantidad, " marcas (", porcentaje, "%)"),
      textposition = "outside",
      hovertemplate = paste0(
        "<b>%{y}</b><br>",
        "Marcas: %{x}<br>",
        "Porcentaje: ", datos_categoria$porcentaje, "%<br>",
        "<extra></extra>"
      )
    ) %>%
      layout(
        title = "DistribuciÃ³n del Parque Vehicular por CategorÃ­a",
        xaxis = list(title = "NÃºmero de Marcas"),
        yaxis = list(title = ""),
        margin = list(l = 150)
      )
  })
  
  # Matriz de potencial
  
  output$grafico_matriz_potencial <- renderPlotly({
    
    if(is.null(metricas_global) || nrow(metricas_global) == 0) {
      return(plot_ly() %>% layout(title = "Sin datos disponibles"))
    }
    
    datos_matriz <- metricas_global %>%
      count(Potencial_Analytics, name = "cantidad") %>%
      mutate(
        porcentaje = round(cantidad / sum(cantidad) * 100, 1),
        color_categoria = case_when(
          str_detect(Potencial_Analytics, "ALTA") ~ "#ef4444",
          str_detect(Potencial_Analytics, "MEDIA") ~ "#f59e0b", 
          str_detect(Potencial_Analytics, "EMERGENTE") ~ "#3b82f6",
          TRUE ~ "#64748b"
        ),
        # Ordenar por prioridad
        orden = case_when(
          str_detect(Potencial_Analytics, "ALTA") ~ 1,
          str_detect(Potencial_Analytics, "MEDIA") ~ 2,
          str_detect(Potencial_Analytics, "EMERGENTE") ~ 3,
          TRUE ~ 4
        )
      ) %>%
      arrange(orden) %>%
      mutate(Potencial_Analytics = factor(Potencial_Analytics, levels = Potencial_Analytics))
    
    plot_ly(
      datos_matriz,
      x = ~cantidad,
      y = ~Potencial_Analytics,
      type = "bar",
      orientation = "h",
      marker = list(color = ~color_categoria),
      text = ~paste0(cantidad, " marcas (", porcentaje, "%)"),
      textposition = "outside",
      hovertemplate = paste0(
        "<b>%{y}</b><br>",
        "Marcas: %{x}<br>",
        "Porcentaje: ", datos_matriz$porcentaje, "%<br>",
        "<extra></extra>"
      )
    ) %>%
      layout(
        title = "Matriz de Potencial de Analytics",
        xaxis = list(title = "NÃºmero de Marcas"),
        yaxis = list(title = ""),
        margin = list(l = 180)
      )
  })
  
  # Tabla panorama completo
  output$tabla_panorama_completo <- renderDataTable({
    
    if(is.null(metricas_global) || nrow(metricas_global) == 0) {
      return(data.frame(Mensaje = "Sin datos disponibles"))
    }
    
    tabla_panorama <- datos_filtrados() %>%
      select(
        Marca = Marca_Vehiculo,
        Volumen = Volumen_Final,
        `Crecimiento %` = Crecimiento_Relativo,
        Score = Score_Oportunidad,
        `ParticipaciÃ³n %` = Participacion_Mercado,
        CategorÃ­a = Categoria_Volumen,
        Potencial = Potencial_Analytics
      ) %>%
      mutate(
        Volumen = format(Volumen, big.mark = ","),
        `Crecimiento %` = round(`Crecimiento %`, 1),
        Score = round(Score, 1),
        `ParticipaciÃ³n %` = round(`ParticipaciÃ³n %`, 2)
      ) %>%
      arrange(desc(Score))
    
    datatable(
      tabla_panorama,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel'),
        columnDefs = list(
          list(targets = 1, className = "dt-right"),
          list(targets = c(2,3,4), className = "dt-center")
        )
      ),
      rownames = FALSE,
      class = "compact stripe hover",
      extensions = 'Buttons'
    ) %>%
      formatStyle(
        "Score",
        backgroundColor = styleInterval(c(40, 60, 80), c("#f8f9fa", "#fff3cd", "#d1ecf1", "#d4edda"))
      )
    
  }, server = FALSE)
  
  # =============================================================================
  # OUTPUTS FALTANTES PARA RANKINGS DE MARCAS
  # =============================================================================
  
  # Ranking por volumen
  output$grafico_ranking_volumen <- renderPlotly({
    crear_top_marcas(metricas_global, tipo = "volumen", n = 15)
  })
  
  # Ranking por crecimiento
  output$grafico_ranking_crecimiento <- renderPlotly({
    crear_top_marcas(metricas_global, tipo = "crecimiento", n = 15)
  })
  
  # Ranking por score
  output$grafico_ranking_score <- renderPlotly({
    
    if(is.null(metricas_global) || nrow(metricas_global) == 0) {
      return(plot_ly() %>% layout(title = "Sin datos disponibles"))
    }
    
    datos_score <- metricas_global %>%
      arrange(desc(Score_Oportunidad)) %>%
      head(15) %>%
      mutate(Marca_Vehiculo = fct_reorder(Marca_Vehiculo, Score_Oportunidad))
    
    plot_ly(
      datos_score,
      x = ~Score_Oportunidad,
      y = ~Marca_Vehiculo,
      type = "bar",
      orientation = "h",
      marker = list(color = "#2c3e50", opacity = 0.8),
      text = ~paste0(round(Score_Oportunidad, 1), " pts"),
      textposition = "outside",
      hovertemplate = paste0(
        "<b>%{y}</b><br>",
        "Score: %{x} puntos<br>",
        "<extra></extra>"
      )
    ) %>%
      layout(
        title = "Top 15 Marcas por Score de Oportunidad",
        xaxis = list(title = "Score de Oportunidad"),
        yaxis = list(title = ""),
        margin = list(l = 120)
      )
  })
  
  # Tabla ranking comparativo
  output$tabla_ranking_comparativo <- renderDataTable({
    
    if(is.null(metricas_global) || nrow(metricas_global) == 0) {
      return(data.frame(Mensaje = "Sin datos disponibles"))
    }
    
    tabla_ranking <- metricas_global %>%
      mutate(
        Ranking_Volumen = rank(desc(Volumen_Final)),
        Ranking_Crecimiento = rank(desc(Crecimiento_Relativo)),
        Ranking_Score = rank(desc(Score_Oportunidad))
      ) %>%
      select(
        Marca = Marca_Vehiculo,
        `R. Volumen` = Ranking_Volumen,
        `R. Crecimiento` = Ranking_Crecimiento,
        `R. Score` = Ranking_Score,
        Volumen = Volumen_Final,
        `Crecimiento %` = Crecimiento_Relativo,
        Score = Score_Oportunidad
      ) %>%
      mutate(
        Volumen = format(Volumen, big.mark = ","),
        `Crecimiento %` = round(`Crecimiento %`, 1),
        Score = round(Score, 1)
      ) %>%
      arrange(`R. Score`)
    
    datatable(
      tabla_ranking,
      options = list(
        pageLength = 20,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      rownames = FALSE,
      class = "compact stripe hover",
      extensions = 'Buttons'
    )
    
  }, server = FALSE)
  
  
  
  # =============================================================================
  # TAB: MAPA DE OPORTUNIDADES ESTRATÃ‰GICAS
  # =============================================================================
  
  # -----------------------------------------------------------------------------
  # DATOS REACTIVOS CON FILTROS AVANZADOS
  # -----------------------------------------------------------------------------
  
  datos_filtrados_oportunidades <- reactive({
    
    cat("ðŸ” Aplicando filtros avanzados de oportunidades...\n")
    
    datos <- metricas_global
    
    if(is.null(datos) || nrow(datos) == 0) {
      return(data.frame())
    }
    
    # Aplicar filtros solo si el botÃ³n fue presionado
    if(input$btn_aplicar_filtros_oport > 0) {
      
      # Filtro por volumen
      datos <- datos %>%
        filter(Volumen_Final >= input$filtro_volumen_min_oport,
               Volumen_Final <= input$filtro_volumen_max_oport)
      
      # Filtro por crecimiento
      datos <- datos %>%
        filter(Crecimiento_Relativo >= input$filtro_crecimiento_min_oport,
               Crecimiento_Relativo <= input$filtro_crecimiento_max_oport)
      
      # Filtro por score
      datos <- datos %>%
        filter(Score_Oportunidad >= input$filtro_score_min_oport)
      
      # Filtro por categorÃ­as
      if(input$filtro_categorias_oport != "todas") {
        if(input$filtro_categorias_oport == "alto") {
          datos <- datos %>% filter(Categoria_Volumen == "Alto Volumen (100K+)")
        } else if(input$filtro_categorias_oport == "medio") {
          datos <- datos %>% filter(Categoria_Volumen == "Volumen Medio (10K-100K)")
        } else if(input$filtro_categorias_oport == "bajo_emergente") {
          datos <- datos %>% filter(Categoria_Volumen %in% c("Volumen Bajo (1K-10K)", "Volumen MÃ­nimo (<1K)"))
        }
      }
      
      # Filtro por potencial
      if(!is.null(input$filtro_potencial_oport) && length(input$filtro_potencial_oport) > 0) {
        potencial_seleccionado <- c()
        if("alta" %in% input$filtro_potencial_oport) potencial_seleccionado <- c(potencial_seleccionado, "ðŸ”´ ALTA PRIORIDAD")
        if("media" %in% input$filtro_potencial_oport) potencial_seleccionado <- c(potencial_seleccionado, "ðŸŸ  MEDIA PRIORIDAD")
        if("emergente" %in% input$filtro_potencial_oport) potencial_seleccionado <- c(potencial_seleccionado, "ðŸ”µ EMERGENTE")
        if("baja" %in% input$filtro_potencial_oport) potencial_seleccionado <- c(potencial_seleccionado, "âš« BAJA PRIORIDAD")
        
        datos <- datos %>% filter(Potencial_Analytics %in% potencial_seleccionado)
      }
    }
    
    cat("âœ… Filtros aplicados:", nrow(datos), "marcas resultantes\n")
    return(datos)
  })
  
  # Resetear filtros
  observeEvent(input$btn_resetear_filtros_oport, {
    updateSliderInput(session, "filtro_volumen_min_oport", value = 1000)
    updateSliderInput(session, "filtro_volumen_max_oport", value = 500000)
    updateSliderInput(session, "filtro_crecimiento_min_oport", value = 0)
    updateSliderInput(session, "filtro_crecimiento_max_oport", value = 200)
    updateSliderInput(session, "filtro_score_min_oport", value = 40)
    updateSelectInput(session, "filtro_categorias_oport", selected = "todas")
    updateCheckboxGroupInput(session, "filtro_potencial_oport", selected = c("alta", "media", "emergente"))
    
    showNotification("âœ… Filtros reseteados", type = "success", duration = 2)
  })
  
  # -----------------------------------------------------------------------------
  # VALUE BOXES DINÃMICOS
  # -----------------------------------------------------------------------------
  
  output$vb_marcas_filtradas_oport <- renderValueBox({
    datos <- datos_filtrados_oportunidades()
    
    valueBox(
      value = nrow(datos),
      subtitle = "Marcas en AnÃ¡lisis",
      icon = icon("filter"),
      color = "blue"
    )
  })
  
  output$vb_volumen_oportunidad_oport <- renderValueBox({
    datos <- datos_filtrados_oportunidades()
    
    volumen_total <- sum(datos$Volumen_Final, na.rm = TRUE)
    
    valueBox(
      value = formato_numero(volumen_total),
      subtitle = "VehÃ­culos Totales",
      icon = icon("car"),
      color = "red"
    )
  })
  
  output$vb_score_promedio_oport <- renderValueBox({
    datos <- datos_filtrados_oportunidades()
    
    score_prom <- mean(datos$Score_Oportunidad, na.rm = TRUE)
    
    valueBox(
      value = round(score_prom, 1),
      subtitle = "Score Promedio",
      icon = icon("star"),
      color = "yellow"
    )
  })
  
  output$vb_potencial_mercado_oport <- renderValueBox({
    datos <- datos_filtrados_oportunidades()
    
    participacion <- sum(datos$Participacion_Mercado, na.rm = TRUE)
    
    valueBox(
      value = paste0(round(participacion, 1), "%"),
      subtitle = "ParticipaciÃ³n de Mercado",
      icon = icon("chart-pie"),
      color = "green"
    )
  })
  
  # -----------------------------------------------------------------------------
  # MAPA ESTRATÃ‰GICO DE BURBUJAS AVANZADO
  # -----------------------------------------------------------------------------
  
  output$mapa_burbujas_avanzado_oport <- renderPlotly({
    
    datos <- datos_filtrados_oportunidades()
    
    if(is.null(datos) || nrow(datos) == 0) {
      return(plot_ly() %>% layout(title = "No hay datos con los filtros aplicados"))
    }
    
    # Calcular tamaÃ±o de burbujas proporcional al score
    datos <- datos %>%
      mutate(
        tamano_burbuja = pmax(8, pmin(40, Score_Oportunidad / 2.5)),
        log_volumen = log10(pmax(1, Volumen_Final))
      )
    
    plot_ly(
      datos,
      x = ~log_volumen,
      y = ~Crecimiento_Relativo,
      size = ~tamano_burbuja,
      color = ~Potencial_Analytics,
      colors = c("ðŸ”´ ALTA PRIORIDAD" = "#ef4444",
                 "ðŸŸ  MEDIA PRIORIDAD" = "#f59e0b",
                 "ðŸ”µ EMERGENTE" = "#3b82f6",
                 "âš« BAJA PRIORIDAD" = "#64748b"),
      text = ~paste0("<b>", Marca_Vehiculo, "</b>",
                     "<br>Volumen: ", format(Volumen_Final, big.mark = ","),
                     "<br>Crecimiento: ", round(Crecimiento_Relativo, 1), "%",
                     "<br>Score: ", round(Score_Oportunidad, 1),
                     "<br>ParticipaciÃ³n: ", round(Participacion_Mercado, 2), "%"),
      hovertemplate = "%{text}<extra></extra>",
      type = "scatter",
      mode = "markers",
      marker = list(
        opacity = 0.85,
        line = list(width = 2, color = "#000000"),
        sizemode = "diameter"
      ),
      sizes = c(8, 40)
    ) %>%
      add_annotations(
        data = datos %>% 
          filter(str_detect(Potencial_Analytics, "ALTA|MEDIA"),
                 Score_Oportunidad >= 60),
        x = ~log_volumen,
        y = ~Crecimiento_Relativo + 3,
        text = ~Marca_Vehiculo,
        showarrow = FALSE,
        font = list(size = 9, color = "#000000", family = "Arial", weight = "bold")
      ) %>%
      layout(
        title = list(
          text = "<b>MAPA ESTRATÃ‰GICO DE OPORTUNIDADES</b><br><sub>AnÃ¡lisis Multidimensional: Volumen â€¢ Crecimiento â€¢ Score</sub>",
          font = list(size = 14)
        ),
        xaxis = list(
          title = "Volumen de VehÃ­culos (escala Log10)",
          showgrid = TRUE,
          gridcolor = "#E0E0E0"
        ),
        yaxis = list(
          title = "Crecimiento Relativo (%)",
          showgrid = TRUE,
          gridcolor = "#E0E0E0",
          zeroline = TRUE,
          zerolinewidth = 2,
          zerolinecolor = "#999999"
        ),
        legend = list(
          title = list(text = "<b>Nivel de Prioridad</b>"),
          orientation = "v",
          x = 1.02,
          y = 0.5
        ),
        hovermode = "closest"
      )
  })
  
  # -----------------------------------------------------------------------------
  # MATRIZ DE DECISIÃ“N 2X2
  # -----------------------------------------------------------------------------
  
  # output$matriz_decision_2x2_oport <- renderPlotly({
  #   
  #   datos <- datos_filtrados_oportunidades()
  #   
  #   if(is.null(datos) || nrow(datos) == 0) {
  #     return(plot_ly() %>% layout(title = "No hay datos disponibles"))
  #   }
  #   
  #   # Calcular medianas para los ejes
  #   mediana_volumen <- median(datos$Volumen_Final, na.rm = TRUE)
  #   mediana_crecimiento <- median(datos$Crecimiento_Relativo, na.rm = TRUE)
  #   
  #   # Clasificar en cuadrantes
  #   datos <- datos %>%
  #     mutate(
  #       cuadrante = case_when(
  #         Volumen_Final >= mediana_volumen & Crecimiento_Relativo >= mediana_crecimiento ~ "â­ Estrellas",
  #         Volumen_Final < mediana_volumen & Crecimiento_Relativo >= mediana_crecimiento ~ "ðŸš€ Promesas",
  #         Volumen_Final >= mediana_volumen & Crecimiento_Relativo < mediana_crecimiento ~ "ðŸ’° Base Consolidada",
  #         TRUE ~ "â“ Interrogantes"
  #       ),
  #       color_cuadrante = case_when(
  #         cuadrante == "â­ Estrellas" ~ "#10b981",
  #         cuadrante == "ðŸš€ Promesas" ~ "#3498db",
  #         cuadrante == "ðŸ’° Base Consolidada" ~ "#f59e0b",
  #         TRUE ~ "#95a5a6"
  #       )
  #     )
  #   
  #   plot_ly(
  #     datos,
  #     x = ~Volumen_Final,
  #     y = ~Crecimiento_Relativo,
  #     color = ~cuadrante,
  #     colors = ~unique(color_cuadrante),
  #     type = "scatter",
  #     mode = "markers",
  #     marker = list(size = 10, opacity = 0.7),
  #     text = ~paste0("<b>", Marca_Vehiculo, "</b><br>Cuadrante: ", cuadrante),
  #     hovertemplate = "%{text}<extra></extra>"
  #   ) %>%
  #     add_segments(
  #       x = mediana_volumen, xend = mediana_volumen,
  #       y = min(datos$Crecimiento_Relativo, na.rm = TRUE),
  #       yend = max(datos$Crecimiento_Relativo, na.rm = TRUE),
  #       line = list(dash = "dash", color = "#2c3e50", width = 2),
  #       showlegend = FALSE,
  #       hoverinfo = "skip"
  #     ) %>%
  #     add_segments(
  #       x = min(datos$Volumen_Final, na.rm = TRUE),
  #       xend = max(datos$Volumen_Final, na.rm = TRUE),
  #       y = mediana_crecimiento, yend = mediana_crecimiento,
  #       line = list(dash = "dash", color = "#2c3e50", width = 2),
  #       showlegend = FALSE,
  #       hoverinfo = "skip"
  #     ) %>%
  #     layout(
  #       title = "Matriz 2x2<br><sub>Volumen vs Crecimiento</sub>",
  #       xaxis = list(title = "Volumen", type = "log"),
  #       yaxis = list(title = "Crecimiento (%)"),
  #       legend = list(title = list(text = "<b>Cuadrantes</b>"))
  #     )
  # })
  
  
  # MATRIZ DE DECISIÃ“N 2X2
  # -----------------------------------------------------------------------------
  
  output$matriz_decision_2x2_oport <- renderPlotly({
    
    datos <- datos_filtrados_oportunidades()
    
    #datos <- metricas_global  ## Previo
    
    if(is.null(datos) || nrow(datos) == 0) {
      return(plot_ly() %>% layout(title = "No hay datos disponibles"))
    }
    
    # Calcular medianas para los ejes
    mediana_volumen <- median(datos$Volumen_Final, na.rm = TRUE)
    mediana_crecimiento <- median(datos$Crecimiento_Relativo, na.rm = TRUE)
    
    # Clasificar en cuadrantes
    datos <- datos %>%
      mutate(
        cuadrante = case_when(
          Volumen_Final >= mediana_volumen & Crecimiento_Relativo >= mediana_crecimiento ~ "Estrellas",
          Volumen_Final < mediana_volumen & Crecimiento_Relativo >= mediana_crecimiento ~ "Promesas",
          Volumen_Final >= mediana_volumen & Crecimiento_Relativo < mediana_crecimiento ~ "Base Consolidada",
          TRUE ~ "Interrogantes"
        )
      )
    
    colores_cuadrantes <- c(
      "Estrellas" = "#10b981",
      "Promesas" = "#3498db",
      "Base Consolidada" = "#f59e0b",
      "Interrogantes" = "#95a5a6"
    )
    
    #datos <- datos %>% filter(Marca_Vehiculo != 'THOMAS')
    
    plot_ly(
      datos,
      x = ~Volumen_Final,
      y = ~Crecimiento_Relativo,
      color = ~cuadrante,
      colors = colores_cuadrantes,  # CORREGIDO: Usar el vector nombrado estÃ¡tico
      type = "scatter",
      mode = "markers",
      marker = list(size = 10, opacity = 0.7),
      text = ~paste0("<b>", Marca_Vehiculo, "</b><br>Cuadrante: ", cuadrante),
      hovertemplate = "%{text}<extra></extra>"
    ) %>%
      add_segments(
        x = mediana_volumen, xend = mediana_volumen,
        y = min(datos$Crecimiento_Relativo, na.rm = TRUE),
        yend = max(datos$Crecimiento_Relativo, na.rm = TRUE),
        line = list(dash = "dash", color = "#2c3e50", width = 2),
        showlegend = FALSE,
        hoverinfo = "skip"
      ) %>%
      add_segments(
        x = min(datos$Volumen_Final, na.rm = TRUE),
        xend = max(datos$Volumen_Final, na.rm = TRUE),
        y = mediana_crecimiento, yend = mediana_crecimiento,
        line = list(dash = "dash", color = "#2c3e50", width = 2),
        showlegend = FALSE,
        hoverinfo = "skip"
      ) %>%
      layout(
        title = "Matriz 2x2<br><sub>Volumen vs Crecimiento</sub>",
        xaxis = list(title = "Volumen", type = "log"),
        yaxis = list(title = "Crecimiento (%)"),
        legend = list(title = list(text = "<b>Cuadrantes</b>"))
      )
  })
  
  
  # -----------------------------------------------------------------------------
  # TABLAS POR CUADRANTES
  # -----------------------------------------------------------------------------
  
  # FunciÃ³n auxiliar para clasificar cuadrantes
  clasificar_cuadrantes <- function(datos) {
    if(nrow(datos) == 0) return(datos)
    
    mediana_volumen <- median(datos$Volumen_Final, na.rm = TRUE)
    mediana_crecimiento <- median(datos$Crecimiento_Relativo, na.rm = TRUE)
    
    datos %>%
      mutate(
        cuadrante = case_when(
          Volumen_Final >= mediana_volumen & Crecimiento_Relativo >= mediana_crecimiento ~ "estrellas",
          Volumen_Final < mediana_volumen & Crecimiento_Relativo >= mediana_crecimiento ~ "promesas",
          Volumen_Final >= mediana_volumen & Crecimiento_Relativo < mediana_crecimiento ~ "consolidada",
          TRUE ~ "interrogantes"
        )
      )
  }
  
  output$tabla_cuadrante_estrellas_oport <- renderDataTable({
    datos <- datos_filtrados_oportunidades() %>%
      clasificar_cuadrantes() %>%
      filter(cuadrante == "estrellas") %>%
      arrange(desc(Score_Oportunidad)) %>%
      select(Marca = Marca_Vehiculo, Volumen = Volumen_Final, 
             `Crecimiento %` = Crecimiento_Relativo, Score = Score_Oportunidad) %>%
      mutate(Volumen = format(Volumen, big.mark = ","),
             `Crecimiento %` = round(`Crecimiento %`, 1),
             Score = round(Score, 1))
    
    datatable(datos, options = list(pageLength = 10, dom = 't'), rownames = FALSE)
  })
  
  output$tabla_cuadrante_promesas_oport <- renderDataTable({
    datos <- datos_filtrados_oportunidades() %>%
      clasificar_cuadrantes() %>%
      filter(cuadrante == "promesas") %>%
      arrange(desc(Crecimiento_Relativo)) %>%
      select(Marca = Marca_Vehiculo, Volumen = Volumen_Final, 
             `Crecimiento %` = Crecimiento_Relativo, Score = Score_Oportunidad) %>%
      mutate(Volumen = format(Volumen, big.mark = ","),
             `Crecimiento %` = round(`Crecimiento %`, 1),
             Score = round(Score, 1))
    
    datatable(datos, options = list(pageLength = 10, dom = 't'), rownames = FALSE)
  })
  
  output$tabla_cuadrante_base_consolidada_oport <- renderDataTable({
    datos <- datos_filtrados_oportunidades() %>%
      clasificar_cuadrantes() %>%
      filter(cuadrante == "base_consolidada") %>%
      arrange(desc(Volumen_Final)) %>%
      select(Marca = Marca_Vehiculo, Volumen = Volumen_Final, 
             `Crecimiento %` = Crecimiento_Relativo, Score = Score_Oportunidad) %>%
      mutate(Volumen = format(Volumen, big.mark = ","),
             `Crecimiento %` = round(`Crecimiento %`, 1),
             Score = round(Score, 1))
    
    datatable(datos, options = list(pageLength = 10, dom = 't'), rownames = FALSE)
  })
  
  output$tabla_cuadrante_interrogantes_oport <- renderDataTable({
    datos <- datos_filtrados_oportunidades() %>%
      clasificar_cuadrantes() %>%
      filter(cuadrante == "interrogantes") %>%
      arrange(desc(Score_Oportunidad)) %>%
      select(Marca = Marca_Vehiculo, Volumen = Volumen_Final, 
             `Crecimiento %` = Crecimiento_Relativo, Score = Score_Oportunidad) %>%
      mutate(Volumen = format(Volumen, big.mark = ","),
             `Crecimiento %` = round(`Crecimiento %`, 1),
             Score = round(Score, 1))
    
    datatable(datos, options = list(pageLength = 10, dom = 't'), rownames = FALSE)
  })
  
  # -----------------------------------------------------------------------------
  # DISTRIBUCIONES Y ANÃLISIS ESTADÃSTICO
  # -----------------------------------------------------------------------------
  
  output$hist_distribucion_score_oport <- renderPlotly({
    datos <- datos_filtrados_oportunidades()
    
    if(nrow(datos) == 0) return(plot_ly() %>% layout(title = "Sin datos"))
    
    plot_ly(datos, x = ~Score_Oportunidad, type = "histogram",
            marker = list(color = "#1a365d", opacity = 0.7),
            nbinsx = 20) %>%
      layout(
        title = "DistribuciÃ³n de Score",
        xaxis = list(title = "Score de Oportunidad"),
        yaxis = list(title = "Frecuencia")
      )
  })
  
  output$curva_lorenz_oport <- renderPlotly({
    datos <- datos_filtrados_oportunidades()
    
    if(nrow(datos) == 0) return(plot_ly() %>% layout(title = "Sin datos"))
    
    # Calcular curva de Lorenz
    datos_lorenz <- datos %>%
      arrange(Volumen_Final) %>%
      mutate(
        pct_marcas = (1:n()) / n() * 100,
        volumen_acum = cumsum(Volumen_Final) / sum(Volumen_Final) * 100
      )
    
    plot_ly() %>%
      add_trace(
        data = datos_lorenz,
        x = ~pct_marcas,
        y = ~volumen_acum,
        type = "scatter",
        mode = "lines",
        name = "Curva de Lorenz",
        line = list(color = "#06b6d4", width = 3)
      ) %>%
      add_trace(
        x = c(0, 100),
        y = c(0, 100),
        type = "scatter",
        mode = "lines",
        name = "LÃ­nea de Igualdad",
        line = list(color = "#95a5a6", dash = "dash", width = 2)
      ) %>%
      layout(
        title = "Curva de Lorenz<br><sub>ConcentraciÃ³n del Mercado</sub>",
        xaxis = list(title = "% Acumulado de Marcas"),
        yaxis = list(title = "% Acumulado de Volumen")
      )
  })
  
  output$boxplot_comparativo_oport <- renderPlotly({
    datos <- datos_filtrados_oportunidades()
    
    if(nrow(datos) == 0) return(plot_ly() %>% layout(title = "Sin datos"))
    
    plot_ly(datos, y = ~Score_Oportunidad, color = ~Potencial_Analytics,
            type = "box",
            colors = c("ðŸ”´ ALTA PRIORIDAD" = "#ef4444",
                       "ðŸŸ  MEDIA PRIORIDAD" = "#f59e0b",
                       "ðŸ”µ EMERGENTE" = "#3b82f6",
                       "âš« BAJA PRIORIDAD" = "#64748b")) %>%
      layout(
        title = "Box Plot por Prioridad",
        yaxis = list(title = "Score de Oportunidad"),
        showlegend = FALSE
      )
  })
  
  # -----------------------------------------------------------------------------
  # EVOLUCIÃ“N TEMPORAL DE TOP OPORTUNIDADES
  # -----------------------------------------------------------------------------
  
  output$grafico_evolucion_top_oport <- renderPlotly({
    
    req(input$btn_actualizar_temporal_oport)
    
    datos_filtrados <- datos_filtrados_oportunidades()
    
    if(nrow(datos_filtrados) == 0 || is.null(datos_long_global)) {
      return(plot_ly() %>% layout(title = "No hay datos temporales disponibles"))
    }
    
    # Seleccionar top N segÃºn criterio
    top_n <- as.numeric(input$top_n_temporal_oport)
    
    if(input$criterio_temporal_oport == "score") {
      top_marcas <- datos_filtrados %>%
        arrange(desc(Score_Oportunidad)) %>%
        head(top_n) %>%
        pull(Marca_Vehiculo)
    } else if(input$criterio_temporal_oport == "volumen") {
      top_marcas <- datos_filtrados %>%
        arrange(desc(Volumen_Final)) %>%
        head(top_n) %>%
        pull(Marca_Vehiculo)
    } else {
      top_marcas <- datos_filtrados %>%
        arrange(desc(Crecimiento_Relativo)) %>%
        head(top_n) %>%
        pull(Marca_Vehiculo)
    }
    
    # Filtrar datos temporales
    datos_temp <- datos_long_global %>%
      filter(Marca_Vehiculo %in% top_marcas)
    
    if(nrow(datos_temp) == 0) {
      return(plot_ly() %>% layout(title = "No hay datos temporales para las marcas seleccionadas"))
    }
    
    # Normalizar si se solicita
    if(input$normalizar_temporal_oport) {
      datos_temp <- datos_temp %>%
        group_by(Marca_Vehiculo) %>%
        mutate(
          valor_min = min(Vehiculos_Registrados, na.rm = TRUE),
          valor_max = max(Vehiculos_Registrados, na.rm = TRUE),
          Vehiculos_Registrados = ifelse(valor_max - valor_min > 0,
                                         (Vehiculos_Registrados - valor_min) / (valor_max - valor_min) * 100,
                                         50)
        ) %>%
        ungroup()
    }
    
    # Crear grÃ¡fico
    fig <- plot_ly()
    
    for(marca in top_marcas) {
      datos_marca <- datos_temp %>% filter(Marca_Vehiculo == marca)
      
      fig <- fig %>%
        add_trace(
          data = datos_marca,
          x = ~Fecha,
          y = ~Vehiculos_Registrados,
          type = "scatter",
          mode = "lines+markers",
          name = marca,
          line = list(width = 2.5),
          marker = list(size = 6)
        )
    }
    
    fig %>%
      layout(
        title = paste("EvoluciÃ³n Temporal - Top", top_n, "por", 
                      ifelse(input$criterio_temporal_oport == "score", "Score",
                             ifelse(input$criterio_temporal_oport == "volumen", "Volumen", "Crecimiento"))),
        xaxis = list(title = "PerÃ­odo"),
        yaxis = list(title = ifelse(input$normalizar_temporal_oport, "Valor Normalizado (0-100)", "VehÃ­culos Registrados")),
        hovermode = "x unified"
      )
  })
  
  # -----------------------------------------------------------------------------
  # PANEL DE RECOMENDACIONES INTELIGENTES
  # -----------------------------------------------------------------------------
  
  output$panel_recomendaciones_inteligentes_oport <- renderUI({
    
    datos <- datos_filtrados_oportunidades()
    
    if(nrow(datos) == 0) {
      return(p("No hay suficientes datos para generar recomendaciones", style = "color: #6c757d;"))
    }
    
    # AnÃ¡lisis de cuadrantes
    datos_cuadrantes <- clasificar_cuadrantes(datos)
    
    n_estrellas <- sum(datos_cuadrantes$cuadrante == "estrellas", na.rm = TRUE)
    n_promesas <- sum(datos_cuadrantes$cuadrante == "promesas", na.rm = TRUE)
    n_base_consolidada <- sum(datos_cuadrantes$cuadrante == "base_consolidada", na.rm = TRUE)
    n_interrogantes <- sum(datos_cuadrantes$cuadrante == "interrogantes", na.rm = TRUE)
    
    # Top 3 recomendaciones
    top_3 <- datos %>%
      arrange(desc(Score_Oportunidad)) %>%
      head(3)
    
    # Generar recomendaciones
    tagList(
      div(
        style = "background: linear-gradient(135deg, #fff3cd, #ffe8a1); padding: 20px; border-radius: 10px; margin-bottom: 15px; border-left: 5px solid #f59e0b;",
        h4("ðŸŽ¯ RECOMENDACIONES PRIORITARIAS", style = "color: #856404; margin-bottom: 15px;"),
        
        if(n_estrellas > 0) {
          p(paste0("â­ ", n_estrellas, " marcas ESTRELLAS detectadas: Invertir en mantener liderazgo y participaciÃ³n de mercado."),
            style = "margin: 8px 0; font-size: 14px; color: #856404;")
        },
        
        if(n_promesas >= 3) {
          p(paste0("ðŸš€ ", n_promesas, " marcas PROMESAS con alto potencial: Considerar inversiÃ³n temprana para ganar participaciÃ³n."),
            style = "margin: 8px 0; font-size: 14px; color: #856404;")
        },
        
        if(n_base_consolidada > 0) {
          p(paste0("ðŸ’° ", n_base_consolidada, " marcas BASE CONSOLIDADA: Mantener presencia con bajo costo, son generadoras de flujo estable."),
            style = "margin: 8px 0; font-size: 14px; color: #856404;")
        },
        
        if(n_interrogantes > 5) {
          p(paste0("â“ ", n_interrogantes, " marcas INTERROGANTES: Monitorear evoluciÃ³n antes de comprometer recursos."),
            style = "margin: 8px 0; font-size: 14px; color: #856404;")
        }
      ),
      
      div(
        style = "background: linear-gradient(135deg, #d4edda, #c3e6cb); padding: 20px; border-radius: 10px; margin-bottom: 15px; border-left: 5px solid #10b981;",
        h4("ðŸ† TOP 3 OPORTUNIDADES INMEDIATAS", style = "color: #155724; margin-bottom: 15px;"),
        
        lapply(1:min(3, nrow(top_3)), function(i) {
          marca <- top_3[i, ]
          div(
            style = "background: white; padding: 12px; border-radius: 8px; margin-bottom: 10px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
            p(paste0(i, ". ", marca$Marca_Vehiculo),
              style = "margin: 0; font-weight: bold; color: #2c3e50; font-size: 15px;"),
            p(paste0("Score: ", round(marca$Score_Oportunidad, 1), " | ",
                     "Volumen: ", format(marca$Volumen_Final, big.mark = ","), " | ",
                     "Crecimiento: ", round(marca$Crecimiento_Relativo, 1), "%"),
              style = "margin: 5px 0 0 0; font-size: 13px; color: #6c757d;")
          )
        })
      ),
      
      div(
        style = "background: linear-gradient(135deg, #d1ecf1, #bee5eb); padding: 20px; border-radius: 10px; border-left: 5px solid #17a2b8;",
        h4("ðŸ’¡ INSIGHTS ESTRATÃ‰GICOS", style = "color: #0c5460; margin-bottom: 15px;"),
        
        p(paste0("ðŸ“Š ConcentraciÃ³n del mercado: ", 
                 ifelse(sum(head(datos$Participacion_Mercado, 5), na.rm = TRUE) > 60,
                        "ALTA - Los top 5 controlan mÃ¡s del 60% del mercado",
                        "MODERADA - Mercado fragmentado con mÃºltiples jugadores")),
          style = "margin: 8px 0; font-size: 14px; color: #0c5460;"),
        
        p(paste0("ðŸ“ˆ Tendencia general: ",
                 ifelse(mean(datos$Crecimiento_Relativo, na.rm = TRUE) > 10,
                        "EXPANSIÃ“N - Mercado en crecimiento sostenido",
                        ifelse(mean(datos$Crecimiento_Relativo, na.rm = TRUE) > 0,
                               "ESTABLE - Crecimiento moderado",
                               "CONTRACCIÃ“N - Mercado en declive"))),
          style = "margin: 8px 0; font-size: 14px; color: #0c5460;"),
        
        p(paste0("ðŸŽ¯ Estrategia sugerida: ",
                 ifelse(n_estrellas >= 3, "Consolidar posiciÃ³n en lÃ­deres y explorar nichos emergentes",
                        ifelse(n_promesas >= 5, "InversiÃ³n agresiva en marcas prometedoras",
                               "Diversificar portfolio con anÃ¡lisis marca por marca"))),
          style = "margin: 8px 0; font-size: 14px; color: #0c5460;")
      )
    )
  })
  
  # -----------------------------------------------------------------------------
  # PANEL DE MÃ‰TRICAS DE RIESGO
  # -----------------------------------------------------------------------------
  
  output$panel_metricas_riesgo_oport <- renderUI({
    
    datos <- datos_filtrados_oportunidades()
    
    if(nrow(datos) == 0) {
      return(p("Sin datos", style = "color: #6c757d;"))
    }
    
    # Calcular HHI
    hhi <- sum((datos$Participacion_Mercado)^2, na.rm = TRUE)
    
    # Nivel de concentraciÃ³n
    nivel_concentracion <- ifelse(hhi < 1500, "Baja",
                                  ifelse(hhi < 2500, "Moderada", "Alta"))
    color_hhi <- ifelse(hhi < 1500, "#10b981",
                        ifelse(hhi < 2500, "#f59e0b", "#06b6d4"))
    
    # Volatilidad promedio
    volatilidad <- mean(datos$Coef_Variacion, na.rm = TRUE) * 100
    
    # Marcas en riesgo (declive)
    marcas_riesgo <- sum(datos$Crecimiento_Relativo < -10, na.rm = TRUE)
    
    tagList(
      div(
        style = "background: white; padding: 15px; border-radius: 8px; margin-bottom: 15px; border-left: 4px solid #06b6d4;",
        h5("ðŸ“Š Ãndice HHI", style = "margin: 0 0 8px 0; color: #2c3e50;"),
        h3(round(hhi, 0), style = paste0("margin: 0; color: ", color_hhi, ";")),
        p(nivel_concentracion, style = "margin: 5px 0 0 0; font-size: 12px; color: #6c757d;")
      ),
      
      div(
        style = "background: white; padding: 15px; border-radius: 8px; margin-bottom: 15px; border-left: 4px solid #f59e0b;",
        h5("ðŸ“‰ Volatilidad Promedio", style = "margin: 0 0 8px 0; color: #2c3e50;"),
        h3(paste0(round(volatilidad, 1), "%"), style = "margin: 0; color: #f59e0b;"),
        p("Coef. VariaciÃ³n", style = "margin: 5px 0 0 0; font-size: 12px; color: #6c757d;")
      ),
      
      div(
        style = "background: white; padding: 15px; border-radius: 8px; margin-bottom: 15px; border-left: 4px solid #06b6d4;",
        h5("âš ï¸ Marcas en Riesgo", style = "margin: 0 0 8px 0; color: #2c3e50;"),
        h3(marcas_riesgo, style = "margin: 0; color: #06b6d4;"),
        p("Declive > -10%", style = "margin: 5px 0 0 0; font-size: 12px; color: #6c757d;")
      ),
      
      div(
        style = "background: #fff3cd; padding: 15px; border-radius: 8px; border-left: 4px solid #f59e0b;",
        h5("ðŸ’¡ InterpretaciÃ³n", style = "margin: 0 0 10px 0; color: #856404;"),
        p(
          ifelse(hhi > 2500,
                 "Mercado altamente concentrado. Evaluar estrategias de diferenciaciÃ³n.",
                 ifelse(hhi > 1500,
                        "ConcentraciÃ³n moderada. Balance entre lÃ­deres y competidores.",
                        "Mercado fragmentado. MÃºltiples oportunidades de crecimiento.")),
          style = "margin: 0; font-size: 12px; color: #856404; line-height: 1.5;"
        )
      )
    )
  })
  
  # -----------------------------------------------------------------------------
  # TABLA MAESTRA DE OPORTUNIDADES
  # -----------------------------------------------------------------------------
  
  output$tabla_maestra_oportunidades_oport <- renderDataTable({
    
    datos <- datos_filtrados_oportunidades()
    
    if(nrow(datos) == 0) {
      return(data.frame(Mensaje = "No hay datos con los filtros aplicados"))
    }
    
    # Preparar tabla exportable
    tabla_maestra <- datos %>%
      arrange(desc(Score_Oportunidad)) %>%
      mutate(
        Ranking = 1:n()
      ) %>%
      select(
        Ranking,
        Marca = Marca_Vehiculo,
        Volumen = Volumen_Final,
        `Crecimiento %` = Crecimiento_Relativo,
        Score = Score_Oportunidad,
        `ParticipaciÃ³n %` = Participacion_Mercado,
        CategorÃ­a = Categoria_Volumen,
        Potencial = Potencial_Analytics,
        `Volatilidad %` = Coef_Variacion
      ) %>%
      mutate(
        Volumen = format(Volumen, big.mark = ","),
        `Crecimiento %` = round(`Crecimiento %`, 1),
        Score = round(Score, 1),
        `ParticipaciÃ³n %` = round(`ParticipaciÃ³n %`, 2),
        `Volatilidad %` = round(`Volatilidad %` * 100, 1)
      )
    
    datatable(
      tabla_maestra,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = list(
          list(extend = 'copy', text = 'ðŸ“‹ Copiar'),
          list(extend = 'csv', text = 'ðŸ“„ CSV'),
          list(extend = 'excel', text = 'ðŸ“Š Excel')
        ),
        columnDefs = list(
          list(targets = 0, className = "dt-center", width = "8%"),
          list(targets = c(2:5, 8), className = "dt-right")
        )
      ),
      rownames = FALSE,
      class = "compact stripe hover",
      extensions = 'Buttons'
    ) %>%
      formatStyle(
        "Ranking",
        backgroundColor = styleInterval(c(1, 2, 3), c("#FFD700", "#C0C0C0", "#CD7F32", "#f8f9fa")),
        fontWeight = "bold"
      ) %>%
      formatStyle(
        "Score",
        backgroundColor = styleInterval(c(40, 60, 80), c("#f8f9fa", "#fff3cd", "#d1ecf1", "#d4edda")),
        color = styleInterval(c(40, 60, 80), c("#6c757d", "#856404", "#0c5460", "#155724"))
      )
    
  }, server = FALSE)
  
  # -----------------------------------------------------------------------------
  # FUNCIONES DE EXPORTACIÃ“N
  # -----------------------------------------------------------------------------
  
  output$btn_exportar_oportunidades_excel <- downloadHandler(
    filename = function() {
      paste0("Oportunidades_DS Conexión_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      datos <- datos_filtrados_oportunidades()
      
      if(nrow(datos) > 0) {
        wb <- createWorkbook()
        addWorksheet(wb, "Oportunidades")
        writeData(wb, "Oportunidades", datos)
        saveWorkbook(wb, file, overwrite = TRUE)
      }
    }
  )
  
  output$btn_exportar_oportunidades_csv <- downloadHandler(
    filename = function() {
      paste0("Oportunidades_DS Conexión_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      datos <- datos_filtrados_oportunidades()
      
      if(nrow(datos) > 0) {
        write_csv(datos, file)
      }
    }
  )
  
  
  # =============================================================================
  # MENSAJE DE FINALIZACIÃ“N DEL SERVIDOR
  # =============================================================================
  
  # Log de inicio del servidor
  cat("\nðŸš€ SERVIDOR DS Conexión INICIADO CORRECTAMENTE\n")
  cat("ðŸ“Š Dashboard ejecutivo: ACTIVO\n")
  cat("ðŸ” AnÃ¡lisis de oportunidades: DISPONIBLE\n") 
  cat("âš¡ Sistema responsivo: CONFIGURADO\n")
  cat("âœ… Listo para recibir usuarios\n")
  cat(paste(rep("=", 50), collapse = ""), "\n")
  
  
  
}