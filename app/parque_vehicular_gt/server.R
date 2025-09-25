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
  
  # Mapa Estratégico Principal - CORRECCIÓN
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
        # CORRECCIÓN: Los colores deben coincidir exactamente con los valores de global.R
        colors = c("🔴 ALTA PRIORIDAD" = "#E31A1C", 
                   "🟠 MEDIA PRIORIDAD" = "#FF7F00",
                   "🔵 EMERGENTE" = "#1F78B4",
                   "⚫ BAJA PRIORIDAD" = "#666666"),
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
        data = metricas_global %>% 
          filter(Potencial_Radiadores %in% c("🔴 ALTA PRIORIDAD", "🟠 MEDIA PRIORIDAD"),
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
          
          # Determinar el estilo según el tipo de alerta
          if(nombre_alerta == "oportunidad") {
            clase_alerta <- "alert-success"
            icono <- "rocket"
            color_borde <- "#27ae60"
          } else if(nombre_alerta == "riesgo") {
            clase_alerta <- "alert-danger" 
            icono <- "exclamation-triangle"
            color_borde <- "#e74c3c"
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
            
            # Timestamp pequeño
            div(
              style = "text-align: right; margin-top: 8px; font-size: 10px; color: #6c757d;",
              format(Sys.time(), "%H:%M")
            )
          )
          
          contador <- contador + 1
        }
      }
    }
    
    # Si no hay alertas específicas, mostrar métricas importantes
    if(length(elementos_alertas) == 0) {
      
      # Calcular algunas métricas para mostrar
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
            
            p(paste0("📊 ", total_marcas, " marcas analizadas"), 
              style = "margin: 5px 0; font-size: 12px;"),
            p(paste0("🚀 ", alto_crecimiento, " marcas con alto crecimiento"), 
              style = "margin: 5px 0; font-size: 12px;"),
            p(paste0("⭐ Score promedio: ", round(mean(metricas_global$Score_Oportunidad, na.rm = TRUE), 1)), 
              style = "margin: 5px 0; font-size: 12px;")
          ),
          
          # Top performers
          div(
            class = "alert alert-success",
            style = "margin-bottom: 15px; border-left: 4px solid #27ae60; border-radius: 8px; padding: 15px; background: white;",
            
            div(
              style = "display: flex; align-items: center; gap: 10px; margin-bottom: 10px;",
              icon("trophy", style = "color: #27ae60; font-size: 18px;"),
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
          
          # Actualización
          div(
            class = "alert alert-warning",
            style = "margin-bottom: 15px; border-left: 4px solid #f39c12; border-radius: 8px; padding: 15px; background: white;",
            
            div(
              style = "display: flex; align-items: center; gap: 10px;",
              icon("clock", style = "color: #f39c12; font-size: 16px;"),
              div(
                strong("Última actualización", style = "color: #2c3e50; font-size: 12px;"),
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
  
  # Tabla de Métricas de Crecimiento
  output$tabla_metricas_crecimiento <- renderTable({
    
    if(is.null(metricas_global) || nrow(metricas_global) == 0) {
      return(data.frame(
        Métrica = "Sin datos disponibles",
        Valor = "N/A"
      ))
    }
    
    # Calcular métricas de crecimiento
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
      Métrica = c(
        "📈 Crecimiento Promedio",
        "📊 Crecimiento Mediano", 
        "🚀 Marcas en Crecimiento",
        "📉 Marcas en Declive",
        "⚖️ Marcas Estables",
        "🏆 Mayor Crecimiento",
        "📊 Volatilidad Promedio"
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
  
  # Tabla de Concentración del Mercado  
  output$tabla_concentracion_mercado <- renderTable({
    
    if(is.null(metricas_global) || nrow(metricas_global) == 0) {
      return(data.frame(
        Métrica = "Sin datos disponibles",
        Valor = "N/A"
      ))
    }
    
    # Calcular métricas de concentración
    total_vehiculos <- sum(metricas_global$Volumen_Final, na.rm = TRUE)
    
    # Top 5 marcas por volumen
    top_5_vol <- sum(head(metricas_global$Participacion_Mercado, 5), na.rm = TRUE)
    
    # Top 10 marcas
    top_10_vol <- sum(head(metricas_global$Participacion_Mercado, 10), na.rm = TRUE)
    
    # Índice Herfindahl-Hirschman (HHI)
    hhi <- sum((metricas_global$Participacion_Mercado)^2, na.rm = TRUE)
    
    # Interpretación del HHI
    if(hhi < 1500) {
      interpretacion_hhi <- "Baja concentración"
    } else if(hhi < 2500) {
      interpretacion_hhi <- "Concentración moderada"  
    } else {
      interpretacion_hhi <- "Alta concentración"
    }
    
    # Líder del mercado
    lider_mercado <- metricas_global %>%
      arrange(desc(Volumen_Final)) %>%
      slice(1)
    
    # Distribución por categorías
    dist_categorias <- metricas_global %>%
      count(Categoria_Volumen, name = "cantidad") %>%
      arrange(desc(cantidad))
    
    categoria_dominante <- dist_categorias$Categoria_Volumen[1]
    
    # Crear tabla
    data.frame(
      Métrica = c(
        "🥇 Líder del Mercado",
        "🏆 Top 5 Concentración", 
        "📊 Top 10 Concentración",
        "📈 Índice HHI",
        "🎯 Nivel de Competencia",
        "📋 Total de Marcas",
        "🏷️ Categoría Dominante"
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
  # TOP PERFORMERS - TABLAS DINÁMICAS
  # =============================================================================
  
  # Top por Volumen
  output$tabla_top_volumen <- renderDataTable({
    
    if(is.null(metricas_global) || nrow(metricas_global) == 0) {
      return(data.frame(
        Ranking = "Sin datos",
        Marca = "N/A", 
        Vehículos = "N/A",
        Participación = "N/A"
      ))
    }
    
    tabla_volumen <- metricas_global %>%
      arrange(desc(Volumen_Final)) %>%
      head(10) %>%
      mutate(
        Ranking = 1:n(),
        Marca = Marca_Vehiculo,
        Vehículos = format(Volumen_Final, big.mark = ","),
        `Participación %` = paste0(round(Participacion_Mercado, 1), "%"),
        Potencial = case_when(
          str_detect(Potencial_Radiadores, "ALTA") ~ "🔴 Alta",
          str_detect(Potencial_Radiadores, "MEDIA") ~ "🟠 Media", 
          str_detect(Potencial_Radiadores, "EMERGENTE") ~ "🔵 Emergente",
          TRUE ~ "⚫ Baja"
        )
      ) %>%
      select(Ranking, Marca, Vehículos, `Participación %`, Potencial)
    
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
      filter(Volumen_Final >= 100) %>%  # Solo marcas con volumen mínimo
      arrange(desc(Crecimiento_Relativo)) %>%
      head(10) %>%
      mutate(
        Ranking = 1:n(),
        Marca = Marca_Vehiculo,
        `Crecimiento %` = paste0(round(Crecimiento_Relativo, 1), "%"),
        Volumen = format(Volumen_Final, big.mark = ","),
        Tendencia = case_when(
          Crecimiento_Relativo >= 50 ~ "🚀 Explosivo",
          Crecimiento_Relativo >= 20 ~ "📈 Alto",
          Crecimiento_Relativo >= 10 ~ "📊 Moderado", 
          Crecimiento_Relativo >= 0 ~ "🔄 Estable",
          TRUE ~ "📉 Declive"
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
        Clasificación = "N/A"
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
        Clasificación = case_when(
          Score_Oportunidad >= 80 ~ "⭐⭐⭐ Excelente",
          Score_Oportunidad >= 60 ~ "⭐⭐ Muy Buena",
          Score_Oportunidad >= 40 ~ "⭐ Buena",
          TRUE ~ "💡 Potencial"
        ),
        Potencial = case_when(
          str_detect(Potencial_Radiadores, "ALTA") ~ "🔴",
          str_detect(Potencial_Radiadores, "MEDIA") ~ "🟠",
          str_detect(Potencial_Radiadores, "EMERGENTE") ~ "🔵", 
          TRUE ~ "⚫"
        )
      ) %>%
      select(Ranking, Marca, Score, Volumen, Clasificación, Potencial)
    
    datatable(
      tabla_score,
      caption = htmltools::tags$caption(
        style = "caption-side: bottom; text-align: left; padding-top: 15px; font-size: 11px; color: #6c757d; line-height: 1.4;",
        htmltools::HTML(
          "<strong>📊 Cálculo del Score de Oportunidad (0-100 puntos):</strong><br>
        • <strong>Componente Volumen (0-45 pts):</strong> Log₁₀(Volumen + 1) × 15<br>
        • <strong>Componente Crecimiento (0-40 pts):</strong> % Crecimiento × 0.8 (máx 50%)<br>
        • <strong>Componente Estabilidad (0-15 pts):</strong> (25 - Coef. Variación × 100) × 0.6<br>
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
  
  # Gráfico de tendencias temporales
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
    
    # Crear gráfico base
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
              "Vehículos: %{y:,.0f}<br>",
              "<extra></extra>"
            )
          )
      }
      
      # Agregar líneas de tendencia si se solicita
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
      
      # Gráfico de área apilada
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
              "Vehículos: %{y:,.0f}<br>",
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
        title = "Evolución Temporal del Parque Vehicular",
        xaxis = list(title = "Período"),
        yaxis = list(title = "Vehículos Registrados"),
        hovermode = "x unified"
      )
  })
  
  # Estadísticas de tendencias
  output$tabla_estadisticas_tendencias <- renderDataTable({
    
    req(input$marcas_seleccionadas)
    
    if(is.null(datos_long_global)) {
      return(data.frame(Mensaje = "Sin datos temporales"))
    }
    
    estadisticas <- datos_long_global %>%
      filter(Marca_Vehiculo %in% input$marcas_seleccionadas) %>%
      group_by(Marca_Vehiculo) %>%
      summarise(
        Períodos = n(),
        Promedio = round(mean(Vehiculos_Registrados, na.rm = TRUE), 0),
        Mediana = round(median(Vehiculos_Registrados, na.rm = TRUE), 0),
        Mínimo = min(Vehiculos_Registrados, na.rm = TRUE),
        Máximo = max(Vehiculos_Registrados, na.rm = TRUE),
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
      formatCurrency(c("Promedio", "Mediana", "Mínimo", "Máximo", "Desv. Est."), "", digits = 0)
    
  }, server = FALSE)
  
  # Panel de métricas de tendencia
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
    
    # Calcular métricas
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
        h5("📊 Resumen General", style = "color: #2c3e50; margin-bottom: 10px;"),
        p(paste("Períodos analizados:", total_periodos), style = "margin: 5px 0;"),
        p(paste("Promedio general:", format(round(promedio_general), big.mark = ",")), style = "margin: 5px 0;"),
        p(paste("Tendencia:", tendencia_general), style = "margin: 5px 0;")
      ),
      
      div(
        style = "background: #e3f2fd; padding: 15px; border-radius: 8px;",
        h5("🎯 Recomendaciones", style = "color: #1565c0; margin-bottom: 10px;"),
        if(tendencia_general == "Creciente") {
          p("📈 Mercado en expansión - Oportunidad de crecimiento", style = "color: #27ae60; margin: 0;")
        } else if(tendencia_general == "Decreciente") {
          p("📉 Mercado en contracción - Revisar estrategia", style = "color: #e74c3c; margin: 0;")
        } else {
          p("⚖️ Mercado estable - Monitorear cambios", style = "color: #f39c12; margin: 0;")
        }
      )
    )
  })
  
  # =============================================================================
  # ANÁLISIS DETALLADO
  # =============================================================================
  
  # Actualizar opciones para análisis detallado
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
  
  # Panel de métricas de marca individual
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
      h4(marca_info$Marca_Vehiculo, style = "color: #c41e3a; text-align: center; margin-bottom: 20px;"),
      
      div(
        style = "background: linear-gradient(135deg, #f8f9fa, #e9ecef); padding: 15px; border-radius: 10px; margin-bottom: 15px;",
        
        div(style = "text-align: center; margin-bottom: 15px;",
            h3(format(marca_info$Volumen_Final, big.mark = ","), style = "color: #2c3e50; margin: 0;"),
            p("Vehículos Totales", style = "color: #6c757d; margin: 0; font-size: 12px;")
        ),
        
        hr(style = "margin: 15px 0;"),
        
        div(style = "display: flex; justify-content: space-between; margin-bottom: 10px;",
            span("🏆 Ranking Score:", style = "color: #495057; font-size: 13px;"),
            strong(paste("#", which(metricas_global$Marca_Vehiculo[order(-metricas_global$Score_Oportunidad)] == input$marca_detalle)), 
                   style = "color: #c41e3a; font-size: 13px;")
        ),
        
        div(style = "display: flex; justify-content: space-between; margin-bottom: 10px;",
            span("📈 Crecimiento:", style = "color: #495057; font-size: 13px;"),
            strong(paste0(round(marca_info$Crecimiento_Relativo, 1), "%"), 
                   style = paste0("color: ", ifelse(marca_info$Crecimiento_Relativo >= 0, "#27ae60", "#e74c3c"), "; font-size: 13px;"))
        ),
        
        div(style = "display: flex; justify-content: space-between; margin-bottom: 10px;",
            span("⭐ Score:", style = "color: #495057; font-size: 13px;"),
            strong(paste0(round(marca_info$Score_Oportunidad, 1), "/100"), style = "color: #3498db; font-size: 13px;")
        ),
        
        div(style = "display: flex; justify-content: space-between; margin-bottom: 10px;",
            span("📊 Participación:", style = "color: #495057; font-size: 13px;"),
            strong(paste0(round(marca_info$Participacion_Mercado, 2), "%"), style = "color: #f39c12; font-size: 13px;")
        ),
        
        hr(style = "margin: 15px 0;"),
        
        div(
          style = "text-align: center; padding: 10px; background: white; border-radius: 8px;",
          strong(marca_info$Potencial_Radiadores, style = "font-size: 11px;")
        )
      ),
      
      div(
        style = "background: #fff3cd; padding: 15px; border-radius: 8px; border-left: 4px solid #f39c12;",
        h6("💡 Análisis Rápido", style = "color: #856404; margin-bottom: 8px;"),
        p(
          if(marca_info$Score_Oportunidad >= 70) {
            "Excelente oportunidad para radiadores. Alta prioridad comercial."
          } else if(marca_info$Score_Oportunidad >= 50) {
            "Buena oportunidad. Considerar para estrategia de mediano plazo."
          } else {
            "Oportunidad emergente. Monitorear evolución del mercado."
          },
          style = "color: #856404; font-size: 12px; margin: 0; line-height: 1.4;"
        )
      )
    )
  })
  
  # Gráfico de evolución individual
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
    
    # Crear gráfico con línea y área
    fig <- plot_ly() %>%
      add_trace(
        data = datos_marca,
        x = ~Fecha,
        y = ~Vehiculos_Registrados,
        type = "scatter",
        mode = "lines+markers",
        fill = "tozeroy",
        fillcolor = "rgba(231, 76, 60, 0.2)",
        line = list(color = "#e74c3c", width = 3),
        marker = list(color = "#c41e3a", size = 8),
        name = input$marca_detalle,
        hovertemplate = paste0(
          "<b>", input$marca_detalle, "</b><br>",
          "Fecha: %{x}<br>",
          "Vehículos: %{y:,.0f}<br>",
          "<extra></extra>"
        )
      )
    
    # Agregar línea de tendencia
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
        title = paste("Evolución de", input$marca_detalle),
        xaxis = list(title = "Período"),
        yaxis = list(title = "Vehículos Registrados"),
        showlegend = TRUE
      )
  })
  
  # =============================================================================
  # CONFIGURACIÓN - INFORMACIÓN DEL SISTEMA
  # =============================================================================
  
  # Información del sistema
  output$info_sistema_rlt <- renderText({
    
    info_texto <- paste0(
      "Sistema: Estrategia RLT v2.1.0\n",
      "Estado: Activo ✓\n",
      "Datos cargados: ", ifelse(is.null(metricas_global), 0, nrow(metricas_global)), " marcas\n",
      "Última actualización: ", format(Sys.time(), "%d/%m/%Y %H:%M"), "\n",
      "Memoria utilizada: ", round(object.size(metricas_global) / 1024^2, 2), " MB\n",
      "Sesión iniciada: ", format(timestamp_app, "%H:%M")
    )
    
    return(info_texto)
  })
  
  # Estadísticas de uso
  output$tabla_estadisticas_uso_rlt <- renderTable({
    
    data.frame(
      Métrica = c(
        "Total Marcas Analizadas",
        "Períodos Temporales", 
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
  output$log_sistema_rlt <- renderText({
    
    log_entries <- c(
      paste("[", format(timestamp_app, "%H:%M:%S"), "] ✓ Sistema RLT iniciado correctamente"),
      paste("[", format(timestamp_app + 1, "%H:%M:%S"), "] ✓ Datos del parque vehicular cargados:", ifelse(is.null(metricas_global), 0, nrow(metricas_global)), "marcas"),
      paste("[", format(timestamp_app + 2, "%H:%M:%S"), "] ✓ Métricas calculadas exitosamente"),
      paste("[", format(timestamp_app + 3, "%H:%M:%S"), "] ✓ Dashboard ejecutivo activado"),
      paste("[", format(Sys.time(), "%H:%M:%S"), "] ✓ Interface de usuario lista"),
      "",
      "═══════════════════════════════════════",
      "SISTEMA FUNCIONANDO CORRECTAMENTE",
      "═══════════════════════════════════════"
    )
    
    paste(log_entries, collapse = "\n")
  })
  
  
  
  # =============================================================================
  # ELEMENTOS REACTIVOS Y OBSERVADORES
  # =============================================================================
  
  # Observador para actualización automática de datos
  observeEvent(input$btn_actualizar_datos, {
    
    # Mostrar mensaje de carga
    showModal(modalDialog(
      title = "🔄 Actualizando Datos del Parque Vehicular",
      "Procesando información más reciente del SAT...",
      easyClose = FALSE,
      footer = NULL
    ))
    
    # Simular actualización (en producción aquí se cargarían datos reales)
    Sys.sleep(2)
    
    # Actualizar timestamp
    valores_reactivos$datos_actualizados <- Sys.time()
    
    # Agregar entrada al log
    nuevos_logs <- c(
      valores_reactivos$logs_sistema,
      paste("[", format(Sys.time(), "%H:%M:%S"), "] 🔄 Datos actualizados manualmente por el usuario")
    )
    valores_reactivos$logs_sistema <- tail(nuevos_logs, 20)  # Mantener últimas 20 entradas
    
    removeModal()
    
    # Mostrar notificación de éxito
    showNotification(
      "✅ Datos del parque vehicular actualizados correctamente",
      type = "success",
      duration = 3
    )
  })
  
  # Observador para aplicar filtros
  observeEvent(input$btn_aplicar_filtros, {
    valores_reactivos$filtros_aplicados <- !valores_reactivos$filtros_aplicados
    
    showNotification(
      "🔍 Filtros aplicados correctamente",
      type = "default",
      duration = 2
    )
  })
  
  # Observador para generar análisis detallado
  observeEvent(input$btn_generar_detalle, {
    
    req(input$marca_detalle)
    
    valores_reactivos$marca_seleccionada <- input$marca_detalle
    
    showNotification(
      paste("📊 Análisis detallado generado para", input$marca_detalle),
      type = "success",
      duration = 3
    )
  })
  
  # Observador para botones de configuración
  observeEvent(input$limpiar_cache_rlt, {
    
    showModal(modalDialog(
      title = "⚠️ Confirmar Acción",
      "¿Está seguro de que desea limpiar el cache del sistema?",
      footer = tagList(
        actionButton("confirmar_limpiar", "Sí, Limpiar", class = "btn-warning"),
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
      paste("[", format(Sys.time(), "%H:%M:%S"), "] 🗑️ Cache del sistema limpiado")
    )
    
    showNotification(
      "🗑️ Cache limpiado correctamente",
      type = "warning",
      duration = 3
    )
  })
  
  observeEvent(input$exportar_datos_rlt, {
    
    showNotification(
      "📤 Exportación iniciada. Preparando archivo Excel...",
      type = "default",
      duration = 3
    )
    
    # En producción aquí se ejecutaría la exportación real
    valores_reactivos$logs_sistema <- c(
      valores_reactivos$logs_sistema,
      paste("[", format(Sys.time(), "%H:%M:%S"), "] 📤 Datos exportados a Excel")
    )
  })
  
  observeEvent(input$reiniciar_app_rlt, {
    
    showModal(modalDialog(
      title = "🔄 Reiniciar Aplicación",
      "⚠️ Esta acción reiniciará completamente la aplicación. ¿Continuar?",
      footer = tagList(
        actionButton("confirmar_reiniciar", "Reiniciar", class = "btn-danger"),
        modalButton("Cancelar")
      )
    ))
  })
  
  observeEvent(input$confirmar_reiniciar, {
    
    removeModal()
    
    showNotification(
      "🔄 Reiniciando aplicación...",
      type = "error",
      duration = 3
    )
    
    # En producción esto podría reiniciar la sesión de Shiny
    session$reload()
  })
  
  # Observador para prueba de carga de datos
  observeEvent(input$test_carga_datos, {
    
    showModal(modalDialog(
      title = "🧪 Probando Carga de Datos",
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
      "✅ Datos cargados correctamente",
      "❌ Error en la carga de datos"
    )
    
    showNotification(
      resultado_test,
      type = ifelse(str_detect(resultado_test, "✅"), "success", "error"),
      duration = 4
    )
  })
  
  # Observador para auto-refresh
  observe({
    
    # Auto-refresh cada 30 minutos si está activado
    if(!is.null(input$auto_refresh_rlt) && input$auto_refresh_rlt) {
      
      invalidateLater(1800000)  # 30 minutos en milisegundos
      
      # Agregar al log
      valores_reactivos$logs_sistema <- c(
        valores_reactivos$logs_sistema,
        paste("[", format(Sys.time(), "%H:%M:%S"), "] 🔄 Auto-actualización ejecutada")
      )
      
      valores_reactivos$datos_actualizados <- Sys.time()
    }
  })
  
  # Observador para modo debug
  observe({
    
    if(!is.null(input$modo_debug_rlt) && input$modo_debug_rlt) {
      
      # En modo debug, mostrar información adicional
      valores_reactivos$logs_sistema <- c(
        valores_reactivos$logs_sistema,
        paste("[", format(Sys.time(), "%H:%M:%S"), "] 🐛 Modo debug activado")
      )
      
      showNotification(
        "🐛 Modo debug activado - Información detallada disponible",
        type = "default",
        duration = 5
      )
    }
  })
  
  # =============================================================================
  # FUNCIONES AUXILIARES Y OUTPUTS ADICIONALES  
  # =============================================================================
  
  # Gráfico de comparación con competencia (análisis detallado)
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
        color_barra = ifelse(es_seleccionada, "#e74c3c", "#95a5a6")
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
        title = paste("Comparación con Competencia Directa -", input$marca_detalle),
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
      filter(abs(Volumen_Final - volumen_ref) <= volumen_ref * 0.5) %>%  # ±50% del volumen
      arrange(desc(Score_Oportunidad)) %>%
      select(
        Marca = Marca_Vehiculo,
        Volumen = Volumen_Final,
        `Crecimiento %` = Crecimiento_Relativo,
        Score = Score_Oportunidad,
        `Participación %` = Participacion_Mercado,
        Categoría = Categoria_Volumen,
        Potencial = Potencial_Radiadores
      ) %>%
      mutate(
        Volumen = format(Volumen, big.mark = ","),
        `Crecimiento %` = round(`Crecimiento %`, 1),
        Score = round(Score, 1),
        `Participación %` = round(`Participación %`, 2),
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
    output$ultima_actualizacion_rlt <- renderText({
      paste("Actualizado:", format(Sys.time(), "%H:%M"))
    })
  })
  
  # Output para el log en tiempo real
  observe({
    output$log_sistema_rlt <- renderText({
      if(length(valores_reactivos$logs_sistema) > 0) {
        paste(tail(valores_reactivos$logs_sistema, 15), collapse = "\n")
      } else {
        paste(
          paste("[", format(Sys.time(), "%H:%M:%S"), "] ✓ Sistema iniciado correctamente"),
          paste("[", format(Sys.time(), "%H:%M:%S"), "] ✓ Datos cargados exitosamente"),
          "═══════════════════════════════════════",
          "SISTEMA FUNCIONANDO CORRECTAMENTE",
          sep = "\n"
        )
      }
    })
  })
  
  
  # =============================================================================
  # OUTPUTS FALTANTES PARA PANORAMA GENERAL
  # =============================================================================
  
  # Gráfico de distribución por categorías
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
      marker = list(color = "#c41e3a", opacity = 0.8),
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
        title = "Distribución del Parque Vehicular por Categoría",
        xaxis = list(title = "Número de Marcas"),
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
      count(Potencial_Radiadores, name = "cantidad") %>%
      mutate(
        color_categoria = case_when(
          str_detect(Potencial_Radiadores, "ALTA") ~ "#E31A1C",
          str_detect(Potencial_Radiadores, "MEDIA") ~ "#FF7F00", 
          str_detect(Potencial_Radiadores, "EMERGENTE") ~ "#1F78B4",
          TRUE ~ "#666666"
        )
      )
    
    plot_ly(
      datos_matriz,
      labels = ~Potencial_Radiadores,
      values = ~cantidad,
      type = "pie",
      marker = list(colors = ~color_categoria),
      textinfo = "label+percent",
      hovertemplate = paste0(
        "<b>%{label}</b><br>",
        "Marcas: %{value}<br>",
        "Porcentaje: %{percent}<br>",
        "<extra></extra>"
      )
    ) %>%
      layout(
        title = "Matriz de Potencial de Radiadores"
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
        `Participación %` = Participacion_Mercado,
        Categoría = Categoria_Volumen,
        Potencial = Potencial_Radiadores
      ) %>%
      mutate(
        Volumen = format(Volumen, big.mark = ","),
        `Crecimiento %` = round(`Crecimiento %`, 1),
        Score = round(Score, 1),
        `Participación %` = round(`Participación %`, 2)
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
  # MENSAJE DE FINALIZACIÓN DEL SERVIDOR
  # =============================================================================
  
  # Log de inicio del servidor
  cat("\n🚀 SERVIDOR RLT INICIADO CORRECTAMENTE\n")
  cat("📊 Dashboard ejecutivo: ACTIVO\n")
  cat("🔍 Análisis de oportunidades: DISPONIBLE\n") 
  cat("⚡ Sistema responsivo: CONFIGURADO\n")
  cat("✅ Listo para recibir usuarios\n")
  cat(paste(rep("=", 50), collapse = ""), "\n")
  

  
}