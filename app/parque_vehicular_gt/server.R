function(input, output, session) {
  
  # ============================================================================
  # 1. REACTIVOS BASE (SINCRONIZADOS CON global.R)
  # ============================================================================
  
  # Objetos globales definidos en global.R
  # - datos_rfv_global
  # - datos_long_global
  # - metricas_global
  # - kpis_global
  # - alertas_global
  # y funciones:
  # - procesar_datos_long()
  # - calcular_metricas_marcas()
  # - calcular_kpis_mercado()
  # - crear_top_marcas()
  # - generar_alertas_mercado()
  
  metricas_react      <- reactiveVal(metricas_global)
  datos_long_react    <- reactiveVal(datos_long_global)
  kpis_react          <- reactiveVal(kpis_global)
  alertas_react       <- reactiveVal(alertas_global)
  ultima_actualizacion <- reactiveVal(Sys.time())
  
  # ============================================================================
  # 2. APLICACIÓN / REFRESCO DE FILTROS (VERSIÓN SIMPLE)
  # ============================================================================
  # En esta versión los filtros visuales del UI tienen un efecto simplificado:
  # se filtra por categorías de volumen / potencial si se seleccionan, pero se
  # mantiene la lógica base de global.R para cálculos de KPIs y alertas.
  # ============================================================================
  
  aplicar_filtros_metricas <- function() {
    met <- metricas_global
    
    # Filtro por categoría de volumen (tab visión ejecutiva / panorama general)
    if (!is.null(input$filtro_categoria_volumen) && 
        length(input$filtro_categoria_volumen) > 0) {
      met <- met %>%
        dplyr::filter(Categoria_Volumen %in% input$filtro_categoria_volumen)
    }
    
    # Filtro por "potencial" (etiquetas Potencial_Analytics)
    if (!is.null(input$filtro_potencial) && 
        length(input$filtro_potencial) > 0 &&
        !"Todas" %in% input$filtro_potencial) {
      met <- met %>%
        dplyr::filter(Potencial_Analytics %in% input$filtro_potencial)
    }
    
    # Filtros de la pestaña de oportunidades (si están definidos)
    met_filt_oport <- met
    
    # Volumen mínimo / máximo
    if (!is.null(input$filtro_volumen_min_oport)) {
      met_filt_oport <- met_filt_oport %>%
        dplyr::filter(Volumen_Final >= input$filtro_volumen_min_oport)
    }
    if (!is.null(input$filtro_volumen_max_oport) &&
        !is.na(input$filtro_volumen_max_oport) &&
        input$filtro_volumen_max_oport > 0) {
      met_filt_oport <- met_filt_oport %>%
        dplyr::filter(Volumen_Final <= input$filtro_volumen_max_oport)
    }
    
    # Crecimiento
    if (!is.null(input$filtro_crecimiento_min_oport)) {
      met_filt_oport <- met_filt_oport %>%
        dplyr::filter(Crecimiento_Relativo >= input$filtro_crecimiento_min_oport)
    }
    if (!is.null(input$filtro_crecimiento_max_oport)) {
      met_filt_oport <- met_filt_oport %>%
        dplyr::filter(Crecimiento_Relativo <= input$filtro_crecimiento_max_oport)
    }
    
    # Score mínimo
    if (!is.null(input$filtro_score_min_oport)) {
      met_filt_oport <- met_filt_oport %>%
        dplyr::filter(Score_Oportunidad >= input$filtro_score_min_oport)
    }
    
    # Categorías de oportunidad
    if (!is.null(input$filtro_categorias_oport) &&
        length(input$filtro_categorias_oport) > 0) {
      met_filt_oport <- met_filt_oport %>%
        dplyr::filter(Categoria_Volumen %in% input$filtro_categorias_oport)
    }
    
    # Potencial oportunidad
    if (!is.null(input$filtro_potencial_oport) &&
        length(input$filtro_potencial_oport) > 0 &&
        !"Todas" %in% input$filtro_potencial_oport) {
      met_filt_oport <- met_filt_oport %>%
        dplyr::filter(Potencial_Analytics %in% input$filtro_potencial_oport)
    }
    
    list(
      metricas_generales = met,
      metricas_oportunidades = met_filt_oport
    )
  }
  
  # Botón "Aplicar filtros" (panorama general / visión ejecutiva)
  observeEvent(input$btn_aplicar_filtros, {
    filtros <- aplicar_filtros_metricas()
    met_gen <- filtros$metricas_generales
    
    metricas_react(met_gen)
    kpis_react(calcular_kpis_mercado(met_gen, datos_long_global))
    alertas_react(generar_alertas_mercado(met_gen))
    datos_long_react(datos_long_global)  # en esta versión no se filtra periodo temporal
    
    ultima_actualizacion(Sys.time())
  }, ignoreInit = TRUE)
  
  # Botones de filtros para la pestaña de oportunidades
  observeEvent(input$btn_aplicar_filtros_oport, {
    filtros <- aplicar_filtros_metricas()
    met_oport <- filtros$metricas_oportunidades
    
    # En oportunidades, usamos el subconjunto como base de métricas
    metricas_react(met_oport)
    kpis_react(calcular_kpis_mercado(met_oport, datos_long_global))
    alertas_react(generar_alertas_mercado(met_oport))
    
    ultima_actualizacion(Sys.time())
  }, ignoreInit = TRUE)
  
  observeEvent(input$btn_resetear_filtros_oport, {
    metricas_react(metricas_global)
    kpis_react(kpis_global)
    alertas_react(alertas_global)
    datos_long_react(datos_long_global)
    ultima_actualizacion(Sys.time())
  }, ignoreInit = TRUE)
  
  # Auto-refresh desde JS (checkbox auto_refresh_ds)
  observeEvent(input$trigger_refresh_parque, {
    metricas_react(metricas_global)
    kpis_react(kpis_global)
    alertas_react(alertas_global)
    datos_long_react(datos_long_global)
    ultima_actualizacion(Sys.time())
  }, ignoreInit = TRUE)
  
  # Actualizar lista de marcas para análisis detallado
  observe({
    met <- metricas_react()
    if (!is.null(met) && nrow(met) > 0) {
      updateSelectInput(
        session,
        "marca_detalle",
        choices = sort(unique(met$Marca_Vehiculo)),
        selected = head(sort(unique(met$Marca_Vehiculo)), 1)
      )
    }
  })
  
  # ============================================================================
  # 3. NOTIFICACIONES Y TEXTOS DE CABECERA / SISTEMA
  # ============================================================================
  
  output$menu_notificaciones_ds <- shinydashboard::renderMenu({
    met <- metricas_react()
    kpi <- kpis_react()
    
    shinydashboard::dropdownMenu(
      type = "notifications",
      icon = icon("bell"),
      badgeStatus = "info",
      notificationItem(
        text = "Dashboard DS Conexion iniciado",
        icon = icon("check-circle"),
        status = "success"
      ),
      notificationItem(
        text = paste(
          "Marcas analizadas:",
          ifelse(is.null(met), 0, nrow(met))
        ),
        icon = icon("car-side"),
        status = "info"
      ),
      notificationItem(
        text = paste(
          "Total vehículos:",
          ifelse(is.null(kpi), 0, format(round(kpi$total_vehiculos, 0), big.mark = ","))
        ),
        icon = icon("database"),
        status = "warning"
      )
    )
  })
  
  output$ultima_actualizacion_ds <- renderText({
    paste(
      "Última actualización:",
      format(ultima_actualizacion(), "%d/%m/%Y %H:%M:%S")
    )
  })
  
  output$timestamp_sistema_ds <- renderText({
    paste("Sesión iniciada:", format(timestamp_app, "%d/%m/%Y %H:%M:%S"))
  })
  
 
  output$fecha_actual_ds <- renderText({
    # Usa la última columna de datos_rfv_global como referencia de actualización
    if (is.null(datos_rfv_global) || ncol(datos_rfv_global) == 0) {
      return("Sin datos")
    }
    
    ultima_col <- tail(colnames(datos_rfv_global), 1)
    # Limpieza sencilla para mostrar “Noviembre 2025” en lugar de “Noviembre_2025”
    ultima_col_limpia <- gsub("_", " ", ultima_col)
    
    ultima_col_limpia
  })
  
  
  output$version_app_ds <- renderText({
    "Versión del módulo: 1.0.0"
  })
  
  # ============================================================================
  # 4. VALUEBOX PRINCIPALES (TAB VISIÓN EJECUTIVA)
  # ============================================================================
  
  output$vb_total_vehiculos <- renderValueBox({
    kpi <- kpis_react()
    total <- ifelse(is.null(kpi), 0, kpi$total_vehiculos)
    
    shinydashboard::valueBox(
      value = format(round(total, 0), big.mark = ","),
      subtitle = "Vehículos Registrados (Total)",
      icon = icon("car-side"),
      color = "blue"
    )
  })
  
  output$vb_marcas_alta_prioridad <- renderValueBox({
    kpi <- kpis_react()
    met <- metricas_react()
    
    n_alta <- ifelse(is.null(kpi), 0, kpi$marcas_alta_prioridad)
    total  <- ifelse(is.null(met), 0, nrow(met))
    perc   <- ifelse(total > 0, round(100 * n_alta / total, 1), 0)
    
    shinydashboard::valueBox(
      value = paste0(n_alta, " (", perc, "%)"),
      subtitle = "Marcas Alta Prioridad",
      icon = icon("exclamation-circle"),
      color = "red"
    )
  })
  
  output$vb_crecimiento_mercado <- renderValueBox({
    kpi <- kpis_react()
    crec <- ifelse(is.null(kpi), 0, kpi$crecimiento_promedio)
    
    shinydashboard::valueBox(
      value = paste0(round(crec, 1), " %"),
      subtitle = "Crecimiento Promedio del Parque",
      icon = icon("chart-line"),
      color = "green"
    )
  })
  
  output$vb_score_oportunidad <- renderValueBox({
    kpi <- kpis_react()
    score <- ifelse(is.null(kpi), 0, kpi$score_promedio)
    
    shinydashboard::valueBox(
      value = round(score, 1),
      subtitle = "Score Promedio de Oportunidad (0-100)",
      icon = icon("bullseye"),
      color = "orange"
    )
  })
  
  # ============================================================================
  # 5. TABLA DE MÉTRICAS Y CONCENTRACIÓN (TAB VISIÓN EJECUTIVA)
  # ============================================================================
  
  # Panel de alertas de oportunidades
  output$panel_alertas_oportunidades <- renderUI({
    alt <- alertas_react()
    met <- metricas_react()
    
    if(is.null(alt) || is.null(met)) {
      return(div(
        style = "padding: 15px; text-align: center; color: #6c757d;",
        icon("info-circle", class = "fa-2x"),
        br(), br(),
        "Cargando alertas..."
      ))
    }
    
    # Lista de alertas HTML
    alertas_html <- tagList()
    
    # Alertas de OPORTUNIDAD (marcas emergentes)
    if(!is.null(alt$oportunidad)) {
      alertas_html <- tagList(
        alertas_html,
        div(
          class = "alert alert-success",
          style = "margin: 10px 0; padding: 12px; border-left: 4px solid #10b981; background: linear-gradient(135deg, #d1fae5, #ecfdf5);",
          icon("chart-line", style = "color: #10b981; margin-right: 8px;"),
          strong("OPORTUNIDAD", style = "color: #065f46;"), br(),
          tags$span(style = "color: #065f46;", gsub("\\[OPORTUNIDAD\\] ", "", alt$oportunidad))
        )
      )
    }
    
    # Alertas de RIESGO (marcas en declive)
    if(!is.null(alt$riesgo)) {
      alertas_html <- tagList(
        alertas_html,
        div(
          class = "alert alert-danger",
          style = "margin: 10px 0; padding: 12px; border-left: 4px solid #ef4444; background: linear-gradient(135deg, #fee2e2, #fef2f2);",
          icon("exclamation-triangle", style = "color: #ef4444; margin-right: 8px;"),
          strong("RIESGO", style = "color: #7f1d1d;"), br(),
          tags$span(style = "color: #7f1d1d;", gsub("\\[RIESGO\\] ", "", alt$riesgo))
        )
      )
    }
    
    # Alertas de INFO (mercado estable)
    if(!is.null(alt$info)) {
      alertas_html <- tagList(
        alertas_html,
        div(
          class = "alert alert-info",
          style = "margin: 10px 0; padding: 12px; border-left: 4px solid #3b82f6; background: linear-gradient(135deg, #e0f2fe, #f0f9ff); color: #0c4a6e;",
          icon("info-circle", style = "color: #3b82f6; margin-right: 8px;"),
          strong("INFO", style = "color: #0c4a6e;"), br(),
          gsub("\\[INFO\\] ", "", alt$info)
        )
      )
    }
    
    # Agregar marcas de alta prioridad
    marcas_alta <- met %>%
      filter(str_detect(Potencial_Analytics, "ALTA PRIORIDAD")) %>%
      arrange(desc(Score_Oportunidad)) %>%
      head(5)
    
    if(nrow(marcas_alta) > 0) {
      alertas_html <- tagList(
        alertas_html,
        div(
          style = "margin: 15px 0; padding: 12px; background: #fff3cd; border-left: 4px solid #f59e0b; border-radius: 4px; color: #78350f;",
          icon("star", style = "color: #f59e0b; margin-right: 8px;"),
          strong("TOP PRIORIDAD", style = "color: #78350f;"), br(),
          tags$small(
            style = "color: #78350f;",
            paste(marcas_alta$Marca_Vehiculo, collapse = " • ")
          )
        )
        
        
      )
    }
    
    # Si no hay alertas
    if(length(alertas_html) == 0) {
      return(div(
        style = "padding: 20px; text-align: center; color: #6c757d;",
        icon("check-circle", class = "fa-3x", style = "color: #10b981;"),
        br(), br(),
        h5("Sin alertas activas"),
        p("El mercado está operando normalmente")
      ))
    }
    
    return(alertas_html)
  })
  
  output$tabla_metricas_crecimiento <- renderTable({
    met <- metricas_react()
    req(met)
    
    met %>%
      dplyr::select(
        Marca_Vehiculo,
        Volumen_Final,
        Crecimiento_Relativo,
        Categoria_Volumen,
        Categoria_Crecimiento,
        Potencial_Analytics,
        Score_Oportunidad
      ) %>%
      dplyr::arrange(desc(Volumen_Final))
  }, bordered = TRUE, striped = TRUE, spacing = "s")
  
  
  output$tabla_concentracion_mercado <- renderTable({
    met <- metricas_react()
    kpi <- kpis_react()
    req(met, kpi)
    
    # Top 5 marcas por concentración
    top_conc <- met %>%
      dplyr::arrange(desc(Participacion_Mercado)) %>%
      dplyr::select(
        Marca_Vehiculo,
        Participacion_Mercado
      ) %>%
      head(5) %>%
      dplyr::mutate(
        Marca_Vehiculo = paste0(row_number(), ". ", Marca_Vehiculo),
        Participacion_Mercado = paste0(round(Participacion_Mercado, 2), "%")
      )
    
    # Agregar fila separadora
    separador <- data.frame(
      Marca_Vehiculo = "--- Indicadores ---",
      Participacion_Mercado = ""
    )
    
    # Indicadores resumen
    resumen <- data.frame(
      Marca_Vehiculo = c(
        "Top 5 Concentración",
        "Índice Herfindahl",
        "Marcas Creciendo"
      ),
      Participacion_Mercado = c(
        paste0(round(kpi$top_5_concentracion, 1), "%"),
        round(kpi$indice_herfindahl, 2),
        kpi$marcas_creciendo
      )
    )
    
    # Combinar todo
    rbind(top_conc, separador, resumen)
    
  }, bordered = TRUE, striped = TRUE, spacing = "s", colnames = FALSE)
  
  
  
  # Estadísticas de uso del sistema (tab sistema)
  output$tabla_estadisticas_uso_ds <- renderTable({
    data.frame(
      Métrica = c(
        "Inicio de sesión",
        "Última actualización",
        "Marcas en análisis actual",
        "Total vehículos (análisis actual)"
      ),
      Valor = c(
        format(timestamp_app, "%d/%m/%Y %H:%M:%S"),
        format(ultima_actualizacion(), "%d/%m/%Y %H:%M:%S"),
        ifelse(is.null(metricas_react()), 0, nrow(metricas_react())),
        ifelse(is.null(kpis_react()), 0, round(kpis_react()$total_vehiculos, 0))
      )
    )
  }, bordered = TRUE, striped = TRUE, spacing = "s")
  
  
  # Tablas Top Performers
  output$tabla_top_volumen <- renderDataTable({
    met <- metricas_react()
    req(met)
    
    met %>%
      dplyr::arrange(desc(Volumen_Final)) %>%
      head(15) %>%
      dplyr::select(
        Marca = Marca_Vehiculo,
        Volumen = Volumen_Final,
        `Crec.%` = Crecimiento_Relativo,
        Prioridad = Potencial_Analytics,
        Score = Score_Oportunidad
      ) %>%
      dplyr::mutate(
        Volumen = format(Volumen, big.mark = ",", digits = 0),
        `Crec.%` = round(`Crec.%`, 1),
        Score = round(Score, 1)
      )
  }, options = list(
    pageLength = 10,
    dom = 't',
    ordering = FALSE,
    scrollY = "280px",
    scrollCollapse = TRUE
  ), rownames = FALSE)
  
  output$tabla_top_crecimiento <- renderDataTable({
    met <- metricas_react()
    req(met)
    
    met %>%
      dplyr::filter(Volumen_Final >= 1000) %>%
      dplyr::arrange(desc(Crecimiento_Relativo)) %>%
      head(15) %>%
      dplyr::select(
        Marca = Marca_Vehiculo,
        `Crec.%` = Crecimiento_Relativo,
        Volumen = Volumen_Final,
        Prioridad = Potencial_Analytics,
        Score = Score_Oportunidad
      ) %>%
      dplyr::mutate(
        Volumen = format(Volumen, big.mark = ",", digits = 0),
        `Crec.%` = round(`Crec.%`, 1),
        Score = round(Score, 1)
      )
  }, options = list(
    pageLength = 10,
    dom = 't',
    ordering = FALSE,
    scrollY = "280px",
    scrollCollapse = TRUE
  ), rownames = FALSE)
  
  output$tabla_top_score <- renderDataTable({
    met <- metricas_react()
    req(met)
    
    met %>%
      dplyr::arrange(desc(Score_Oportunidad)) %>%
      head(15) %>%
      dplyr::select(
        Marca = Marca_Vehiculo,
        Score = Score_Oportunidad,
        Volumen = Volumen_Final,
        `Crec.%` = Crecimiento_Relativo,
        Prioridad = Potencial_Analytics
      ) %>%
      dplyr::mutate(
        Volumen = format(Volumen, big.mark = ",", digits = 0),
        `Crec.%` = round(`Crec.%`, 1),
        Score = round(Score, 1)
      )
  }, options = list(
    pageLength = 10,
    dom = 't',
    ordering = FALSE,
    scrollY = "280px",
    scrollCollapse = TRUE
  ), rownames = FALSE)
  
  
  
  # ============================================================================
  # 6. GRÁFICOS PRINCIPALES (VISIÓN EJECUTIVA)
  # ============================================================================
  
  # 6.1 Mapa estratégico principal (Volumen vs Crecimiento)
  
  output$mapa_estrategico_principal <- renderPlotly({
    met <- metricas_react()
    req(met, nrow(met) > 0)
    
    # Definir colores especificos para cada categoria (evita duplicacion en leyenda)
    colores_potencial <- c(
      "\U0001F534 ALTA PRIORIDAD" = "#ef4444",    # Rojo
      "\U0001F7E0 MEDIA PRIORIDAD" = "#f59e0b",   # Naranja
      "\U0001F535 EMERGENTE" = "#3b82f6",         # Azul
      "\u26AB BAJA PRIORIDAD" = "#64748b"         # Gris
    )
    
    plot_ly(
      data = met,
      x = ~Volumen_Final,
      y = ~Crecimiento_Relativo,
      type = "scatter",
      mode = "markers",
      color = ~Potencial_Analytics,
      colors = colores_potencial,
      sizes = c(10, 40),
      size = ~Score_Oportunidad,
      text = ~paste0(
        "<b>", Marca_Vehiculo, "</b><br>",
        "Volumen final: ", format(Volumen_Final, big.mark = ","), "<br>",
        "Crecimiento: ", round(Crecimiento_Relativo, 1), "%<br>",
        "Score: ", round(Score_Oportunidad, 1)
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        xaxis = list(title = "Volumen Final de Vehiculos"),
        yaxis = list(title = "Crecimiento Relativo (%)"),
        legend = list(
          orientation = "h", 
          x = 0, 
          y = -0.2,
          title = list(text = "")
        ),
        showlegend = TRUE
      )
  })
  
  # 6.2 Distribución de categorías (volumen / potencial)
  output$grafico_distribucion_categorias <- renderPlotly({
    met <- metricas_react()
    req(met)
    
    dist_pot <- met %>%
      dplyr::count(Potencial_Analytics, name = "Conteo") %>%
      dplyr::arrange(desc(Conteo))
    
    plot_ly(
      data = dist_pot,
      x = ~Potencial_Analytics,
      y = ~Conteo,
      type = "bar",
      text = ~Conteo,
      textposition = "outside"
    ) %>%
      layout(
        xaxis = list(title = "Potencial Analytics"),
        yaxis = list(title = "Número de marcas")
      )
  })
  
  # 6.3 Matriz de potencial (crecimiento vs participación)
  output$grafico_matriz_potencial <- renderPlotly({
    met <- metricas_react()
    req(met)
    
    plot_ly(
      data = met,
      x = ~Participacion_Mercado,
      y = ~Crecimiento_Relativo,
      type = "scatter",
      mode = "markers",
      color = ~Potencial_Analytics,
      text = ~paste0(
        "<b>", Marca_Vehiculo, "</b><br>",
        "Participación: ", round(Participacion_Mercado, 2), "%<br>",
        "Crecimiento: ", round(Crecimiento_Relativo, 1), "%"
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        xaxis = list(title = "Participación de mercado (%)"),
        yaxis = list(title = "Crecimiento relativo (%)")
      )
  })
  
  # 6.4 Rankings (volumen / crecimiento / score)
  output$grafico_ranking_volumen <- renderPlotly({
    met <- metricas_react()
    req(met)
    crear_top_marcas(met, tipo = "volumen", n = 15)
  })
  
  output$grafico_ranking_crecimiento <- renderPlotly({
    met <- metricas_react()
    req(met)
    crear_top_marcas(met, tipo = "crecimiento", n = 15)
  })
  
  output$grafico_ranking_score <- renderPlotly({
    met <- metricas_react()
    req(met)
    
    if (nrow(met) == 0) {
      return(plot_ly() %>% layout(title = "Sin datos disponibles"))
    }
    
    datos_grafico <- met %>%
      dplyr::arrange(desc(Score_Oportunidad)) %>%
      head(15) %>%
      dplyr::mutate(
        Marca_Vehiculo = forcats::fct_reorder(Marca_Vehiculo, Score_Oportunidad),
        texto_label = round(Score_Oportunidad, 1)
      )
    
    plot_ly(
      data = datos_grafico,
      x = ~Score_Oportunidad,
      y = ~Marca_Vehiculo,
      type = "bar",
      orientation = "h",
      text = ~texto_label,
      textposition = "outside"
    ) %>%
      layout(
        title = "Top Marcas por Score de Oportunidad",
        xaxis = list(title = "Score de oportunidad (0-100)"),
        yaxis = list(title = "")
      )
  })
  
  # 6.5 Tendencias temporales globales
  output$grafico_tendencias_temporal <- renderPlotly({
    datos_long <- datos_long_react()
    req(datos_long)
    
    serie <- datos_long %>%
      dplyr::group_by(Fecha) %>%
      dplyr::summarise(
        Vehiculos_Registrados = sum(Vehiculos_Registrados, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::arrange(Fecha)
    
    plot_ly(
      data = serie,
      x = ~Fecha,
      y = ~Vehiculos_Registrados,
      type = "scatter",
      mode = "lines+markers"
    ) %>%
      layout(
        title = "Evolución temporal del parque vehicular (total)",
        xaxis = list(title = "Fecha"),
        yaxis = list(title = "Vehículos registrados")
      )
  })
  
  # 6.6 Evolución individual de una marca
  output$grafico_evolucion_individual <- renderPlotly({
    datos_long <- datos_long_react()
    req(datos_long, input$marca_detalle)
    
    df_marca <- datos_long %>%
      dplyr::filter(Marca_Vehiculo == input$marca_detalle) %>%
      dplyr::arrange(Fecha)
    
    req(nrow(df_marca) > 0)
    
    plot_ly(
      data = df_marca,
      x = ~Fecha,
      y = ~Vehiculos_Registrados,
      type = "scatter",
      mode = "lines+markers",
      name = input$marca_detalle
    ) %>%
      layout(
        title = paste("Evolución de", input$marca_detalle),
        xaxis = list(title = "Fecha"),
        yaxis = list(title = "Vehículos registrados")
      )
  })
  
  # 6.7 Comparación competencia (Top 10 por score)
  output$grafico_comparacion_competencia <- renderPlotly({
    met <- metricas_react()
    req(met)
    
    top_comp <- met %>%
      dplyr::arrange(desc(Score_Oportunidad)) %>%
      head(10)
    
    plot_ly(
      data = top_comp,
      x = ~Participacion_Mercado,
      y = ~Crecimiento_Relativo,
      type = "scatter",
      mode = "markers+text",
      text = ~Marca_Vehiculo,
      textposition = "top center",
      size = ~Volumen_Final,
      sizes = c(10, 40)
    ) %>%
      layout(
        title = "Comparación de competencia (Top 10 Score)",
        xaxis = list(title = "Participación de mercado (%)"),
        yaxis = list(title = "Crecimiento relativo (%)")
      )
  })
  
  # ============================================================================
  # 7. GRÁFICOS DE OPORTUNIDADES (TAB OPORTUNIDADES ESTRATÉGICAS)
  # ============================================================================
  
  # 7.1 Mapa de burbujas avanzado
  output$mapa_burbujas_avanzado_oport <- renderPlotly({
    met <- metricas_react()
    req(met)
    
    plot_ly(
      data = met,
      x = ~Volumen_Final,
      y = ~Score_Oportunidad,
      type = "scatter",
      mode = "markers",
      color = ~Potencial_Analytics,
      size = ~Crecimiento_Relativo,
      sizes = c(10, 40),
      text = ~paste0(
        "<b>", Marca_Vehiculo, "</b><br>",
        "Volumen: ", format(Volumen_Final, big.mark = ","), "<br>",
        "Crecimiento: ", round(Crecimiento_Relativo, 1), "%<br>",
        "Score: ", round(Score_Oportunidad, 1)
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        title = "Mapa de Oportunidades (Volumen vs Score)",
        xaxis = list(title = "Volumen final"),
        yaxis = list(title = "Score de oportunidad")
      )
  })
  
  # 7.2 Matriz de decisión 2x2 (simple)
  output$matriz_decision_2x2_oport <- renderPlotly({
    met <- metricas_react()
    req(met)
    
    plot_ly(
      data = met,
      x = ~Volumen_Final,
      y = ~Crecimiento_Relativo,
      type = "scatter",
      mode = "markers",
      color = ~Categoria_Volumen,
      symbol = ~Categoria_Crecimiento,
      text = ~Marca_Vehiculo,
      hoverinfo = "text"
    ) %>%
      layout(
        title = "Matriz de Decisión (Volumen vs Crecimiento)",
        xaxis = list(title = "Volumen final"),
        yaxis = list(title = "Crecimiento relativo (%)")
      )
  })
  
  # 7.3 Histograma de score de oportunidad
  output$hist_distribucion_score_oport <- renderPlotly({
    met <- metricas_react()
    req(met)
    
    plot_ly(
      data = met,
      x = ~Score_Oportunidad,
      type = "histogram",
      nbinsx = 20
    ) %>%
      layout(
        title = "Distribución del Score de Oportunidad",
        xaxis = list(title = "Score de oportunidad (0-100)"),
        yaxis = list(title = "Frecuencia")
      )
  })
  
  # 7.4 Curva de Lorenz aproximada (por volumen)
  output$curva_lorenz_oport <- renderPlotly({
    met <- metricas_react()
    req(met)
    
    df <- met %>%
      dplyr::arrange(Volumen_Final) %>%
      dplyr::mutate(
        Volumen_Total = sum(Volumen_Final, na.rm = TRUE),
        Volumen_Acum = cumsum(Volumen_Final),
        Pct_Marcas = dplyr::row_number() / n(),
        Pct_Volumen = Volumen_Acum / Volumen_Total
      )
    
    plot_ly(
      data = df,
      x = ~Pct_Marcas,
      y = ~Pct_Volumen,
      type = "scatter",
      mode = "lines+markers",
      name = "Curva de Lorenz"
    ) %>%
      add_trace(
        x = c(0, 1),
        y = c(0, 1),
        mode = "lines",
        name = "Línea de igualdad",
        line = list(dash = "dash")
      ) %>%
      layout(
        title = "Curva de Lorenz (Distribución de Volumen)",
        xaxis = list(title = "Proporción de marcas"),
        yaxis = list(title = "Proporción de volumen acumulado")
      )
  })
  
  # 7.5 Boxplot comparativo por categoría de volumen
  output$boxplot_comparativo_oport <- renderPlotly({
    met <- metricas_react()
    req(met)
    
    plot_ly(
      data = met,
      x = ~Categoria_Volumen,
      y = ~Score_Oportunidad,
      type = "box"
    ) %>%
      layout(
        title = "Distribución del Score por Categoría de Volumen",
        xaxis = list(title = "Categoría de volumen"),
        yaxis = list(title = "Score de oportunidad")
      )
  })
  
  # 7.6 Evolución de top oportunidades (simple: top 5)
  output$grafico_evolucion_top_oport <- renderPlotly({
    datos_long <- datos_long_react()
    met <- metricas_react()
    req(datos_long, met)
    
    top5 <- met %>%
      dplyr::arrange(desc(Score_Oportunidad)) %>%
      head(5) %>%
      dplyr::pull(Marca_Vehiculo)
    
    df_top <- datos_long %>%
      dplyr::filter(Marca_Vehiculo %in% top5) %>%
      dplyr::arrange(Fecha)
    
    req(nrow(df_top) > 0)
    
    plot_ly(
      data = df_top,
      x = ~Fecha,
      y = ~Vehiculos_Registrados,
      color = ~Marca_Vehiculo,
      type = "scatter",
      mode = "lines+markers"
    ) %>%
      layout(
        title = "Evolución temporal de las principales oportunidades",
        xaxis = list(title = "Fecha"),
        yaxis = list(title = "Vehículos registrados")
      )
  })
  
  # ============================================================================
  # 8. VALUEBOX DE OPORTUNIDADES
  # ============================================================================
  
  output$vb_marcas_filtradas_oport <- renderValueBox({
    met <- metricas_react()
    total <- ifelse(is.null(met), 0, nrow(met))
    
    shinydashboard::valueBox(
      value = total,
      subtitle = "Marcas en análisis de oportunidades",
      icon = icon("list-ul"),
      color = "purple"
    )
  })
  
  output$vb_volumen_oportunidad_oport <- renderValueBox({
    kpi <- kpis_react()
    vol <- ifelse(is.null(kpi), 0, kpi$volumen_oportunidad)
    
    shinydashboard::valueBox(
      value = format(round(vol, 0), big.mark = ","),
      subtitle = "Volumen en marcas con potencial",
      icon = icon("chart-bar"),
      color = "teal"
    )
  })
  
  output$vb_score_promedio_oport <- renderValueBox({
    kpi <- kpis_react()
    score <- ifelse(is.null(kpi), 0, kpi$score_promedio)
    
    shinydashboard::valueBox(
      value = round(score, 1),
      subtitle = "Score promedio (oportunidades)",
      icon = icon("star-half-alt"),
      color = "olive"
    )
  })
  
  output$vb_potencial_mercado_oport <- renderValueBox({
    kpi <- kpis_react()
    pot <- ifelse(is.null(kpi), 0, kpi$top_5_concentracion)
    
    shinydashboard::valueBox(
      value = paste0(round(pot, 1), " %"),
      subtitle = "Concentración Top 5 (proxy potencial)",
      icon = icon("percent"),
      color = "maroon"
    )
  })
  
  # ============================================================================
  # 9. INFO Y LOG DEL SISTEMA
  # ============================================================================
  
  output$info_sistema_ds <- renderPrint({
    cat("DS Conexion - Analisis Parque Vehicular Guatemala\n")
    cat("------------------------------------------------\n")
    cat("Version modulo: 1.0.0\n")
    cat("R version    :", R.version.string, "\n")
    cat("Plataforma   :", R.version$platform, "\n")
    cat("Sesion desde :", format(timestamp_app, "%d/%m/%Y %H:%M:%S"), "\n")
    cat("Ultima act.  :", format(ultima_actualizacion(), "%d/%m/%Y %H:%M:%S"), "\n")
  })
  
  output$log_sistema_ds <- renderPrint({
    met <- metricas_react()
    kpi <- kpis_react()
    alt <- alertas_react()
    
    cat("[LOG DS Conexion - Parque Vehicular]\n")
    cat("-------------------------------------\n")
    cat("Timestamp          :", format(Sys.time(), "%d/%m/%Y %H:%M:%S"), "\n")
    cat("Marcas analizadas  :", ifelse(is.null(met), 0, nrow(met)), "\n")
    if (!is.null(kpi)) {
      cat("Total vehiculos    :", round(kpi$total_vehiculos, 0), "\n")
      cat("Crec. promedio (%) :", round(kpi$crecimiento_promedio, 2), "\n")
      cat("Score promedio     :", round(kpi$score_promedio, 2), "\n")
      cat("Top5 concent. (%)  :", round(kpi$top_5_concentracion, 2), "\n")
    }
    cat("\n[Alertas Estrategicas]\n")
    if (!is.null(alt)) {
      if (!is.null(alt$oportunidad)) cat(alt$oportunidad, "\n")
      if (!is.null(alt$riesgo))      cat(alt$riesgo, "\n")
      if (!is.null(alt$info))        cat(alt$info, "\n")
    } else {
      cat("Sin alertas generadas.\n")
    }
  })
  
  # ============================================================================
  # 10. BOTÓN "ACTUALIZAR PARQUE VEHICULAR" (SIMULADO)
  # ============================================================================
  
  observeEvent(input$btn_actualizar_datos, {
    showNotification(
      "Actualizando datos del parque vehicular (simulado)...",
      type = "message",
      duration = 4
    )
    metricas_react(metricas_global)
    kpis_react(kpis_global)
    alertas_react(alertas_global)
    datos_long_react(datos_long_global)
    ultima_actualizacion(Sys.time())
  }, ignoreInit = TRUE)
  
  # ============================================================================
  # 11. LOG DE INICIO DEL SERVIDOR
  # ============================================================================
  
  cat("\n[OK] SERVIDOR DS Conexion INICIADO CORRECTAMENTE\n")
  cat("Dashboard ejecutivo y modulos de oportunidades cargados.\n")
  cat("=======================================================\n")
  
}
