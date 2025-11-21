# =============================================================================
# UI.R - DS CONEXIÓN: ANALISIS PARQUE VEHICULAR
# Sistema de Business Analytics & Predictive Modeling para Analytics y Sistemas de RefrigeraciÃ³n
# =============================================================================

# =============================================================================
# 1. ENCABEZADO DEL DASHBOARD 
# =============================================================================

encabezado <- dashboardHeader(
  title = "DS Conexión - AnÃ¡lisis Parque Vehicular",
  titleWidth = 450,
  dropdownMenuOutput("menu_notificaciones_ds")
)

# Logo corporativo DS_Conexion en el encabezado
encabezado$children[[2]]$children <- tags$a(
  href = 'https://dsconexion.com/',
  tags$img(
    src = 'https://dsconexion.com/wp-content/uploads/2020/07/logo-data-science-conexion.png',
    height = '45', 
    width = '200',
    style = "max-height: 45px; object-fit: contain; margin-top: 3px;"
  ),
  target = "_blank"
)

# =============================================================================
# 2. SIDEBAR CORPORATIVO DS_Conexion
# =============================================================================

lateral <- dashboardSidebar(
  width = 300,
  
  # CSS personalizado DS_Conexion
  includeCSS('www/styles.css'),
  
  # JavaScript para funcionalidades especÃ­ficas DS_Conexion
  tags$head(
    tags$script(HTML("
      // FunciÃ³n para actualizaciÃ³n de datos del parque vehicular
      Shiny.addCustomMessageHandler('actualizar_parque_vehicular', function(message) {
        $('#btn_actualizar_datos').addClass('loading');
        setTimeout(function() {
          $('#btn_actualizar_datos').removeClass('loading');
          toastr.success('Datos del parque vehicular actualizados', 'ActualizaciÃ³n Completa');
        }, 3000);
      });
      
      // FunciÃ³n para mostrar alertas de oportunidades
      Shiny.addCustomMessageHandler('mostrar_alerta_oportunidad', function(message) {
        if(message.tipo === 'alta_prioridad') {
          toastr.error(message.mensaje, 'Alta Prioridad');
        } else if(message.tipo === 'emergente') {
          toastr.info(message.mensaje, 'Marca Emergente');
        } else {
          toastr.success(message.mensaje, 'Oportunidad');
        }
      });
      
      // Auto-refresh para datos del parque vehicular
      setInterval(function() {
        if($('#auto_refresh_ds').prop('checked')) {
          Shiny.onInputChange('trigger_refresh_parque', Math.random());
        }
      }, 1800000); // 30 minutos
    "))
  ),
  
  # ENCABEZADO PERSONALIZADO DS_Conexion
  div(
    style = "
      text-align: center; 
      padding: 25px 15px; 
      background: linear-gradient(135deg, #1a365d 0%, #0891b2 100%);
      border-bottom: 4px solid #1e293b;
      margin-bottom: 15px;
      position: relative;
      overflow: hidden;
    ",
    
    # Efecto de fondo
    tags$div(
      style = "
        position: absolute;
        top: 0; left: 0; right: 0; bottom: 0;
        background: radial-gradient(circle at 30% 70%, rgba(255,255,255,0.1) 0%, transparent 50%);
        z-index: 1;
      "
    ),
    
    # Logo centrado
    tags$img(
      src = "https://dsconexion.com/wp-content/uploads/2020/07/logo-data-science-conexion.png",
      style = "
        height: 60px; 
        width: auto; 
        margin-bottom: 12px;
        filter: brightness(1.3) drop-shadow(0 2px 4px rgba(0,0,0,0.3));
        position: relative;
        z-index: 2;
      "
    ),
    
    # TÃ­tulo principal
    h3(
      "DS CONEXIÓN",
      style = "
        color: white; 
        margin: 10px 0 6px 0; 
        font-weight: 700;
        font-size: 18px;
        text-shadow: 2px 2px 4px rgba(0,0,0,0.5);
        letter-spacing: 1px;
        position: relative;
        z-index: 2;
      "
    ),
    
    # SubtÃ­tulo
    p(
      "Business Analytics & Predictive Modeling",
      style = "
        color: #f8f9fa; 
        margin: 0 0 4px 0; 
        font-size: 13px;
        font-weight: 500;
        position: relative;
        z-index: 2;
      "
    ),
    
    p(
      "Parque Vehicular Guatemala",
      style = "
        color: #e9ecef; 
        margin: 0; 
        font-size: 11px;
        font-style: italic;
        position: relative;
        z-index: 2;
      "
    )
  ),
  
  # MENÃš PRINCIPAL DE NAVEGACIÃ“N DS_Conexion
  sidebarMenu(
    id = "menu_principal_ds",
    
    menuItem(
      'Dashboard Ejecutivo',
      tabName = 'dashboard_ejecutivo_ds',
      icon = icon('tachometer-alt', lib = 'font-awesome'),
      badgeLabel = "Principal", 
      badgeColor = "red"
    ),
    
    # CORREGIDO: Eliminado tabName = 'analisis_marcas' que causaba el problema
    menuItem(
      'AnÃ¡lisis de Marcas',
      icon = icon('chart-bar', lib = 'font-awesome'),
      startExpanded = TRUE,
      menuSubItem('Panorama General', tabName = 'panorama_general'),
      menuSubItem('Rankings de Marcas', tabName = 'rankings_marcas'),
      menuSubItem('Tendencias Temporales', tabName = 'tendencias_temporales'),
      menuSubItem('AnÃ¡lisis Detallado', tabName = 'analisis_detallado')
    ),
    
    menuItem(
      'Oportunidades EstratÃ©gicas',
      icon = icon('bullseye', lib = 'font-awesome'),
      startExpanded = FALSE,
      menuSubItem('Mapa de Oportunidades', tabName = 'mapa_oportunidades')#,
      #menuSubItem('Marcas Emergentes', tabName = 'marcas_emergentes'),
      #menuSubItem('AnÃ¡lisis Competitivo', tabName = 'competitivo_marcas')
    ),
    
    # menuItem(
    #   'Proyecciones y Predicciones',
    #   icon = icon('crystal-ball', lib = 'font-awesome'),
    #   startExpanded = FALSE,
    #   menuSubItem('Modelos Predictivos', tabName = 'modelos_predictivos_ds'),
    #   menuSubItem('Escenarios Futuros', tabName = 'escenarios_futuros')
    # ),
    
    # menuItem(
    #   'Inteligencia de Mercado',
    #   icon = icon('brain', lib = 'font-awesome'),
    #   startExpanded = FALSE,
    #   menuSubItem('Alertas AutomÃ¡ticas', tabName = 'alertas_automaticas'),
    #   menuSubItem('SegmentaciÃ³n Avanzada', tabName = 'segmentacion_avanzada')
    # ),
    
    menuItem(
      'ConfiguraciÃ³n',
      tabName = 'configuracion_ds',
      icon = icon('cogs', lib = 'font-awesome')
    )
  ),
  
  # Panel de control de datos
  hr(style = "border-color: #495057; margin: 20px 0;"),
  div(
    style = "padding: 20px;",
    h5("ðŸ“Š Control de Datos", 
       style = "color: white; margin-bottom: 15px; font-weight: 600;"),
    
    actionButton(
      "btn_actualizar_datos",
      "ðŸ”„ Actualizar Parque Vehicular",
      icon = icon("sync-alt"),
      class = "btn-danger btn-block",
      style = "margin-bottom: 12px; font-weight: 500;"
    ),
    
    checkboxInput(
      "auto_refresh_ds",
      "Auto-actualizaciÃ³n (30 min)",
      value = FALSE
    ),
    
    div(
      style = "margin-top: 10px;",
      textOutput("ultima_actualizacion_ds"),
      tags$style("#ultima_actualizacion_ds {color: #e9ecef; font-size: 11px;}")
    )
  ),
  
  # Indicadores de estado especÃ­ficos DS_Conexion
  hr(style = "border-color: #495057; margin: 20px 0;"),
  div(
    style = "padding: 20px;",
    h5("ðŸŽ¯ Estado del Sistema", 
       style = "color: white; margin-bottom: 15px; font-weight: 600;"),
    
    div(
      style = "display: flex; justify-content: space-between; margin-bottom: 8px;",
      span("Datos SAT:", style = "color: #ced4da; font-size: 12px;"),
      span(id = "status_sat", "â—", style = "color: #10b981; font-size: 16px;")
    ),
    
    div(
      style = "display: flex; justify-content: space-between; margin-bottom: 8px;",
      span("AnÃ¡lisis ML:", style = "color: #ced4da; font-size: 12px;"),
      span(id = "status_ml", "â—", style = "color: #10b981; font-size: 16px;")
    ),
    
    div(
      style = "display: flex; justify-content: space-between; margin-bottom: 8px;",
      span("Alertas:", style = "color: #ced4da; font-size: 12px;"),
      span(id = "status_alertas", "â—", style = "color: #f59e0b; font-size: 16px;")
    )
  ),
  
  # InformaciÃ³n del sistema al final
  div(
    style = "
      position: absolute;
      bottom: 0;
      width: calc(100% - 30px);
      padding: 20px 15px;
      background: linear-gradient(to top, rgba(44, 62, 80, 0.4), transparent);
      border-top: 1px solid #495057;
    ",
    p("Ãšltima actualizaciÃ³n:", 
      style = "color: #ced4da; font-size: 11px; margin: 0;"),
    p(textOutput("timestamp_sistema_ds", inline = TRUE), 
      style = "color: #f8f9fa; font-size: 10px; margin: 0;"),
    br(),
    p("DS. William V. Paredes P. | DS Conexión", 
      style = "color: #adb5bd; font-size: 10px; margin: 0; font-style: italic;")
  )
)

# =============================================================================
# 3. CUERPO PRINCIPAL DEL DASHBOARD
# =============================================================================

cuerpo <- dashboardBody(
  
  # Usar waitress para loading screens
  useWaitress(),
  
  # CSS adicional personalizado DS_Conexion
  tags$head(
    tags$style(HTML("
      .content-wrapper, .right-side {
        background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
      }
      
      .main-header .navbar {
        background: linear-gradient(135deg, #1a365d 0%, #0891b2 100%) !important;
        border-bottom: 3px solid #1e293b;
      }
      
      .main-header .logo {
        background: linear-gradient(135deg, #0891b2 0%, #1a365d 100%) !important;
      }
      
      .skin-red .main-sidebar {
        background: linear-gradient(180deg, #1e293b 0%, #34495e 100%);
      }
      
      .box.box-primary {
        border-top-color: #1a365d;
        box-shadow: 0 4px 12px rgba(196, 30, 58, 0.15);
      }
      
      .box.box-danger {
        border-top-color: #06b6d4;
        box-shadow: 0 4px 12px rgba(231, 76, 60, 0.15);
      }
      
      .box.box-warning {
        border-top-color: #f59e0b;
        box-shadow: 0 4px 12px rgba(243, 156, 18, 0.15);
      }
      
      .box.box-success {
        border-top-color: #10b981;
        box-shadow: 0 4px 12px rgba(39, 174, 96, 0.15);
      }
      
      .btn-primary {
        background: linear-gradient(135deg, #1a365d 0%, #0891b2 100%);
        border-color: #0891b2;
        box-shadow: 0 2px 8px rgba(196, 30, 58, 0.3);
      }
      
      .btn-primary:hover {
        background: linear-gradient(135deg, #0891b2 0%, #1a365d 100%);
        border-color: #6d1129;
        transform: translateY(-1px);
        box-shadow: 0 4px 12px rgba(196, 30, 58, 0.4);
      }
      
      .btn-danger {
        background: linear-gradient(135deg, #06b6d4 0%, #c0392b 100%);
        border-color: #c0392b;
      }
      
      .btn-danger:hover {
        background: linear-gradient(135deg, #c0392b 0%, #06b6d4 100%);
        transform: translateY(-1px);
      }
      
      /* Estilos para value boxes DS_Conexion */
      .small-box {
        border-radius: 12px;
        box-shadow: 0 6px 20px rgba(0,0,0,0.15);
        border: none;
        overflow: hidden;
        position: relative;
      }
      
      .small-box::before {
        content: '';
        position: absolute;
        top: 0;
        left: 0;
        right: 0;
        height: 4px;
        background: linear-gradient(90deg, #1a365d, #1e293b);
        z-index: 1;
      }
      
      .small-box h3 {
        font-size: 32px;
        font-weight: 700;
        text-shadow: 1px 1px 2px rgba(0,0,0,0.1);
      }
      
      .small-box p {
        font-size: 14px;
        font-weight: 500;
      }
      
      .small-box .icon {
        font-size: 80px;
        opacity: 0.15;
      }
      
      /* Estilos para grÃ¡ficos */
      .plotly-container {
        height: 100% !important;
        border-radius: 8px;
      }
      
      /* Estilos especÃ­ficos para tablas DS_Conexion */
      .dataTables_wrapper {
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
      }
      
      .table-striped > tbody > tr:nth-child(odd) {
        background-color: rgba(248, 249, 250, 0.7);
      }
      
      /* Loading animations mejoradas */
      .loading {
        position: relative;
        pointer-events: none;
        opacity: 0.7;
      }
      
      .loading::after {
        content: '';
        position: absolute;
        top: 50%;
        left: 50%;
        width: 24px;
        height: 24px;
        margin: -12px 0 0 -12px;
        border: 3px solid #f3f3f3;
        border-top: 3px solid #1a365d;
        border-radius: 50%;
        animation: spin 1.2s linear infinite;
        z-index: 1000;
      }
      
      @keyframes spin {
        0% { transform: rotate(0deg); }
        100% { transform: rotate(360deg); }
      }
      
      /* Efectos hover para boxes */
      .box:hover {
        transform: translateY(-2px);
        transition: all 0.3s ease;
        box-shadow: 0 8px 25px rgba(0,0,0,0.15);
      }
    "))
  ),
  
  # Contenido de las pestaÃ±as
  tabItems(
    
    # =============================================================================
    # TAB 1: DASHBOARD EJECUTIVO PRINCIPAL DS_Conexion
    # =============================================================================
    tabItem(
      tabName = "dashboard_ejecutivo_ds",
      
      # Encabezado ejecutivo DS_Conexion
      fluidRow(
        box(
          title = "ðŸŽ¯ Dashboard Ejecutivo - DS Conexión",
          status = "danger",
          solidHeader = TRUE,
          width = 12,
          height = 130,
          
          div(
            style = "display: flex; justify-content: space-between; align-items: center; padding: 10px 0;",
            div(
              h4("Sistema de Business Analytics & Predictive Modeling para Analytics", 
                 style = "margin: 0; color: #1a365d; font-weight: 600;"),
              p("Parque Vehicular Guatemala â€¢ Machine Learning â€¢ AnÃ¡lisis Predictivo â€¢ SAT Data", 
                style = "margin: 8px 0 0 0; color: #6c757d; font-size: 14px; font-weight: 500;")
            ),
            div(
              style = "text-align: right;",
              h5("ðŸ“ˆ LIVE DATA", 
                 style = "margin: 0; color: #06b6d4; font-weight: 600;"),
              p(textOutput("fecha_actual_ds"), 
                style = "margin: 2px 0 0 0; color: #6c757d; font-size: 12px;")
            )
          )
        )
      ),
      
      # KPIs principales DS_Conexion
      fluidRow(
        valueBoxOutput("vb_total_vehiculos", width = 3),
        valueBoxOutput("vb_marcas_alta_prioridad", width = 3),
        valueBoxOutput("vb_crecimiento_mercado", width = 3),
        valueBoxOutput("vb_score_oportunidad", width = 3)
      ),
      
      # GrÃ¡ficos principales
      fluidRow(
        box(
          title = "ðŸ—ºï¸ Mapa EstratÃ©gico de Oportunidades",
          status = "danger",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 9,
          height = 520,
          footer = HTML(paste0(
            "<b>InterpretaciÃ³n:</b> ",
            "ðŸ”´ Alta Prioridad: >50K vehÃ­culos + >10% crecimiento | ",
            "ðŸŸ  Media Prioridad: >10K vehÃ­culos + >15% crecimiento | ",
            "ðŸ”µ Emergente: >30% crecimiento + >1K vehÃ­culos | ",
            "âš« Baja Prioridad: Resto de marcas | ",
            "Fuente: Superintendencia de AdministraciÃ³n Tributaria (SAT)"
          )),
          
          div(
            style = "height: 440px;",
            plotlyOutput("mapa_estrategico_principal", height = "100%")
          )
        ),
        
        box(
          title = "ðŸš¨ Alertas de Oportunidades",
          status = "warning",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 3,
          height = 520,
          
          div(
            style = "max-height: 440px; overflow-y: auto; padding: 10px;",
            uiOutput("panel_alertas_oportunidades")
          )
        )
      ),
      
      # AnÃ¡lisis detallado
      fluidRow(
        box(
          title = "ðŸ“Š Resumen Ejecutivo del Mercado",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 8,
          
          div(
            style = "padding: 15px;",
            
            fluidRow(
              column(6,
                     h5("ðŸ“ˆ MÃ©tricas de Crecimiento", 
                        style = "color: #1a365d; margin-bottom: 15px;"),
                     tableOutput("tabla_metricas_crecimiento")
              ),
              column(6,
                     h5("ðŸŽ¯ ConcentraciÃ³n del Mercado", 
                        style = "color: #1a365d; margin-bottom: 15px;"),
                     tableOutput("tabla_concentracion_mercado")
              )
            )
          )
        ),
        
        box(
          title = "ðŸ† Top Performers",
          status = "success",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 4,
          
          tabBox(
            width = 12,
            height = "400px",
            tabPanel("ðŸ”¥ Por Volumen", 
                     div(style = "max-height: 320px; overflow-y: auto;",
                         dataTableOutput("tabla_top_volumen"))),
            tabPanel("ðŸ“ˆ Por Crecimiento", 
                     div(style = "max-height: 320px; overflow-y: auto;",
                         dataTableOutput("tabla_top_crecimiento"))),
            tabPanel("â­ Por Score", 
                     div(style = "max-height: 320px; overflow-y: auto;",
                         dataTableOutput("tabla_top_score")))
          )
        )
      )
    ),
    
    # =============================================================================
    # TAB 2: PANORAMA GENERAL
    # =============================================================================
    tabItem(
      tabName = "panorama_general",
      
      fluidRow(
        box(
          title = "âš™ï¸ ConfiguraciÃ³n de AnÃ¡lisis",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 12,
          
          fluidRow(
            column(3,
                   selectInput(
                     "filtro_categoria_volumen",
                     "CategorÃ­a por Volumen:",
                     choices = list(
                       "Todas las CategorÃ­as" = "todas",
                       "Alto Volumen (100K+)" = "alto",
                       "Volumen Medio (10K-100K)" = "medio",
                       "Volumen Bajo (1K-10K)" = "bajo",
                       "Volumen MÃ­nimo (<1K)" = "minimo"
                     ),
                     selected = "todas"
                   )
            ),
            column(3,
                   selectInput(
                     "filtro_potencial",
                     "Filtrar por Potencial:",
                     choices = list(
                       "Todos los Potenciales" = "todos",
                       "ðŸ”´ Alta Prioridad" = "alta",
                       "ðŸŸ  Media Prioridad" = "media", 
                       "ðŸ”µ Emergente" = "emergente",
                       "âš« Baja Prioridad" = "baja"
                     ),
                     selected = "todos"
                   )
            ),
            column(3,
                   numericInput(
                     "min_vehiculos",
                     "VehÃ­culos MÃ­nimos:",
                     value = 0,
                     min = 0,
                     step = 1000
                   )
            ),
            column(3,
                   br(),
                   actionButton(
                     "btn_aplicar_filtros",
                     "ðŸ” Aplicar Filtros",
                     class = "btn-primary btn-block"
                   )
            )
          )
        )
      ),
      
      # Visualizaciones del panorama general
      fluidRow(
        box(
          title = "ðŸ“Š DistribuciÃ³n del Parque Vehicular por CategorÃ­a",
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 6,
          height = 480,
          
          div(
            style = "height: 400px;",
            plotlyOutput("grafico_distribucion_categorias", height = "100%")
          )
        ),
        
        box(
          title = "ðŸŽ¯ Matriz de Potencial de Analytics",
          status = "warning",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 6,
          height = 480,
          
          div(
            style = "height: 400px;",
            plotlyOutput("grafico_matriz_potencial", height = "100%")
          )
        )
      ),
      
      # Tabla resumen panorama
      fluidRow(
        box(
          title = "ðŸ“‹ Vista PanorÃ¡mica Completa",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 12,
          
          div(
            style = "max-height: 500px; overflow-y: auto;",
            dataTableOutput("tabla_panorama_completo")
          )
        )
      )
    ),
    
    # =============================================================================
    # TAB 3: RANKINGS DE MARCAS
    # =============================================================================
    tabItem(
      tabName = "rankings_marcas",
      
      fluidRow(
        box(
          title = "ðŸ† Top 15 Marcas por Volumen de VehÃ­culos",
          status = "success",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 6,
          height = 550,
          footer = "Ranking basado en el volumen total de vehÃ­culos registrados al Ãºltimo perÃ­odo disponible",
          
          div(
            style = "height: 470px;",
            plotlyOutput("grafico_ranking_volumen", height = "100%")
          )
        ),
        
        box(
          title = "ðŸ“ˆ Top 15 Marcas por Crecimiento",
          status = "warning",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 6,
          height = 550,
          footer = "Ranking basado en el crecimiento relativo entre primer y Ãºltimo perÃ­odo registrado",
          
          div(
            style = "height: 470px;",
            plotlyOutput("grafico_ranking_crecimiento", height = "100%")
          )
        )
      ),
      
      fluidRow(
        box(
          title = "â­ Top 15 Marcas por Score de Oportunidad",
          status = "danger",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 12,
          height = 550,
          footer = "Score integral que combina volumen, crecimiento y estabilidad (0-100 puntos)",
          
          div(
            style = "height: 470px;",
            plotlyOutput("grafico_ranking_score", height = "100%")
          )
        )
      ),
      
      # Tabla comparativa de rankings
      fluidRow(
        box(
          title = "ðŸ“Š Ranking Comparativo Completo",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 12,
          
          div(
            style = "max-height: 500px; overflow-y: auto;",
            dataTableOutput("tabla_ranking_comparativo")
          )
        )
      )
    ),
    
    # =============================================================================
    # TAB 4: TENDENCIAS TEMPORALES
    # =============================================================================
    tabItem(
      tabName = "tendencias_temporales",
      
      # ConfiguraciÃ³n de tendencias
      fluidRow(
        box(
          title = "ðŸ“… ConfiguraciÃ³n de AnÃ¡lisis Temporal",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 12,
          
          fluidRow(
            column(4,
                   selectInput(
                     "marcas_seleccionadas",
                     "Seleccionar Marcas:",
                     choices = NULL, # Se llenarÃ¡ dinÃ¡micamente
                     selected = NULL,
                     multiple = TRUE
                   )
            ),
            column(4,
                   selectInput(
                     "tipo_visualizacion",
                     "Tipo de VisualizaciÃ³n:",
                     choices = list(
                       "LÃ­neas de Tiempo" = "lineas",
                       "Ãrea Apilada" = "area",
                       "Barras por PerÃ­odo" = "barras"
                     ),
                     selected = "lineas"
                   )
            ),
            column(4,
                   checkboxInput(
                     "mostrar_tendencia",
                     "Mostrar LÃ­nea de Tendencia",
                     value = TRUE
                   )
            )
          )
        )
      ),
      
      # GrÃ¡fico principal de tendencias
      fluidRow(
        box(
          title = "ðŸ“ˆ EvoluciÃ³n Temporal del Parque Vehicular",
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 12,
          height = 600,
          footer = "EvoluciÃ³n mensual de las marcas seleccionadas â€¢ Datos: Enero 2024 - Agosto 2025",
          
          div(
            style = "height: 520px;",
            plotlyOutput("grafico_tendencias_temporal", height = "100%")
          )
        )
      ),
      
      # AnÃ¡lisis estadÃ­stico de tendencias
      fluidRow(
        box(
          title = "ðŸ“Š AnÃ¡lisis EstadÃ­stico de Tendencias",
          status = "success",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 8,
          
          div(
            style = "max-height: 400px; overflow-y: auto;",
            dataTableOutput("tabla_estadisticas_tendencias")
          )
        ),
        
        box(
          title = "ðŸŽ¯ MÃ©tricas de Tendencia",
          status = "warning",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 4,
          
          div(
            style = "padding: 15px;",
            uiOutput("panel_metricas_tendencia")
          )
        )
      )
    ),
    
    # =============================================================================
    # TAB 5: ANÃLISIS DETALLADO
    # =============================================================================
    tabItem(
      tabName = "analisis_detallado",
      
      # Selector de marca para anÃ¡lisis detallado
      fluidRow(
        box(
          title = "ðŸ” SelecciÃ³n para AnÃ¡lisis Detallado",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 12,
          
          fluidRow(
            column(6,
                   selectInput(
                     "marca_detalle",
                     "Seleccionar Marca para AnÃ¡lisis:",
                     choices = NULL, # Se llenarÃ¡ dinÃ¡micamente
                     selected = NULL,
                     width = "100%"
                   )
            ),
            column(6,
                   br(),
                   actionButton(
                     "btn_generar_detalle",
                     "ðŸ“Š Generar AnÃ¡lisis Detallado",
                     class = "btn-primary btn-block"
                   )
            )
          )
        )
      ),
      
      # InformaciÃ³n detallada de la marca seleccionada
      fluidRow(
        # Panel de mÃ©tricas clave
        box(
          title = "ðŸ“ˆ MÃ©tricas Clave de la Marca",
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 4,
          height = 450,
          
          div(
            style = "padding: 15px;",
            uiOutput("panel_metricas_marca")
          )
        ),
        
        # GrÃ¡fico de evoluciÃ³n individual
        box(
          title = "ðŸ“Š EvoluciÃ³n Individual",
          status = "success",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 8,
          height = 450,
          
          div(
            style = "height: 370px;",
            plotlyOutput("grafico_evolucion_individual", height = "100%")
          )
        )
      ),
      
      # AnÃ¡lisis comparativo con competencia
      fluidRow(
        box(
          title = "ðŸ†š ComparaciÃ³n con Competencia Directa",
          status = "warning",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 12,
          height = 500,
          footer = "ComparaciÃ³n con las 5 marcas mÃ¡s similares en volumen y caracterÃ­sticas",
          
          div(
            style = "height: 420px;",
            plotlyOutput("grafico_comparacion_competencia", height = "100%")
          )
        )
      ),
      
      # Tabla de anÃ¡lisis detallado
      fluidRow(
        box(
          title = "ðŸ“‹ Reporte Detallado de la Marca",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 12,
          
          div(
            style = "max-height: 400px; overflow-y: auto;",
            dataTableOutput("tabla_reporte_detallado")
          )
        )
      )
    ),
    
    # =============================================================================
    # TAB: MAPA DE OPORTUNIDADES ESTRATÃ‰GICAS
    # =============================================================================
    tabItem(
      tabName = "mapa_oportunidades",
      
      # Encabezado estratÃ©gico
      fluidRow(
        box(
          title = "ðŸ—ºï¸ MAPA ESTRATÃ‰GICO DE OPORTUNIDADES - NIVEL EJECUTIVO",
          status = "danger",
          solidHeader = TRUE,
          width = 12,
          height = 140,
          
          div(
            style = "display: flex; justify-content: space-between; align-items: center; padding: 10px 0;",
            div(
              h4("Sistema de IdentificaciÃ³n y PriorizaciÃ³n de Oportunidades", 
                 style = "margin: 0; color: #1a365d; font-weight: 600;"),
              p("AnÃ¡lisis Multidimensional â€¢ SegmentaciÃ³n Inteligente â€¢ Recomendaciones Accionables", 
                style = "margin: 8px 0 0 0; color: #6c757d; font-size: 14px; font-weight: 500;")
            ),
            div(
              style = "text-align: right;",
              h5("ðŸŽ¯ STRATEGIC VIEW", 
                 style = "margin: 0; color: #06b6d4; font-weight: 600;"),
              p("Nivel: Gerencia General", 
                style = "margin: 2px 0 0 0; color: #6c757d; font-size: 12px;")
            )
          )
        )
      ),
      
      # Panel de control avanzado
      fluidRow(
        box(
          title = "âš™ï¸ CENTRO DE CONTROL ESTRATÃ‰GICO",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = FALSE,
          width = 12,
          
          fluidRow(
            # Columna 1: Filtros de Volumen y Crecimiento
            column(3,
                   h5("ðŸ“Š DimensiÃ³n: Volumen", style = "color: #1e293b; margin-bottom: 10px;"),
                   sliderInput(
                     "filtro_volumen_min_oport",
                     "Volumen MÃ­nimo:",
                     min = 0,
                     max = 500000,
                     value = 1000,
                     step = 1000,
                     post = " veh."
                   ),
                   sliderInput(
                     "filtro_volumen_max_oport",
                     "Volumen MÃ¡ximo:",
                     min = 1000,
                     max = 1500000,
                     value = 500000,
                     step = 1000,
                     post = " veh."
                   )
            ),
            
            # Columna 2: Filtros de Crecimiento
            column(3,
                   h5("ðŸ“ˆ DimensiÃ³n: Crecimiento", style = "color: #1e293b; margin-bottom: 10px;"),
                   sliderInput(
                     "filtro_crecimiento_min_oport",
                     "Crecimiento MÃ­nimo:",
                     min = -50,
                     max = 200,
                     value = 0,
                     step = 5,
                     post = "%"
                   ),
                   sliderInput(
                     "filtro_crecimiento_max_oport",
                     "Crecimiento MÃ¡ximo:",
                     min = -50,
                     max = 200,
                     value = 200,
                     step = 5,
                     post = "%"
                   )
            ),
            
            # Columna 3: Filtros de Score y CategorÃ­as
            column(3,
                   h5("â­ DimensiÃ³n: Score", style = "color: #1e293b; margin-bottom: 10px;"),
                   sliderInput(
                     "filtro_score_min_oport",
                     "Score MÃ­nimo:",
                     min = 0,
                     max = 100,
                     value = 40,
                     step = 5,
                     post = " pts"
                   ),
                   selectInput(
                     "filtro_categorias_oport",
                     "CategorÃ­as:",
                     choices = list(
                       "Todas" = "todas",
                       "Alto Volumen" = "alto",
                       "Volumen Medio" = "medio",
                       "Volumen Bajo + Emergente" = "bajo_emergente"
                     ),
                     selected = "todas"
                   )
            ),
            
            # Columna 4: Filtros de Potencial y Acciones
            column(3,
                   h5("ðŸŽ¯ DimensiÃ³n: Potencial", style = "color: #1e293b; margin-bottom: 10px;"),
                   checkboxGroupInput(
                     "filtro_potencial_oport",
                     "Niveles de Prioridad:",
                     choices = list(
                       "ðŸ”´ Alta Prioridad" = "alta",
                       "ðŸŸ  Media Prioridad" = "media",
                       "ðŸ”µ Emergente" = "emergente",
                       "âš« Baja Prioridad" = "baja"
                     ),
                     selected = c("alta", "media", "emergente")
                   ),
                   br(),
                   actionButton(
                     "btn_aplicar_filtros_oport",
                     "ðŸ” Aplicar Filtros",
                     class = "btn-danger btn-block",
                     style = "font-weight: 600;"
                   ),
                   actionButton(
                     "btn_resetear_filtros_oport",
                     "â†º Resetear",
                     class = "btn-default btn-block",
                     style = "margin-top: 5px;"
                   )
            )
          )
        )
      ),
      
      # KPIs DinÃ¡micos del Filtrado
      fluidRow(
        valueBoxOutput("vb_marcas_filtradas_oport", width = 3),
        valueBoxOutput("vb_volumen_oportunidad_oport", width = 3),
        valueBoxOutput("vb_score_promedio_oport", width = 3),
        valueBoxOutput("vb_potencial_mercado_oport", width = 3)
      ),
      
      # Mapa estratÃ©gico principal + matriz de decisiÃ³n
      fluidRow(
        # Mapa de burbujas avanzado
        box(
          title = "ðŸ—ºï¸ MAPA ESTRATÃ‰GICO INTERACTIVO: Volumen vs Crecimiento vs Score",
          status = "danger",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 8,
          height = 600,
          footer = HTML(paste0(
            "<b>Eje X:</b> Volumen de VehÃ­culos (escala logarÃ­tmica) | ",
            "<b>Eje Y:</b> Crecimiento Relativo (%) | ",
            "<b>TamaÃ±o burbuja:</b> Score de Oportunidad | ",
            "<b>Color:</b> Nivel de Prioridad"
          )),
          
          div(
            style = "height: 510px;",
            plotlyOutput("mapa_burbujas_avanzado_oport", height = "100%")
          )
        ),
        
        # Matriz de decisiÃ³n estratÃ©gica
        box(
          title = "ðŸ“Š MATRIZ DE DECISIÃ“N 2x2",
          status = "warning",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 4,
          height = 600,
          
          div(
            style = "height: 510px;",
            plotlyOutput("matriz_decision_2x2_oport", height = "100%")
          )
        )
      ),
      
      # AnÃ¡lisis por cuadrantes + distribuciones
      fluidRow(
        # Tabla de cuadrantes estratÃ©gicos
        box(
          title = "ðŸŽ¯ SEGMENTACIÃ“N POR CUADRANTES ESTRATÃ‰GICOS",
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 6,
          
          tabBox(
            width = 12,
            tabPanel("â­ Estrellas (Alto Vol + Alto Crec)", 
                     div(style = "max-height: 320px; overflow-y: auto;",
                         dataTableOutput("tabla_cuadrante_estrellas_oport"))),
            tabPanel("ðŸš€ Promesas (Bajo Vol + Alto Crec)", 
                     div(style = "max-height: 320px; overflow-y: auto;",
                         dataTableOutput("tabla_cuadrante_promesas_oport"))),
            tabPanel("ðŸ’° Base Consolidada (Alto Vol + Bajo Crec)", 
                     div(style = "max-height: 320px; overflow-y: auto;",
                         dataTableOutput("tabla_cuadrante_base_consolidada_oport"))),
            tabPanel("â“ Interrogantes (Bajo Vol + Bajo Crec)", 
                     div(style = "max-height: 320px; overflow-y: auto;",
                         dataTableOutput("tabla_cuadrante_interrogantes_oport")))
          )
        ),
        
        # Distribuciones y concentraciÃ³n
        box(
          title = "ðŸ“ˆ ANÃLISIS DE DISTRIBUCIÃ“N",
          status = "success",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 6,
          
          tabBox(
            width = 12,
            tabPanel("Score Distribution", 
                     plotlyOutput("hist_distribucion_score_oport", height = "300px")),
            tabPanel("Curva de Lorenz", 
                     plotlyOutput("curva_lorenz_oport", height = "300px")),
            tabPanel("Box Plot Comparativo", 
                     plotlyOutput("boxplot_comparativo_oport", height = "300px"))
          )
        )
      ),
      
      # AnÃ¡lisis temporal de oportunidades
      fluidRow(
        box(
          title = "ðŸ“… EVOLUCIÃ“N TEMPORAL DE OPORTUNIDADES CLAVE",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 12,
          height = 500,
          
          fluidRow(
            column(3,
                   selectInput(
                     "top_n_temporal_oport",
                     "Mostrar Top:",
                     choices = list("5 marcas" = 5, "10 marcas" = 10, "15 marcas" = 15, "20 marcas" = 20),
                     selected = 10
                   )
            ),
            column(3,
                   selectInput(
                     "criterio_temporal_oport",
                     "Criterio de SelecciÃ³n:",
                     choices = list(
                       "Por Score" = "score",
                       "Por Volumen" = "volumen",
                       "Por Crecimiento" = "crecimiento"
                     ),
                     selected = "score"
                   )
            ),
            column(3,
                   checkboxInput(
                     "normalizar_temporal_oport",
                     "Normalizar valores (0-100)",
                     value = FALSE
                   )
            ),
            column(3,
                   br(),
                   actionButton(
                     "btn_actualizar_temporal_oport",
                     "ðŸ”„ Actualizar",
                     class = "btn-primary btn-block"
                   )
            )
          ),
          
          div(
            style = "height: 370px; margin-top: 15px;",
            plotlyOutput("grafico_evolucion_top_oport", height = "100%")
          )
        )
      ),
      
      # Panel de recomendaciones inteligentes
      fluidRow(
        box(
          title = "ðŸŽ¯ RECOMENDACIONES ESTRATÃ‰GICAS AUTOMÃTICAS",
          status = "warning",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 8,
          
          div(
            style = "padding: 20px;",
            uiOutput("panel_recomendaciones_inteligentes_oport")
          )
        ),
        
        # MÃ©tricas de concentraciÃ³n y riesgo
        box(
          title = "âš ï¸ ANÃLISIS DE RIESGO Y CONCENTRACIÃ“N",
          status = "danger",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 4,
          
          div(
            style = "padding: 15px;",
            uiOutput("panel_metricas_riesgo_oport")
          )
        )
      ),
      
      # Tabla maestra de oportunidades
      fluidRow(
        box(
          title = "ðŸ“‹ TABLA MAESTRA DE OPORTUNIDADES - EXPORTABLE",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 12,
          
          div(
            style = "margin-bottom: 15px;",
            downloadButton("btn_exportar_oportunidades_excel", "ðŸ“Š Exportar a Excel", class = "btn-success"),
            downloadButton("btn_exportar_oportunidades_csv", "ðŸ“„ Exportar a CSV", class = "btn-info", style = "margin-left: 10px;")
          ),
          
          div(
            style = "max-height: 500px; overflow-y: auto;",
            dataTableOutput("tabla_maestra_oportunidades_oport")
          )
        )
      )
    ),
    
    
    # =============================================================================
    # TAB 6: CONFIGURACIÃ“N
    # =============================================================================
    tabItem(
      tabName = "configuracion_ds",
      
      fluidRow(
        box(
          title = "âš™ï¸ ConfiguraciÃ³n del Sistema DS_Conexion",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 6,
          
          h4("ðŸ“ GestiÃ³n de Datos"),
          selectInput(
            "fuente_datos",
            "Fuente de Datos:",
            choices = list(
              "Archivo RDS (Recomendado)" = "rds",
              "Archivo CSV" = "csv",
              "Datos de Ejemplo" = "ejemplo"
            ),
            selected = "rds"
          ),
          
          br(),
          actionButton("test_carga_datos", "ðŸ§ª Probar Carga de Datos", class = "btn-info"),
          
          hr(),
          
          h4("ðŸ”„ ActualizaciÃ³n AutomÃ¡tica"),
          selectInput(
            "frecuencia_actualizacion_ds",
            "Frecuencia de ActualizaciÃ³n:",
            choices = list(
              "Manual" = "manual",
              "Cada 30 minutos" = "30min",
              "Cada hora" = "1hora",
              "Diaria" = "diaria"
            ),
            selected = "manual"
          ),
          
          checkboxInput("notificaciones_activas", "Activar Notificaciones", value = TRUE),
          checkboxInput("modo_debug_ds", "Modo Debug", value = FALSE)
        ),
        
        box(
          title = "ðŸ“Š InformaciÃ³n del Sistema",
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 6,
          
          h4("ðŸ”§ Estado Actual"),
          verbatimTextOutput("info_sistema_ds"),
          
          hr(),
          
          h4("ðŸ“ˆ EstadÃ­sticas de Uso"),
          tableOutput("tabla_estadisticas_uso_ds"),
          
          hr(),
          
          h4("ðŸ’¾ Acciones del Sistema"),
          div(
            style = "text-align: center;",
            actionButton("limpiar_cache_ds", "ðŸ—‘ï¸ Limpiar Cache", 
                         class = "btn-warning", style = "margin: 5px;"),
            actionButton("exportar_datos_ds", "ðŸ“¤ Exportar AnÃ¡lisis", 
                         class = "btn-info", style = "margin: 5px;"),
            actionButton("reiniciar_app_ds", "ðŸ”„ Reiniciar App", 
                         class = "btn-danger", style = "margin: 5px;")
          )
        )
      ),
      
      # Log del sistema
      fluidRow(
        box(
          title = "ðŸ“‹ Log del Sistema DS_Conexion",
          status = "success",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 12,
          
          div(
            style = "max-height: 400px; overflow-y: auto; background-color: #1e2124; color: #dcddde; padding: 15px; font-family: 'Courier New', monospace; font-size: 12px; border-radius: 5px;",
            verbatimTextOutput("log_sistema_ds")
          )
        )
      )
    )
    
    # AquÃ­ se agregarÃ¡n mÃ¡s tabs segÃºn se vayan desarrollando
    # Los otros menÃºs (Mapa de Oportunidades, Marcas Emergentes, etc.) se implementarÃ¡n en iteraciones futuras
    
  )
)

# =============================================================================
# 4. PIE DE PÃGINA DS_Conexion
# =============================================================================

pie_dashboard_ds <- dashboardFooter(
  left = div(
    style = "color: #6c757d; font-size: 13px;",
    "DS Conexión â€¢ Sistema de Business Analytics & Predictive Modeling â€¢ Parque Vehicular Guatemala"
  ),
  right = div(
    style = "color: #6c757d; font-size: 13px;",
    "DS. William V. Paredes P. | DS Conexión â€¢ ", 
    textOutput("version_app_ds", inline = TRUE),
    " â€¢ ",
    tags$a("DS Conexión", 
           href = "https://dsconexion.com/", 
           target = "_blank", 
           style = "color: #1a365d; text-decoration: none;")
  )
)

# =============================================================================
# 5. ESTRUCTURA FINAL DEL UI DS_Conexion
# =============================================================================

dashboardPage(
  header = encabezado,
  sidebar = lateral,
  body = cuerpo,
  footer = pie_dashboard_ds,
  title = "DS Conexión - AnÃ¡lisis Parque Vehicular",
  skin = "blue"
)