# =============================================================================
# UI.R - ESTRATEGIA RLT: ANÁLISIS PARQUE VEHICULAR
# Sistema de Análisis de Oportunidades para Radiadores y Sistemas de Refrigeración
# =============================================================================

# =============================================================================
# 1. ENCABEZADO DEL DASHBOARD 
# =============================================================================

encabezado <- dashboardHeader(
  title = "Estrategia RLT - Análisis Parque Vehicular",
  titleWidth = 450,
  dropdownMenuOutput("menu_notificaciones_rlt")
)

# Logo corporativo RLT en el encabezado
encabezado$children[[2]]$children <- tags$a(
  href = 'https://radiadoreslatorre.com/',
  tags$img(
    src = 'https://radiadoreslatorre.com/wp-content/uploads/thegem-logos/logo_e5a4204d9a7275357eb41561b84c0969_1x.webp',
    height = '45', 
    width = '200',
    style = "max-height: 45px; object-fit: contain; margin-top: 3px;"
  ),
  target = "_blank"
)

# =============================================================================
# 2. SIDEBAR CORPORATIVO RLT
# =============================================================================

lateral <- dashboardSidebar(
  width = 300,
  
  # CSS personalizado RLT
  includeCSS('www/styles.css'),
  
  # JavaScript para funcionalidades específicas RLT
  tags$head(
    tags$script(HTML("
      // Función para actualización de datos del parque vehicular
      Shiny.addCustomMessageHandler('actualizar_parque_vehicular', function(message) {
        $('#btn_actualizar_datos').addClass('loading');
        setTimeout(function() {
          $('#btn_actualizar_datos').removeClass('loading');
          toastr.success('Datos del parque vehicular actualizados', 'Actualización Completa');
        }, 3000);
      });
      
      // Función para mostrar alertas de oportunidades
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
        if($('#auto_refresh_rlt').prop('checked')) {
          Shiny.onInputChange('trigger_refresh_parque', Math.random());
        }
      }, 1800000); // 30 minutos
    "))
  ),
  
  # ENCABEZADO PERSONALIZADO RLT
  div(
    style = "
      text-align: center; 
      padding: 25px 15px; 
      background: linear-gradient(135deg, #c41e3a 0%, #8b1538 100%);
      border-bottom: 4px solid #2c3e50;
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
      src = "https://radiadoreslatorre.com/wp-content/uploads/thegem-logos/logo_e5a4204d9a7275357eb41561b84c0969_1x.webp",
      style = "
        height: 60px; 
        width: auto; 
        margin-bottom: 12px;
        filter: brightness(1.3) drop-shadow(0 2px 4px rgba(0,0,0,0.3));
        position: relative;
        z-index: 2;
      "
    ),
    
    # Título principal
    h3(
      "ESTRATEGIA RLT",
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
    
    # Subtítulo
    p(
      "Análisis de Oportunidades",
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
  
  # MENÚ PRINCIPAL DE NAVEGACIÓN RLT
  sidebarMenu(
    id = "menu_principal_rlt",
    
    menuItem(
      'Dashboard Ejecutivo',
      tabName = 'dashboard_ejecutivo_rlt',
      icon = icon('tachometer-alt', lib = 'font-awesome'),
      badgeLabel = "Principal", 
      badgeColor = "red"
    ),
    
    # CORREGIDO: Eliminado tabName = 'analisis_marcas' que causaba el problema
    menuItem(
      'Análisis de Marcas',
      icon = icon('chart-bar', lib = 'font-awesome'),
      startExpanded = TRUE,
      menuSubItem('Panorama General', tabName = 'panorama_general'),
      menuSubItem('Rankings de Marcas', tabName = 'rankings_marcas'),
      menuSubItem('Tendencias Temporales', tabName = 'tendencias_temporales'),
      menuSubItem('Análisis Detallado', tabName = 'analisis_detallado')
    ),
    
    menuItem(
      'Oportunidades Estratégicas',
      icon = icon('bullseye', lib = 'font-awesome'),
      startExpanded = FALSE,
      menuSubItem('Mapa de Oportunidades', tabName = 'mapa_oportunidades')#,
      #menuSubItem('Marcas Emergentes', tabName = 'marcas_emergentes'),
      #menuSubItem('Análisis Competitivo', tabName = 'competitivo_marcas')
    ),
    
    # menuItem(
    #   'Proyecciones y Predicciones',
    #   icon = icon('crystal-ball', lib = 'font-awesome'),
    #   startExpanded = FALSE,
    #   menuSubItem('Modelos Predictivos', tabName = 'modelos_predictivos_rlt'),
    #   menuSubItem('Escenarios Futuros', tabName = 'escenarios_futuros')
    # ),
    
    # menuItem(
    #   'Inteligencia de Mercado',
    #   icon = icon('brain', lib = 'font-awesome'),
    #   startExpanded = FALSE,
    #   menuSubItem('Alertas Automáticas', tabName = 'alertas_automaticas'),
    #   menuSubItem('Segmentación Avanzada', tabName = 'segmentacion_avanzada')
    # ),
    
    menuItem(
      'Configuración',
      tabName = 'configuracion_rlt',
      icon = icon('cogs', lib = 'font-awesome')
    )
  ),
  
  # Panel de control de datos
  hr(style = "border-color: #495057; margin: 20px 0;"),
  div(
    style = "padding: 20px;",
    h5("📊 Control de Datos", 
       style = "color: white; margin-bottom: 15px; font-weight: 600;"),
    
    actionButton(
      "btn_actualizar_datos",
      "🔄 Actualizar Parque Vehicular",
      icon = icon("sync-alt"),
      class = "btn-danger btn-block",
      style = "margin-bottom: 12px; font-weight: 500;"
    ),
    
    checkboxInput(
      "auto_refresh_rlt",
      "Auto-actualización (30 min)",
      value = FALSE
    ),
    
    div(
      style = "margin-top: 10px;",
      textOutput("ultima_actualizacion_rlt"),
      tags$style("#ultima_actualizacion_rlt {color: #e9ecef; font-size: 11px;}")
    )
  ),
  
  # Indicadores de estado específicos RLT
  hr(style = "border-color: #495057; margin: 20px 0;"),
  div(
    style = "padding: 20px;",
    h5("🎯 Estado del Sistema", 
       style = "color: white; margin-bottom: 15px; font-weight: 600;"),
    
    div(
      style = "display: flex; justify-content: space-between; margin-bottom: 8px;",
      span("Datos SAT:", style = "color: #ced4da; font-size: 12px;"),
      span(id = "status_sat", "●", style = "color: #27ae60; font-size: 16px;")
    ),
    
    div(
      style = "display: flex; justify-content: space-between; margin-bottom: 8px;",
      span("Análisis ML:", style = "color: #ced4da; font-size: 12px;"),
      span(id = "status_ml", "●", style = "color: #27ae60; font-size: 16px;")
    ),
    
    div(
      style = "display: flex; justify-content: space-between; margin-bottom: 8px;",
      span("Alertas:", style = "color: #ced4da; font-size: 12px;"),
      span(id = "status_alertas", "●", style = "color: #f39c12; font-size: 16px;")
    )
  ),
  
  # Información del sistema al final
  div(
    style = "
      position: absolute;
      bottom: 0;
      width: calc(100% - 30px);
      padding: 20px 15px;
      background: linear-gradient(to top, rgba(44, 62, 80, 0.4), transparent);
      border-top: 1px solid #495057;
    ",
    p("Última actualización:", 
      style = "color: #ced4da; font-size: 11px; margin: 0;"),
    p(textOutput("timestamp_sistema_rlt", inline = TRUE), 
      style = "color: #f8f9fa; font-size: 10px; margin: 0;"),
    br(),
    p("DS. William V. Paredes P.", 
      style = "color: #adb5bd; font-size: 10px; margin: 0; font-style: italic;")
  )
)

# =============================================================================
# 3. CUERPO PRINCIPAL DEL DASHBOARD
# =============================================================================

cuerpo <- dashboardBody(
  
  # Usar waitress para loading screens
  useWaitress(),
  
  # CSS adicional personalizado RLT
  tags$head(
    tags$style(HTML("
      .content-wrapper, .right-side {
        background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
      }
      
      .main-header .navbar {
        background: linear-gradient(135deg, #c41e3a 0%, #8b1538 100%) !important;
        border-bottom: 3px solid #2c3e50;
      }
      
      .main-header .logo {
        background: linear-gradient(135deg, #8b1538 0%, #c41e3a 100%) !important;
      }
      
      .skin-red .main-sidebar {
        background: linear-gradient(180deg, #2c3e50 0%, #34495e 100%);
      }
      
      .box.box-primary {
        border-top-color: #c41e3a;
        box-shadow: 0 4px 12px rgba(196, 30, 58, 0.15);
      }
      
      .box.box-danger {
        border-top-color: #e74c3c;
        box-shadow: 0 4px 12px rgba(231, 76, 60, 0.15);
      }
      
      .box.box-warning {
        border-top-color: #f39c12;
        box-shadow: 0 4px 12px rgba(243, 156, 18, 0.15);
      }
      
      .box.box-success {
        border-top-color: #27ae60;
        box-shadow: 0 4px 12px rgba(39, 174, 96, 0.15);
      }
      
      .btn-primary {
        background: linear-gradient(135deg, #c41e3a 0%, #8b1538 100%);
        border-color: #8b1538;
        box-shadow: 0 2px 8px rgba(196, 30, 58, 0.3);
      }
      
      .btn-primary:hover {
        background: linear-gradient(135deg, #8b1538 0%, #c41e3a 100%);
        border-color: #6d1129;
        transform: translateY(-1px);
        box-shadow: 0 4px 12px rgba(196, 30, 58, 0.4);
      }
      
      .btn-danger {
        background: linear-gradient(135deg, #e74c3c 0%, #c0392b 100%);
        border-color: #c0392b;
      }
      
      .btn-danger:hover {
        background: linear-gradient(135deg, #c0392b 0%, #e74c3c 100%);
        transform: translateY(-1px);
      }
      
      /* Estilos para value boxes RLT */
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
        background: linear-gradient(90deg, #c41e3a, #2c3e50);
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
      
      /* Estilos para gráficos */
      .plotly-container {
        height: 100% !important;
        border-radius: 8px;
      }
      
      /* Estilos específicos para tablas RLT */
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
        border-top: 3px solid #c41e3a;
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
  
  # Contenido de las pestañas
  tabItems(
    
    # =============================================================================
    # TAB 1: DASHBOARD EJECUTIVO PRINCIPAL RLT
    # =============================================================================
    tabItem(
      tabName = "dashboard_ejecutivo_rlt",
      
      # Encabezado ejecutivo RLT
      fluidRow(
        box(
          title = "🎯 Dashboard Ejecutivo - Estrategia RLT",
          status = "danger",
          solidHeader = TRUE,
          width = 12,
          height = 130,
          
          div(
            style = "display: flex; justify-content: space-between; align-items: center; padding: 10px 0;",
            div(
              h4("Sistema de Análisis de Oportunidades para Radiadores", 
                 style = "margin: 0; color: #c41e3a; font-weight: 600;"),
              p("Parque Vehicular Guatemala • Machine Learning • Análisis Predictivo • SAT Data", 
                style = "margin: 8px 0 0 0; color: #6c757d; font-size: 14px; font-weight: 500;")
            ),
            div(
              style = "text-align: right;",
              h5("📈 LIVE DATA", 
                 style = "margin: 0; color: #e74c3c; font-weight: 600;"),
              p(textOutput("fecha_actual_rlt"), 
                style = "margin: 2px 0 0 0; color: #6c757d; font-size: 12px;")
            )
          )
        )
      ),
      
      # KPIs principales RLT
      fluidRow(
        valueBoxOutput("vb_total_vehiculos", width = 3),
        valueBoxOutput("vb_marcas_alta_prioridad", width = 3),
        valueBoxOutput("vb_crecimiento_mercado", width = 3),
        valueBoxOutput("vb_score_oportunidad", width = 3)
      ),
      
      # Gráficos principales
      fluidRow(
        box(
          title = "🗺️ Mapa Estratégico de Oportunidades",
          status = "danger",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 9,
          height = 520,
          footer = HTML(paste0(
            "<b>Interpretación:</b> ",
            "🔴 Alta Prioridad: >50K vehículos + >10% crecimiento | ",
            "🟠 Media Prioridad: >10K vehículos + >15% crecimiento | ",
            "🔵 Emergente: >30% crecimiento + >1K vehículos | ",
            "⚫ Baja Prioridad: Resto de marcas | ",
            "Fuente: Superintendencia de Administración Tributaria (SAT)"
          )),
          
          div(
            style = "height: 440px;",
            plotlyOutput("mapa_estrategico_principal", height = "100%")
          )
        ),
        
        box(
          title = "🚨 Alertas de Oportunidades",
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
      
      # Análisis detallado
      fluidRow(
        box(
          title = "📊 Resumen Ejecutivo del Mercado",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 8,
          
          div(
            style = "padding: 15px;",
            
            fluidRow(
              column(6,
                     h5("📈 Métricas de Crecimiento", 
                        style = "color: #c41e3a; margin-bottom: 15px;"),
                     tableOutput("tabla_metricas_crecimiento")
              ),
              column(6,
                     h5("🎯 Concentración del Mercado", 
                        style = "color: #c41e3a; margin-bottom: 15px;"),
                     tableOutput("tabla_concentracion_mercado")
              )
            )
          )
        ),
        
        box(
          title = "🏆 Top Performers",
          status = "success",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 4,
          
          tabBox(
            width = 12,
            height = "400px",
            tabPanel("🔥 Por Volumen", 
                     div(style = "max-height: 320px; overflow-y: auto;",
                         dataTableOutput("tabla_top_volumen"))),
            tabPanel("📈 Por Crecimiento", 
                     div(style = "max-height: 320px; overflow-y: auto;",
                         dataTableOutput("tabla_top_crecimiento"))),
            tabPanel("⭐ Por Score", 
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
          title = "⚙️ Configuración de Análisis",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 12,
          
          fluidRow(
            column(3,
                   selectInput(
                     "filtro_categoria_volumen",
                     "Categoría por Volumen:",
                     choices = list(
                       "Todas las Categorías" = "todas",
                       "Alto Volumen (100K+)" = "alto",
                       "Volumen Medio (10K-100K)" = "medio",
                       "Volumen Bajo (1K-10K)" = "bajo",
                       "Volumen Mínimo (<1K)" = "minimo"
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
                       "🔴 Alta Prioridad" = "alta",
                       "🟠 Media Prioridad" = "media", 
                       "🔵 Emergente" = "emergente",
                       "⚫ Baja Prioridad" = "baja"
                     ),
                     selected = "todos"
                   )
            ),
            column(3,
                   numericInput(
                     "min_vehiculos",
                     "Vehículos Mínimos:",
                     value = 0,
                     min = 0,
                     step = 1000
                   )
            ),
            column(3,
                   br(),
                   actionButton(
                     "btn_aplicar_filtros",
                     "🔍 Aplicar Filtros",
                     class = "btn-primary btn-block"
                   )
            )
          )
        )
      ),
      
      # Visualizaciones del panorama general
      fluidRow(
        box(
          title = "📊 Distribución del Parque Vehicular por Categoría",
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
          title = "🎯 Matriz de Potencial de Radiadores",
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
          title = "📋 Vista Panorámica Completa",
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
          title = "🏆 Top 15 Marcas por Volumen de Vehículos",
          status = "success",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 6,
          height = 550,
          footer = "Ranking basado en el volumen total de vehículos registrados al último período disponible",
          
          div(
            style = "height: 470px;",
            plotlyOutput("grafico_ranking_volumen", height = "100%")
          )
        ),
        
        box(
          title = "📈 Top 15 Marcas por Crecimiento",
          status = "warning",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 6,
          height = 550,
          footer = "Ranking basado en el crecimiento relativo entre primer y último período registrado",
          
          div(
            style = "height: 470px;",
            plotlyOutput("grafico_ranking_crecimiento", height = "100%")
          )
        )
      ),
      
      fluidRow(
        box(
          title = "⭐ Top 15 Marcas por Score de Oportunidad",
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
          title = "📊 Ranking Comparativo Completo",
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
      
      # Configuración de tendencias
      fluidRow(
        box(
          title = "📅 Configuración de Análisis Temporal",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 12,
          
          fluidRow(
            column(4,
                   selectInput(
                     "marcas_seleccionadas",
                     "Seleccionar Marcas:",
                     choices = NULL, # Se llenará dinámicamente
                     selected = NULL,
                     multiple = TRUE
                   )
            ),
            column(4,
                   selectInput(
                     "tipo_visualizacion",
                     "Tipo de Visualización:",
                     choices = list(
                       "Líneas de Tiempo" = "lineas",
                       "Área Apilada" = "area",
                       "Barras por Período" = "barras"
                     ),
                     selected = "lineas"
                   )
            ),
            column(4,
                   checkboxInput(
                     "mostrar_tendencia",
                     "Mostrar Línea de Tendencia",
                     value = TRUE
                   )
            )
          )
        )
      ),
      
      # Gráfico principal de tendencias
      fluidRow(
        box(
          title = "📈 Evolución Temporal del Parque Vehicular",
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 12,
          height = 600,
          footer = "Evolución mensual de las marcas seleccionadas • Datos: Enero 2024 - Agosto 2025",
          
          div(
            style = "height: 520px;",
            plotlyOutput("grafico_tendencias_temporal", height = "100%")
          )
        )
      ),
      
      # Análisis estadístico de tendencias
      fluidRow(
        box(
          title = "📊 Análisis Estadístico de Tendencias",
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
          title = "🎯 Métricas de Tendencia",
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
    # TAB 5: ANÁLISIS DETALLADO
    # =============================================================================
    tabItem(
      tabName = "analisis_detallado",
      
      # Selector de marca para análisis detallado
      fluidRow(
        box(
          title = "🔍 Selección para Análisis Detallado",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 12,
          
          fluidRow(
            column(6,
                   selectInput(
                     "marca_detalle",
                     "Seleccionar Marca para Análisis:",
                     choices = NULL, # Se llenará dinámicamente
                     selected = NULL,
                     width = "100%"
                   )
            ),
            column(6,
                   br(),
                   actionButton(
                     "btn_generar_detalle",
                     "📊 Generar Análisis Detallado",
                     class = "btn-primary btn-block"
                   )
            )
          )
        )
      ),
      
      # Información detallada de la marca seleccionada
      fluidRow(
        # Panel de métricas clave
        box(
          title = "📈 Métricas Clave de la Marca",
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
        
        # Gráfico de evolución individual
        box(
          title = "📊 Evolución Individual",
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
      
      # Análisis comparativo con competencia
      fluidRow(
        box(
          title = "🆚 Comparación con Competencia Directa",
          status = "warning",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 12,
          height = 500,
          footer = "Comparación con las 5 marcas más similares en volumen y características",
          
          div(
            style = "height: 420px;",
            plotlyOutput("grafico_comparacion_competencia", height = "100%")
          )
        )
      ),
      
      # Tabla de análisis detallado
      fluidRow(
        box(
          title = "📋 Reporte Detallado de la Marca",
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
    # TAB: MAPA DE OPORTUNIDADES ESTRATÉGICAS
    # =============================================================================
    tabItem(
      tabName = "mapa_oportunidades",
      
      # Encabezado estratégico
      fluidRow(
        box(
          title = "🗺️ MAPA ESTRATÉGICO DE OPORTUNIDADES - NIVEL EJECUTIVO",
          status = "danger",
          solidHeader = TRUE,
          width = 12,
          height = 140,
          
          div(
            style = "display: flex; justify-content: space-between; align-items: center; padding: 10px 0;",
            div(
              h4("Sistema de Identificación y Priorización de Oportunidades", 
                 style = "margin: 0; color: #c41e3a; font-weight: 600;"),
              p("Análisis Multidimensional • Segmentación Inteligente • Recomendaciones Accionables", 
                style = "margin: 8px 0 0 0; color: #6c757d; font-size: 14px; font-weight: 500;")
            ),
            div(
              style = "text-align: right;",
              h5("🎯 STRATEGIC VIEW", 
                 style = "margin: 0; color: #e74c3c; font-weight: 600;"),
              p("Nivel: Gerencia General", 
                style = "margin: 2px 0 0 0; color: #6c757d; font-size: 12px;")
            )
          )
        )
      ),
      
      # Panel de control avanzado
      fluidRow(
        box(
          title = "⚙️ CENTRO DE CONTROL ESTRATÉGICO",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = FALSE,
          width = 12,
          
          fluidRow(
            # Columna 1: Filtros de Volumen y Crecimiento
            column(3,
                   h5("📊 Dimensión: Volumen", style = "color: #2c3e50; margin-bottom: 10px;"),
                   sliderInput(
                     "filtro_volumen_min_oport",
                     "Volumen Mínimo:",
                     min = 0,
                     max = 500000,
                     value = 1000,
                     step = 1000,
                     post = " veh."
                   ),
                   sliderInput(
                     "filtro_volumen_max_oport",
                     "Volumen Máximo:",
                     min = 1000,
                     max = 1500000,
                     value = 500000,
                     step = 1000,
                     post = " veh."
                   )
            ),
            
            # Columna 2: Filtros de Crecimiento
            column(3,
                   h5("📈 Dimensión: Crecimiento", style = "color: #2c3e50; margin-bottom: 10px;"),
                   sliderInput(
                     "filtro_crecimiento_min_oport",
                     "Crecimiento Mínimo:",
                     min = -50,
                     max = 200,
                     value = 0,
                     step = 5,
                     post = "%"
                   ),
                   sliderInput(
                     "filtro_crecimiento_max_oport",
                     "Crecimiento Máximo:",
                     min = -50,
                     max = 200,
                     value = 200,
                     step = 5,
                     post = "%"
                   )
            ),
            
            # Columna 3: Filtros de Score y Categorías
            column(3,
                   h5("⭐ Dimensión: Score", style = "color: #2c3e50; margin-bottom: 10px;"),
                   sliderInput(
                     "filtro_score_min_oport",
                     "Score Mínimo:",
                     min = 0,
                     max = 100,
                     value = 40,
                     step = 5,
                     post = " pts"
                   ),
                   selectInput(
                     "filtro_categorias_oport",
                     "Categorías:",
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
                   h5("🎯 Dimensión: Potencial", style = "color: #2c3e50; margin-bottom: 10px;"),
                   checkboxGroupInput(
                     "filtro_potencial_oport",
                     "Niveles de Prioridad:",
                     choices = list(
                       "🔴 Alta Prioridad" = "alta",
                       "🟠 Media Prioridad" = "media",
                       "🔵 Emergente" = "emergente",
                       "⚫ Baja Prioridad" = "baja"
                     ),
                     selected = c("alta", "media", "emergente")
                   ),
                   br(),
                   actionButton(
                     "btn_aplicar_filtros_oport",
                     "🔍 Aplicar Filtros",
                     class = "btn-danger btn-block",
                     style = "font-weight: 600;"
                   ),
                   actionButton(
                     "btn_resetear_filtros_oport",
                     "↺ Resetear",
                     class = "btn-default btn-block",
                     style = "margin-top: 5px;"
                   )
            )
          )
        )
      ),
      
      # KPIs Dinámicos del Filtrado
      fluidRow(
        valueBoxOutput("vb_marcas_filtradas_oport", width = 3),
        valueBoxOutput("vb_volumen_oportunidad_oport", width = 3),
        valueBoxOutput("vb_score_promedio_oport", width = 3),
        valueBoxOutput("vb_potencial_mercado_oport", width = 3)
      ),
      
      # Mapa estratégico principal + matriz de decisión
      fluidRow(
        # Mapa de burbujas avanzado
        box(
          title = "🗺️ MAPA ESTRATÉGICO INTERACTIVO: Volumen vs Crecimiento vs Score",
          status = "danger",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 8,
          height = 600,
          footer = HTML(paste0(
            "<b>Eje X:</b> Volumen de Vehículos (escala logarítmica) | ",
            "<b>Eje Y:</b> Crecimiento Relativo (%) | ",
            "<b>Tamaño burbuja:</b> Score de Oportunidad | ",
            "<b>Color:</b> Nivel de Prioridad"
          )),
          
          div(
            style = "height: 510px;",
            plotlyOutput("mapa_burbujas_avanzado_oport", height = "100%")
          )
        ),
        
        # Matriz de decisión estratégica
        box(
          title = "📊 MATRIZ DE DECISIÓN 2x2",
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
      
      # Análisis por cuadrantes + distribuciones
      fluidRow(
        # Tabla de cuadrantes estratégicos
        box(
          title = "🎯 SEGMENTACIÓN POR CUADRANTES ESTRATÉGICOS",
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 6,
          
          tabBox(
            width = 12,
            tabPanel("⭐ Estrellas (Alto Vol + Alto Crec)", 
                     div(style = "max-height: 320px; overflow-y: auto;",
                         dataTableOutput("tabla_cuadrante_estrellas_oport"))),
            tabPanel("🚀 Promesas (Bajo Vol + Alto Crec)", 
                     div(style = "max-height: 320px; overflow-y: auto;",
                         dataTableOutput("tabla_cuadrante_promesas_oport"))),
            tabPanel("💰 Base Consolidada (Alto Vol + Bajo Crec)", 
                     div(style = "max-height: 320px; overflow-y: auto;",
                         dataTableOutput("tabla_cuadrante_base_consolidada_oport"))),
            tabPanel("❓ Interrogantes (Bajo Vol + Bajo Crec)", 
                     div(style = "max-height: 320px; overflow-y: auto;",
                         dataTableOutput("tabla_cuadrante_interrogantes_oport")))
          )
        ),
        
        # Distribuciones y concentración
        box(
          title = "📈 ANÁLISIS DE DISTRIBUCIÓN",
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
      
      # Análisis temporal de oportunidades
      fluidRow(
        box(
          title = "📅 EVOLUCIÓN TEMPORAL DE OPORTUNIDADES CLAVE",
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
                     "Criterio de Selección:",
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
                     "🔄 Actualizar",
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
          title = "🎯 RECOMENDACIONES ESTRATÉGICAS AUTOMÁTICAS",
          status = "warning",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 8,
          
          div(
            style = "padding: 20px;",
            uiOutput("panel_recomendaciones_inteligentes_oport")
          )
        ),
        
        # Métricas de concentración y riesgo
        box(
          title = "⚠️ ANÁLISIS DE RIESGO Y CONCENTRACIÓN",
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
          title = "📋 TABLA MAESTRA DE OPORTUNIDADES - EXPORTABLE",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 12,
          
          div(
            style = "margin-bottom: 15px;",
            downloadButton("btn_exportar_oportunidades_excel", "📊 Exportar a Excel", class = "btn-success"),
            downloadButton("btn_exportar_oportunidades_csv", "📄 Exportar a CSV", class = "btn-info", style = "margin-left: 10px;")
          ),
          
          div(
            style = "max-height: 500px; overflow-y: auto;",
            dataTableOutput("tabla_maestra_oportunidades_oport")
          )
        )
      )
    ),
    
    
    # =============================================================================
    # TAB 6: CONFIGURACIÓN
    # =============================================================================
    tabItem(
      tabName = "configuracion_rlt",
      
      fluidRow(
        box(
          title = "⚙️ Configuración del Sistema RLT",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 6,
          
          h4("📁 Gestión de Datos"),
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
          actionButton("test_carga_datos", "🧪 Probar Carga de Datos", class = "btn-info"),
          
          hr(),
          
          h4("🔄 Actualización Automática"),
          selectInput(
            "frecuencia_actualizacion_rlt",
            "Frecuencia de Actualización:",
            choices = list(
              "Manual" = "manual",
              "Cada 30 minutos" = "30min",
              "Cada hora" = "1hora",
              "Diaria" = "diaria"
            ),
            selected = "manual"
          ),
          
          checkboxInput("notificaciones_activas", "Activar Notificaciones", value = TRUE),
          checkboxInput("modo_debug_rlt", "Modo Debug", value = FALSE)
        ),
        
        box(
          title = "📊 Información del Sistema",
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 6,
          
          h4("🔧 Estado Actual"),
          verbatimTextOutput("info_sistema_rlt"),
          
          hr(),
          
          h4("📈 Estadísticas de Uso"),
          tableOutput("tabla_estadisticas_uso_rlt"),
          
          hr(),
          
          h4("💾 Acciones del Sistema"),
          div(
            style = "text-align: center;",
            actionButton("limpiar_cache_rlt", "🗑️ Limpiar Cache", 
                         class = "btn-warning", style = "margin: 5px;"),
            actionButton("exportar_datos_rlt", "📤 Exportar Análisis", 
                         class = "btn-info", style = "margin: 5px;"),
            actionButton("reiniciar_app_rlt", "🔄 Reiniciar App", 
                         class = "btn-danger", style = "margin: 5px;")
          )
        )
      ),
      
      # Log del sistema
      fluidRow(
        box(
          title = "📋 Log del Sistema RLT",
          status = "success",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 12,
          
          div(
            style = "max-height: 400px; overflow-y: auto; background-color: #1e2124; color: #dcddde; padding: 15px; font-family: 'Courier New', monospace; font-size: 12px; border-radius: 5px;",
            verbatimTextOutput("log_sistema_rlt")
          )
        )
      )
    )
    
    # Aquí se agregarán más tabs según se vayan desarrollando
    # Los otros menús (Mapa de Oportunidades, Marcas Emergentes, etc.) se implementarán en iteraciones futuras
    
  )
)

# =============================================================================
# 4. PIE DE PÁGINA RLT
# =============================================================================

pie_dashboard_rlt <- dashboardFooter(
  left = div(
    style = "color: #6c757d; font-size: 13px;",
    "Estrategia RLT • Sistema de Análisis de Oportunidades • Parque Vehicular Guatemala"
  ),
  right = div(
    style = "color: #6c757d; font-size: 13px;",
    "DS. William V. Paredes P. • ", 
    textOutput("version_app_rlt", inline = TRUE),
    " • ",
    tags$a("Radiadores La Torre", 
           href = "https://radiadoreslatorre.com/", 
           target = "_blank", 
           style = "color: #c41e3a; text-decoration: none;")
  )
)

# =============================================================================
# 5. ESTRUCTURA FINAL DEL UI RLT
# =============================================================================

dashboardPage(
  header = encabezado,
  sidebar = lateral,
  body = cuerpo,
  footer = pie_dashboard_rlt,
  title = "Estrategia RLT - Análisis Parque Vehicular",
  skin = "red"
)