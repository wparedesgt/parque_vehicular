# =============================================================================
# GLOBAL.R - ESTRATEGIA RLT: ANÁLISIS PARQUE VEHICULAR
# Sistema de Análisis de Oportunidades para Radiadores y Sistemas de Refrigeración
# =============================================================================

# =============================================================================
# 1. CARGA DE LIBRERÍAS
# =============================================================================

suppressPackageStartupMessages({
  # Shiny y UI
  library(shiny)
  library(shinydashboard)
  library(shinydashboardPlus)
  library(shinyWidgets)
  library(waiter)
  
  # Manipulación de datos
  library(tidyverse)
  library(glue)
  library(janitor)
  library(lubridate)
  library(readr)
  
  # Análisis y modelos
  library(forecast)
  library(cluster)
  library(corrplot)
  library(zoo)
  
  # Visualización
  library(plotly)
  library(highcharter)
  library(DT)
  library(scales)
  library(RColorBrewer)
  library(viridis)
  library(formattable)
  
  # Utilidades
  library(openxlsx)
  library(htmlwidgets)
})

options(scipen = 9999)

# =============================================================================
# 2. CONFIGURACIÓN DE RUTAS Y DIRECTORIOS
# =============================================================================

# Configuración de rutas (adaptar según el entorno)
oficina <- "/home/wparedes/Documentos/Ciencia_Datos/Radiadores_La_Torre/parque_vehicular/app/parque_vehicular_gt"
servidor <- '/opt/shiny-server/estrategia_rlt'
casa <- 'C:/Users/wpare/Documents/William/Ciencia_Datos/parque_vehicular'
setwd(oficina)
rm(list =ls())

# =============================================================================
# 3. COLORES CORPORATIVOS RLT
# =============================================================================

colores_rlt <- list(
  # Colores principales
  rlt_red = "#c41e3a",
  rlt_dark_red = "#8b1538", 
  rlt_light_red = "#e74c3c",
  rlt_blue = "#2c3e50",
  rlt_light_blue = "#3498db",
  
  # Colores de análisis 
  alta_prioridad = "#E31A1C",
  media_prioridad = "#FF7F00", 
  emergente = "#1F78B4",
  baja_prioridad = "#666666",
  
  # Escala de performance
  excelente = "#27ae60",
  bueno = "#2ecc71",
  regular = "#f39c12", 
  deficiente = "#e67e22",
  critico = "#e74c3c"
)

# =============================================================================
# 4. FUNCIONES DE CARGA Y PROCESAMIENTO DE DATOS (CORREGIDAS)
# =============================================================================

datos_rfv <- readRDS('Datos/rfv.rds')

# =============================================================================
# 5. FUNCIONES DE ANÁLISIS DEL PARQUE VEHICULAR (CORREGIDAS)
# =============================================================================

# Función para procesar datos en formato long (CORREGIDA)
procesar_datos_long <- function(datos_rfv) {
  cat("🔄 Procesando datos a formato long...\n")
  
  tryCatch({
    # Asegurar que tenemos la columna de marcas
    if(!"Marca_Vehiculo" %in% names(datos_rfv)) {
      stop("No se encuentra la columna 'Marca_Vehiculo' en los datos")
    }
    
    datos_long <- datos_rfv %>%
      pivot_longer(cols = -Marca_Vehiculo, 
                   names_to = "Periodo", 
                   values_to = "Vehiculos_Registrados") %>%
      mutate(
        Año = case_when(
          str_detect(Periodo, "2024") ~ 2024,
          str_detect(Periodo, "2025") ~ 2025,
          TRUE ~ 2024
        ),
        Mes = case_when(
          str_detect(Periodo, "Enero") ~ 1,
          str_detect(Periodo, "Febrero") ~ 2,
          str_detect(Periodo, "Marzo") ~ 3,
          str_detect(Periodo, "Abril") ~ 4,
          str_detect(Periodo, "Mayo") ~ 5,
          str_detect(Periodo, "Junio") ~ 6,
          str_detect(Periodo, "Julio") ~ 7,
          str_detect(Periodo, "Agosto") ~ 8,
          str_detect(Periodo, "Septiembre") ~ 9,
          str_detect(Periodo, "Octubre") ~ 10,
          str_detect(Periodo, "Noviembre") ~ 11,
          str_detect(Periodo, "Diciembre") ~ 12,
          TRUE ~ 1
        ),
        Fecha = make_date(Año, Mes, 1),
        Vehiculos_Registrados = replace_na(as.numeric(Vehiculos_Registrados), 0)
      ) %>%
      arrange(Marca_Vehiculo, Fecha) %>%
      filter(Vehiculos_Registrados >= 0) %>%  # Permitir ceros pero no negativos
      filter(!is.na(Vehiculos_Registrados))   # Eliminar NAs
    
    cat("✅ Datos procesados:", nrow(datos_long), "registros\n")
    return(datos_long)
    
  }, error = function(e) {
    cat("❌ Error procesando datos:", e$message, "\n")
    # Retornar un data frame mínimo para evitar que la app falle completamente
    return(data.frame(
      Marca_Vehiculo = character(0),
      Periodo = character(0),
      Vehiculos_Registrados = numeric(0),
      Año = numeric(0),
      Mes = numeric(0),
      Fecha = as.Date(character(0))
    ))
  })
}

# Función para calcular métricas clave por marca (CORREGIDA)
calcular_metricas_marcas <- function(datos_long) {
  cat("📈 Calculando métricas por marca...\n")
  
  tryCatch({
    if(nrow(datos_long) == 0) {
      cat("⚠️ No hay datos para procesar\n")
      return(data.frame())
    }
    
    metricas_marcas <- datos_long %>%
      group_by(Marca_Vehiculo) %>%
      filter(n() > 1) %>%  # Asegurar que tenemos al menos 2 puntos de datos
      summarise(
        Volumen_Inicial = first(Vehiculos_Registrados),
        Volumen_Final = last(Vehiculos_Registrados),
        Volumen_Maximo = max(Vehiculos_Registrados, na.rm = TRUE),
        Volumen_Promedio = mean(Vehiculos_Registrados, na.rm = TRUE),
        Crecimiento_Absoluto = Volumen_Final - Volumen_Inicial,
        Crecimiento_Relativo = ifelse(Volumen_Inicial > 0, 
                                      (Volumen_Final - Volumen_Inicial) / Volumen_Inicial * 100, 
                                      0),
        Volatilidad = ifelse(n() > 2, sd(Vehiculos_Registrados, na.rm = TRUE), 0),
        Coef_Variacion = ifelse(Volumen_Promedio > 0, Volatilidad / Volumen_Promedio, 0),
        Num_Periodos = n(),
        .groups = "drop"
      ) %>%
      filter(Volumen_Final > 0) %>%
      mutate(
        # Categorización por volumen
        Categoria_Volumen = case_when(
          Volumen_Final >= 100000 ~ "Alto Volumen (100K+)",
          Volumen_Final >= 10000 ~ "Volumen Medio (10K-100K)",
          Volumen_Final >= 1000 ~ "Volumen Bajo (1K-10K)",
          TRUE ~ "Volumen Mínimo (<1K)"
        ),
        
        # Categorización por crecimiento
        Categoria_Crecimiento = case_when(
          Crecimiento_Relativo >= 20 ~ "Alto Crecimiento (20%+)",
          Crecimiento_Relativo >= 10 ~ "Crecimiento Moderado (10-20%)",
          Crecimiento_Relativo >= 0 ~ "Crecimiento Bajo (0-10%)",
          TRUE ~ "En Declive"
        ),
        
        # Potencial para radiadores (clasificación estratégica) - CORREGIDO
        Potencial_Radiadores = case_when(
          Volumen_Final >= 50000 & Crecimiento_Relativo >= 10 ~ "🔴 ALTA PRIORIDAD",
          Volumen_Final >= 10000 & Crecimiento_Relativo >= 15 ~ "🟠 MEDIA PRIORIDAD",
          Crecimiento_Relativo >= 30 & Volumen_Final >= 1000 ~ "🔵 EMERGENTE",
          TRUE ~ "⚫ BAJA PRIORIDAD"
        ),
        
        # Score de oportunidad (0-100) - MEJORADO
        Score_Oportunidad = pmin(100, pmax(0,
                                           (log10(pmax(Volumen_Final, 1) + 1) * 15) +  # Componente volumen (0-45)
                                             (pmin(50, pmax(-50, Crecimiento_Relativo)) * 0.8) +  # Componente crecimiento (0-40)
                                             (pmin(25, 25 - (pmax(0, Coef_Variacion) * 100)) * 0.6)  # Componente estabilidad (0-15)
        )),
        
        # Participación en el mercado
        Participacion_Mercado = (Volumen_Final / sum(Volumen_Final, na.rm = TRUE)) * 100
      ) %>%
      arrange(desc(Score_Oportunidad))
    
    cat("✅ Métricas calculadas para", nrow(metricas_marcas), "marcas\n")
    return(metricas_marcas)
    
  }, error = function(e) {
    cat("❌ Error calculando métricas:", e$message, "\n")
    return(data.frame())
  })
}

# =============================================================================
# 6. FUNCIONES DE VISUALIZACIÓN 
# =============================================================================


# Función para crear gráfico de top marcas 
crear_top_marcas <- function(metricas_marcas, tipo = "volumen", n = 15) {
  tryCatch({
    if(is.null(metricas_marcas) || nrow(metricas_marcas) == 0) {
      return(plot_ly() %>% layout(title = "Sin datos disponibles"))
    }
    
    if(tipo == "volumen") {
      datos_grafico <- metricas_marcas %>%
        arrange(desc(Volumen_Final)) %>%
        head(n) %>%
        mutate(Marca_Vehiculo = fct_reorder(Marca_Vehiculo, Volumen_Final))
      
      titulo <- "Top Marcas por Volumen de Vehículos"
      y_var <- "Volumen_Final"
      y_label <- "Vehículos Registrados"
      
    } else if(tipo == "crecimiento") {
      datos_grafico <- metricas_marcas %>%
        filter(Volumen_Final >= 100) %>%
        arrange(desc(Crecimiento_Relativo)) %>%
        head(n) %>%
        mutate(Marca_Vehiculo = fct_reorder(Marca_Vehiculo, Crecimiento_Relativo))
      
      titulo <- "Top Marcas por Crecimiento"
      y_var <- "Crecimiento_Relativo"
      y_label <- "Crecimiento (%)"
    }
    
    if(nrow(datos_grafico) == 0) {
      return(plot_ly() %>% layout(title = "No hay datos suficientes para mostrar"))
    }
    
    fig <- plot_ly(
      data = datos_grafico,
      x = ~get(y_var),
      y = ~Marca_Vehiculo,
      type = 'bar',
      orientation = 'h',
      marker = list(color = "#2c3e50", opacity = 0.8),
      text = ~paste0(ifelse(tipo == "volumen", 
                            format(get(y_var), big.mark = ","), 
                            paste0(round(get(y_var), 1), "%"))),
      textposition = 'outside',
      hovertemplate = paste0(
        "<b>%{y}</b><br>",
        y_label, ": %{x}<br>",
        "<extra></extra>"
      )
    ) %>%
      layout(
        title = list(
          text = titulo,
          font = list(size = 14, color = "#2c3e50")
        ),
        xaxis = list(title = y_label),
        yaxis = list(title = ""),
        margin = list(l = 120)
      )
    
    return(fig)
    
  }, error = function(e) {
    cat("❌ Error creando gráfico top marcas:", e$message, "\n")
    return(plot_ly() %>% layout(title = "Error al cargar el gráfico"))
  })
}

# =============================================================================
# 7. KPIs, OKRs Y KRIs
# =============================================================================

# Función para calcular KPIs principales (CORREGIDA)
calcular_kpis_radiadores <- function(metricas_marcas, datos_long) {
  cat("🎯 Calculando KPIs para radiadores...\n")
  
  tryCatch({
    if(is.null(metricas_marcas) || nrow(metricas_marcas) == 0) {
      return(list(
        total_vehiculos = 0,
        marcas_alta_prioridad = 0,
        volumen_oportunidad = 0,
        crecimiento_promedio = 0,
        marcas_creciendo = 0,
        top_5_concentracion = 0,
        indice_herfindahl = 0,
        score_promedio = 0
      ))
    }
    
    kpis <- list(
      # KPIs de Penetración
      total_vehiculos = sum(metricas_marcas$Volumen_Final, na.rm = TRUE),
      marcas_alta_prioridad = sum(str_detect(metricas_marcas$Potencial_Radiadores, "ALTA"), na.rm = TRUE),
      volumen_oportunidad = sum(metricas_marcas$Volumen_Final[str_detect(metricas_marcas$Potencial_Radiadores, "ALTA|MEDIA")], na.rm = TRUE),
      
      # KPIs de Crecimiento
      crecimiento_promedio = mean(metricas_marcas$Crecimiento_Relativo, na.rm = TRUE),
      marcas_creciendo = sum(metricas_marcas$Crecimiento_Relativo > 0, na.rm = TRUE),
      
      # KPIs de Concentración
      top_5_concentracion = sum(head(metricas_marcas$Participacion_Mercado, 5), na.rm = TRUE),
      indice_herfindahl = sum((metricas_marcas$Participacion_Mercado)^2, na.rm = TRUE),
      
      # Score promedio del mercado
      score_promedio = mean(metricas_marcas$Score_Oportunidad, na.rm = TRUE)
    )
    
    cat("✅ KPIs calculados exitosamente\n")
    return(kpis)
    
  }, error = function(e) {
    cat("❌ Error calculando KPIs:", e$message, "\n")
    return(list(
      total_vehiculos = 0,
      marcas_alta_prioridad = 0,
      volumen_oportunidad = 0,
      crecimiento_promedio = 0,
      marcas_creciendo = 0,
      top_5_concentracion = 0,
      indice_herfindahl = 0,
      score_promedio = 0
    ))
  })
}

# Función para generar alertas automáticas (CORREGIDA)
generar_alertas_mercado <- function(metricas_marcas) {
  tryCatch({
    if(is.null(metricas_marcas) || nrow(metricas_marcas) == 0) {
      return(list(info = "ℹ️ No hay datos suficientes para generar alertas"))
    }
    
    alertas <- list()
    
    # Alertas de oportunidad
    marcas_emergentes <- metricas_marcas %>%
      filter(str_detect(Potencial_Radiadores, "EMERGENTE")) %>%
      arrange(desc(Crecimiento_Relativo)) %>%
      head(3)
    
    if(nrow(marcas_emergentes) > 0) {
      alertas$oportunidad <- paste0("🚀 OPORTUNIDAD: ", nrow(marcas_emergentes), 
                                    " marcas emergentes detectadas: ", 
                                    paste(marcas_emergentes$Marca_Vehiculo, collapse = ", "))
    }
    
    # Alertas de riesgo
    marcas_declive <- metricas_marcas %>%
      filter(Crecimiento_Relativo < -10) %>%
      arrange(Crecimiento_Relativo) %>%
      head(3)
    
    if(nrow(marcas_declive) > 0) {
      alertas$riesgo <- paste0("⚠️ RIESGO: ", nrow(marcas_declive), 
                               " marcas en declive significativo: ", 
                               paste(marcas_declive$Marca_Vehiculo, collapse = ", "))
    }
    
    # Alertas de concentración
    if(length(alertas) == 0) {
      alertas$info <- "ℹ️ Mercado estable - No se detectaron alertas críticas"
    }
    
    return(alertas)
    
  }, error = function(e) {
    return(list(error = paste("Error generando alertas:", e$message)))
  })
}

# =============================================================================
# 8. INICIALIZACIÓN DE DATOS GLOBALES (CORREGIDA)
# =============================================================================

# Funciones auxiliares para la UI (CORREGIDAS)
formato_numero <- function(x) {
  if(is.na(x) || is.null(x) || !is.numeric(x)) return("0")
  if(x >= 1000000) {
    return(paste0(round(x/1000000, 1), "M"))
  } else if(x >= 1000) {
    return(paste0(round(x/1000, 1), "K"))
  } else {
    return(formatC(x, format = "f", big.mark = ",", digits = 0))
  }
}

# Cargar y procesar datos al inicializar la aplicación (CON MEJOR MANEJO DE ERRORES)
cat("\n🚀 INICIALIZANDO ESTRATEGIA RLT\n")
cat(paste(rep("=", 50), collapse = ""), "\n")

# Variables globales inicializadas de forma segura
datos_rfv_global <- NULL
datos_long_global <- NULL
metricas_global <- NULL
kpis_global <- NULL
alertas_global <- NULL

tryCatch({
  # Cargar datos
  datos_rfv_global <- datos_rfv
  datos_long_global <- procesar_datos_long(datos_rfv_global)
  metricas_global <- calcular_metricas_marcas(datos_long_global)
  kpis_global <- calcular_kpis_radiadores(metricas_global, datos_long_global)
  alertas_global <- generar_alertas_mercado(metricas_global)
  
  cat("\n✅ INICIALIZACIÓN COMPLETADA\n")
  cat("📊 Marcas analizadas:", ifelse(is.null(metricas_global), 0, nrow(metricas_global)), "\n")
  if(!is.null(datos_long_global) && nrow(datos_long_global) > 0) {
    cat("📈 Período:", min(datos_long_global$Fecha), "a", max(datos_long_global$Fecha), "\n")
  }
  cat("🎯 Total vehículos:", formato_numero(ifelse(is.null(kpis_global), 0, kpis_global$total_vehiculos)), "\n")
  cat("🔴 Marcas alta prioridad:", ifelse(is.null(kpis_global), 0, kpis_global$marcas_alta_prioridad), "\n")
  
}, error = function(e) {
  cat("❌ ERROR EN INICIALIZACIÓN:", e$message, "\n")
  cat("🔧 Inicializando con datos de ejemplo...\n")
  
  # Cargar datos de ejemplo como fallback
  datos_rfv_global <- datos_rfv
  datos_long_global <- procesar_datos_long(datos_rfv_global)
  metricas_global <- calcular_metricas_marcas(datos_long_global)
  kpis_global <- calcular_kpis_radiadores(metricas_global, datos_long_global)
  alertas_global <- generar_alertas_mercado(metricas_global)
  
  cat("✅ Inicialización con datos de ejemplo completada\n")
})

# Timestamp para la aplicación
timestamp_app <- Sys.time()

# Verificación final
if(is.null(metricas_global) || nrow(metricas_global) == 0) {
  cat("⚠️ ADVERTENCIA: No hay métricas disponibles. Usando datos mínimos.\n")
}

cat("🔧 Sistema listo para usar\n")
cat(paste(rep("=", 50), collapse = ""), "\n")
