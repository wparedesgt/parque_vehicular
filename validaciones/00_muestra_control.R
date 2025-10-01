#### Validaciones 

### carga de datos parque vehicular

oficina <- '/home/wparedes/Documentos/Ciencia_Datos/Radiadores_La_Torre/parque_vehicular'


setwd(oficina)
rm(list = ls())

library(tidyverse)
library(readr)
library(janitor)
library(readxl)
library(plotly)


datos_rfv <- readRDS('datos/rfv.rds')

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

datos_rfv_global <- datos_rfv
datos_long_global <- procesar_datos_long(datos_rfv_global)
metricas_global <- calcular_metricas_marcas(datos_long_global)

datos <- metricas_global

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
  "Estrellas" = "#27ae60",
  "Promesas" = "#3498db",
  "Base Consolidada" = "#f39c12",
  "Interrogantes" = "#95a5a6"
)

#datos <- datos %>% filter(Marca_Vehiculo != 'THOMAS')

plot_ly(
  datos,
  x = ~Volumen_Final,
  y = ~Crecimiento_Relativo,
  color = ~cuadrante,
  colors = colores_cuadrantes,  # CORREGIDO: Usar el vector nombrado estático
  type = "scatter",
  mode = "markers",
  marker = list(size = 10, opacity = 0.7),
  text = ~paste0("<b>", Marca_Vehiculo, "</b><br>Cuadrante: ", cuadrante),
  hovertemplate = "%{text}<extra></extra>"
) 




