# =============================================================================
# ANÁLISIS DE MERCADO AUTOMOTRIZ PARA OPORTUNIDADES DE VENTA DE RADIADORES
# =============================================================================
# Científico de Datos: Análisis Profundo del Registro Fiscal de Vehículos
# Período: Enero 2024 - Agosto 2025
# =============================================================================

remoto <- "E:/wparedes/Documentos/Ciencia_Datos/Radiadores_La_Torre/Forecast_Inventario_RealCaruz/Estretegia_Comercial"
oficina <- '/home/wparedes/Documentos/Ciencia_Datos/Radiadores_La_Torre/Forecast_Inventario_RealCaruz/02_Estrategia_Comercial'
casa <- 'C:/Users/wpare/Documents/William/Ciencia_Datos/parque_vehicular'

setwd(casa)
rm(list = ls())

# Cargar librerías necesarias
library(tidyverse)
library(readr)
library(plotly)
library(DT)
library(corrplot)
library(gridExtra)
library(viridis)
library(scales)
library(forecast)
library(knitr)
library(kableExtra)

# =============================================================================
# 1. CARGA Y PREPARACIÓN DE DATOS
# =============================================================================

# Cargar datos
datos_rfv <- readRDS('datos/rfv.rds')

# Inspección inicial
cat("=== RESUMEN DEL DATASET ===\n")
cat("Dimensiones:", dim(datos_rfv), "\n")
cat("Marcas únicas:", nrow(datos_rfv), "\n")
cat("Período de análisis: Enero 2024 - Agosto 2025\n\n")

# Limpiar y transformar datos
datos_clean <- datos_rfv %>%
  select(-1) %>%  # Eliminar columna índice
  rename_with(~str_replace_all(.x, "_", " "), -Marca_Vehiculo)

# Convertir a formato long para análisis temporal
datos_long <- datos_rfv %>%
  pivot_longer(cols = -Marca_Vehiculo, 
               names_to = "Periodo", 
               values_to = "Vehiculos_Registrados") %>%
  mutate(
    Año = case_when(
      str_detect(Periodo, "2024") ~ 2024,
      str_detect(Periodo, "2025") ~ 2025
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
      str_detect(Periodo, "Diciembre") ~ 12
    ),
    Fecha = make_date(Año, Mes, 1),
    Vehiculos_Registrados = replace_na(Vehiculos_Registrados, 0)
  ) %>%
  arrange(Marca_Vehiculo, Fecha)

# =============================================================================
# 2. ANÁLISIS DE MÉTRICAS CLAVE
# =============================================================================

# Calcular métricas por marca
metricas_marcas <- datos_long %>%
  group_by(Marca_Vehiculo) %>%
  summarise(
    Volumen_Inicial = first(Vehiculos_Registrados),
    Volumen_Final = last(Vehiculos_Registrados),
    Volumen_Maximo = max(Vehiculos_Registrados),
    Volumen_Promedio = mean(Vehiculos_Registrados),
    Crecimiento_Absoluto = Volumen_Final - Volumen_Inicial,
    Crecimiento_Relativo = ifelse(Volumen_Inicial > 0, 
                                  (Volumen_Final - Volumen_Inicial) / Volumen_Inicial * 100, 
                                  0),
    Volatilidad = sd(Vehiculos_Registrados, na.rm = TRUE),
    Coef_Variacion = ifelse(Volumen_Promedio > 0, Volatilidad / Volumen_Promedio, 0),
    .groups = "drop"
  ) %>%
  filter(Volumen_Final > 0) %>%
  mutate(
    Categoria_Volumen = case_when(
      Volumen_Final >= 100000 ~ "Alto Volumen (100K+)",
      Volumen_Final >= 10000 ~ "Volumen Medio (10K-100K)",
      Volumen_Final >= 1000 ~ "Volumen Bajo (1K-10K)",
      TRUE ~ "Volumen Mínimo (<1K)"
    ),
    Categoria_Crecimiento = case_when(
      Crecimiento_Relativo >= 20 ~ "Alto Crecimiento (20%+)",
      Crecimiento_Relativo >= 10 ~ "Crecimiento Moderado (10-20%)",
      Crecimiento_Relativo >= 0 ~ "Crecimiento Bajo (0-10%)",
      TRUE ~ "En Declive"
    ),
    Potencial_Radiadores = case_when(
      Volumen_Final >= 50000 & Crecimiento_Relativo >= 10 ~ "ALTA PRIORIDAD",
      Volumen_Final >= 10000 & Crecimiento_Relativo >= 15 ~ "MEDIA PRIORIDAD",
      Crecimiento_Relativo >= 30 & Volumen_Final >= 1000 ~ "EMERGENTE",
      TRUE ~ "BAJA PRIORIDAD"
    )
  )

# Estadísticas generales del mercado
cat("=== ESTADÍSTICAS GENERALES DEL MERCADO ===\n")
cat("Total vehículos registrados (Agosto 2025):", format(sum(metricas_marcas$Volumen_Final), big.mark = ","), "\n")
cat("Crecimiento total del mercado:", format(sum(metricas_marcas$Crecimiento_Absoluto), big.mark = ","), "vehículos\n")
cat("Crecimiento promedio del mercado:", round(sum(metricas_marcas$Crecimiento_Absoluto) / sum(metricas_marcas$Volumen_Inicial) * 100, 1), "%\n\n")

# =============================================================================
# 3. IDENTIFICACIÓN DE OPORTUNIDADES CLAVE
# =============================================================================

# Top marcas por diferentes criterios
top_volumen <- metricas_marcas %>%
  arrange(desc(Volumen_Final)) %>%
  head(15)

top_crecimiento_abs <- metricas_marcas %>%
  arrange(desc(Crecimiento_Absoluto)) %>%
  head(15)

top_crecimiento_rel <- metricas_marcas %>%
  filter(Volumen_Inicial >= 100) %>%  # Filtrar para evitar distorsiones
  arrange(desc(Crecimiento_Relativo)) %>%
  head(15)

marcas_emergentes <- metricas_marcas %>%
  filter(Volumen_Inicial >= 50, Volumen_Inicial <= 5000, Crecimiento_Relativo > 20) %>%
  arrange(desc(Crecimiento_Relativo)) %>%
  head(15)

marcas_declive <- metricas_marcas %>%
  filter(Volumen_Inicial >= 100) %>%
  arrange(Crecimiento_Relativo) %>%
  head(15)

# Análisis de segmentos estratégicos
segmentos_estrategicos <- metricas_marcas %>%
  group_by(Potencial_Radiadores) %>%
  summarise(
    Num_Marcas = n(),
    Volumen_Total = sum(Volumen_Final),
    Crecimiento_Promedio = mean(Crecimiento_Relativo),
    .groups = "drop"
  ) %>%
  arrange(desc(Volumen_Total))

# =============================================================================
# 4. VISUALIZACIONES AVANZADAS
# =============================================================================

# 1. Mapa de Calor - Volumen vs Crecimiento
p1 <- ggplot(metricas_marcas %>% filter(Volumen_Final >= 1000), 
             aes(x = log10(Volumen_Final), y = Crecimiento_Relativo)) +
  geom_point(aes(size = Volumen_Final, color = Potencial_Radiadores), alpha = 0.7) +
  geom_text(data = metricas_marcas %>% 
              filter(Potencial_Radiadores %in% c("ALTA PRIORIDAD", "MEDIA PRIORIDAD")),
            aes(label = Marca_Vehiculo), vjust = -0.5, size = 3) +
  scale_size_continuous(range = c(2, 12), guide = "none") +
  scale_color_manual(values = c("ALTA PRIORIDAD" = "#E31A1C", 
                                "MEDIA PRIORIDAD" = "#FF7F00",
                                "EMERGENTE" = "#1F78B4",
                                "BAJA PRIORIDAD" = "#CCCCCC")) +
  labs(title = "MAPA ESTRATÉGICO: Volumen vs Crecimiento",
       subtitle = "Identificación de oportunidades para radiadores",
       x = "Volumen de Vehículos (Log10)",
       y = "Crecimiento Relativo (%)",
       color = "Potencial para Radiadores") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        legend.position = "bottom")

# 2. Top 15 Marcas por Volumen
p2 <- top_volumen %>%
  mutate(Marca_Vehiculo = fct_reorder(Marca_Vehiculo, Volumen_Final)) %>%
  ggplot(aes(x = Marca_Vehiculo, y = Volumen_Final, fill = Categoria_Crecimiento)) +
  geom_col() +
  geom_text(aes(label = paste0(round(Crecimiento_Relativo, 1), "%")), 
            hjust = -0.1, size = 3) +
  scale_y_continuous(labels = label_number(scale = 1e-3, suffix = "K")) +
  scale_fill_viridis_d(option = "plasma") +
  coord_flip() +
  labs(title = "TOP 15 MARCAS POR VOLUMEN",
       subtitle = "Mercados establecidos con mayor potencial",
       x = "", y = "Vehículos Registrados (Miles)",
       fill = "Categoría de Crecimiento") +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold"))

# 3. Marcas Emergentes con Alto Potencial
p3 <- marcas_emergentes %>%
  mutate(Marca_Vehiculo = fct_reorder(Marca_Vehiculo, Crecimiento_Relativo)) %>%
  ggplot(aes(x = Marca_Vehiculo, y = Crecimiento_Relativo)) +
  geom_col(fill = "#1F78B4", alpha = 0.8) +
  geom_text(aes(label = paste0("+", format(Crecimiento_Absoluto, big.mark = ","))), 
            hjust = -0.1, size = 3) +
  coord_flip() +
  labs(title = "MARCAS EMERGENTES CON ALTO POTENCIAL",
       subtitle = "Oportunidades de crecimiento para radiadores",
       x = "", y = "Crecimiento Relativo (%)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold"))

# 4. Evolución Temporal de Top Marcas
top_5_marcas <- top_volumen$Marca_Vehiculo[1:5]
datos_top5 <- datos_long %>%
  filter(Marca_Vehiculo %in% top_5_marcas) %>%
  mutate(Vehiculos_Miles = Vehiculos_Registrados / 1000)

p4 <- ggplot(datos_top5, aes(x = Fecha, y = Vehiculos_Miles, color = Marca_Vehiculo)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_viridis_d(option = "turbo") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  labs(title = "EVOLUCIÓN TEMPORAL - TOP 5 MARCAS",
       subtitle = "Tendencias de crecimiento en el tiempo",
       x = "Período", y = "Vehículos Registrados (Miles)",
       color = "Marca") +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

# 5. Distribución por Categorías de Potencial
p5 <- metricas_marcas %>%
  count(Potencial_Radiadores) %>%
  mutate(Potencial_Radiadores = fct_reorder(Potencial_Radiadores, n)) %>%
  ggplot(aes(x = Potencial_Radiadores, y = n, fill = Potencial_Radiadores)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -0.3, size = 4, fontface = "bold") +
  scale_fill_manual(values = c("ALTA PRIORIDAD" = "#E31A1C", 
                               "MEDIA PRIORIDAD" = "#FF7F00",
                               "EMERGENTE" = "#1F78B4",
                               "BAJA PRIORIDAD" = "#CCCCCC")) +
  labs(title = "DISTRIBUCIÓN DE MARCAS POR POTENCIAL",
       subtitle = "Segmentación estratégica del mercado",
       x = "Categoría de Potencial", y = "Número de Marcas") +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.position = "none")

# Crear layout de gráficos
grid.arrange(p1, arrangeGrob(p2, p3, ncol = 2), p4, p5, 
             layout_matrix = rbind(c(1,1), c(2,2), c(3,3), c(4,4)),
             heights = c(1, 1, 1, 0.8))

# =============================================================================
# 6. ANÁLISIS PREDICTIVO Y FORECASTING
# =============================================================================

# Predicción para las top 5 marcas
predicciones <- data.frame()

for(marca in top_5_marcas) {
  datos_marca <- datos_long %>%
    filter(Marca_Vehiculo == marca) %>%
    arrange(Fecha)
  
  # Crear serie temporal
  ts_data <- ts(datos_marca$Vehiculos_Registrados, frequency = 12, start = c(2024, 1))
  
  # Modelo ARIMA
  modelo <- auto.arima(ts_data)
  forecast_resultado <- forecast(modelo, h = 6)  # 6 meses adelante
  
  # Almacenar predicciones
  pred_temp <- data.frame(
    Marca_Vehiculo = marca,
    Fecha = seq(as.Date("2025-09-01"), by = "month", length.out = 6),
    Prediccion = as.numeric(forecast_resultado$mean),
    Lower = as.numeric(forecast_resultado$lower[,2]),
    Upper = as.numeric(forecast_resultado$upper[,2])
  )
  
  predicciones <- rbind(predicciones, pred_temp)
}

# Visualizar predicciones
p_forecast <- datos_long %>%
  filter(Marca_Vehiculo %in% top_5_marcas, Fecha >= as.Date("2024-06-01")) %>%
  ggplot(aes(x = Fecha, y = Vehiculos_Registrados / 1000, color = Marca_Vehiculo)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_line(data = predicciones, 
            aes(x = Fecha, y = Prediccion / 1000), 
            linetype = "dashed", size = 1) +
  geom_ribbon(data = predicciones,
              aes(x = Fecha, ymin = Lower / 1000, ymax = Upper / 1000, fill = Marca_Vehiculo),
              alpha = 0.2, color = NA) +
  scale_color_viridis_d(option = "turbo") +
  scale_fill_viridis_d(option = "turbo") +
  labs(title = "PREDICCIONES A 6 MESES - TOP 5 MARCAS",
       subtitle = "Proyección de crecimiento hasta febrero 2026",
       x = "Período", y = "Vehículos Registrados (Miles)",
       color = "Marca", fill = "Marca") +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold"))

print(p_forecast)

# =============================================================================
# 7. RECOMENDACIONES ESTRATÉGICAS
# =============================================================================

cat("\n=== RECOMENDACIONES ESTRATÉGICAS PARA RADIADORES ===\n\n")

cat("🎯 ALTA PRIORIDAD (Inversión Inmediata):\n")
alta_prioridad <- metricas_marcas %>% filter(Potencial_Radiadores == "ALTA PRIORIDAD")
for(i in 1:min(5, nrow(alta_prioridad))) {
  marca <- alta_prioridad[i,]
  cat(sprintf("• %s: %s vehículos (+%.1f%% crecimiento)\n", 
              marca$Marca_Vehiculo, 
              format(marca$Volumen_Final, big.mark = ","),
              marca$Crecimiento_Relativo))
}

cat("\n📈 EMERGENTES (Oportunidad de Crecimiento):\n")
emergentes_top <- metricas_marcas %>% filter(Potencial_Radiadores == "EMERGENTE") %>% head(5)
for(i in 1:nrow(emergentes_top)) {
  marca <- emergentes_top[i,]
  cat(sprintf("• %s: %.1f%% crecimiento (%s vehículos)\n", 
              marca$Marca_Vehiculo,
              marca$Crecimiento_Relativo,
              format(marca$Volumen_Final, big.mark = ",")))
}

cat("\n⚠️ MARCAS EN DECLIVE (Reducir Inversión):\n")
for(i in 1:min(5, nrow(marcas_declive))) {
  marca <- marcas_declive[i,]
  cat(sprintf("• %s: %.1f%% declive (%s vehículos)\n", 
              marca$Marca_Vehiculo,
              marca$Crecimiento_Relativo,
              format(marca$Volumen_Final, big.mark = ",")))
}

# =============================================================================
# 8. TABLAS DE RESUMEN EJECUTIVO
# =============================================================================

# Tabla de segmentos estratégicos
cat("\n=== RESUMEN EJECUTIVO POR SEGMENTOS ===\n")
kable(segmentos_estrategicos, 
      col.names = c("Potencial", "# Marcas", "Volumen Total", "Crecimiento Promedio (%)"),
      format.args = list(big.mark = ",")) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# Top 10 oportunidades combinadas
top_oportunidades <- metricas_marcas %>%
  filter(Potencial_Radiadores %in% c("ALTA PRIORIDAD", "MEDIA PRIORIDAD", "EMERGENTE")) %>%
  arrange(desc(Volumen_Final * (1 + Crecimiento_Relativo/100))) %>%
  head(10) %>%
  select(Marca_Vehiculo, Volumen_Final, Crecimiento_Relativo, Potencial_Radiadores)

cat("\n=== TOP 10 OPORTUNIDADES PARA RADIADORES ===\n")
kable(top_oportunidades,
      col.names = c("Marca", "Volumen Actual", "Crecimiento (%)", "Categoría"),
      format.args = list(big.mark = ",")) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

cat("\n=== ANÁLISIS COMPLETADO ===\n")
cat("📊 Dataset procesado:", nrow(datos_rfv), "marcas\n")
cat("📈 Período analizado: 20 meses (Ene 2024 - Ago 2025)\n")
cat("🎯 Oportunidades identificadas:", nrow(alta_prioridad), "alta prioridad,", nrow(emergentes_top), "emergentes\n")
cat("💡 Recomendación: Priorizar inversión en marcas de alta prioridad y monitorear emergentes\n")
