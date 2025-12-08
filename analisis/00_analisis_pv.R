# =============================================================================
# ANÁLISIS DE MERCADO AUTOMOTRIZ 
# =============================================================================
# Científico de Datos: Análisis Profundo del Registro Fiscal de Vehículos
# Período: Enero 2024 - Noviembre 2025
# =============================================================================

casa <- 'C:/Users/wpare/Documents/William/Ciencia_Datos/parque_vehicular'
casa_ws <- 'H:/wparedes/Documentos/Ciencia_Datos/parque_vehicular'

setwd(casa_ws)
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
cat("Total vehículos registrados (Noviembre 2025):", format(sum(metricas_marcas$Volumen_Final), big.mark = ","), "\n")
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

# 1. Mapa de Calor Interactivo - Volumen vs Crecimiento
p1 <- metricas_marcas %>% 
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

# Mostrar la gráfica interactiva
p1

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

p2

# 3. Marcas Emergentes con Alto Potencial - Interactivo
p3 <- marcas_emergentes %>%
  arrange(Crecimiento_Relativo) %>%  # Ordenar de menor a mayor
  mutate(
    etiqueta_crecimiento = paste0("+", format(Crecimiento_Absoluto, big.mark = ","))
  ) %>%
  plot_ly(
    y = ~factor(Marca_Vehiculo, levels = Marca_Vehiculo),  # Mantener orden
    x = ~Crecimiento_Relativo,
    type = "bar",
    orientation = "h",
    marker = list(
      color = "#1F78B4",
      opacity = 0.8,
      line = list(color = "#0F4C75", width = 1)
    ),
    text = ~paste0("Marca: ", Marca_Vehiculo,
                   "<br>Crecimiento: ", round(Crecimiento_Relativo, 1), "%",
                   "<br>Volumen Actual: ", format(Volumen_Final, big.mark = ","),
                   "<br>Crecimiento Absoluto: +", format(Crecimiento_Absoluto, big.mark = ",")),
    hovertemplate = "%{text}<extra></extra>",
    showlegend = FALSE
  ) %>%
  add_annotations(
    y = ~factor(Marca_Vehiculo, levels = Marca_Vehiculo),
    x = ~Crecimiento_Relativo + 2,
    text = ~etiqueta_crecimiento,
    showarrow = FALSE,
    font = list(size = 10, color = "#000000"),
    xanchor = "left"
  ) %>%
  layout(
    title = list(
      text = "<b>MARCAS EMERGENTES CON ALTO POTENCIAL</b><br><sub>Oportunidades de crecimiento para radiadores</sub>",
      font = list(size = 14, color = "#000000", family = "Arial")
    ),
    xaxis = list(
      title = "Crecimiento Relativo (%)",
      showgrid = TRUE,
      gridcolor = "#E0E0E0",
      titlefont = list(color = "#000000", size = 12),
      tickfont = list(color = "#000000", size = 10)
    ),
    yaxis = list(
      title = "",
      showgrid = FALSE,
      titlefont = list(color = "#000000", size = 12),
      tickfont = list(color = "#000000", size = 10)
    ),
    plot_bgcolor = "#FAFAFA",
    paper_bgcolor = "#FFFFFF",
    margin = list(l = 150, r = 100, t = 80, b = 50),
    hovermode = "y"
  )

# Mostrar la gráfica interactiva
p3

# 4. Evolución Temporal de Top Marcas - Interactivo
top_5_marcas <- top_volumen$Marca_Vehiculo[1:5]
datos_top5 <- datos_long %>%
  filter(Marca_Vehiculo %in% top_5_marcas) %>%
  mutate(Vehiculos_Miles = Vehiculos_Registrados / 1000)

p4 <- datos_top5 %>%
  plot_ly(
    x = ~Fecha,
    y = ~Vehiculos_Miles,
    color = ~Marca_Vehiculo,
    colors = viridis::turbo(5),
    type = "scatter",
    mode = "lines+markers",
    line = list(width = 3),
    marker = list(size = 6, opacity = 0.8),
    text = ~paste0("Marca: ", Marca_Vehiculo,
                   "<br>Fecha: ", format(Fecha, "%b %Y"),
                   "<br>Vehículos: ", format(round(Vehiculos_Miles, 1), big.mark = ","), "K",
                   "<br>Registrados: ", format(Vehiculos_Registrados, big.mark = ",")),
    hovertemplate = "%{text}<extra></extra>"
  ) %>%
  layout(
    title = list(
      text = "<b>EVOLUCIÓN TEMPORAL - TOP 5 MARCAS</b><br><sub>Tendencias de crecimiento en el tiempo</sub>",
      font = list(size = 14, color = "#000000", family = "Arial")
    ),
    xaxis = list(
      title = "Período",
      showgrid = TRUE,
      gridcolor = "#E0E0E0",
      titlefont = list(color = "#000000", size = 12),
      tickfont = list(color = "#000000", size = 10),
      tickangle = -45,
      dtick = "M3"  # Cada 3 meses
    ),
    yaxis = list(
      title = "Vehículos Registrados (Miles)",
      showgrid = TRUE,
      gridcolor = "#E0E0E0",
      titlefont = list(color = "#000000", size = 12),
      tickfont = list(color = "#000000", size = 10)
    ),
    legend = list(
      title = list(text = "<b>Marca</b>"),
      orientation = "v",
      x = 1.02,
      y = 1,
      font = list(color = "#000000", size = 10)
    ),
    plot_bgcolor = "#FAFAFA",
    paper_bgcolor = "#FFFFFF",
    hovermode = "x unified",
    margin = list(l = 80, r = 120, t = 80, b = 80)
  )

# Mostrar la gráfica interactiva
p4

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

p5

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

# Datos históricos filtrados
datos_historicos <- datos_long %>%
  filter(Marca_Vehiculo %in% top_5_marcas, Fecha >= as.Date("2024-06-01")) %>%
  mutate(Vehiculos_Miles = Vehiculos_Registrados / 1000)

# Colores viridis turbo para consistencia
colores_marcas <- viridis::turbo(5)
names(colores_marcas) <- top_5_marcas

# Crear gráfico base
p_forecast <- plot_ly()

# Agregar datos históricos (líneas sólidas + puntos)
for(i in 1:length(top_5_marcas)) {
  marca <- top_5_marcas[i]
  datos_marca <- datos_historicos %>% filter(Marca_Vehiculo == marca)
  
  p_forecast <- p_forecast %>%
    add_trace(
      data = datos_marca,
      x = ~Fecha,
      y = ~Vehiculos_Miles,
      type = "scatter",
      mode = "lines+markers",
      line = list(color = colores_marcas[i], width = 3),
      marker = list(color = colores_marcas[i], size = 6),
      name = marca,
      legendgroup = marca,
      text = ~paste0("Marca: ", Marca_Vehiculo,
                     "<br>Fecha: ", format(Fecha, "%b %Y"),
                     "<br>Vehículos: ", format(round(Vehiculos_Miles, 1), big.mark = ","), "K"),
      hovertemplate = "%{text}<extra></extra>"
    )
}

# Agregar predicciones (líneas punteadas)
for(i in 1:length(top_5_marcas)) {
  marca <- top_5_marcas[i]
  datos_pred <- predicciones %>% filter(Marca_Vehiculo == marca)
  
  p_forecast <- p_forecast %>%
    add_trace(
      data = datos_pred,
      x = ~Fecha,
      y = ~Prediccion / 1000,
      type = "scatter",
      mode = "lines",
      line = list(color = colores_marcas[i], width = 3, dash = "dash"),
      name = paste(marca, "(Predicción)"),
      legendgroup = marca,
      showlegend = FALSE,
      text = ~paste0("Marca: ", Marca_Vehiculo,
                     "<br>Fecha: ", format(Fecha, "%b %Y"),
                     "<br>Predicción: ", format(round(Prediccion / 1000, 1), big.mark = ","), "K",
                     "<br>Rango: ", format(round(Lower / 1000, 1), big.mark = ","), "K - ", 
                     format(round(Upper / 1000, 1), big.mark = ","), "K"),
      hovertemplate = "%{text}<extra></extra>"
    )
}

# Agregar intervalos de confianza (ribbons)
for(i in 1:length(top_5_marcas)) {
  marca <- top_5_marcas[i]
  datos_pred <- predicciones %>% filter(Marca_Vehiculo == marca)
  
  p_forecast <- p_forecast %>%
    add_ribbons(
      data = datos_pred,
      x = ~Fecha,
      ymin = ~Lower / 1000,
      ymax = ~Upper / 1000,
      fillcolor = colores_marcas[i],
      opacity = 0.2,
      line = list(color = "transparent"),
      name = paste(marca, "(Intervalo)"),
      legendgroup = marca,
      showlegend = FALSE,
      hoverinfo = "none"
    )
}

# Layout final
p_forecast <- p_forecast %>%
  layout(
    title = list(
      text = "<b>PREDICCIONES A 6 MESES - TOP 5 MARCAS</b><br><sub>Proyección de crecimiento hasta febrero 2026</sub>",
      font = list(size = 14, color = "#000000", family = "Arial")
    ),
    xaxis = list(
      title = "Período",
      showgrid = TRUE,
      gridcolor = "#E0E0E0",
      titlefont = list(color = "#000000", size = 12),
      tickfont = list(color = "#000000", size = 10)
    ),
    yaxis = list(
      title = "Vehículos Registrados (Miles)",
      showgrid = TRUE,
      gridcolor = "#E0E0E0",
      titlefont = list(color = "#000000", size = 12),
      tickfont = list(color = "#000000", size = 10)
    ),
    legend = list(
      title = list(text = "<b>Marca</b>"),
      orientation = "v",
      x = 1.02,
      y = 1,
      font = list(color = "#000000", size = 10)
    ),
    plot_bgcolor = "#FAFAFA",
    paper_bgcolor = "#FFFFFF",
    hovermode = "x unified",
    margin = list(l = 80, r = 120, t = 80, b = 80)
  )

# Mostrar la gráfica
p_forecast

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
