### carga de datos parque vehicular

casa <- 'C:/Users/wpare/Documents/William/Ciencia_Datos/parque_vehicular'
casa_ws <- 'H:/wparedes/Documentos/Ciencia_Datos/parque_vehicular'

setwd(casa_ws)
rm(list = ls())

options(scipen = 999)

# Librerías necesarias
library(tidyverse)
library(plotly)
library(leaflet)
library(DT)
library(sf)
library(viridis)
library(scales)
library(htmlwidgets)

normalizar_texto <- function(x) {
  x %>%
    toupper() %>%
    stringr::str_replace_all(c(
      "Á"="A","É"="E","Í"="I","Ó"="O","Ú"="U","Ü"="U","Ñ"="N"
    )) %>%
    stringr::str_squish()
}

rfv_w_geo <- readRDS('datos/rfv_w_geo.rds')

# Cargar el Shapefile de Guatemala
departamentos_shp <- st_read("shapes/gadm41_GTM_1.shp", quiet = TRUE)
departamentos_shp <- st_transform(departamentos_shp, crs = 4326)  # Convertir a WGS84
municipios_shp <- st_read("shapes/gadm41_GTM_2.shp", quiet = TRUE)

#Corrigiendo Error en Dpto.
municipios_shp$NAME_1 <- str_replace_all(municipios_shp$NAME_1, 'Quezaltenango', 'Quetzaltenango')
municipios_shp$NAME_2 <- str_replace_all(municipios_shp$NAME_2, 'Ostuncalco', str_to_title('SAN JUAN OSTUNCALCO'))
municipios_shp$NAME_2 <- str_replace_all(municipios_shp$NAME_2, 'Santa Cruz El Chol', str_to_title('EL CHOL'))
municipios_shp$NAME_2 <- str_replace_all(municipios_shp$NAME_2, 'Petapa', str_to_title('SAN MIGUEL PETAPA'))
municipios_shp$NAME_2 <- str_replace_all(municipios_shp$NAME_2, 'San Sibinal', str_to_title('Sibinal'))



index_solola <- which(municipios_shp$NAME_2 == '?' & municipios_shp$NAME_1 == 'Sololá')
municipios_shp$NAME_2[index_solola] <- 'Solola'



municipios_shp_muni <- municipios_shp %>%
  st_transform(4326) %>%
  mutate(
    DEPTO_N = normalizar_texto(NAME_1),
    MUNI_N  = normalizar_texto(NAME_2),
    MUNI_BASE = stringr::str_replace(MUNI_N, "^ZONA\\s*\\d+\\s*([,\\-–—])?\\s*", ""),
    MUNI_BASE = stringr::str_squish(MUNI_BASE)
  ) %>%
  group_by(DEPTO_N, MUNI_BASE) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

# --- SU corrección de Guatemala (se deja tal cual) ---
index_err <- which(municipios_shp_muni$MUNI_BASE == '')
municipios_shp_muni$MUNI_BASE[index_err] <- 'GUATEMALA'

# Agregando area
municipios_shp_muni <- municipios_shp_muni %>%
  st_transform(32615) %>%                          # metros (UTM 15N)
  mutate(AREA_KM2 = as.numeric(st_area(.)) / 1e6) %>%  # km²
  st_transform(4326)                               # volver a WGS84 (leaflet)

# =====================================================
# (A) ESCALA FIJA GLOBAL (esto evita que el "rojo" cambie por año)
# =====================================================

# 1) Densidad global (en TODO el histórico) para definir bins/paleta UNA sola vez
rfv_muni_all <- rfv_w_geo %>%
  filter(USO_VEHICULO == "PARTICULAR") %>%
  group_by(NOMBRE_DEPARTAMENTO, NOMBRE_MUNICIPIO) %>%
  summarise(CANTIDAD = sum(CANTIDAD, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    DEPTO_N   = normalizar_texto(NOMBRE_DEPARTAMENTO),
    MUNI_BASE = normalizar_texto(NOMBRE_MUNICIPIO)
  ) %>%
  select(DEPTO_N, MUNI_BASE, CANTIDAD)

muni_all <- municipios_shp_muni %>%
  left_join(rfv_muni_all, by = c("DEPTO_N", "MUNI_BASE")) %>%
  mutate(
    CANTIDAD = tidyr::replace_na(CANTIDAD, 0),
    DENS_VEH_KM2 = if_else(AREA_KM2 > 0, CANTIDAD / AREA_KM2, NA_real_)
  )

# 2) Cap P99 opcional para que la capital no aplaste la escala (recomendado)
cap_sup <- quantile(muni_all$DENS_VEH_KM2, 0.99, na.rm = TRUE)
muni_all <- muni_all %>% mutate(DENS_CAP = pmin(DENS_VEH_KM2, cap_sup))

bins <- quantile(muni_all$DENS_CAP, probs = seq(0, 1, 0.1), na.rm = TRUE)
bins <- unique(bins)
if (length(bins) < 2) bins <- c(0, max(muni_all$DENS_CAP, na.rm = TRUE))

pal <- colorBin(
  palette = c("green", "yellow", "red"),
  domain  = muni_all$DENS_CAP,
  bins    = bins,
  na.color = "transparent"
)

# =====================================================
# (B) SU FILTRO POR AÑOS (se mantiene) + mapa
# =====================================================

anio_inicio <- 1980
anio_fin    <- 2010
fil_uso_vehiculo <- c(unique(rfv_w_geo$USO_VEHICULO))
vector_fechas <- c(anio_inicio:anio_fin)

rfv_muni <- rfv_w_geo %>%
  filter(ANIO_ALZA %in% vector_fechas,
         USO_VEHICULO %in% fil_uso_vehiculo) %>%
  group_by(NOMBRE_DEPARTAMENTO, NOMBRE_MUNICIPIO) %>%
  summarise(CANTIDAD = sum(CANTIDAD, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    DEPTO_N   = normalizar_texto(NOMBRE_DEPARTAMENTO),
    MUNI_BASE = normalizar_texto(NOMBRE_MUNICIPIO)
  ) %>%
  select(DEPTO_N, MUNI_BASE, CANTIDAD)

muni_veh_area <- municipios_shp_muni %>%
  left_join(rfv_muni, by = c("DEPTO_N", "MUNI_BASE")) %>%
  mutate(
    CANTIDAD = tidyr::replace_na(CANTIDAD, 0),
    DENS_VEH_KM2 = if_else(AREA_KM2 > 0, CANTIDAD / AREA_KM2, NA_real_),
    DENS_CAP = pmin(DENS_VEH_KM2, cap_sup)
  )

popup <- ~paste0(
  "<b>", MUNI_BASE, ", ", DEPTO_N, "</b><br>",
  "Años: ", anio_inicio, " - ", anio_fin, "<br>",
  "Vehículos: ", scales::comma(CANTIDAD), "<br>",
  "Área: ", round(AREA_KM2, 2), " km²<br>",
  "Densidad: ", round(DENS_VEH_KM2, 1), " veh/km²"
)

leaflet(muni_veh_area) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng = -90.5, lat = 15.5, zoom = 7) %>%
  addPolygons(
    fillColor = ~pal(DENS_CAP),
    fillOpacity = 0.75,
    color = "black",
    weight = 0.6,
    popup = popup
  ) %>%
  addLegend("bottomright",
            pal = pal, values = ~DENS_CAP,
            title = "Vehículos por km² (escala fija, cap P99)", opacity = 1)




