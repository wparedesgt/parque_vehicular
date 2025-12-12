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
library(leaflet)

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
departamentos_shp <- st_read("shapes/gadm41_GTM_1.shp")
departamentos_shp <- st_transform(departamentos_shp, crs = 4326)  # Convertir a WGS84
municipios_shp <- st_read("shapes/gadm41_GTM_2.shp")

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




#Agregando area

municipios_shp_muni <- municipios_shp_muni %>%
  st_transform(32615) %>%                          # metros (UTM 15N)
  mutate(AREA_KM2 = as.numeric(st_area(.)) / 1e6) %>%  # km²
  st_transform(4326)                               # volver a WGS84 (leaflet)



####Vehiculos por municipio

anio_inicio <- 2024
anio_fin    <- 2025
fil_uso_vehiculo <- "PARTICULAR"

rfv_muni <- rfv_w_geo %>%
  filter(ANIO_ALZA %in% c(2024, 2025),
         USO_VEHICULO == "PARTICULAR") %>%
  group_by(NOMBRE_DEPARTAMENTO, NOMBRE_MUNICIPIO) %>%
  summarise(CANTIDAD = sum(CANTIDAD, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    DEPTO_N    = normalizar_texto(NOMBRE_DEPARTAMENTO),
    MUNI_BASE  = normalizar_texto(NOMBRE_MUNICIPIO)
  ) %>%
  select(DEPTO_N, MUNI_BASE, CANTIDAD)

muni_veh_area <- municipios_shp_muni %>%
  left_join(rfv_muni, by = c("DEPTO_N", "MUNI_BASE")) %>%
  mutate(
    CANTIDAD = tidyr::replace_na(CANTIDAD, 0),
    DENS_VEH_KM2 = if_else(AREA_KM2 > 0, CANTIDAD / AREA_KM2, NA_real_)
  )


bins <- quantile(muni_veh_area$DENS_VEH_KM2, probs = seq(0, 1, 0.1), na.rm = TRUE)
bins <- unique(bins)

pal <- colorBin(
  palette = c("green","yellow","red"),
  domain  = muni_veh_area$DENS_VEH_KM2,
  bins    = bins,
  na.color = "transparent"
)


popup <- ~paste0(
  "<b>", MUNI_BASE, ", ", DEPTO_N, "</b><br>",
  "Vehículos: ", scales::comma(CANTIDAD), "<br>",
  "Área: ", round(AREA_KM2, 2), " km²<br>",
  "Densidad: ", round(DENS_VEH_KM2, 1), " veh/km²"
)


leaflet(muni_veh_area) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng = -90.5, lat = 15.5, zoom = 7) %>%
  addPolygons(
    fillColor = ~pal(DENS_VEH_KM2),
    fillOpacity = 0.75,
    color = "black",
    weight = 0.6,
    popup = popup
  ) %>%
  addLegend("bottomright", pal = pal, values = ~DENS_VEH_KM2,
            title = "Vehículos por km²", opacity = 1)


municipios_shp %>%
  st_drop_geometry() %>%
  filter(normalizar_texto(NAME_1) == "GUATEMALA") %>%
  distinct(NAME_2) %>%
  head(40)
Vas a ver exactamente cómo están escritos los “ZONA …”.


