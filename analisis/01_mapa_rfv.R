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


rfv_w_geo <- readRDS('datos/rfv_w_geo.rds')
# Cargar el Shapefile de Guatemala
departamentos_shp <- st_read("shapes/gadm41_GTM_1.shp")
departamentos_shp <- st_transform(departamentos_shp, crs = 4326)  # Convertir a WGS84
municipios_shp <- st_read("shapes/gadm41_GTM_2.shp")

setdiff(rfv_w_geo$USO_VEHICULO)
unique(rfv_w_geo$USO_VEHICULO)

