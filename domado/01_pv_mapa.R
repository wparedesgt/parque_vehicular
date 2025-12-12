### carga de datos parque vehicular

casa <- 'C:/Users/wpare/Documents/William/Ciencia_Datos/parque_vehicular'
casa_ws <- 'H:/wparedes/Documentos/Ciencia_Datos/parque_vehicular'

setwd(casa_ws)
rm(list = ls())

library(tidyverse)
library(readr)
library(janitor)
library(readxl)


base_dir <- 'C:/Registro_Fiscal_Vehiculos'

#base_dir <- '/run/media/wparedes/wparedesBD/Registro_Fiscal_Vehiculos/'
archivo <- 'INE_PARQUE_VEHICULAR.txt'


# Mes más reciente a considerar: SIEMPRE el mes anterior al actual
ultimo_mes_disponible <- floor_date(Sys.Date(), "month") - months(1)


subcarpetas <- tail(list.dirs(base_dir, recursive = FALSE, full.names = FALSE) %>%
  keep(~ grepl("^\\d{4}-\\d{2}$", .x)), 1)

fechas <- ymd(paste0(subcarpetas, "-01"))
idx_validas <- which(!is.na(fechas) & fechas <= ultimo_mes_disponible)
subcarpetas <- subcarpetas[idx_validas]
fechas      <- fechas[idx_validas]

# Orden cronológico
ord <- order(fechas)
subcarpetas <- subcarpetas[ord]
fechas      <- fechas[ord]

stopifnot(length(subcarpetas) > 0)

# Etiquetas de meses en español para las columnas
meses_es <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio",
              "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")
mes_label <- function(d) paste0(meses_es[month(d)], "_", year(d))

ruta_archivo <- paste0(base_dir, '/', subcarpetas, '/', archivo)


f_lee_pv <- function(ruta){
  
  Parque_Veh <- read_delim(ruta,
                           delim = '|', col_names = TRUE, 
                           locale = readr::locale(encoding = readr::guess_encoding(ruta, n_max = 50000)[[1]] ))
  
  if ("...11" %in% names(Parque_Veh)) Parque_Veh$...11 <- NULL
  
  
  Parque_Veh$MARCA_VEHICULO <- str_replace_all(Parque_Veh$MARCA_VEHICULO, 'ASIA$', 'ASIA HERO')
  Parque_Veh$MARCA_VEHICULO <- str_replace_all(Parque_Veh$MARCA_VEHICULO, 'BUDD', 'BUD')
  Parque_Veh$MARCA_VEHICULO <- str_replace_all(Parque_Veh$MARCA_VEHICULO, 'CHANGAN BRAND', 'CHANGAN')
  Parque_Veh$MARCA_VEHICULO <- str_replace_all(Parque_Veh$MARCA_VEHICULO, 'DONG FENG', 'DONGFENG')
  Parque_Veh$MARCA_VEHICULO <- str_replace_all(Parque_Veh$MARCA_VEHICULO, 'FAW JIABAO', 'FAW')
  Parque_Veh$MARCA_VEHICULO <- str_replace_all(Parque_Veh$MARCA_VEHICULO, 'FREUHAUF', 'FRUEHAUF')
  Parque_Veh$MARCA_VEHICULO <- str_replace_all(Parque_Veh$MARCA_VEHICULO, 'HOBS', 'HOBBS')
  Parque_Veh$MARCA_VEHICULO <- str_replace_all(Parque_Veh$MARCA_VEHICULO, 'MITSUBISHI FUSO', 'MITSUBISHI')
  Parque_Veh$MARCA_VEHICULO <- str_replace_all(Parque_Veh$MARCA_VEHICULO, 'TRAIL MOBIL', 'TRAILMOBILE')
  Parque_Veh$MARCA_VEHICULO <- str_replace_all(Parque_Veh$MARCA_VEHICULO, 'TRANS CRAFT', 'TRANSCRAFT')
  Parque_Veh$MARCA_VEHICULO <- str_replace_all(Parque_Veh$MARCA_VEHICULO, 'UD$', 'UD TRUCKS')
  Parque_Veh$MARCA_VEHICULO <- str_replace_all(Parque_Veh$MARCA_VEHICULO, 'UD NISSAN', 'UD TRUCKS')
  Parque_Veh$MARCA_VEHICULO <- str_replace_all(Parque_Veh$MARCA_VEHICULO, 'UD/NISSAN', 'UD TRUCKS')
  Parque_Veh$MARCA_VEHICULO <- str_replace_all(Parque_Veh$MARCA_VEHICULO, 'WHITE$', 'WHITE GMC')
  Parque_Veh$MARCA_VEHICULO <- str_replace_all(Parque_Veh$MARCA_VEHICULO, 'WHITEGMC$', 'WHITE GMC')
  Parque_Veh$MARCA_VEHICULO <- str_replace_all(Parque_Veh$MARCA_VEHICULO, 'BOYD$', 'BYD')
  Parque_Veh$MARCA_VEHICULO <- str_replace_all(Parque_Veh$MARCA_VEHICULO, 'DATSUN NISSAN', 'DATSUN')
  Parque_Veh$NOMBRE_MUNICIPIO <- str_replace_all(Parque_Veh$NOMBRE_MUNICIPIO, 'Á', 'A')
  Parque_Veh$NOMBRE_MUNICIPIO <- str_replace_all(Parque_Veh$NOMBRE_MUNICIPIO, 'É', 'E')
  Parque_Veh$NOMBRE_MUNICIPIO <- str_replace_all(Parque_Veh$NOMBRE_MUNICIPIO, 'Í', 'I')
  Parque_Veh$NOMBRE_MUNICIPIO <- str_replace_all(Parque_Veh$NOMBRE_MUNICIPIO, 'Ó', 'O')
  Parque_Veh$NOMBRE_MUNICIPIO <- str_replace_all(Parque_Veh$NOMBRE_MUNICIPIO, 'Ú', 'U')
  
  return(Parque_Veh)
  
}

rfv_total <- f_lee_pv(ruta_archivo)
geolocalizacion <- read.csv('H:/catalogos/gt_geolocalizacion.csv', 
                            header = TRUE, 
                            sep = ';')

geolocalizacion$city <- str_replace_all(geolocalizacion$city, 'á', 'a')
geolocalizacion$city <- str_replace_all(geolocalizacion$city, 'é', 'e')
geolocalizacion$city <- str_replace_all(geolocalizacion$city, 'í', 'i')
geolocalizacion$city <- str_replace_all(geolocalizacion$city, 'ó', 'o')
geolocalizacion$city <- str_replace_all(geolocalizacion$city, 'ú', 'u')
geolocalizacion$city <- str_replace_all(geolocalizacion$city, 'Í', 'I')
geolocalizacion$admin_name <- str_replace_all(geolocalizacion$admin_name,'á', 'a')
geolocalizacion$admin_name <- str_replace_all(geolocalizacion$admin_name,'é', 'e')
geolocalizacion$admin_name <- str_replace_all(geolocalizacion$admin_name,'í', 'i')
geolocalizacion$admin_name <- str_replace_all(geolocalizacion$admin_name,'ó', 'o')
geolocalizacion$admin_name <- str_replace_all(geolocalizacion$admin_name,'ú', 'u')

geolocalizacion <- geolocalizacion %>% 
  select(NOMBRE_MUNICIPIO = city, LAT = lat, LNG = lng, NOMBRE_DEPARTAMENTO = admin_name, 
         CODIGO_DEPARTAMENTO = codigo_departamento, CODIGO_MUNICIPIO = codigo_municipio)

geolocalizacion$NOMBRE_MUNICIPIO <- str_replace_all(geolocalizacion$NOMBRE_MUNICIPIO, 'Guatemala City', 'Guatemala')
geolocalizacion$NOMBRE_MUNICIPIO <- toupper(geolocalizacion$NOMBRE_MUNICIPIO)
geolocalizacion$NOMBRE_DEPARTAMENTO <- toupper(geolocalizacion$NOMBRE_DEPARTAMENTO)

muestra_int <- merge(rfv_total, geolocalizacion, 
                     by.x = c('NOMBRE_DEPARTAMENTO', 'NOMBRE_MUNICIPIO'), 
                     by.y = c('NOMBRE_DEPARTAMENTO', 'NOMBRE_MUNICIPIO'))


resumen_anual <- muestra_int %>% 
  group_by(ANIO_ALZA) %>% 
  summarise(
    TOTAL_CANTIDAD = sum(CANTIDAD, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  arrange(ANIO_ALZA)



saveRDS(muestra_int, 'datos/rfv_w_geo.rds')
