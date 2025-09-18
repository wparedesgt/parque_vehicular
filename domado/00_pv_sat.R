### carga de datos parque vehicular

remoto <- "E:/wparedes/Documentos/Ciencia_Datos/Radiadores_La_Torre/Forecast_Inventario_RealCaruz/Estretegia_Comercial"
oficina <- '/home/wparedes/Documentos/Ciencia_Datos/Radiadores_La_Torre/Forecast_Inventario_RealCaruz/02_Estrategia_Comercial'

setwd(oficina)
rm(list = ls())

library(tidyverse)
library(readr)
library(janitor)
library(readxl)

### Prueba de codificacion ###

ruta <- '/run/media/wparedes/wparedesBD/Parque_Vehicular/informacion_est/INE_PARQUE_VEHICULAR.txt'

# (Opcional) Detectar codificación probable en una muestra
readr::guess_encoding(ruta, n_max = 50000)[[1]]
#> suele devolver "WINDOWS-1252" o "ISO-8859-1"


############ Carga de Datos  ###############

Parque_Veh <- read_delim('/run/media/wparedes/wparedesBD/Parque_Vehicular/informacion_est/INE_PARQUE_VEHICULAR.txt',
                         delim = '|', col_names = TRUE, 
                         locale = readr::locale(encoding = readr::guess_encoding(ruta, n_max = 50000)[[1]] ))

Parque_Veh <- Parque_Veh %>% filter(!TIPO_VEHICULO %in% c('MOTOCICLETA', 'MOTO', 'PLATAFORMA', 
                                                         'PORTA CONTENEDOR'))

Parque_Veh <- Parque_Veh %>% filter(!USO_VEHICULO %in% c('MOTOCICLETA'))


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



Marcas_men_100 <- Parque_Veh %>% 
  group_by(MARCA_VEHICULO) %>% 
  summarise(total = sum(CANTIDAD))  %>% 
  filter(total <= 100)


Marcas <- Parque_Veh %>% 
  group_by(MARCA_VEHICULO) %>% 
  summarise(total = sum(CANTIDAD)) %>% 
  filter(total >= 100)
  
Marcas <- Marcas %>% 
  filter(!MARCA_VEHICULO %in% c('RIVAS Y DISEÑO', 'SERVI METAL', 'SERVIMETAL Y DISEÑO', 'SLOWING Y DISEÑO', 
                                'ING CONCEPCION', 'ING MADRE TIERRA', 'ING PANTALEON', 'GONZALES', 
                                "RUBEN'S Y DISEÑO", 'SIN MARCA'))




