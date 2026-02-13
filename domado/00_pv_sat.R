### carga de datos parque vehicular

casa <- 'C:/Users/wpare/Documents/William/Ciencia_Datos/parque_vehicular'
casa_ws <- 'H:/wparedes/Documentos/Ciencia_Datos/parque_vehicular'

setwd(casa_ws)
rm(list = ls())

library(tidyverse)
library(readr)
library(janitor)
library(readxl)


# # (Opcional) Detectar codificación probable en una muestra
# readr::guess_encoding(ruta, n_max = 50000)[[1]]
# #> suele devolver "WINDOWS-1252" o "ISO-8859-1"

############ Carga de Datos  ###############

f_lee_pv <- function(ruta){

Parque_Veh <- read_delim(ruta,
                         delim = '|', col_names = TRUE, 
                         locale = readr::locale(encoding = readr::guess_encoding(ruta, n_max = 50000)[[1]] ))

if ("...11" %in% names(Parque_Veh)) Parque_Veh$...11 <- NULL
#Parque_Veh$...11 <- NULL

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

Marcas <- Parque_Veh %>% 
  group_by(MARCA_VEHICULO) %>% 
  summarise(total = sum(CANTIDAD)) %>% 
  filter(total >= 100)
  
Marcas <- Marcas %>% 
  filter(!MARCA_VEHICULO %in% c('RIVAS Y DISEÑO', 'SERVI METAL', 'SERVIMETAL Y DISEÑO', 'SLOWING Y DISEÑO', 
                                'ING CONCEPCION', 'ING MADRE TIERRA', 'ING PANTALEON', 'GONZALES', 
                                "RUBEN'S Y DISEÑO", 'SIN MARCA'))

return(Marcas)

}

##### Construye DF con todas las fechas ######

# ### Prueba de codificacion ###
# 
# archivo <- 'INE_PARQUE_VEHICULAR.txt'
# carpeta <- '2024-01'
# ruta <- paste0('C:/Registro_Fiscal_Vehiculos', '/', carpeta, '/', archivo) 

base_dir <- 'C:/Registro_Fiscal_Vehiculos'

#base_dir <- '/run/media/wparedes/wparedesBD/Registro_Fiscal_Vehiculos/'
archivo <- 'INE_PARQUE_VEHICULAR.txt'


# Mes más reciente a considerar: SIEMPRE el mes anterior al actual
ultimo_mes_disponible <- floor_date(Sys.Date(), "month") - months(1)

# Detectar subcarpetas válidas con formato YYYY-MM
subcarpetas <- list.dirs(base_dir, recursive = FALSE, full.names = FALSE) %>%
  keep(~ grepl("^\\d{4}-\\d{2}$", .x))

# Parsear las fechas de esas carpetas y quedarnos solo con <= último mes disponible
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

# Leer cada archivo y acumular en formato largo
registros_largos <- purrr::map2_dfr(subcarpetas, fechas, function(carp, fch){
  ruta_archivo <- file.path(base_dir, carp, "INE_PARQUE_VEHICULAR.txt")
  if(!file.exists(ruta_archivo)) return(NULL)
  df <- tryCatch(f_lee_pv(ruta_archivo), error = function(e) NULL)
  if(is.null(df) || !nrow(df)) return(NULL)
  df %>%
    dplyr::rename(Marca_Vehiculo = MARCA_VEHICULO, Unidades = total) %>%
    mutate(Periodo = fch, Columna = mes_label(Periodo))
})

# Por si algún mes no devolvió filas (p. ej., todo < 100 uds), evitar error
stopifnot(nrow(registros_largos) > 0)

# Orden esperado de columnas mes-a-mes
orden_columnas <- c("Marca_Vehiculo", unique(sapply(fechas, mes_label)))

# Pivot a formato ancho con ceros cuando falte una marca en un mes
muestra <- registros_largos %>%
  group_by(Marca_Vehiculo, Columna) %>%
  summarise(Unidades = sum(Unidades, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = Columna, values_from = Unidades, values_fill = 0) %>%
  # reordenar columnas: Marca_Vehiculo + meses en orden cronológico
  dplyr::select(all_of(orden_columnas)) %>%
  arrange(Marca_Vehiculo)


saveRDS(muestra, 'datos/rfv.rds')
#write.csv(muestra, 'datos/rfv.csv')

# Chequeo rápido en consola
cat("Meses incluidos:\n"); print(unique(sapply(fechas, mes_label)))
cat("Dimensiones de la tabla final (filas x columnas): ", paste(dim(muestra), collapse=" x "), "\n")
print(head(muestra, 10))


