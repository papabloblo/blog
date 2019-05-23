#' 
#' SE IMPORTAN LA IDEOLOGÍA DE LOS PARTIDOS POLÍTICOS
#' A PARTIR DE LOS .XLS EXTRAÍDOS MANUALMENTE DEL CIS.
#' 
#' Se obtiene un único tibble almacenado en 
#'   cis_ideologia/data/ideologia_partidos.rds


# DEPENDENCIAS ------------------------------------------------------------

library(tidyverse)
source("cis_ideologia/src/01_etl/utils.R")


# IMPORTACIÓN -------------------------------------------------------------

# Aunque se han descargado más archivos anteriores a las elecciones del 2015,
# estos no siguen la misma estructura.
# Por eso solo se importan 2015, 2016 y 2019.
rutas <- list.files("cis_ideologia/data/raw",
                    pattern = "201[569]_Escala_de_ubicacion_ideologica_1-10_de_partidos_politicos.xls",
                    full.names = TRUE)

purrr::map_df(rutas, 
           function(x){
             importar_ideologia_partidos(x) %>% 
               mutate(anyo = as.integer(stringr::str_extract(x, "(?<=/)[:digit:]+(?=_)")))
             }
           ) %>% 
  
  # Reordenación lógica de variables
  dplyr::select( 
    anyo,
    partido, 
    ideologia, 
    porc, 
    porc_contestadas
  ) %>% 
  write_rds("cis_ideologia/data/ideologia_partidos.rds")