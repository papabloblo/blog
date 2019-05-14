#' 
#' Lectura de los archivos PROV_02_*_1.xlsx
#' con la información histórica
#' de las elecciones.
#' 
#' Los Excel se han extraído de
#' http://www.infoelectoral.mir.es/infoelectoral/min/areaDescarga.html?method=inicio
#' 
#' 
#' Se necesita la función lectura_excel de
#' ley_dhont/src/01-etl/utils.R.


# DEPENDENCIAS ------------------------------------------------------------

library(tidyverse)
source("ley_dhont/src/01-etl/utils.R")


# LECTURA -----------------------------------------------------------------

files <- list.files("ley_dhont/data/raw", pattern = "PROV*", full.names = T)

elecciones <- purrr::map_df(files, lectura_excel)

readr::write_rds(elecciones, "ley_dhont/data/elecciones_1977_2016.rds")
