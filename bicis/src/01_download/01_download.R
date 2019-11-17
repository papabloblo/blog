#'
#' DESCARGA DE DATOS
#' Pablo Hidalgo (pablohigar@gmail.com)
#' 
#' Descarga de datos de uso de las bicicletas de varias ciudades.
#' 


# DEPENDENCIAS ------------------------------------------------------------

library(tidyverse)
library(rvest)
source("bicis/src/01_download/utils_download.R")

dl_data_bikes(data_dir = "bicis/data")
