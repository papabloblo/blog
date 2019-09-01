#' 
#' DESCARGA DE DATOS DE CITIBIKENYC (NEW YORK)
#' 
#' 
#' 1. Se crea el directorio bicis/data/ny (si no existe)
#' 2. Se descargan los datos en bicis/data/ny a través del paquete bikedata.
#' 
#' FUENTE: https://github.com/ropensci/bikedata



# CREACIÓN DIRECTORIO -----------------------------------------------------

path <- "bicis/data/ny/"
if (!dir.exists(path)){
  dir.create(path, recursive = TRUE)  
}



# DESCARGA DE DATOS -------------------------------------------------------

bikedata::dl_bikedata("ny", data_dir = path)
