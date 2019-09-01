#' 
#' DESCARGA DE DATOS DE BICIMAD (MADRID)
#' 
#' 1. Se crea el directorio bicis/data/madrid (si no existe)
#' 2. Se accede a la web de la EMT y se extraen los enlaces de descarga.
#' 3. Se descargan en bicimad/data/madrid solo aquellos archivos que no se 
#'    hayan descargado previamente.
#' 




# DEPENDENCIAS ------------------------------------------------------------

library(tidyverse)
library(rvest)
path <- "bicis/data/madrid/"

# CREACIÓN DIRECTORIO -----------------------------------------------------

if (!dir.exists(path)){
  dir.create(path, recursive = TRUE)  
}


# EXTRACCIÓN DE ENLACES DE DESCARGA ---------------------------------------

url_files <- 
  read_html("https://opendata.emtmadrid.es/Datos-estaticos/Datos-generales-(1)") %>% 
  html_nodes("#Datosdebicimadanonimizados .ficheros ul li a") %>% 
  html_attr("href") %>% 
  str_subset("Usage_Bicimad")

url_files <- paste0(
  "https://opendata.emtmadrid.es", 
  url_files
)




# DESCARGA DE ARCHIVOS ----------------------------------------------------

# Solo archivos que no hayan sido ya descargados
new_files <-
  url_files[
    !paste0(str_extract(url_files, "[:digit:]{6}_Usage_Bicimad"), ".zip") 
    %in% 
      list.files(path)
    ]


walk(
  new_files,
  function(x)
    download.file(x, 
                  paste0(
                    path, 
                    str_extract(x, "/[:digit:]{6}_Usage_Bicimad"), 
                    ".zip"
                    )
                  )
  )



