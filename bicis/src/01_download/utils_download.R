
dl_data_bikes <- function(city = c("bo",
                                   "ch",
                                   "dc",
                                   "la",
                                   "lo",
                                   # "mn",
                                   "mad",
                                   "ny",
                                   "ph",
                                   "sf"
                                   ), 
                          data_dir
                          ){
  
  
  for (i in city){
    cat("Downloading data of", i, "...\n")
    city_dir <- file.path(data_dir, i)
    
    if (!dir.exists(city_dir)){
      dir.create(city_dir, recursive = TRUE)  
    }
    
    if (tolower(i) %in% c("madrid", "mad")) {
      dl_madrid(city_dir)
    } else {
      bikedata::dl_bikedata(i, data_dir = city_dir)
    }  
  }
  
}




dl_madrid <- function(path){
  
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
  
  if (length(new_files) == 0) cat("All data files already exist\n")
  
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
  
}


# url_files <- 
#   read_html("https://opendata-ajuntament.barcelona.cat/data/es/dataset/us-del-servei-bicing") %>% 
#   html_nodes(".resource-item")
# 
# url_files %>% 
#   html_attrs()
#   str_subset("Usage_Bicimad")
# 
# url_files %>% 
#     html_text()



