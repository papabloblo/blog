#' 
#' Extracción de la pregunta 20:
#'  ¿Y en qué casilla de esa misma escala colocaría Ud. a cada uno de
#'  los siguientes partidos o coaliciones?
#'  


# DEPENDENCIAS ------------------------------------------------------------

library(tidyverse)

# LECTURA -----------------------------------------------------------------

metadatos <- readr::read_file("cis_ideologia/data/raw/2015/ES3117_utf8")
metadatos <- readr::read_delim("cis_ideologia/data/raw/2015/ES3117_utf8",
                               delim = "\r\n\r\n")

metadatos <- readr::read_lines("cis_ideologia/data/raw/2015/ES3117_utf8")

# METADATOS ---------------------------------------------------------------
metadatos <- paste(metadatos, " ")
separacion <- which(metadatos == "  ")
mi <- list()
j <- 1
for (i in separacion){
  mi <-  c(mi, paste(metadatos[j:(i - 1)], collapse = ""))
  j <- i
}

mi <- mi[[1]]

mi <- stringr::str_remove(mi, ".*/")

separacion <- stringr::str_split(mi, "\\s+")[[1]]

nombre_var <- separacion[seq(1, length(separacion), by = 2)]
posiciones <- separacion[seq(2, length(separacion), by = 2)]

metadata_var <- tibble(
  nombre_var,
  posiciones
)

metadata_var <- metadata_var %>% 
  mutate(
    inicio = as.integer(stringr::str_extract(metadata_var$posiciones, "[:digit:]+(?=-*)")),
    fin = as.integer(stringr::str_extract(metadata_var$posiciones, "(?<=-)[:digit:]+")),
    fin = ifelse(is.na(fin), inicio, fin)
  )

cis <- read_fwf("cis_ideologia/data/raw/2015/DA3117",
         fwf_positions(metadata_var$inicio,
                       metadata_var$fin,
                       col_names = metadata_var$nombre_var
         )
         )

cis <- readr::read_delim("cis_ideologia/data/raw/2015/DA3117", " ", col_names = metadata_var$nombre_var)

readr::read_tsv("cis_ideologia/data/raw/2015/DA3117", col_names = metadata_var$nombre_var)

cis[, 1:2]

as.integer(cis$CUES[1:10])


read_file("cis_ideologia/data/raw/2015/DA3117")


memisc::spss.fixed.file(file = "cis_ideologia/data/raw/2015/DA3117", 
                        columns.file = "cis_ideologia/data/raw/2015/ES3117_utf8")
-metadatos_p20 <- 
  stringr::str_subset(metadatos, "/P20\\d\\d") %>% 
  stringr::str_split(" '")

metadatos_p20 <- tibble(
  pregunta = purrr::map_chr(metadatos_p20,
                            function(x) stringr::str_remove(x[1], "/")
                            ),
  
  partido = purrr::map_chr(metadatos_p20, 
                           function(x) stringr::str_remove(x[2], "'")
                           )  
  ) %>% 
  filter(!is.na(partido))

metadatos_p20 <- metadatos_p20 %>% 
  mutate(
    partido = stringr::str_remove(partido, " \\(Sólo en.*\\)")
  )




# DATOS -------------------------------------------------------------------

# Extracción de la pregunta 20
cis <- cis[, stringr::str_detect(names(cis), "P20")]

p20 <- tibble()
for (i in seq_along(metadatos_p20$pregunta)){
  p20 <- p20 %>% 
    bind_rows(
      tibble(
        partido = metadatos_p20$partido[i],
        ideologia = as.integer(cis[[metadatos_p20$pregunta[i]]])  
        )
      )
}


# GUARDADO DE DATOS -------------------------------------------------------

readr::write_rds(p20, "cis_ideologia/data/p20_0.RDS")
