#' 
#' Extracción de la pregunta 20:
#'  ¿Y en qué casilla de esa misma escala colocaría Ud. a cada uno de
#'  los siguientes partidos o coaliciones?
#'  


# DEPENDENCIAS ------------------------------------------------------------

library(tidyverse)

# LECTURA -----------------------------------------------------------------

cis <- haven::read_sav("cis_ideologia/data/raw/3242.sav")
metadatos <- readr::read_lines("cis_ideologia/data/raw/ES3242_utf8")


# METADATOS ---------------------------------------------------------------

metadatos_p20 <- 
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
