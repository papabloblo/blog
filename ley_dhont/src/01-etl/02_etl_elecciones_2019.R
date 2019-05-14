#' 
#' Tratamiento de datos para los resultados
#' de las elecciones de 2019.
#' 
#' Como estos datos se han extraído de la aplicación,
#' necesitan un tratamiento distinto del histórico.
#' 


# DEPENDECIAS -------------------------------------------------------------

library(tidyverse)

# CARGA DE DATOS ----------------------------------------------------------

elec_2019 <- readr::read_rds("ley_dhont/data/elecciones_2019_0.rds")


# REESTRUCTURACIÓN --------------------------------------------------------

elec_2019 <- elec_2019 %>% 
  mutate(
    anyo = 2019L,
    mes = 4L
  )

names(elec_2019) <- str_remove(names(elec_2019), "_2019")

elec_2019 <- elec_2019 %>% 
  # orden lógico de variables
  select(anyo, mes, partido, votos, votos_porc, diputados)



# TRATAMIENTO -------------------------------------------------------------

elec_2019 <- elec_2019 %>% 
  # partidos que no se presentaron en 2019
  # aparecen como NA
  filter(!is.na(partido))


# conversión de porcentaje a 0-1
elec_2019$votos_porc <- elec_2019$votos_porc/100


# EXPORTACIÓN DE DATOS ----------------------------------------------------

readr::write_rds(elec_2019, "ley_dhont/data/elecciones_2019.rds")


