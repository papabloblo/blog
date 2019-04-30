#' 
#' Importación y tratamiendo de los resultados de las elecciones de 2019.
#' 
#' Fichero de origen: 
#'     data/raw/des-con2.csv
#'     
#' NOTA: El fichero data/raw/des-con2.csv se ha cambiado
#'       manualmente a partir de data/raw/des-con.csv
#'       eliminando todos los ", ., y -
#'       TODO: hacerlo de forma programática.
#'     




# DEPENDECIAS -------------------------------------------------------------

library(readr)
library(tidyverse)

# IMPORTACIÓN DEL CSV -----------------------------------------------------



elec_2019 <- readr::read_csv2("ley_dhont/data/raw/des-con2.csv",
                              col_types = cols(
                                `Candidaturas 2019` = col_character(),
                                `Votos 2019`        = col_integer(),
                                `%  2019`           = col_double(),
                                `Diputados/as 2019` = col_integer(),
                                `Candidaturas 2016` = col_character(),
                                `Votos 2016`        = col_integer(),
                                `%  2016`           = col_double(),
                                `Diputados/as 2016` = col_integer()
                                )
                              )  


# CAMBIO DE NOMBRE DE LAS VARIABLES ---------------------------------------

elec_2019 <- elec_2019 %>% 
  rename(
    partido_2019    = `Candidaturas 2019`,
    votos_2019      = `Votos 2019`, 
    votos_2019_porc = `%  2019`,
    diputados_2019  = `Diputados/as 2019`,
    
    partido_2016    = `Candidaturas 2016`,
    votos_2016      = `Votos 2016`, 
    votos_2016_porc = `%  2016`,
    diputados_2016  = `Diputados/as 2016`
  )



# REESTRUCTURACIÓN --------------------------------------------------------

elec_2016 <- elec_2019 %>% 
  select(
    partido_2016, 
    votos_2016, 
    votos_2016_porc,
    diputados_2016
    ) %>% 
  mutate(
    anyo = 2016L
  )

names(elec_2016) <- str_remove(names(elec_2016), "_2016")

elec_2019 <- elec_2019 %>% 
  select(
    partido_2019, 
    votos_2019, 
    votos_2019_porc,
    diputados_2019
  ) %>% 
  mutate(
    anyo = 2019L
  )

names(elec_2019) <- str_remove(names(elec_2019), "_2019")

elecciones <- bind_rows(elec_2019, elec_2016)


# EXPORTACIÓN -------------------------------------------------------------

readr::write_rds(elecciones, "ley_dhont/data/elecciones.rds")
