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

library(tidyverse)

# IMPORTACIÓN DEL CSV -----------------------------------------------------


elec_2019_2016 <- readr::read_csv2("ley_dhont/data/raw/des-con2.csv",
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


elec_2019 <- elec_2019_2016 %>% 
  select(contains("2019")) %>% 
  rename(
    partido_2019    = `Candidaturas 2019`,
    votos_2019      = `Votos 2019`, 
    votos_2019_porc = `%  2019`,
    diputados_2019  = `Diputados/as 2019`
  )


# EXPORTACIÓN -------------------------------------------------------------

readr::write_rds(elec_2019, "ley_dhont/data/elecciones_2019_0.rds")
