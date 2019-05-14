#'
#' Homogeneización de los datos de las
#' elecciones de 2019 y el histórico
#' de elecciones de 1977 a 2016.
#' 


# DEPENDENCIAS ------------------------------------------------------------

library(tidyverse)




# CARGA -------------------------------------------------------------------

elecciones_2019 <- readr::read_rds("ley_dhont/data/elecciones_2019.rds")

elecciones_1977_2016 <- readr::read_rds("ley_dhont/data/elecciones_1977_2016.rds")



# HOMOGENEIZACIÓN ---------------------------------------------------------

elecciones <- bind_rows(elecciones_1977_2016,
                        elecciones_2019
                        )



elecciones <- elecciones %>% 
  group_by(anyo, mes) %>% 
  mutate(
    votos_porc = votos/sum(votos),
    diputados_porc = diputados/sum(diputados),
    
    diputados_proporcionales = 350*votos_porc
    ) %>% 
  ungroup



# GUARDADO ----------------------------------------------------------------

readr::write_rds(elecciones, "ley_dhont/data/elecciones.rds")

