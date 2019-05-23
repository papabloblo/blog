#'
#' TRATAMIENTO DE LA PREGUNTA 20
#' 


# DEPENDENCIAS ------------------------------------------------------------

library(tidyverse)

# CARGA DE DATOS ----------------------------------------------------------

p20 <- readr::read_rds("cis_ideologia/data/p20_0.RDS")



# TRATAMIENTO -------------------------------------------------------------

p20 <- p20 %>% 
  # En regiones donde no se presenta un partido,
  # la pregunta no se realiza y aparece NA.
  filter(!is.na(ideologia)) %>% 
  group_by(partido, ideologia) %>% 
  summarise(n = n()) %>% 
  group_by(partido) %>% 
  mutate(porc = n/sum(n)) %>% 
  ungroup()


p20_respondida <- p20 %>% 
  select(partido, ideologia, n) %>% 
  # 98 -> No sabe/No contesta
  # 99 -> No contesta
  filter(!(ideologia %in% c(98, 99))) %>% 
  group_by(partido) %>% 
  mutate(porc_respondidas = n/sum(n)) %>% 
  select(-n) %>% 
  ungroup()

p20 <- p20 %>% 
  left_join(p20_respondida, 
            by = c("partido", "ideologia")
            )


# GUARDADO ----------------------------------------------------------------

readr::write_rds(p20, "cis_ideologia/data/p20.RDS")
