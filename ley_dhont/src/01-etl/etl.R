#' 
#' Tratamiento de datos
#' 


# DEPENDECIAS -------------------------------------------------------------

library(tidyverse)

# CARGA DE DATOS ----------------------------------------------------------

elec_2019_2016 <- readr::read_rds("ley_dhont/data/des-con.rds")


# REESTRUCTURACIÓN --------------------------------------------------------

elec_2016 <- elec_2019_2016 %>% 
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

elec_2019 <- elec_2019_2016 %>% 
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

elec_2019_2016 <- bind_rows(elec_2019, 
                            elec_2016
                            ) %>% 
  select(anyo, partido, votos, votos_porc, diputados)



# TRATAMIENTO -------------------------------------------------------------

elec_2019_2016 <- elec_2019_2016 %>% 
  # partidos que no se presentaron en 2019 o 2016
  # aparecen como NA
  filter(!is.na(partido))


# conversión de porcentaje a 0-1
elec_2019_2016$votos_porc <- elec_2019_2016$votos_porc/100


# porcentaje de diputados para cada partido en cada año
elec_2019_2016 <- elec_2019_2016 %>% 
  group_by(anyo) %>% 
  mutate(diputados_porc = diputados/sum(diputados, na.rm = T)) %>% 
  ungroup()


# EXPORTACIÓN DE DATOS ----------------------------------------------------

readr::write_rds(elec_2019_2016, "ley_dhont/data/elecciones.rds")


  mutate(
    inc_votos = diputados_porc - votos_porc,
    hjust = ifelse(inc_votos >= 0,
                   0,
                   1),
    padding = ifelse(inc_votos >= 0,
                     -0.05,
                     +0.05),
    label = map2_dbl(votos_porc, diputados_porc, max),
    # partido2 = paste(partido, "(", round(inc_votos*100, 2), "%)")
    partido2 = paste0(partido, 
                      " (",
                      scales::number(votos, big.mark = ".", decimal.mark = ","),
                      ")"
    )
    
  )



# orden lógico de las variables
elecciones <- elecciones %>% 
  select(anyo, partido, votos)


