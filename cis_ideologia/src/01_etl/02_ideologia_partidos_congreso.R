#' 
#' IDEOLOGÍA DEL CONGRESO DE LOS DIPUTADOS
#' 
#' El cálculo de la ideología se realiza teniendo en
#' cuenta la distribución ideológica aportada por la 
#' encuesta preelectoral del CIS para cada partido
#' y ponderando por la representación de cada
#' partido en el congreso de los diputados.
#' 
#' 


# DEPENDENCIAS ------------------------------------------------------------

library(tidyverse)


# CARGA DE DATOS ----------------------------------------------------------

ideologia <- readr::read_rds("cis_ideologia/data/ideologia_partidos.rds")
congreso <- readr::read_rds("ley_dhont/data/elecciones.rds")


# TRATAMIENTO -------------------------------------------------------------

congreso <- congreso %>% 
  filter(
    
    # Solo partidos con representación parlamentaria
    diputados > 0,
    
    # SOLO hay datos de ideologia a partir
    # del 2015
    anyo >= 2015
    )
  
  
  

# Las siglas de los partidos en los conjuntos de datos
# congreso e ideologia son distintas.

# 1. Se inspeccionan las inconsistencias

partidos_ideologia <- ideologia %>% 
  select(anyo, partido) %>% 
  distinct() %>% 
  mutate(partido_ideologia = partido)

partidos_congreso <- congreso %>% 
  select(anyo, partido) %>% 
  distinct() %>% 
  mutate(partido_congreso = partido)

partidos <- partidos_ideologia %>% 
  full_join(partidos_congreso,
            by = c("anyo", "partido")
            ) %>% 
  filter(is.na(partido_congreso) | is.na(partido_ideologia)) %>% 
  arrange(anyo)



# 2. Se normalizan en ambos conjuntos de datos
congreso <- congreso %>% 
  mutate(
    partido = case_when(
      partido == "Cs" ~ "Ciudadanos",
      
      partido %in% c("ERC-CATSI", "ERC-CATSÍ") ~ "ERC",
      partido == "ERCSOBIRANISTES" ~ "ERC",
      
      partido %in% c("EAJ-PNV", "EAJPNV") ~ "PNV",
      
      partido == "COMPROMÍS 2019" ~ "Compromís",
      
      # https://es.wikipedia.org/wiki/Navarra_Suma
      partido == "NA+" ~ "UPN",
      
      partido %in% c("CCaPNC", "CCa-PNC")  ~ "CC",
      
      stringr::str_detect(partido, "PODEMOS") ~ "Podemos",
      stringr::str_detect(partido, "EN COMÚ") ~ "Podemos",
      
      partido == "JxCATJUNTS" ~ "PDeCAT (JxCat)",
      
      partido %in% c("ECPGUANYEM EL CANVI", "ECP") ~ "En Comú Podem",
      
      partido %in% c("DL", "CDC") ~ "Convergència",
      
      partido == "IU-UPeC" ~ "IU",
      
      
      TRUE ~ partido
    )
  )

ideologia <- ideologia %>% 
  mutate(
    partido = case_when(
      partido == "IU (ICV en Cataluña)" ~ "IU",
      
      partido %in% c("Convergència (Democràcia i Llibertat)") ~ "Convergència",
    
      partido %in% c("EAJ-PNV") ~ "PNV",
      
      TRUE ~ partido
    )
  )

# Se fusionan todas las corrientes de podemos:
podemos_agrupacion <- c("IU", "Compromís", "En Comú Podem")

ideologia <- ideologia %>% 
  mutate(
    partido = case_when(
      partido %in% podemos_agrupacion  ~ "Podemos",
      TRUE ~ partido
    ) 
  ) %>% 
  group_by(anyo, partido, ideologia) %>% 
  summarise(
    porc_respondidas = mean(porc_respondidas, na.rm = TRUE)
  ) %>% 
  ungroup()

congreso <- congreso %>% 
  mutate(
    partido = case_when(
      partido %in% podemos_agrupacion  ~ "Podemos",
      TRUE ~ partido
    ) 
  ) %>% 
  group_by(anyo, mes, partido) %>% 
  summarise_all(sum) %>% 
  ungroup()



# CÁLCULO DE IDEOLOGÍA ----------------------------------------------------

ideologia <- congreso %>% 
  select(anyo, partido, diputados_porc) %>% 
  left_join(ideologia, 
            by = c("anyo", "partido")
            ) %>% 
  mutate(pond_congreso = porc_contestadas*diputados_porc)

ideologia_congreso <- ideologia %>% 
  group_by(anyo, ideologia) %>% 
  summarise(
    porc_contestadas = sum(pond_congreso)
  ) %>% 
  mutate(partido = "Congreso")


ideologia <- ideologia %>% 
  bind_rows(ideologia_congreso)


readr::write_rds(ideologia, "cis_ideologia/data/ideologia_partidos_congreso.RDS")



ideologia %>% 
  filter(partido == "Congreso") %>% 
  ggplot(
    aes(x = ideologia,
        y = porc_contestadas)
  ) +
  geom_col(width = 1) +
  geom_vline(xintercept = 5.5, size = 2) +
  scale_x_continuous(breaks = 1:10) +
  facet_wrap(.~anyo, ncol = 1) +
  tema +
  # theme(
  #   axis.title.y = element_blank(),
  #   axis.text.y = element_blank(),
  #   panel.grid  = element_blank()
  # ) +
  labs(
    title = "Percepción de la ideología",
    x = "")


ideologia %>% 
  filter(partido != "Congreso") %>% 
  ggplot(
    aes(x = ideologia,
        y = pond_congreso)
  ) +
  geom_col(width = 1, aes(fill = partido)) +
  geom_vline(xintercept = 5.5, size = 2) +
  scale_x_continuous(breaks = 1:10) +
  scale_fill_manual(values = color_partidos) +
  facet_wrap(.~anyo, ncol = 1) +
  tema +
  # theme(
  #   axis.title.y = element_blank(),
  #   axis.text.y = element_blank(),
  #   panel.grid  = element_blank()
  # ) +
  labs(
    title = "Percepción de la ideología",
    x = "")

ideologia %>% 
  group_by(anyo, partido) %>% 
  summarise(ideologia = sum(porc_contestadas*ideologia, na.rm = TRUE)) %>% 
  ggplot(aes(x = as.character(anyo), y = ideologia, color = partido))+
  geom_line(aes(group = partido), size = 3)+
  geom_point(size = 5) +
  tema+
  scale_color_manual(values = color_partidos) +
  scale_y_continuous(breaks = 1:10, limits = c(1,10))



ideologia %>% 
  filter(partido == "Ciudadanos") %>% 
  ggplot(
    aes(x = ideologia,
        y = porc_contestadas)
  ) +
  geom_col(width = 1, aes(fill = partido)) +
  geom_vline(xintercept = 5.5, size = 2) +
  geom_vline(data = ideologia %>% 
               filter(partido == "Ciudadanos") %>% 
               group_by(anyo, partido) %>% 
               summarise(ideologia = sum(porc_contestadas*ideologia, na.rm = TRUE)),
             aes(xintercept = ideologia), size = 2) +
  scale_x_continuous(breaks = 1:10) +
  scale_fill_manual(values = color_partidos) +
  facet_wrap(.~anyo, ncol = 1) +
  tema +
  # theme(
  #   axis.title.y = element_blank(),
  #   axis.text.y = element_blank(),
  #   panel.grid  = element_blank()
  # ) +
  labs(
    title = "Percepción de la ideología",
    x = "")









ideologia %>% 
  ggplot(
    aes(x = ideologia,
        y = prob)
  ) +
  geom_col(width = 1) +
  geom_vline(xintercept = 5.5, size = 2) +
  scale_x_continuous(breaks = 1:10) +
  tema +
  # theme(
  #   axis.title.y = element_blank(),
  #   axis.text.y = element_blank(),
  #   panel.grid  = element_blank()
  # ) +
  labs(
    title = "Percepción de la ideología",
    x = "")

ideologia %>% 
  mutate(
    zona = ifelse(ideologia <= 5, "izquierda", "derecha")
  ) %>% 
  group_by(zona) %>% 
  summarise(prob = sum(prob, na.rm = TRUE))





  

ideologia %>% 
  mutate(
    prob = diputados_porc*porc_respondidas,
    partido = case_when(
      !(partido %in% c("PP", "PSOE", "Unidas Podemos", "VOX", "Ciudadanos", "ERC", "CC", "EAJ-PNV")) ~ "Otros",
      TRUE ~ partido
    )
  ) %>% 
  group_by(anyo, ideologia) %>% 
  mutate(
    prob = prob/sum(prob)
  ) %>% 
  ggplot(
    aes(x = ideologia,
        y = prob)
  ) +
  geom_col(width = 1, aes(fill = partido)) +
  geom_vline(xintercept = 5.5, size = 2) +
  scale_x_continuous(breaks = 1:10) +
  scale_fill_manual(values = color_partidos) +
  tema +
  # theme(
  #   axis.title.y = element_blank(),
  #   axis.text.y = element_blank(),
  #   panel.grid  = element_blank()
  # ) +
  labs(
    title = "Percepción de la ideología",
    x = "")
