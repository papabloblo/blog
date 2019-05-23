



# DEPENDECIAS -------------------------------------------------------------

library(tidyverse)
source("cis_ideologia/src/02_dataviz/00_tema_grafico.R")
source("cis_ideologia/src/02_dataviz/00_color_partidos.R")


# DATOS -------------------------------------------------------------------

ideologia <- readr::read_rds("cis_ideologia/data/ideologia_partidos_congreso.RDS")



# TRATAMIENTO -------------------------------------------------------------

ideologia_media_anyo <- ideologia %>% 
  group_by(anyo, partido) %>% 
  summarise(ideologia_media = sum(porc_contestadas*ideologia, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(anyo, ideologia_media)
  


ideologia_media <- ideologia_media_anyo %>% 
  group_by(partido) %>% 
  summarise(ideologia_media = mean(ideologia_media)) %>% 
  arrange(-ideologia_media)

ideologia %>% 
  filter(anyo == "2019") %>% 
  mutate(partido = factor(partido, levels = ideologia_media$partido)) %>% 
  group_by(partido) %>% 
  mutate(porc_contestadas = porc_contestadas/max(porc_contestadas, na.rm = TRUE)) %>% 
  ggplot(
    aes(x = ideologia,
        y = porc_contestadas)
  ) +
  geom_col(aes(fill = partido), width = 1) +
  geom_vline(xintercept = 5.5, size = 2) +
  scale_x_continuous(breaks = 1:10) +
  # facet_wrap(.~factor(partido, levels = orden_partidos), ncol = 1, scales = "free_y") +
  facet_grid(partido~anyo,  scales = "free_y") +
  tema +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    panel.grid  = element_blank()
  ) +
  scale_fill_manual(values = color_partidos) +
  labs(
    title = "Percepción de la ideología",
    x = "")

ideologia_partidos %>% 
  filter(partido %in% partido_selec,
         ideologia %in% 1:10) %>% 
  mutate(partido = factor(partido, levels = orden_partidos)) %>% 
  group_by(partido) %>% 
  mutate(porc_contestadas = porc_contestadas/max(porc_contestadas)) %>% 

  ggplot(
    aes(x = ideologia,
        y = partido)
  ) +
  geom_tile(
    aes(fill = porc_contestadas)
  ) +
  scale_fill_distiller(direction = 1)+
  tema
