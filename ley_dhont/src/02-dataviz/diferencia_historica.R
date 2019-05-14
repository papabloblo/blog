#'
#' HISTORICO DE DIFERENCIA DE DIPUTADOS
#' CON Y SIN LEY D'HONDT
#' 
#' 


# DEPENDENCIAS ------------------------------------------------------------

library(tidyverse)

source("ley_dhont/src/02-dataviz/00_tema_grafico.R")
source("ley_dhont/src/02-dataviz/00_color_partidos.R")


# DATOS -------------------------------------------------------------------

elecciones <- readr::read_rds("ley_dhont/data/elecciones.rds")


# TRATAMIENTO -------------------------------------------------------------

# Normalización de las siglas de partidos
elecciones <- elecciones %>% 
  mutate(
    partido = case_when(
      str_detect(partido, "PODEMOS") ~ "PODEMOS",
      str_detect(partido, "IU-") ~ "IU",
      partido == "PSOE-PROGR" ~ "PSOE",
      TRUE ~ partido
    )
  ) %>% 
  group_by(anyo, mes, partido) %>% 
  # En el 2016 hay:
  #   PODEMOS-IU-EQUO
  #   PODEMOS-EN MAREA-ANOVA-EU
  #   PODEMOS-COMPROMÍS-EUPV
  summarise(
    votos = sum(votos),
    diputados = sum(diputados)
  ) %>% 
  ungroup() %>% 
  group_by(anyo, mes) %>% 
  mutate(
    votos_porc = votos/sum(votos),
    diputados_porc = diputados/sum(diputados),
    
    diputados_proporcionales = 350*votos_porc
  ) %>% 
  ungroup


# Solo partidos con representación en el sistema d'Hondt (diputados >=1)
# o en un sistema proporcional (diputados_proporcionales >= 1)

elecciones <- elecciones %>% 
  filter(
    diputados >=1 | diputados_proporcionales >= 1
  )


elecciones <- elecciones %>% 
  mutate(
    dif_diputados = diputados - diputados_proporcionales,
    mayoria_absoluta = ifelse(diputados > 350/2,
                              1,
                              0)
  )


# MAE ---------------------------------------------------------------------

mae <- elecciones %>% 
  group_by(anyo) %>% 
  summarise(mae = round(mean(abs(dif_diputados)), 2))

# ANOTACIONES -------------------------------------------------------------



max_error <- elecciones %>% 
  group_by(anyo) %>% 
  top_n(1, dif_diputados)

partidos_anotacion <- c("PP", "PSOE", "UCD", "Cs", "IU", "PODEMOS")
elecciones_anotacion <- elecciones %>% 
  filter(partido %in% partidos_anotacion)

# GRÁFICO -----------------------------------------------------------------

# Gráfico base
p <- elecciones %>% 
  ggplot(
    aes(y = dif_diputados, 
        x = factor(anyo, 
                   levels = sort(unique(anyo),
                                 decreasing = T)
                   )
        )
    ) +
  
  geom_point(
    size   = 4,
    alpha  = 0.3, 
    stroke = 0)+
  
  geom_hline(yintercept = 0)

# Resaltado

p <- p +
  
  geom_line(data = elecciones_anotacion,
            aes(color = partido, 
                group = partido
                ),
            alpha = 1,
            size  = .5
  ) +
  
  geom_point(data = elecciones_anotacion, 
             aes(color = partido,
                 stroke = mayoria_absoluta),
             size = 4,
             stroke = 0
             ) +
geom_point(data = elecciones_anotacion %>%
             filter(mayoria_absoluta == 1), 
           aes(color = partido),
           shape = 21,
           fill = "white",
           size = 4,
           stroke = 2
)

# Anotación con etiquetas

p <- p +
  
  geom_label(data = elecciones_anotacion %>%
               filter(anyo == 1977), 
             
             aes(label = partido, 
                 color = partido
                 ),
             
             nudge_x = 0.5,
             label.size = 0
             ) +
  geom_label(data = elecciones_anotacion %>%
               filter(partido == "IU", 
                      anyo == 1986
                      ), 
             aes(label = partido,
                 color = partido
                 ),
             nudge_x = 0.4,
             label.size = 0
  ) +
  geom_label(data = elecciones_anotacion %>% 
               filter(anyo == 2019), 
             aes(label = partido, 
                 color = partido
                 ),
             nudge_x = -0.5,
             label.size = 0
  )

# MAE
p <- p +
  geom_line(data = mae,
            aes(y = mae, group = 1),
            alpha = 1,
            size  = .5,
            color = "#5a5050"
  ) +
  
  geom_label(data = mae , 
             aes(y = mae, 
                 label = mae
             ),
             color = "#5a5050",
             # nudge_x = -0.5,
             label.size = 0,
             size = 3
  ) +
  
  # geom_point(data = mae, 
  #            aes(y = mae),
  #            size = 4,
  #            stroke = 0
  # ) +
  geom_label(data = mae %>% 
               filter(anyo == 2019), 
             aes(y = mae, 
                 label = "MAE"
                 ),
             nudge_x = -0.5,
             label.size = 0,
             size = 3,
             color = "#5a5050"
  ) +
  geom_label(data = mae %>%
               filter(anyo == 1977), 
             
             aes(y = mae, 
                 label = "MAE"
                 ),
             
             nudge_x = 0.5,
             label.size = 0,
             size = 3,
             color = "#5a5050"
  )


# Escalas

p <- p +
  scale_color_manual(
    values = color_partidos
  ) +
  scale_y_continuous(breaks = c(-10, -5, seq(0, 45, by = 10)), 
                     labels = 
                       function(x){
                         y <- ifelse(x > 0, 
                                     paste("+", x),
                                     ifelse(x < 0, 
                                            paste("-",-x),
                                            x
                                     )
                         )
                         y[y == "+ 10"] <- "+ 10 diputados"
                         return(y)
                         },
                     minor_breaks = NULL
                     ) +
  scale_x_discrete(expand = expand_scale(add = 2))


# Títulos
p <- p +
  labs(
    title   = "La Ley d'Hondt favorece\na los grandes partidos",
    caption = "Fuente: Ministerio del Interior\n\n@papabloblog",
    y       = "Diferencia entre diputados obtenidos por la Ley d'Hondt\nfrente a diputados proporcionales a los votos",
    y       = "",
    x       = ""
  )

# Tema gráfico
p <- p +
  tema
  
# FIN
p <- p +
  coord_flip()
  

# EXPORTACIÓN -------------------------------------------------------------

ggsave("ley_dhont/dataviz/diferencia_historica_0.svg", plot = p, dpi = 400, width = 20, height = 30, units = "cm")
