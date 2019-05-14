#' 
#' Visualización de diferencia entre el porcentaje de votos
#' y los diputados efectivos conseguidos.
#' Se utiliza un gráfico SLOPEGRAPH
#' 


# DEPENDENCIAS ------------------------------------------------------------

library(tidyverse)
source("ley_dhont/src/02-dataviz/tema_grafico.R")

# CARGA DE DATOS ----------------------------------------------------------

elecciones <- readr::read_rds("ley_dhont/data/elecciones.rds")
 


# TRANSFORMACIÓN ----------------------------------------------------------

elecciones <- filter(elecciones, anyo == 2019L)

elecciones <- elecciones %>%
  mutate(
    inc_porc_diput_votos = diputados_porc - votos_porc,
    
    mas_diput_votos = ifelse(
      inc_porc_diput_votos >= 0,
      0L,
      1L
      ),
    
    # padding = ifelse(inc_porc_diput_votos >= 0, -0.05, +0.05),
    # label = map2_dbl(votos_porc, diputados_porc, max),
    partido_etiqueta = 
      paste0(
        partido,
        "\n(",
          scales::number(votos, big.mark = ".", decimal.mark = ","),
        ")"
        )
    )


elecciones_slope <- elecciones %>% 
  select(partido, 
         votos_porc, 
         diputados_porc,
         mas_diput_votos
         ) %>% 
  gather(key = "tipo",
         value = "porc",
         -partido, 
         -mas_diput_votos
         )





# SPLOPEGRAPH GLOBAL ------------------------------------------------------

gridlines_diputados <- tibble(
  y = seq(0, 125, by = 25),
  etiqueta = paste(y,
                   "diputados"),
  diputado_porc = y/350
  )


p <- elecciones_slope %>% 
   ggplot(
     aes(
       x = factor(tipo, levels = c("votos_porc", "diputados_porc")),
       y = porc
       )
     ) +
  
   geom_hline(
     data = gridlines_diputados,
     aes(
       yintercept = diputado_porc
       ),
     linetype = "dashed"
     ) +
  
  geom_label(
    data = gridlines_diputados,
    aes(y = diputado_porc,
        x     = "diputados_porc",
        label = etiqueta), 
    nudge_x = -0.15, 
    label.size=0, 
    size = 5,
    hjust = 1
    )


p <- p +
  geom_line(aes(group = partido), alpha = 0.25)

partidos_annotate <- elecciones %>% 
  top_n(6, wt = diputados_porc) %>% 
  pull(partido)

p <- p +
   geom_line(
     data = elecciones_slope %>% 
       filter(partido %in% partidos_annotate),
     aes(group = partido, 
         color = as.character(mas_diput_votos)
         ),
     size = 1.5
     ) +
   geom_point(
     data = elecciones_slope %>% 
       filter(partido %in% partidos_annotate),
     aes(color = as.character(mas_diput_votos)),
     size = 3
     ) +
   scale_color_manual(
     values = c("1" = "steelblue",
                "0" ="firebrick"
                )
   ) 
   
p <- p +
  geom_label(
    data = elecciones %>% 
      filter(partido %in% partidos_annotate),
    
    aes(
      x = "votos_porc",
      y = votos_porc, 
      label = partido_etiqueta
      ),
    hjust = 0,
    family="sans",
    size = 3.5, 
    label.size=0,
    nudge_x = 0.02
   )
   
   
p <- p +
  labs(
     x = "",
     y = "",
     title = "Unidas Podemos y Vox obtendrían \nmás diputados",
     caption = "@papabloblog"
   ) +
   scale_x_discrete(
     labels = c("votos_porc" = "Diputados proporcionales\na los votos",
                "diputados_porc" = "Diputados según\nla Ley d'Hondt"
     )
   )+
  
   tema +
   theme(
     panel.grid.major.y  = element_blank(),
     panel.grid.minor.y  = element_blank(),
     axis.text.y = element_blank(),
     axis.text.x = element_text(size = 17)
   )

ggsave("ley_dhont/dataviz/slopegraph_global.png", plot = p, dpi = 400, width = 20, height = 30, units = "cm")


# SLOPEGRAPH --------------------------------------------------------------


gridlines_diputados <- tibble(
  y = seq(0, 5, by = 1),
  etiqueta = paste(y,
                   "diputados"),
  diputado_porc = y/350
)

elecciones_slope <- elecciones_slope %>% 
  filter(porc < 10/350)

elecciones <- elecciones %>% 
  mutate(nudge_y =  case_when(
    partido == "PP" ~ 0.005,
    partido == "Cs" ~ -0.005,
    partido == "VOX" ~ -0.004,
    partido == "FRONT REPUBLICÀ" ~ 0.0005,
    partido == "BNG" ~ -0.0001,
    TRUE ~ 0
  )
  )

p <- elecciones_slope %>% 
  ggplot(
    aes(
      x = factor(tipo, levels = c("votos_porc", "diputados_porc")),
      y = porc
    )
  ) +
  
  geom_hline(
    data = gridlines_diputados,
    aes(
      yintercept = diputado_porc
    ),
    linetype = "dashed"
  ) +
  
  geom_label(
    data = gridlines_diputados,
    aes(y = diputado_porc,
        x     = "diputados_porc",
        label = etiqueta), 
    nudge_x = -0.15, 
    label.size=0, 
    size = 5,
    hjust = 1
  )


p <- p +
  geom_line(aes(group = partido), alpha = 0.25)

partidos_annotate <- elecciones %>% 
  filter(xor(votos_porc >= 1/350, diputados_porc >= 1/350)) %>% 
  pull(partido)

p <- p +
  geom_line(
    data = elecciones_slope %>% 
      filter(partido %in% partidos_annotate),
    aes(group = partido, 
        color = as.character(mas_diput_votos)
    ),
    size = 1.5
  ) +
  geom_point(
    data = elecciones_slope %>% 
      filter(partido %in% partidos_annotate),
    aes(color = as.character(mas_diput_votos)),
    size = 3
  ) +
  scale_color_manual(
    values = c("1" = "steelblue",
               "0" ="firebrick"
    )
  ) 

p <- p +
  geom_label(
    data = elecciones %>% 
      filter(partido %in% partidos_annotate),
    
    aes(
      x = "votos_porc",
      y = votos_porc + nudge_y, 
      label = partido_etiqueta
    ),
    hjust = 0,
    family="sans",
    size = 3.5, 
    label.size=0,
    nudge_x = 0.02
  )


p <- p +
  labs(
    x = "",
    y = "",
    title = "El PACMA pasaría de \nno tener ningún diputado a tener 4",
    caption = "@papabloblog"
  ) +
  scale_x_discrete(
    labels = c("votos_porc" = "Diputados proporcionales\na los votos",
               "diputados_porc" = "Diputados según\nla Ley d'Hondt"
    )
  )+
  
  tema +
  theme(
    panel.grid.major.y  = element_blank(),
    panel.grid.minor.y  = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 17)
  )

ggsave("ley_dhont/dataviz/slopegraph_pacma.png", plot = p, dpi = 400, width = 20, height = 30, units = "cm")


# SLOPEGRAPH GLOBAL ANOTACIÓN ---------------------------------------------

gridlines_diputados <- tibble(
  y = seq(0, 125, by = 25),
  etiqueta = paste(y,
                   "diputados"),
  diputado_porc = y/350
)


p <- elecciones_slope %>% 
  ggplot(
    aes(
      x = factor(tipo, levels = c("diputados_porc", "votos_porc")),
      y = porc
    )
  ) +
  
  geom_hline(
    data = gridlines_diputados,
    aes(
      yintercept = diputado_porc
    ),
    linetype = "dashed"
  ) +
  
  geom_label(
    data = gridlines_diputados,
    aes(y = diputado_porc,
        x     = "diputados_porc",
        label = etiqueta), 
    nudge_x = -0.15, 
    label.size = 0, 
    size = 5,
    hjust = 1
  )


p <- p +
  geom_line(aes(group = partido), alpha = 0.25)

partidos_annotate <- elecciones %>% 
  top_n(6, wt = diputados_porc) %>% 
  pull(partido)

p <- p +
  geom_line(
    data = elecciones_slope %>% 
      filter(partido %in% partidos_annotate),
    aes(group = partido, 
        color = as.character(mas_diput_votos)
    ),
    size = 1.5
  ) +
  geom_point(
    data = elecciones_slope %>% 
      filter(partido %in% partidos_annotate),
    aes(color = as.character(mas_diput_votos)),
    size = 5
  ) +
  geom_label(
    data = elecciones_slope %>% 
      filter(partido %in% partidos_annotate),
    aes(label = round(porc*350, 1)),
    size = 4
  ) +
  scale_color_manual(
    values = c("1" = "steelblue",
               "0" ="firebrick"
    )
  ) 

elecciones <- elecciones %>% 
  mutate(nudge_y =  case_when(
    partido == "PP" ~ 0.005,
    partido == "Cs" ~ -0.005,
    partido == "VOX" ~ -0.004,
    TRUE ~ 0
  )
  )

p <- p +
  geom_label(
    data = elecciones %>% 
      filter(partido %in% partidos_annotate),
    
    aes(
      x = "votos_porc",
      y = votos_porc + nudge_y, 
      label = partido_etiqueta
    ),
    hjust = 0,
    family="sans",
    size = 3.5, 
    label.size=0,
    nudge_x = 0.1
  )

p <- p +
  labs(
    x = "",
    y = "",
    title = "Unidas Podemos y Vox obtendrían \nmás diputados",

    subtitle = "Elecciones generales del 28 de abril de 2019.\n\nAunque los diputados no se pueden repartir en forma decimal,\nse ha respetado la proporción a efectos ilustrativos",
    caption = "Fuente: Ministerio del Interior\n\n@papabloblog"
  ) +
  scale_x_discrete(
    labels = c("votos_porc" = "Diputados proporcionales\na los votos",
               "diputados_porc" = "Diputados según\nla Ley d'Hondt"
    )
  )+
  
  tema +
  theme(
    panel.grid.major.y  = element_blank(),
    panel.grid.minor.y  = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 17)
  )

ggsave("ley_dhont/dataviz/slopegraph_global2.png", plot = p, dpi = 400, width = 20, height = 30, units = "cm")
ggsave("ley_dhont/dataviz/slopegraph_global2.svg", plot = p, dpi = 400, width = 20, height = 30, units = "cm")

