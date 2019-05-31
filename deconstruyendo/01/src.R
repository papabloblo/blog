#' DECONSTRUYENDO GRAFICOS 01
#'
#' UNIDAS PODEMOS PERDIÓ DIPUTADOS EN TODAS
#' LAS COMUNIDADES EL 26 M
#' 
#' Comparación entre el porcentaje de escaños de las diferentes candidaturas de
#' Unidas Podemos en cada comunidad autónoma en 2015 y en 2019
#' 
#'Fuente original: https://www.eldiario.es/politica/Subidas-bajadas-resultados-izquierda-PSOE_0_903959796.html
#'


# DEPENDENCIAS -----------------------------------------------------------

library(tidyverse)


# DATOS -------------------------------------------------------------------

# Datos obtenidos del gráfico de la fuente original
datos <- tibble(
  ccaa = c("Aragón", "Canarias", "Cantabria", "C. La Mancha", "C. León", "C. Madrid", 
           "C.F. Navarra", "Extremadura", " I. Balears", "La Rioja", "P. Asturias", "R. Murcia"),
  escanos_2015 = c(14, 7, 3, 2, 10, 27, 7, 6, 10, 4, 9, 6),
  escanos_2019 = c(5, 4, 0, 0, 1, 7, 2, 4, 6, 2, 4, 2),
  
  # https://es.wikipedia.org/wiki/Parlamento_auton%C3%B3mico_(Espa%C3%B1a)
  escanos_totales = c(67, 60, 35, 33, 84, 129, 50, 65, 59, 33, 45, 45)
)

# Porcentajes relativos a cada CCAA
datos <- datos %>% 
  mutate(
    porc_escanos_2015 = round(escanos_2015/escanos_totales, 2),
    porc_escanos_2019 = round(escanos_2019/escanos_totales, 2),
    
    dif_porc_2015_2019 = porc_escanos_2015 - porc_escanos_2019,
    
    # Ordenar por diferencia de escaños de 2015 a 2019
    ccaa = factor(ccaa, ccaa[order(escanos_2015-escanos_2019)])
  )  


# Color morado de Unidas Podemos.
# Se ha obtenido inspeccionando el html de la fuente original
color <- rgb(124, 64, 128, 255, maxColorValue = 255)



# GRÁFICO -----------------------------------------------------------------

# Separación entre barras del gráfico
tam_col <- .8

# Base
p <- datos %>% 
  ggplot(
    aes(x = ccaa,
        y = -porc_escanos_2019)
  )

# Barras
p <- p +
  # barras transparentes (diferencia de porcentaje)
  geom_col(
    aes(y = dif_porc_2015_2019), 
    alpha = 0.4, 
    fill = color, 
    width = tam_col
    ) +
  # barras sólidas (porcentaje de escaños en 2019)
  geom_col(
    fill = color, 
    width = tam_col
    )

porc <- function(x) paste0(x*100, "%")

p <- p +
  geom_text(
    aes(label = porc(porc_escanos_2019)), 
    hjust = .5,
    nudge_y = -0.005, 
    fontface = "bold"
    ) +
  geom_text(
    aes(
      y = dif_porc_2015_2019, 
      label = porc(porc_escanos_2015)
    ), 
    hjust = 0.5,
    nudge_y = 0.005,
    fontface = "bold"
    ) +
  geom_text(
    aes(
      y = (dif_porc_2015_2019)/2, 
      label = porc(dif_porc_2015_2019)
    ), 
    hjust = 0.5,
    color = "white",
    fontface = "bold"
    )

p <- p +
  theme_void() + 
  theme(
    plot.title = 
      element_text(
        family = "sans",
        face   = "bold",
        size   = 17
      ),
    
    plot.subtitle = 
      element_text(
        family="sans", 
        size = 10, 
        margin = margin(t = .25, b = .5, unit = "cm")
      ), 
    
    plot.caption = 
      element_text(
        family="sans",
        face = "bold",
        size = 12, 
        margin = margin(t = 2, unit = "cm"),
        color = "#5a5050"
        ),
    
    plot.margin = 
      margin(
        t = .5, 
        l = .5, 
        r = .5, 
        b = .5, 
        unit = "cm"
      ),
    
    legend.position = "none",
    
    axis.text.y = element_blank()
      # element_text(
      #   size = 10,
      #   face = "bold"
      # )
  )

p <- p +
  coord_flip()

p <- p +
  labs(title = "Unidas Podemos perdió diputados\nen todas las comunidades el 26M",
       subtitle = "Comparación entre el número de escaños de las diferentes candidaturas\nde Unidas Podemos en cada comunidad autónoma en 2015 y en 2019",
       x = "")


# SVG ---------------------------------------------------------------------

ggsave("deconstruyendo/01/img_svg.svg", p)
