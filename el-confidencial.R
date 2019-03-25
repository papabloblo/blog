#'
#' Gráfico de la estimación de voto
#' de El confidencial.
#' 
#' Enlace:
#'  https://www.elconfidencial.com/elecciones-generales/2019-03-25/elecciones-encuestas-28a-sanchez-independentista-tracking_1900534/
#'


# DATOS -------------------------------------------------------------------

estimacion_votos <- dplyr::tibble(
  partido = c(
    "PSOE",
    "PP",
    "Cs",
    "UP",
    "VOX",
    "ERC",
    "PACMA",
    "PDeCAT",
    "PNV",
    "EH-Bildu",
    "CC",
    "Compr."
  ),
  intencion_28A = c(
    0.35,
    0.195,
    0.144,
    0.124,
    0.11,
    0.035,
    0.021,
    0.013,
    0.013,
    0.01,
    0,
    0
  ),
  intencion_26J = c(
    0.2263,
    0.3301,
    0.1306,
    0.2115,
    0.002,
    0.0263,
    0.0119,
    0.0201,
    0.0119,
    0.0077,
    0.0033,
    0
  ),
  escanos_26J = c(
    85,
    137,
    32,
    71,
    0,
    9,
    0,
    8,
    5,
    2,
    1,
    0
  ),
  escanos_28A = c(
    131,
    75,
    52,
    31,
    29,
    14,
    1,
    5,
    6,
    3,
    1,
    2
  )
)

library(tidyverse)
estimacion_votos %>% 
  ggplot(aes(x = intencion_26J,
             y = intencion_28A)) + 
  geom_point()
