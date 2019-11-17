#' NÚMERO DE VIAJES POR HORAS EN MADRID
#' 
#' Visualización de datos para los viajes que se realizan
#' agregados por hora y día. 
#' 
#' Se tienen en cuenta los días de la semana así como los festivos.
#' Solo se consideran usuarios con abono normal.
#' 


# DEPENDENCIAS ------------------------------------------------------------

library(tidyverse)
db_connection <- DBI::dbConnect(RSQLite::SQLite(),
                                "bicis/data/data-warehouse.db"
                                )

source("bicis/src/03_dataviz/tema_ggplot2.R")
theme_set(tema())
# CARGA DE DATOS ----------------------------------------------------------

use <- tbl(src = db_connection, "use")

calendar <- 
  read_csv2(
    "bicis/data/calendario_laboral_mad.csv",
    col_types = cols(
      Dia = col_date(format = "%d/%m/%Y"),
      Dia_semana = col_character(),
      `laborable / festivo / domingo festivo` = col_character(),
      `Tipo de Festivo` = col_character(),
      Festividad = col_character()
      )
    )


# TRATAMIENTO DE DATOS ----------------------------------------------------

calendar <- calendar %>% 
  transmute(
    date = Dia,
    day_type = `laborable / festivo / domingo festivo`
    )

# En 2019, day_type es "festivo" cyunado es día de fiesta y NA cuando es laboral
calendar <- calendar %>% 
  mutate(
    day_type = if_else(is.na(day_type), 
                       "laborable", 
                       day_type
                       )
    )


use <- use %>% 
  filter(
    city == "mad",
    user_type == "1"
    ) %>% 
  select(-user_type, -city) %>% 
  collect() %>% 
  mutate(date = lubridate::as_date(date))

use <- left_join(use,
                 calendar,
                 by = "date"
                 )



use <- use %>%
  mutate(
    week_day = lubridate::wday(date, 
                               label = TRUE, 
                               abbr = FALSE,
                               week_start = 1
                               ),
    day_type = case_when(
      day_type == "festivo" ~ "Fin de semana y festivos",
      week_day %in% c("sábado", "domingo") ~ "Fin de semana y festivos",
      week_day == "viernes" ~ "Viernes",
      TRUE ~ "Lunes a jueves"
      )
    )

use$day_type <- factor(use$day_type, 
                       levels = c("Lunes a jueves",
                                  "Viernes",
                                  "Fin de semana y festivos"
                                  ),
                       ordered = TRUE
                       )

# Eliminar datos anómalos

use_outliers <- use %>%
  group_by(week_day, date) %>% 
  summarise(viajes = sum(n)) %>% 
  arrange(viajes)

date_outliers <- use_outliers %>% 
  mutate(
    q3 = quantile(viajes, prob = .95),
    q1 = quantile(viajes, prob = .05)
    ) %>% 
  filter(viajes > q3 | viajes < q1) %>% 
  pull(date)

# date_outliers <- use_outliers$date[c(1:5, (nrow(use_outliers)-5):nrow(use_outliers))]

use <- use %>% 
  filter(
    !date %in% date_outliers,
    # Hay un dato anómalo a las 19h
    date != lubridate::as_date("2019-06-26")
    )

use_median <- use %>% 
  group_by(day_type, hour) %>% 
  summarise(n = median(n))

# VISUALIZACIÓN -----------------------------------------------------------

p <- use %>% 
  ggplot(
    aes(x = hour, 
        y = n)
  ) +
  geom_line(aes(group = date), alpha = .03, color = "DimGray") +
  geom_line(data = use_median, alpha = 1, color = "orange2", size = 1.5) +
  facet_wrap(~ day_type, ncol = 1, scales = "free_x") +
  labs(
    title = "¿Cómo nos movemos\nen bici en Madrid?",
    subtitle = "Uso hora a hora de BiciMAD. Cada línea representa un día.",
    x = "",
    y = "",
    caption = "@papabloblo"
  ) +
  scale_x_continuous(
    breaks = c(0, 6, 8, 10, 13, 15, 17, 19), 
    minor_breaks = 0:23,
    labels = function(x) ifelse(x %in% c(0, 6, 13), paste(x, "h"), x)
  ) +
  scale_y_continuous(
    labels = function(x) ifelse(x == 1000, paste(x, "viajes"), x)
  ) 

# EXPORTACIÓN DE GRÁFICOS -------------------------------------------------

ggsave("bicis/dataviz/madrid.png", p, width = 25, height = 40, unit = "cm", dpi = 320)
ggsave("bicis/dataviz/madrid.svg", p, width = 25, height = 25, unit = "cm")



