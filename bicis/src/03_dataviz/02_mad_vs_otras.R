#' COMPARATIVA DE USO DE MADRID CON EL RESTO DE CIUDADES
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


use <- use %>% 
  filter(
    is.na(user_type) | user_type %in% c("1", "Subscriber"),
    !is.na(hour),
    city != "bo"
  ) %>%
  select(-user_type) %>% 
  collect() %>% 
  mutate(date = lubridate::as_date(date))



use <- use %>%
  mutate(
    city = case_when(
      city == "lo" ~ "LONDRES",
      city == "ny" ~ "NUEVA YORK",
      city == "mad" ~ "MADRID",
      city == "bo" ~ "BOSTON",
      city == "bcn" ~ "BARCELONA"
    ),
    week_day = lubridate::wday(date, 
                               label = TRUE, 
                               abbr = FALSE,
                               week_start = 1
    ),
    day_type = case_when(
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


use_median <- use %>% 
  group_by(city, hour, day_type) %>% 
  summarise(
            q1 = quantile(n, probs = .25),
            q3 = quantile(n, probs = .75),
            n = median(n)) %>% 
  ungroup() #%>% 
  # group_by(city, day_type) %>% 
  # mutate(n = n/max(n)) 
# 
# use_median2 <- use_median %>% filter(city == "Madrid") %>% select(-city)
# 
# n_median2 <- nrow(use_median2)
# 
# use_median2 <- bind_rows(
#   use_median2,
#   use_median2) %>% 
#   mutate(city = rep(c("Londres", "Nueva York"), each = n_median2))

use_median$city <- fct_relevel(use_median$city,
                               c("MADRID",
                                 "BARCELONA",
                                 "LONDRES",
                                 "NUEVA YORK"
                                 )
                               )

# VISUALIZACIÓN -----------------------------------------------------------

# use_median <- use_median %>%
#   group_by(city, day_type) %>%
#   mutate(n = n/sum(n),
#          q1 = q1/sum(q1),
#          q3 = q3/sum(q3)) %>%
#   ungroup()

madrid <- use_median %>%
  filter(city == "MADRID") %>%
  select(hour, day_type, n) %>%
  rename(n_mad = n)

use_median <- use_median %>%
  left_join(madrid)

use_median <- use_median %>% 
  group_by(city, day_type) %>% 
  mutate(n_mad = (max(n)/max(n_mad))*n_mad)

p1 <- use_median %>% 
  filter(city != "MADRID") %>%
  ggplot(
    aes(x = hour, 
        y = n)
  ) +
  # geom_line(aes(group = date), alpha = .03, color = "DimGray") +
  geom_ribbon(aes(ymin = q1, ymax = q3), fill = "lightgray") +
  # geom_line(data = use_median2, alpha = 1, color = "orange2", size = 1.5) +
  geom_line(aes(y = n_mad), 
            color = "orange2",
            size = 1.5, 
            alpha = .9
            ) +
  geom_line(aes(color = city), alpha = 1, 
            # color = "#333333",
            size = 1.5) +
  
  
  facet_wrap(city~ day_type, scales = "free", ncol = 3) +
  # facet_grid(city~ day_type, scales = "free", switch = "y") +
  ylim(0, NA) +
  labs(
    title = "Madrid contra el mundo",
    subtitle = "Uso hora a hora en varias ciudades. Cada línea representa un día.",
    x = "",
    y = "",
    caption = "@papabloblo"
  ) +
  scale_color_manual(values = c("MADRID" = "orange2", 
                                "BARCELONA" = "#333333",
                                "NUEVA YORK" = "#333333",
                                "LONDRES" = "#333333"
                                )
                     ) +
  scale_x_continuous(
    # breaks = c(0, 6, 8, 10, 13, 15, 17, 19), 
    breaks = c(0, seq(6, 23, by = 2)),
    minor_breaks = 0:23,
    labels = function(x) ifelse(x %in% c(0, 6, 13), paste0(x, "h"), x)
  ) +
  theme(
    # strip.text.y = element_text(angle = 180,
    #                             size = 30,
    #                             margin = margin(t = 0, r = 50, b = 1, l = 25),
    #                             vjust = .5,
    #                             hjust = .5
    #                             ),

    strip.placement = "outside",
    legend.position = "none"
  )

# EXPORTACIÓN DE GRÁFICOS -------------------------------------------------

ggsave("bicis/dataviz/madrid_vs_otras.png", p1, width = 40, height = 50, unit = "cm", dpi = 320)
ggsave("bicis/dataviz/madrid_vs_otras.svg", p1, width = 40, height = 50, unit = "cm", dpi = 320)



# CIUDADES SUELTAS --------------------------------------------------------


p1 <- use_median %>% 
  filter(city == "LONDRES") %>%
  ggplot(
    aes(x = hour, 
        y = n)
  ) +
  # geom_line(aes(group = date), alpha = .03, color = "DimGray") +
  geom_ribbon(aes(ymin = q1, ymax = q3), fill = "lightgray") +
  # geom_line(data = use_median2, alpha = 1, color = "orange2", size = 1.5) +
  geom_line(aes(y = n_mad), 
            color = "orange2",
            size = 1.5, 
            alpha = .9
  ) +
  geom_line(aes(color = city), alpha = 1, 
            # color = "#333333",
            size = 1.5) +
  
  
  facet_wrap(~ day_type, ncol = 1, scales = "free_x") +
  # facet_grid(city~ day_type, scales = "free", switch = "y") +
  ylim(0, NA) +
  labs(
    title = "Madrid contra el mundo",
    subtitle = "Uso hora a hora en varias ciudades. Cada línea representa un día.",
    x = "",
    y = "",
    caption = "@papabloblo"
  ) +
  scale_color_manual(values = c("MADRID" = "orange2", 
                                "BARCELONA" = "#333333",
                                "NUEVA YORK" = "#333333",
                                "LONDRES" = "#333333"
  )
  ) +
  scale_x_continuous(
    # breaks = c(0, 6, 8, 10, 13, 15, 17, 19), 
    breaks = c(0, seq(6, 23, by = 2)),
    minor_breaks = 0:23,
    labels = function(x) ifelse(x %in% c(0, 6, 13), paste0(x, "h"), x)
  ) +
  theme(
    # strip.text.y = element_text(angle = 180,
    #                             size = 30,
    #                             margin = margin(t = 0, r = 50, b = 1, l = 25),
    #                             vjust = .5,
    #                             hjust = .5
    #                             ),
    
    strip.placement = "outside",
    legend.position = "none"
  )

# EXPORTACIÓN DE GRÁFICOS -------------------------------------------------

ggsave("bicis/dataviz/londres.png", p1, width = 40, height = 50, unit = "cm", dpi = 320)

# PRUEBAS -----------------------------------------------------------------


p1 <- use_median %>% 
  group_by(city, day_type) %>% 
  mutate(n = n/max(n)) %>% 
  
  # filter(city != "Madrid") %>% 
  ggplot(
    aes(x = hour, 
        y = n)
  ) +
  # geom_line(aes(group = date), alpha = .03, color = "DimGray") +
  # geom_ribbon(aes(ymin = q1, ymax = q3), fill = "lightgray") +
  # geom_line(data = use_median2, alpha = 1, color = "orange2", size = 1.5) +
  geom_line(aes(color = city), alpha = 1, 
            # color = "#333333", 
            size = 1.5) +
  facet_wrap(~ day_type, scales = "free", ncol = 1) +
  ylim(0, NA) +
  labs(
    title = "Madrid contra el mundo",
    subtitle = "Uso hora a hora en varias ciudades. Cada línea representa un día.",
    x = "",
    y = "",
    caption = "@papabloblo"
  ) +
  scale_color_manual(values = c("MADRID" = "orange2", "BARCELONA" = "#333333", "NUEVA YORK" = "#333333", "LONDRES" = "#333333")) +
  scale_x_continuous(
    # breaks = c(0, 6, 8, 10, 13, 15, 17, 19), 
    breaks = c(0, seq(6, 23, by = 2)),
    minor_breaks = 0:23,
    labels = function(x) ifelse(x %in% c(0, 6, 13), paste0(x, "h"), x)
  ) +
  theme_minimal() +
  theme(
    text = element_text(color = "#333333"),
    plot.caption = element_text(family = "Montserrat"),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.text.y = element_text(hjust = 0, family = "Oswald", size = 12),
    axis.text.x.top = element_text(hjust = 0, family = "Oswald", size = 12),
    axis.text.x = element_text(family = "Roboto", size = 12),
    
    # strip.text.x = element_text(angle = 0, 
    #                             hjust = 0,
    #                             size = 20,
    #                             family = "Oswald",
    #                             face = "bold",
    #                             margin = margin(t = 5, b = 1),
    #                             color = "#333333"
    # ),
    
    plot.title = element_text(face = "bold",
                              size = 50, 
                              family = "Oswald",
                              margin = margin(b = 10),
                              hjust = 0.5
    ),
    plot.subtitle = element_text(size = 15, 
                                 face = "italic",
                                 family = "Oswald",
                                 margin = margin(b = 20),
                                 hjust = 0.5
    ),
    # panel.grid.minor = element_blank(),
    # axis.text.y = element_blank(),
    
    # panel.grid.major.y  = element_blank(),
    # strip.text.y = element_text(angle = 0, hjust = 0, size = 5, family = "Oswald", face = "plain"),
    panel.spacing  = unit(1, "cm"),
    strip.text = element_text(angle = 0, hjust = 0, size = 20, family = "Oswald", face = "bold")
    
    
    
  )

ggsave("bicis/dataviz/pruebas.png", p1, width = 40, height = 50, unit = "cm", dpi = 320)
