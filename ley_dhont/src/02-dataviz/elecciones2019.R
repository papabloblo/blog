
elecciones <- read_rds("ley_dhont/data/elecciones.rds")

elecciones <- elecciones %>% 
  filter(!is.na(partido)) %>% 
  mutate(
    votos_porc = votos_porc/100
    ) %>% 
  group_by(anyo) %>% 
  mutate(diputados_porc = diputados/sum(diputados, na.rm = T)) %>% 
  ungroup() %>% 
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

elecciones %>% 
  ggplot(aes(x = votos_porc, y = diputados_porc)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "firebrick")

orden <- elecciones %>% 
  filter(anyo == 2019) %>% 
  arrange(diputados, votos) %>% 
  pull(partido)

p <- elecciones %>% 
  filter(anyo == 2019) %>% 
  arrange(-diputados, -votos) %>% 
  head(15) %>% 
  ggplot(aes(y = factor(partido, levels = orden), color = as.character(hjust))) +
  # geom_segment(
  #   aes(
  #     x = votos_porc,
  #     xend = diputados_porc,
  #     yend = partido
  #   ),
  #   size = 3,
  #   lineend = "round",
  #   linejoin = "round",
  #   color = "gray"
  # ) +
  geom_point(aes(x = votos_porc),
             # color = "steelblue",
             size = 2,
             # alpha = 0.7, 
             stroke = 0) +
  
 
  geom_segment(
    aes(
      x = votos_porc,
      xend = diputados_porc,
      yend = partido
    ),
    arrow = arrow(length = unit(0.1, "cm"), type = "closed"),
    # lineend = "round",
    linejoin = "round",
    # color = "steelblue",
    size = .5
  ) +
  # geom_point(aes(x = diputados_porc), 
  #            color = "firebrick",
  #            size = 3,
  #            alpha = 0.7,
  #            stroke = 0
  # ) +
  geom_label(aes(x = label + 0.01, label = partido2, hjust = 0),
             family="sans", #lineheight=0.95,
             size = 4, label.size=0#, 
             # color="#5a5050",
             # vjust = -0.5
             # hjust = 1.5
             ) +
  scale_color_manual(
    values = c("1" = "steelblue",
               "0"="firebrick"
    )
  )+
  xlim(NA, 0.4) +
  theme_minimal() +
  labs(
    x = "",
    y = "",
    title = "La ley d'Hont"
  ) +
  tema +
  theme(
    axis.text = element_blank(),
    panel.grid.major.y = element_blank()
  )

ggsave("mi.png", plot = p, dpi = 1000, width = 20, height = 15, units = "cm")




p <- elecciones %>% 
  filter(anyo == 2019) %>% 
  arrange(-diputados, -votos) %>% 
  head(15) %>% 
  ggplot(aes(y = factor(partido, levels = orden))) +
  geom_segment(
    aes(
      x = votos_porc,
      xend = diputados_porc,
      yend = partido
    ),
    size = 3,
    lineend = "round",
    linejoin = "round",
    color = "gray"
    # color = "steelblue"
  ) +
geom_point(aes(x = votos_porc),
           color = "steelblue",
           size = 3,
           # alpha = 0.7, 
           stroke = 0) +
  
  
  
  geom_point(aes(x = diputados_porc),
             color = "firebrick",
             size = 3,
             alpha = 0.7,
             stroke = 0
  ) +
  geom_label(aes(x = label + 0.01, label = partido2, hjust = 0),
             family="sans", #lineheight=0.95,
             size = 2.5, label.size=0#, 
             # color="#5a5050",
             # vjust = -0.5
             # hjust = 1.5
  ) +
  geom_vline(xintercept = 0) +
  # geom_segment(
  #   aes(
  #     x = votos_porc,
  #     xend = diputados_porc,
  #     yend = partido,
  #     color = as.character(abs(hjust-1))
  #   ),
  #   arrow = arrow(length = unit(0.1, "cm"), type = "closed"),
  #   lineend = "round",
  #   linejoin = "round",
  #   
  #   size = 1
  # ) +
  scale_color_manual(
    values = c("1" = "steelblue",
               "0"="firebrick"
               )
    )+
  xlim(NA, 0.4) +
  theme_minimal() +
  labs(
    x = "",
    y = "",
    title = "La ley d'Hont"
  ) +
  tema +
  theme(
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank()
  )
ggsave("mi2.png", plot = p, dpi = 1000, width = 20, height = 15, units = "cm")



p <- elecciones %>% 
  filter(anyo == 2019) %>% 
  select(partido, votos_porc, diputados_porc, hjust) %>% 
  gather(key = "tipo", value = "porc", -partido, -hjust) %>% 
  ggplot(aes(
    x = tipo,
    y = porc,
    color = as.character(hjust)
  )) +
  geom_label(data = elecciones %>% filter(anyo == 2019, votos_porc >= 0.03), 
             aes(x = "diputados_porc", y = diputados_porc, label = partido),
             hjust = 1,
             family="sans", #lineheight=0.95,
             size = 4, label.size=0#, 
             )+
  geom_label(data = elecciones %>% filter(anyo == 2019, votos_porc >= 0.03), 
             aes(x = "votos_porc", y = votos_porc, label = partido2),
             hjust = 0,
             family="sans", #lineheight=0.95,
             size = 4, label.size=0#, 
  )+
  geom_hline(yintercept = c(0, 25, 50, 75, 100, 125)/350, linetype = "dashed") +
  geom_label(aes(label = "0 diputados", x = "diputados_porc"), y = 0, nudge_x = 0.3, label.size=0, size = 5)+
  geom_label(aes(label = "25 diputado ", x = "diputados_porc"), y = 25/350, nudge_x = 0.3, label.size=0, size = 5)+
  geom_label(aes(label = "50 diputados", x = "diputados_porc"), y = 50/350, nudge_x = 0.3, label.size=0, size = 5)+
  geom_label(aes(label = "75 diputados", x = "diputados_porc"), y = 75/350, nudge_x = 0.3, label.size=0, size = 5)+
  geom_label(aes(label = "100 diputados", x = "diputados_porc"), y = 100/350, nudge_x = 0.3, label.size=0, size = 5)+
  geom_label(aes(label = "125 diputados", x = "diputados_porc"), y = 125/350, nudge_x = 0.3, label.size=0, size = 5)+
  geom_line(aes(group = partido)) +
  geom_hline(yintercept = 1/350) +
  scale_color_manual(
    values = c("1" = "steelblue",
               "0"="firebrick"
    )
  ) +
  tema

ggsave("slope.png", plot = p, dpi = 400, width = 20, height = 30, units = "cm")

elecciones %>% 
  filter(anyo == 2019) %>% 
  summarise(
    diputados = sum(diputados),
    votos = sum(votos)
  ) %>% 
  mutate(
    votos_diputado = votos/diputados
  )


p <- elecciones %>% 
  filter(anyo == 2019, votos_porc < 0.03) %>%
  select(partido, votos_porc, diputados_porc, hjust) %>% 
  gather(key = "tipo", value = "porc", -partido, -hjust) %>% 
  ggplot(aes(
    x = tipo,
    y = porc,
    color = as.character(hjust)
  )) +
  geom_line(aes(group = partido)) +
  scale_color_manual(
    values = c("1" = "steelblue",
               "0"="firebrick"
    )
  ) +
  geom_hline(yintercept = 1/350) +
  tema

ggsave("slope2.png", plot = p, dpi = 400, width = 20, height = 30, units = "cm")



slope <- elecciones %>% 
  filter(anyo == 2019) %>%
  select(partido, votos_porc, diputados_porc, hjust) %>% 
  gather(key = "tipo", value = "porc", -partido, -hjust)


partidos_porc <- elecciones %>% 
  filter(anyo == 2019, 
         votos_porc >= 0.03) %>% 
  pull(partido)

(p <- slope %>% 
    ggplot(aes(
      x = factor(tipo,levels = c("votos_porc", "diputados_porc")),
      y = porc#,
      # color = as.character(hjust)
    )) +
    geom_hline(yintercept = seq(0, 125, by = 25)/350, linetype = "dashed") +
    geom_line(aes(group = partido), alpha = 0.25) +
    geom_line(data = slope %>% filter(partido %in% partidos_porc),
              aes(group = partido, color = as.character(hjust)),
              size = 1.5) +
    geom_point(data = slope %>% filter(partido %in% partidos_porc),aes(color = as.character(hjust)), size = 3)+
    scale_color_manual(
      values = c("1" = "steelblue",
                 "0"="firebrick"
      )
    ) +
    
    geom_label(data = elecciones %>% filter(anyo == 2019, partido %in% partidos_porc), 
               aes(x = "votos_porc", y = votos_porc, label = partido2),
               hjust = 1,
               family="sans", #lineheight=0.95,
               size = 3.5, 
               label.size=0,
               nudge_x = -0.02
    ) +
    # geom_label(data = elecciones %>% filter(anyo == 2019, votos_porc < 0.03, partido %in% partidos_porc), 
    #            aes(x = "diputados_porc", y = diputados_porc, label = partido2),
    #            hjust = 0,
    #            family="sans", #lineheight=0.95,
    #            size = 2.5, label.size=0#, 
    # )+
    
    labs(
      x = "",
      y = "",
      title = "El PACMA sería la décima fuerza política\ncon 4 diputados"
    ) +
    scale_x_discrete(
      labels = c("votos_porc" = "Votos\nobtenidos",
                 "diputados_porc" = "Diputados\nconseguidos"
      )
    )+
    geom_label(aes(label = "0 diputados", x = "diputados_porc"), y = 0, nudge_x = 0.3, label.size=0, size = 5)+
    geom_label(aes(label = "25 diputado ", x = "diputados_porc"), y = 25/350, nudge_x = 0.3, label.size=0, size = 5)+
    geom_label(aes(label = "50 diputados", x = "diputados_porc"), y = 50/350, nudge_x = 0.3, label.size=0, size = 5)+
    geom_label(aes(label = "75 diputados", x = "diputados_porc"), y = 75/350, nudge_x = 0.3, label.size=0, size = 5)+
    geom_label(aes(label = "100 diputados", x = "diputados_porc"), y = 100/350, nudge_x = 0.3, label.size=0, size = 5)+
    geom_label(aes(label = "125 diputados", x = "diputados_porc"), y = 125/350, nudge_x = 0.3, label.size=0, size = 5)+
    tema +
    theme(
      panel.grid.major.y  = element_blank(),
      panel.grid.minor.y  = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(size = 17)
    )
)

ggsave("slope.png", plot = p, dpi = 400, width = 20, height = 30, units = "cm")

slope <- elecciones %>% 
  filter(anyo == 2019, votos_porc < 0.015) %>%
  select(partido, votos_porc, diputados_porc, hjust) %>% 
  gather(key = "tipo", value = "porc", -partido, -hjust)


partidos_porc <- elecciones %>% 
  filter(anyo == 2019, xor(votos_porc >= 1/350, diputados_porc >= 1/350)) %>% 
  pull(partido)

(p <- slope %>% 
  ggplot(aes(
    x = factor(tipo,levels = c("votos_porc", "diputados_porc")),
    y = porc#,
    # color = as.character(hjust)
  )) +
  geom_hline(yintercept = 0:4/350, linetype = "dashed") +
  geom_line(aes(group = partido), alpha = 0.25) +
  geom_line(data = slope %>% filter(partido %in% partidos_porc),
            aes(group = partido, color = as.character(hjust)),
            size = 1.5) +
  geom_point(data = slope %>% filter(partido %in% partidos_porc),aes(color = as.character(hjust)), size = 3)+
  scale_color_manual(
    values = c("1" = "steelblue",
               "0"="firebrick"
    )
  ) +
  
  geom_label(data = elecciones %>% filter(anyo == 2019, votos_porc < 0.03, partido %in% partidos_porc), 
             aes(x = "votos_porc", y = votos_porc, label = partido2),
             hjust = 1,
             family="sans", #lineheight=0.95,
             size = 3.5, 
             label.size=0,
             nudge_x = -0.02
  ) +
  # geom_label(data = elecciones %>% filter(anyo == 2019, votos_porc < 0.03, partido %in% partidos_porc), 
  #            aes(x = "diputados_porc", y = diputados_porc, label = partido2),
  #            hjust = 0,
  #            family="sans", #lineheight=0.95,
  #            size = 2.5, label.size=0#, 
  # )+
  
  labs(
    x = "",
    y = "",
    title = "El PACMA pasaría de 0 a 4 diputados"
    ) +
    scale_x_discrete(
      labels = c("votos_porc" = "Votos\nobtenidos",
                 "diputados_porc" = "Diputados\nconseguidos"
                 )
      )+
    geom_label(aes(label = "0 diputados", x = "diputados_porc"), y = 0, nudge_x = 0.3, label.size=0, size = 5)+
    geom_label(aes(label = "1 diputado ", x = "diputados_porc"), y = 1/350, nudge_x = 0.3, label.size=0, size = 5)+
    geom_label(aes(label = "2 diputados", x = "diputados_porc"), y = 2/350, nudge_x = 0.3, label.size=0, size = 5)+
    geom_label(aes(label = "3 diputados", x = "diputados_porc"), y = 3/350, nudge_x = 0.3, label.size=0, size = 5)+
    geom_label(aes(label = "4 diputados", x = "diputados_porc"), y = 4/350, nudge_x = 0.3, label.size=0, size = 5)+
    tema +
    theme(
      panel.grid.major.y  = element_blank(),
      panel.grid.minor.y  = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(size = 17)
      )
  )


ggsave("slope3.png", plot = p, dpi = 400, width = 20, height = 30, units = "cm")
ggsave("slope3.svg", plot = p, dpi = 400, width = 20, height = 30, units = "cm")

elecciones %>% 
  ggplot(aes(x = votos_porc,
             y = diputados_porc)) +
  geom_point(alpha = 0.25, size = 3, stroke = 0) +
  geom_point(data = elecciones %>% filter(partido == "PSOE"), alpha = 0.7, size = 3, stroke = 0) +
  geom_abline(intercept = 0, slope = 1) +
  geom_smooth() +
  tema



dif_diputados <- elecciones %>% 
  filter(anyo == 2019) %>% 
  arrange(-votos_porc) %>% 
  mutate(
    votos2 = votos_porc %/% (1/360),
    inc_diputados = diputados - votos2)

orden_dif_diputados <- dif_diputados %>% 
 arrange(votos_porc) %>% 
  pull(partido)

dif_diputados %>% 
  head(20) %>% 
ggplot(
    aes(x = inc_diputados,
        y = factor(partido, orden_dif_diputados)
        )
  ) +
  geom_segment(xend = 0, aes(yend = partido)) +
  geom_point() +
  tema
