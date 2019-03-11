library(tidyverse)



participacion %>% 
  mutate(ciudad = factor(ciudad,
                         unique(ciudad))
  ) %>% 
  ggplot(aes(x = ciudad,
             y = asistencia,
             fill = as.character(anyo))) + 
  geom_col(position = "dodge") + 
  # geom_text(aes(label = asistencia, y = 100),
  #         
  #           hjust =-0.25,
  #           color="white",
  #           size=7,
  #           position = position_dodge(0.9),
  #           angle = 90) +
 
  labs(
    fill = "",
    x = "",
    y = "",
    title = "Asistencia manifestaciones 8M") +
  theme_minimal() + 
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_blank()
    ) + 
  scale_fill_manual(values = c("#3eaca8", "#547a82"))




p <- participacion %>% 
  ggplot(
    aes(
      x = factor(anyo, c(2018, 2019)),
      y = asistencia,
      group = ciudad
    )
  ) + 
  geom_point(color = "#3eaca8") + 
  geom_line(color = "#3eaca8") + 
  # geom_text(aes(label = asistencia), nudge_x = 0.5) +
  labs(
    fill = "",
    x = "",
    y = "",
    title = "Asistencia manifestaciones 8M") +
  theme_void() +
  scale_fill_manual(values = c("#3eaca8", "#547a82"))


ggplot2::ggsave(filename = "dataviz/prueba.svg", p)
