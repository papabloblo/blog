tema <- theme_minimal(base_family = "sans") +
  theme(plot.title=element_text(#family ="Poiret One",
    family = "sans",
    face   = "bold",
    margin = margin(t = 5, b=5),
    size   = 25,
    color = "#5a5050"),
    plot.subtitle=element_text(family="sans", 
                               size = 13, 
                               margin = margin(b=5),
                               color = "#5a5050"
    ),
    plot.caption = element_text(family="Poiret One",
                                face = "bold",
                                size = 12, 
                                margin = margin(b = 5),
                                color = "#5a5050"),
    plot.margin = margin(l = 0, r = 2, unit = "cm"),
    legend.position = "none"
  )
