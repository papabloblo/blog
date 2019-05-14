tema <- theme_minimal(base_family = "sans") +
  theme(
    plot.title = 
      element_text(#family ="Poiret One",
        # family = "sans",
        family = "Montserrat",
        face   = "bold",
        # margin = margin( l = 0, b = 0, unit = "cm"),
        size   = 25,
        color = "#5a5050"
        ),
    
    plot.subtitle = 
      element_text(family="sans", 
                   size = 13, 
                   margin = margin(b=1, unit = "cm"),
                   color = "#5a5050"
                   ),
    
    plot.caption = 
      element_text(family="sans",
                   face = "bold",
                   size = 12, 
                   margin = margin(t = 2, unit = "cm"),
                   color = "#5a5050"),
    
    plot.margin = 
      margin(
        t = .5, 
        l = .5, 
        r = .5, 
        b = .5, 
        unit = "cm"
        ),
    
    legend.position = "none",
    
    axis.text = 
      element_text(
        
      ),
    axis.text.y = 
      element_text(
        size = 10,
        face = "bold"
      ),
    
    axis.text.x = 
      element_text(
        size = 11
      ),
    
    axis.title.x = 
      element_text(
        family = "sans",
        face = "bold",
        size = 13,
        margin = margin(t = .5, unit = "cm"),
        color = "#5a5050"
      )
    
  )
