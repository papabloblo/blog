
#' Tratar el .xls obtenido del CIS sobre la
#' ideología de cada partido
#'
#' @param ruta character. Ruta del .xls descargado del 
#' respositorio del CIS.
#'
#' @return tibble
#'
importar_ideologia_partidos <- function(ruta){
  cis <- readxl::read_xls(ruta)
  
  names(cis) <- as.character(cis[4,])
  names(cis)[1] <- "ideologia"
  
  cis <- cis[5:16,]
  
  cis <- cis %>% 
    gather(key = partido, value = porc, -ideologia) %>% 
    mutate(
      partido = stringr::str_remove(partido, " \\(Sólo en.*\\)")
    )
  
  cis <- cis %>% 
    mutate(
      ideologia = stringr::str_remove(ideologia, "\\s(Izquierda|Derecha)"),
      ideologia = case_when(
        ideologia == "N.S." ~ "98",
        ideologia == "N.C." ~ "99",
        TRUE ~ ideologia
      ),
      ideologia = as.integer(ideologia),
      
      porc = as.numeric(porc)/100
    ) 
  
  cis <- cis %>% 
    mutate(
      porc_contestadas = porc,
      porc_contestadas = ifelse(ideologia %in% c(98, 99), 
                                NA, 
                                porc_contestadas
      )
    ) %>% 
    group_by(partido) %>% 
    mutate(
      porc_contestadas = porc_contestadas/sum(porc_contestadas, na.rm = TRUE)
    ) %>% 
    ungroup()
  
  return(cis)
}