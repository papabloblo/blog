

lectura_excel <- function(path){
  elecciones <- readxl::read_xlsx(path)
  
  fecha <- stringr::str_remove(path, "ley_dhont/data/raw/PROV_02_")
  fecha <- stringr::str_remove(fecha, "_1.xlsx")
  
  if (sum(as.vector(is.na(elecciones[1,]))) == ncol(elecciones)){
    elecciones <- elecciones[2:nrow(elecciones),]  
  }
  
  # NOMBRES DE LAS CANDIDATURAS ---------------------------------------------
  candidaturas <- elecciones
  
  ini_partidos <- as.matrix(candidaturas[2, ])
  ini_partidos <- which(!is.na(ini_partidos))[1]
  
  candidaturas <- candidaturas[1:2, ini_partidos:ncol(candidaturas)]
  
  candidaturas <- candidaturas %>% select_if(function(x)!anyNA(x))
  
  siglas <- as.matrix(candidaturas)[2,]
  nombre_partido <- as.matrix(candidaturas)[1,]
  
  candidaturas <- tibble(
    siglas = stringr::str_remove_all(siglas, "\\."),
    nombre_partido = nombre_partido
  ) %>% 
    mutate(
      siglas = stringr::str_remove_all(siglas, "\\'")
    )
  
  conteo_siglas <- table(candidaturas$siglas)
  
  for (i in names(conteo_siglas)[conteo_siglas > 1]){
    candidaturas$siglas[candidaturas$siglas == i] <- paste0(i, 
                                                            1:length(candidaturas$siglas[candidaturas$siglas == i])
    )
  }
  
  # VOTOS POR CANDIDATURA ---------------------------------------------------
  
  names(elecciones) <- as.matrix(elecciones[3,])[1,]
  names(elecciones) <- tolower(names(elecciones))
  names(elecciones) <- stringr::str_replace_all(names(elecciones), 
                                                "( de | )", 
                                                "_"
  )
  
  
  # TODO: quitar acentos
  
  names(elecciones)[ini_partidos:ncol(elecciones)] <- paste(names(elecciones)[ini_partidos:ncol(elecciones)],
                                                            rep(candidaturas$siglas, 
                                                                each = 2
                                                            ), 
                                                            sep = "_")
  
  
  elecciones <- elecciones[4:nrow(elecciones),]
  elecciones2 <- elecciones[,c(1, ini_partidos:ncol(elecciones))]
  
  elecciones2 <- elecciones2 %>% 
    gather(key = "partido", value = "votos", -nombre_comunidad) %>% 
    filter(!is.na(nombre_comunidad)) %>% 
    mutate(votos = as.integer(votos)) %>% 
    group_by(partido) %>% 
    summarise(votos = sum(votos, na.rm = TRUE))
  
  diputados <- elecciones2 %>% 
    filter(stringr::str_detect(partido, "diputados_")) %>% 
    mutate(
      partido = stringr::str_remove_all(partido,"diputados_")
    ) %>% 
    rename(diputados = votos)
  
  votos <- elecciones2 %>% 
    filter(stringr::str_detect(partido, "votos_")) %>% 
    mutate(
      partido = stringr::str_remove_all(partido,"votos_")
    )
  
  elecciones <- votos %>% 
    left_join(diputados, by = "partido")
  
  elecciones$anyo <- as.integer(stringr::str_sub(fecha, 1, 4))
  elecciones$mes <- as.integer(stringr::str_sub(fecha, 5, 6))
  
  elecciones <- elecciones %>% select(anyo, mes, partido, votos, diputados)
  return(elecciones)
}
