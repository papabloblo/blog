#'
#' DESCARGA DEL CALENDARIO LABORAL EN LA CIUDAD DE MADRID
#' 
#' Se genera el archivo data/raw/calendario_laboral.csv
#' 
#' Fuente: 
#' https://datos.madrid.es/portal/site/egob/menuitem.c05c1f754a33a9fbe4b2e4b284f1a5a0/?vgnextoid=9f710c96da3f9510VgnVCM2000001f4a900aRCRD&vgnextchannel=374512b9ace9f310VgnVCM100000171f5a0aRCRD&vgnextfmt=default



download.file("https://datos.madrid.es/egob/catalogo/300082-0-calendario_laboral.csv",
              "bicis/data/calendario_laboral_mad.csv")
