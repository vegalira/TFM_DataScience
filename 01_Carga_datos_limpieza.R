install.packages("readxl")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("xlsx")
library(readxl)
library(dplyr)
library(tidyverse)
library(xlsx)



historico_madrid <- read_xlsx("Historico_Madrid_anonimizado.xlsx")
install.packages("sqldf")
library(sqldf)

#para ver la dimensión del dataframe
dim(historico_madrid)
#para ver los nombres de las columnas del dataframe
names(historico_madrid)

#ahora vamos a seleccionar las columnas que aportan valor para el análisis posterior. 
hist_madrid_filtrado = select(historico_madrid, -c(terrace, storage, 
                                                   garage, floor, summary, property_url, 
                                                   lister_name, subtitle, thumb_url, created,
                                                   id, location_accuracy, Municipio, Provincia))



#De la observación de los datos detectamos que es necesario realizar varios pasos antes de poder hacer el modelo



##PASO 1. ELIMINAR DATOS QUE NO CUMPLEN UNAS DETERMINADAS REGLAS

#1.1 Si el tipo de propiedad está vacío hay que eliminarlo. Se trata de errores de volcado que no podemos subsanar

hist_madrid_filtrado <- hist_madrid_filtrado[!is.na(hist_madrid_filtrado$property_type),]

#1.2 Si el código de distrito está vacío hay que eliminarlo. Se trata de errores de volcado que no podemos subsanar

hist_madrid_filtrado <- hist_madrid_filtrado[!is.na(hist_madrid_filtrado$cod_distrito),]


#1.3 Si el año es inferior a 2002, el unitario es mayor que 15.000€ y la superficie menor o igual a 50 m²
#Se trata de errores de volcado que no podemos subsanar


hist_mad_limpio <- hist_madrid_filtrado %>% filter(Fecha >= 2002) %>% 
                                            filter (price_m2<15000 & size >= 50)


  
  
#write.csv(hist_mad_limpio, file = "prueba7.csv")
  
  
  
  