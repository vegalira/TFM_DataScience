install.packages("readxl")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("xlsx")
library(readxl)
library(dplyr)
library(tidyverse)
library(xlsx)



historico_madrid <- read_xlsx("C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\01_data\\Historico_Madrid_anonimizado.xlsx")
#install.packages("sqldf")
#library(sqldf)

#para ver la dimensión del dataframe
dim(historico_madrid)
#para ver los nombres de las columnas del dataframe
names(historico_madrid)

#ahora vamos a seleccionar las columnas que aportan valor para el análisis posterior. 
hist_madrid_filtrado = select(historico_madrid, -c(property_type,locality, province, terrace, storage, 
                                                   garage, floor, summary, property_url, 
                                                   lister_name, subtitle, thumb_url, created,
                                                   id, location_accuracy, Municipio, Provincia))


#De la observación de los datos detectamos que es necesario realizar varios pasos antes de poder hacer el modelo



##PASO 1. ELIMINAR DATOS QUE NO CUMPLEN UNAS DETERMINADAS REGLAS

#Eimino este paso!!! de momento!!1.1 Si el tipo de propiedad está vacío hay que eliminarlo. Se trata de errores de volcado que no podemos subsanar

#hist_madrid_filtrado <- hist_madrid_filtrado[!is.na(hist_madrid_filtrado$property_type),]

#1.1 Si el código de distrito está vacío hay que eliminarlo. Se trata de errores de volcado que no podemos subsanar

hist_madrid_filtrado <- hist_madrid_filtrado[!is.na(hist_madrid_filtrado$cod_distrito),]


#1.2 Si el año es inferior a 2002, el unitario es mayor que 15.000€ y la superficie menor o igual a 50 m²
#Se trata de errores de volcado que no podemos subsanar


hist_mad_limpio <- hist_madrid_filtrado %>% filter(Fecha >= 2002) %>% 
                                            filter (price_m2<15000 & size >= 50)



##PASO 2. MISSING VALUES Y RECODIFICACIÓN
#Vamos a analizar las variables que presentan missing values y cómo recodificamos esos valores


#2.1 Analizamos las variables categóricas

table(hist_mad_limpio$bedrooms)#no hay missing values. No necesita acción.
table(hist_mad_limpio$bathrooms)#no hay missing values. No necesita acción.
table(hist_mad_limpio$lift)#no hay missing values porque 0 significa que no tiene.
#No obstante habrá que convertir los >1 en 1 para que sea True or False

table(hist_mad_limpio$swimming_pool)
sum(is.na(hist_mad_limpio$swimming_pool))#existen missing values. Los convertiremos en 0.
#unificaremos también private y share como 1 para que sea True or False

table(hist_mad_limpio$garden)
sum(is.na(hist_mad_limpio$garden))#existen missing values. Los convertiremos en 0.


table(hist_mad_limpio$sports)
sum(is.na(hist_mad_limpio$sports))#existen missing values. Los convertiremos en 0.

table(hist_mad_limpio$status)
sum(is.na(hist_mad_limpio$status))#existen missing values. Los convertiremos en Normal.
#asignaremos una codificación numérica a cada estado de conservación (de 0 a 5)

table(hist_mad_limpio$air_conditioning)
sum(is.na(hist_mad_limpio$air_conditioning))#no hay missing values. No necesita acción.

table(hist_mad_limpio$construction_year)
sum(is.na(hist_mad_limpio$construction_year))#hay missing values. Es dato incompleto. Los eliminamos
#se aprecian errores (años de construcción muy bajos) pero de momento no vamos a modificarlos

table(hist_mad_limpio$exterior)
sum(is.na(hist_mad_limpio$exterior))#no hay missing values. No necesita acción.


#2.2 Eliminamos missing values y recodificamos según proceda

#2.2.1 Lift. Recodificamos todos los >1 a 1


hist_mad_limpio$lift[hist_mad_limpio$lift > 1] <- 1

table(hist_mad_limpio$lift)

#2.2.2 Swimming_pool. Recodificamos private y share a 1 y convertimos los missing values en 0
#convertimos en factor

hist_mad_limpio$swimming_pool <- factor(hist_mad_limpio$swimming_pool)
str(hist_mad_limpio$swimming_pool)

#recodificamos 

library(forcats)
library(dplyr)
library(car)

hist_mad_limpio$swimming_pool = car::recode(hist_mad_limpio$swimming_pool,"'private'=1; 'share'=1; NA=0" )
table(hist_mad_limpio$swimming_pool)

sum(is.na(hist_mad_limpio$swimming_pool))


#2.2.3 Garden. Convertimos los missing values en 0

hist_mad_limpio$garden = car::recode(hist_mad_limpio$garden,"NA=0" )

sum(is.na(hist_mad_limpio$garden))

table(hist_mad_limpio$garden)

#2.2.4 Sports. Convertimos los missing values en 0

hist_mad_limpio$sports = car::recode(hist_mad_limpio$sports,"NA=0" )

sum(is.na(hist_mad_limpio$sports))

table(hist_mad_limpio$sports)


#2.2.5 Status. Recodificamos con una asignación numérica el estado de conservación del inmueble
#Convertimos los missing values en el código asignado a status normal (2)


hist_mad_limpio$status = car::recode(hist_mad_limpio$status,"'Bad'=1; 'Normal'=2; 'Good'=3;'Very Good'=4;NA=2" )

table(hist_mad_limpio$status)
sum(is.na(hist_mad_limpio$status))

#2.2.6 Construction_year. Convertimos los missing values en 0


hist_mad_limpio$construction_year = car::recode(hist_mad_limpio$construction_year,"NA=0" )

sum(is.na(hist_mad_limpio$construction_year))

table(hist_mad_limpio$construction_year)

write.csv(hist_mad_limpio, file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\historico_madrid_limpio.csv")

  
  