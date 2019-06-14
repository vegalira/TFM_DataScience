install.packages("tidyverse")
install.packages("readxl")
install.packages("dplyr")
install.packages("prophet")

library(prophet)
library(tidyverse)
library(readxl)
library(dplyr)
library(lubridate)
library(reshape2)
library(ggplot2)



distritos_original <- read_csv("C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\historico_madrid_limpio.csv")

#con el excel conjunto que se generará a partir de este código y con los datos reales del 2018
#visualizaremos en tableau la evolución de los precios por distritos

#dataframe con datos reales de 2018 

distritos_2018 <- distritos_original %>% 
  filter(year == 2018) %>% 
  group_by(year_distrito) %>% 
  summarise(mean(price_m2))

write.table(distritos_2018, file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\precios_distritos_18.csv", sep=",", append=TRUE)


##################DISTRITO 01##########################################


## selecciono distrito 01 y me quedo solo con las columnas Fecha y price

distrito_01 <- distritos_original %>% 
  filter(cod_distrito=="01") %>% 
  select(Fecha, price_m2, year)

#creo una columna para tener el mes, que sera la agrupacion que utilice para mis datos, junto con el año

distrito_01 <- mutate(distrito_01, mes =month(as.POSIXlt(distrito_01$Fecha, format="%y-%m-%d")))

#ahora creo una columna que una el mes y el a?o

distrito_01 <- unite(distrito_01, mes_year,c(4,3),  sep = "", remove = TRUE)


#ahora voy a realizar una agrupacion de los datos por el campo mes_year

distrito_01_group <- distrito_01 %>% 
  group_by(mes_year) %>% 
  mutate(fecha=Fecha) %>% 
  group_by(Fecha)

#agrupado previamente por mes ahora hago la media del unitario por mes_year
distrito_01_mes <- distrito_01_group %>% summarise(y= mean(price_m2))

#renombramos la columna 1 para que pueda utilizarse en prophet

names(distrito_01_mes)[1] = "ds"

#Realizamos el BoxCox para obtener el mejor lambda
lam_01 = BoxCox.lambda(distrito_01_mes$y, method = "loglik")
distrito_01_mes$y = BoxCox(distrito_01_mes$y, lam_01)

ggplot(distrito_01_mes, aes(x=ds, y=y )) + 
  geom_line(colour="grey")  + 
  geom_point( size=2, shape=20, fill="white", colour="blue") + 
  theme_minimal()


prediccion_01 <- prophet(distrito_01_mes, daily.seasonality=TRUE)
future_01 <- make_future_dataframe(prediccion_01, periods = 730)
forecast_01 <- predict(prediccion_01, future_01)
plot(prediccion_01, forecast_01)
prophet_plot_components(prediccion_01, forecast_01)

forecast_inverso_01 <- forecast_01
forecast_inverso_01$yhat_transformado = InvBoxCox(forecast_01$yhat, lam_01)

dyplot.prophet(prediccion_01, forecast_01)


forecast_df_01 <- as.data.frame(forecast_inverso_01)

distrito_01_forecast <- forecast_df_01 %>% 
  select(ds,yhat_transformado) %>% 
  mutate(distrito= "01")


write.table(distrito_01_forecast, file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_prophet_distritos.csv", sep=",", append=TRUE)


############## DISTRITO 02  ################################################

## selecciono distrito 02 y me quedo solo con las columnas Fecha y price

distrito_02 <- distritos_original %>% 
  filter(cod_distrito=="02") %>% 
  select(Fecha, price_m2, year)

#creo una columna para tener el mes, que sera la agrupacion que utilice para mis datos, junto con el año

distrito_02 <- mutate(distrito_02, mes =month(as.POSIXlt(distrito_02$Fecha, format="%y-%m-%d")))

#ahora creo una columna que una el mes y el a?o

distrito_02 <- unite(distrito_02, mes_year,c(4,3),  sep = "", remove = TRUE)


#ahora voy a realizar una agrupacion de los datos por el campo mes_year

distrito_02_group <- distrito_02 %>% 
  group_by(mes_year) %>% 
  mutate(fecha=Fecha) %>% 
  group_by(Fecha)

#agrupado previamente por mes ahora hago la media del unitario por mes_year
distrito_02_mes <- distrito_02_group %>% summarise(y= mean(price_m2))

#renombramos la columna 1 para que pueda utilizarse en prophet

names(distrito_02_mes)[1] = "ds"

#Realizamos el BoxCox para obtener el mejor lambda
lam_02 = BoxCox.lambda(distrito_02_mes$y, method = "loglik")
distrito_02_mes$y = BoxCox(distrito_02_mes$y, lam_02)

ggplot(distrito_02_mes, aes(x=ds, y=y )) + 
  geom_line(colour="grey")  + 
  geom_point( size=2, shape=20, fill="white", colour="blue") + 
  theme_minimal()


prediccion_02 <- prophet(distrito_02_mes, daily.seasonality=TRUE)
future_02 <- make_future_dataframe(prediccion_02, periods = 730)
forecast_02 <- predict(prediccion_02, future_02)
plot(prediccion_02, forecast_02)
prophet_plot_components(prediccion_02, forecast_02)

forecast_inverso_02 <- forecast_02
forecast_inverso_02$yhat_transformado = InvBoxCox(forecast_02$yhat, lam_02)

dyplot.prophet(prediccion_02, forecast_02)


forecast_df_02 <- as.data.frame(forecast_inverso_02)

distrito_02_forecast <- forecast_df_02 %>% 
  select(ds,yhat_transformado) %>% 
  mutate(distrito= "02")


write.table(distrito_02_forecast, file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_prophet_distritos.csv", sep=",", append=TRUE)

############## DISTRITO 03  ################################################

## selecciono distrito 03 y me quedo solo con las columnas Fecha y price

distrito_03 <- distritos_original %>% 
  filter(cod_distrito=="03") %>% 
  select(Fecha, price_m2, year)

#creo una columna para tener el mes, que sera la agrupacion que utilice para mis datos, junto con el año

distrito_03 <- mutate(distrito_03, mes =month(as.POSIXlt(distrito_03$Fecha, format="%y-%m-%d")))

#ahora creo una columna que una el mes y el a?o

distrito_03 <- unite(distrito_03, mes_year,c(4,3),  sep = "", remove = TRUE)


#ahora voy a realizar una agrupacion de los datos por el campo mes_year

distrito_03_group <- distrito_03 %>% 
  group_by(mes_year) %>% 
  mutate(fecha=Fecha) %>% 
  group_by(Fecha)

#agrupado previamente por mes ahora hago la media del unitario por mes_year
distrito_03_mes <- distrito_03_group %>% summarise(y= mean(price_m2))

#renombramos la columna 1 para que pueda utilizarse en prophet

names(distrito_03_mes)[1] = "ds"

#Realizamos el BoxCox para obtener el mejor lambda
lam_03 = BoxCox.lambda(distrito_03_mes$y, method = "loglik")
distrito_03_mes$y = BoxCox(distrito_03_mes$y, lam_03)

ggplot(distrito_03_mes, aes(x=ds, y=y )) + 
  geom_line(colour="grey")  + 
  geom_point( size=2, shape=20, fill="white", colour="blue") + 
  theme_minimal()


prediccion_03 <- prophet(distrito_03_mes, daily.seasonality=TRUE)
future_03 <- make_future_dataframe(prediccion_03, periods = 730)
forecast_03 <- predict(prediccion_03, future_03)
plot(prediccion_03, forecast_03)
prophet_plot_components(prediccion_03, forecast_03)

forecast_inverso_03 <- forecast_03
forecast_inverso_03$yhat_transformado = InvBoxCox(forecast_03$yhat, lam_03)

dyplot.prophet(prediccion_03, forecast_03)


forecast_df_03 <- as.data.frame(forecast_inverso_03)

distrito_03_forecast <- forecast_df_03 %>% 
  select(ds,yhat_transformado) %>% 
  mutate(distrito= "03")


write.table(distrito_03_forecast, file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_prophet_distritos.csv", sep=",", append=TRUE)

############## DISTRITO 04  ################################################

## selecciono distrito 04 y me quedo solo con las columnas Fecha y price

distrito_04 <- distritos_original %>% 
  filter(cod_distrito=="04") %>% 
  select(Fecha, price_m2, year)

#creo una columna para tener el mes, que sera la agrupacion que utilice para mis datos, junto con el año

distrito_04 <- mutate(distrito_04, mes =month(as.POSIXlt(distrito_04$Fecha, format="%y-%m-%d")))

#ahora creo una columna que una el mes y el a?o

distrito_04 <- unite(distrito_04, mes_year,c(4,3),  sep = "", remove = TRUE)


#ahora voy a realizar una agrupacion de los datos por el campo mes_year

distrito_04_group <- distrito_04 %>% 
  group_by(mes_year) %>% 
  mutate(fecha=Fecha) %>% 
  group_by(Fecha)

#agrupado previamente por mes ahora hago la media del unitario por mes_year
distrito_04_mes <- distrito_04_group %>% summarise(y= mean(price_m2))

#renombramos la columna 1 para que pueda utilizarse en prophet

names(distrito_04_mes)[1] = "ds"

#Realizamos el BoxCox para obtener el mejor lambda
lam_04 = BoxCox.lambda(distrito_04_mes$y, method = "loglik")
distrito_04_mes$y = BoxCox(distrito_04_mes$y, lam_04)

ggplot(distrito_04_mes, aes(x=ds, y=y )) + 
  geom_line(colour="grey")  + 
  geom_point( size=2, shape=20, fill="white", colour="blue") + 
  theme_minimal()


prediccion_04 <- prophet(distrito_04_mes, daily.seasonality=TRUE)
future_04 <- make_future_dataframe(prediccion_04, periods = 730)
forecast_04 <- predict(prediccion_04, future_04)
plot(prediccion_04, forecast_04)
prophet_plot_components(prediccion_04, forecast_04)

forecast_inverso_04 <- forecast_04
forecast_inverso_04$yhat_transformado = InvBoxCox(forecast_04$yhat, lam_04)

dyplot.prophet(prediccion_04, forecast_04)


forecast_df_04 <- as.data.frame(forecast_inverso_04)

distrito_04_forecast <- forecast_df_04 %>% 
  select(ds,yhat_transformado) %>% 
  mutate(distrito= "04")


write.table(distrito_04_forecast, file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_prophet_distritos.csv", sep=",", append=TRUE)

############## DISTRITO 05  ################################################

## selecciono distrito 05 y me quedo solo con las columnas Fecha y price

distrito_05 <- distritos_original %>% 
  filter(cod_distrito=="05") %>% 
  select(Fecha, price_m2, year)

#creo una columna para tener el mes, que sera la agrupacion que utilice para mis datos, junto con el año

distrito_05 <- mutate(distrito_05, mes =month(as.POSIXlt(distrito_05$Fecha, format="%y-%m-%d")))

#ahora creo una columna que una el mes y el a?o

distrito_05 <- unite(distrito_05, mes_year,c(4,3),  sep = "", remove = TRUE)


#ahora voy a realizar una agrupacion de los datos por el campo mes_year

distrito_05_group <- distrito_05 %>% 
  group_by(mes_year) %>% 
  mutate(fecha=Fecha) %>% 
  group_by(Fecha)

#agrupado previamente por mes ahora hago la media del unitario por mes_year
distrito_05_mes <- distrito_05_group %>% summarise(y= mean(price_m2))

#renombramos la columna 1 para que pueda utilizarse en prophet

names(distrito_05_mes)[1] = "ds"

#Realizamos el BoxCox para obtener el mejor lambda
lam_05 = BoxCox.lambda(distrito_05_mes$y, method = "loglik")
distrito_05_mes$y = BoxCox(distrito_05_mes$y, lam_05)

ggplot(distrito_05_mes, aes(x=ds, y=y )) + 
  geom_line(colour="grey")  + 
  geom_point( size=2, shape=20, fill="white", colour="blue") + 
  theme_minimal()


prediccion_05 <- prophet(distrito_05_mes, daily.seasonality=TRUE)
future_05 <- make_future_dataframe(prediccion_05, periods = 730)
forecast_05 <- predict(prediccion_05, future_05)
plot(prediccion_05, forecast_05)
prophet_plot_components(prediccion_05, forecast_05)

forecast_inverso_05 <- forecast_05
forecast_inverso_05$yhat_transformado = InvBoxCox(forecast_05$yhat, lam_05)

dyplot.prophet(prediccion_05, forecast_05)


forecast_df_05 <- as.data.frame(forecast_inverso_05)

distrito_05_forecast <- forecast_df_05 %>% 
  select(ds,yhat_transformado) %>% 
  mutate(distrito= "05")


write.table(distrito_05_forecast, file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_prophet_distritos.csv", sep=",", append=TRUE)

############## DISTRITO 06  ################################################

## selecciono distrito 06 y me quedo solo con las columnas Fecha y price

distrito_06 <- distritos_original %>% 
  filter(cod_distrito=="06") %>% 
  select(Fecha, price_m2, year)

#creo una columna para tener el mes, que sera la agrupacion que utilice para mis datos, junto con el año

distrito_06 <- mutate(distrito_06, mes =month(as.POSIXlt(distrito_06$Fecha, format="%y-%m-%d")))

#ahora creo una columna que una el mes y el a?o

distrito_06 <- unite(distrito_06, mes_year,c(4,3),  sep = "", remove = TRUE)


#ahora voy a realizar una agrupacion de los datos por el campo mes_year

distrito_06_group <- distrito_06 %>% 
  group_by(mes_year) %>% 
  mutate(fecha=Fecha) %>% 
  group_by(Fecha)

#agrupado previamente por mes ahora hago la media del unitario por mes_year
distrito_06_mes <- distrito_06_group %>% summarise(y= mean(price_m2))

#renombramos la columna 1 para que pueda utilizarse en prophet

names(distrito_06_mes)[1] = "ds"

#Realizamos el BoxCox para obtener el mejor lambda
lam_06 = BoxCox.lambda(distrito_06_mes$y, method = "loglik")
distrito_06_mes$y = BoxCox(distrito_06_mes$y, lam_06)

ggplot(distrito_06_mes, aes(x=ds, y=y )) + 
  geom_line(colour="grey")  + 
  geom_point( size=2, shape=20, fill="white", colour="blue") + 
  theme_minimal()


prediccion_06 <- prophet(distrito_06_mes, daily.seasonality=TRUE)
future_06 <- make_future_dataframe(prediccion_06, periods = 730)
forecast_06 <- predict(prediccion_06, future_06)
plot(prediccion_06, forecast_06)
prophet_plot_components(prediccion_06, forecast_06)

forecast_inverso_06 <- forecast_06
forecast_inverso_06$yhat_transformado = InvBoxCox(forecast_06$yhat, lam_06)

dyplot.prophet(prediccion_06, forecast_06)


forecast_df_06 <- as.data.frame(forecast_inverso_06)

distrito_06_forecast <- forecast_df_06 %>% 
  select(ds,yhat_transformado) %>% 
  mutate(distrito= "06")


write.table(distrito_06_forecast, file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_prophet_distritos.csv", sep=",", append=TRUE)


############## DISTRITO 07  ################################################

## selecciono distrito 07 y me quedo solo con las columnas Fecha y price

distrito_07 <- distritos_original %>% 
  filter(cod_distrito=="07") %>% 
  select(Fecha, price_m2, year)

#creo una columna para tener el mes, que sera la agrupacion que utilice para mis datos, junto con el año

distrito_07 <- mutate(distrito_07, mes =month(as.POSIXlt(distrito_07$Fecha, format="%y-%m-%d")))

#ahora creo una columna que una el mes y el a?o

distrito_07 <- unite(distrito_07, mes_year,c(4,3),  sep = "", remove = TRUE)


#ahora voy a realizar una agrupacion de los datos por el campo mes_year

distrito_07_group <- distrito_07 %>% 
  group_by(mes_year) %>% 
  mutate(fecha=Fecha) %>% 
  group_by(Fecha)

#agrupado previamente por mes ahora hago la media del unitario por mes_year
distrito_07_mes <- distrito_07_group %>% summarise(y= mean(price_m2))

#renombramos la columna 1 para que pueda utilizarse en prophet

names(distrito_07_mes)[1] = "ds"

#Realizamos el BoxCox para obtener el mejor lambda
lam_07 = BoxCox.lambda(distrito_07_mes$y, method = "loglik")
distrito_07_mes$y = BoxCox(distrito_07_mes$y, lam_07)

ggplot(distrito_07_mes, aes(x=ds, y=y )) + 
  geom_line(colour="grey")  + 
  geom_point( size=2, shape=20, fill="white", colour="blue") + 
  theme_minimal()


prediccion_07 <- prophet(distrito_07_mes, daily.seasonality=TRUE)
future_07 <- make_future_dataframe(prediccion_07, periods = 730)
forecast_07 <- predict(prediccion_07, future_07)
plot(prediccion_07, forecast_07)
prophet_plot_components(prediccion_07, forecast_07)

forecast_inverso_07 <- forecast_07
forecast_inverso_07$yhat_transformado = InvBoxCox(forecast_07$yhat, lam_07)

dyplot.prophet(prediccion_07, forecast_07)


forecast_df_07 <- as.data.frame(forecast_inverso_07)

distrito_07_forecast <- forecast_df_07 %>% 
  select(ds,yhat_transformado) %>% 
  mutate(distrito= "07")


write.table(distrito_07_forecast, file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_prophet_distritos.csv", sep=",", append=TRUE)


############## DISTRITO 08  ################################################

## selecciono distrito 08 y me quedo solo con las columnas Fecha y price

distrito_08 <- distritos_original %>% 
  filter(cod_distrito=="08") %>% 
  select(Fecha, price_m2, year)

#creo una columna para tener el mes, que sera la agrupacion que utilice para mis datos, junto con el año

distrito_08 <- mutate(distrito_08, mes =month(as.POSIXlt(distrito_08$Fecha, format="%y-%m-%d")))

#ahora creo una columna que una el mes y el a?o

distrito_08 <- unite(distrito_08, mes_year,c(4,3),  sep = "", remove = TRUE)


#ahora voy a realizar una agrupacion de los datos por el campo mes_year

distrito_08_group <- distrito_08 %>% 
  group_by(mes_year) %>% 
  mutate(fecha=Fecha) %>% 
  group_by(Fecha)

#agrupado previamente por mes ahora hago la media del unitario por mes_year
distrito_08_mes <- distrito_08_group %>% summarise(y= mean(price_m2))

#renombramos la columna 1 para que pueda utilizarse en prophet

names(distrito_08_mes)[1] = "ds"

#Realizamos el BoxCox para obtener el mejor lambda
lam_08 = BoxCox.lambda(distrito_08_mes$y, method = "loglik")
distrito_08_mes$y = BoxCox(distrito_08_mes$y, lam_08)

ggplot(distrito_08_mes, aes(x=ds, y=y )) + 
  geom_line(colour="grey")  + 
  geom_point( size=2, shape=20, fill="white", colour="blue") + 
  theme_minimal()


prediccion_08 <- prophet(distrito_08_mes, daily.seasonality=TRUE)
future_08 <- make_future_dataframe(prediccion_08, periods = 730)
forecast_08 <- predict(prediccion_08, future_08)
plot(prediccion_08, forecast_08)
prophet_plot_components(prediccion_08, forecast_08)

forecast_inverso_08 <- forecast_08
forecast_inverso_08$yhat_transformado = InvBoxCox(forecast_08$yhat, lam_08)

dyplot.prophet(prediccion_08, forecast_08)


forecast_df_08 <- as.data.frame(forecast_inverso_08)

distrito_08_forecast <- forecast_df_08 %>% 
  select(ds,yhat_transformado) %>% 
  mutate(distrito= "08")


write.table(distrito_08_forecast, file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_prophet_distritos.csv", sep=",", append=TRUE)


############## DISTRITO 09  ################################################

## selecciono distrito 09 y me quedo solo con las columnas Fecha y price

distrito_09 <- distritos_original %>% 
  filter(cod_distrito=="09") %>% 
  select(Fecha, price_m2, year)

#creo una columna para tener el mes, que sera la agrupacion que utilice para mis datos, junto con el año

distrito_09 <- mutate(distrito_09, mes =month(as.POSIXlt(distrito_09$Fecha, format="%y-%m-%d")))

#ahora creo una columna que una el mes y el a?o

distrito_09 <- unite(distrito_09, mes_year,c(4,3),  sep = "", remove = TRUE)


#ahora voy a realizar una agrupacion de los datos por el campo mes_year

distrito_09_group <- distrito_09 %>% 
  group_by(mes_year) %>% 
  mutate(fecha=Fecha) %>% 
  group_by(Fecha)

#agrupado previamente por mes ahora hago la media del unitario por mes_year
distrito_09_mes <- distrito_09_group %>% summarise(y= mean(price_m2))

#renombramos la columna 1 para que pueda utilizarse en prophet

names(distrito_09_mes)[1] = "ds"

#Realizamos el BoxCox para obtener el mejor lambda
lam_09 = BoxCox.lambda(distrito_09_mes$y, method = "loglik")
distrito_09_mes$y = BoxCox(distrito_09_mes$y, lam_09)

ggplot(distrito_09_mes, aes(x=ds, y=y )) + 
  geom_line(colour="grey")  + 
  geom_point( size=2, shape=20, fill="white", colour="blue") + 
  theme_minimal()


prediccion_09 <- prophet(distrito_09_mes, daily.seasonality=TRUE)
future_09 <- make_future_dataframe(prediccion_09, periods = 730)
forecast_09 <- predict(prediccion_09, future_09)
plot(prediccion_09, forecast_09)
prophet_plot_components(prediccion_09, forecast_09)

forecast_inverso_09 <- forecast_09
forecast_inverso_09$yhat_transformado = InvBoxCox(forecast_09$yhat, lam_09)

dyplot.prophet(prediccion_09, forecast_09)


forecast_df_09 <- as.data.frame(forecast_inverso_09)

distrito_09_forecast <- forecast_df_09 %>% 
  select(ds,yhat_transformado) %>% 
  mutate(distrito= "09")


write.table(distrito_09_forecast, file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_prophet_distritos.csv", sep=",", append=TRUE)

############## DISTRITO 10  ################################################

## selecciono distrito 10 y me quedo solo con las columnas Fecha y price

distrito_10 <- distritos_original %>% 
  filter(cod_distrito=="10") %>% 
  select(Fecha, price_m2, year)

#creo una columna para tener el mes, que sera la agrupacion que utilice para mis datos, junto con el año

distrito_10 <- mutate(distrito_10, mes =month(as.POSIXlt(distrito_10$Fecha, format="%y-%m-%d")))

#ahora creo una columna que una el mes y el a?o

distrito_10 <- unite(distrito_10, mes_year,c(4,3),  sep = "", remove = TRUE)


#ahora voy a realizar una agrupacion de los datos por el campo mes_year

distrito_10_group <- distrito_10 %>% 
  group_by(mes_year) %>% 
  mutate(fecha=Fecha) %>% 
  group_by(Fecha)

#agrupado previamente por mes ahora hago la media del unitario por mes_year
distrito_10_mes <- distrito_10_group %>% summarise(y= mean(price_m2))

#renombramos la columna 1 para que pueda utilizarse en prophet

names(distrito_10_mes)[1] = "ds"

#Realizamos el BoxCox para obtener el mejor lambda
lam_10 = BoxCox.lambda(distrito_10_mes$y, method = "loglik")
distrito_10_mes$y = BoxCox(distrito_10_mes$y, lam_10)

ggplot(distrito_10_mes, aes(x=ds, y=y )) + 
  geom_line(colour="grey")  + 
  geom_point( size=2, shape=20, fill="white", colour="blue") + 
  theme_minimal()


prediccion_10 <- prophet(distrito_10_mes, daily.seasonality=TRUE)
future_10 <- make_future_dataframe(prediccion_10, periods = 730)
forecast_10 <- predict(prediccion_10, future_10)
plot(prediccion_10, forecast_10)
prophet_plot_components(prediccion_10, forecast_10)

forecast_inverso_10 <- forecast_10
forecast_inverso_10$yhat_transformado = InvBoxCox(forecast_10$yhat, lam_10)

dyplot.prophet(prediccion_10, forecast_10)


forecast_df_10 <- as.data.frame(forecast_inverso_10)

distrito_10_forecast <- forecast_df_10 %>% 
  select(ds,yhat_transformado) %>% 
  mutate(distrito= "10")


write.table(distrito_10_forecast, file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_prophet_distritos.csv", sep=",", append=TRUE)

############## DISTRITO 11  ################################################

## selecciono distrito 11 y me quedo solo con las columnas Fecha y price

distrito_11 <- distritos_original %>% 
  filter(cod_distrito=="11") %>% 
  select(Fecha, price_m2, year)

#creo una columna para tener el mes, que sera la agrupacion que utilice para mis datos, junto con el año

distrito_11 <- mutate(distrito_11, mes =month(as.POSIXlt(distrito_11$Fecha, format="%y-%m-%d")))

#ahora creo una columna que una el mes y el a?o

distrito_11 <- unite(distrito_11, mes_year,c(4,3),  sep = "", remove = TRUE)


#ahora voy a realizar una agrupacion de los datos por el campo mes_year

distrito_11_group <- distrito_11 %>% 
  group_by(mes_year) %>% 
  mutate(fecha=Fecha) %>% 
  group_by(Fecha)

#agrupado previamente por mes ahora hago la media del unitario por mes_year
distrito_11_mes <- distrito_11_group %>% summarise(y= mean(price_m2))

#renombramos la columna 1 para que pueda utilizarse en prophet

names(distrito_11_mes)[1] = "ds"

#Realizamos el BoxCox para obtener el mejor lambda
lam_11 = BoxCox.lambda(distrito_11_mes$y, method = "loglik")
distrito_11_mes$y = BoxCox(distrito_11_mes$y, lam_11)

ggplot(distrito_11_mes, aes(x=ds, y=y )) + 
  geom_line(colour="grey")  + 
  geom_point( size=2, shape=20, fill="white", colour="blue") + 
  theme_minimal()


prediccion_11 <- prophet(distrito_11_mes, daily.seasonality=TRUE)
future_11 <- make_future_dataframe(prediccion_11, periods = 730)
forecast_11 <- predict(prediccion_11, future_11)
plot(prediccion_11, forecast_11)
prophet_plot_components(prediccion_11, forecast_11)

forecast_inverso_11 <- forecast_11
forecast_inverso_11$yhat_transformado = InvBoxCox(forecast_11$yhat, lam_11)

dyplot.prophet(prediccion_11, forecast_11)


forecast_df_11 <- as.data.frame(forecast_inverso_11)

distrito_11_forecast <- forecast_df_11 %>% 
  select(ds,yhat_transformado) %>% 
  mutate(distrito= "11")


write.table(distrito_11_forecast, file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_prophet_distritos.csv", sep=",", append=TRUE)

############## DISTRITO 12  ################################################

## selecciono distrito 12 y me quedo solo con las columnas Fecha y price

distrito_12 <- distritos_original %>% 
  filter(cod_distrito=="12") %>% 
  select(Fecha, price_m2, year)

#creo una columna para tener el mes, que sera la agrupacion que utilice para mis datos, junto con el año

distrito_12 <- mutate(distrito_12, mes =month(as.POSIXlt(distrito_12$Fecha, format="%y-%m-%d")))

#ahora creo una columna que una el mes y el a?o

distrito_12 <- unite(distrito_12, mes_year,c(4,3),  sep = "", remove = TRUE)


#ahora voy a realizar una agrupacion de los datos por el campo mes_year

distrito_12_group <- distrito_12 %>% 
  group_by(mes_year) %>% 
  mutate(fecha=Fecha) %>% 
  group_by(Fecha)

#agrupado previamente por mes ahora hago la media del unitario por mes_year
distrito_12_mes <- distrito_12_group %>% summarise(y= mean(price_m2))

#renombramos la columna 1 para que pueda utilizarse en prophet

names(distrito_12_mes)[1] = "ds"

#Realizamos el BoxCox para obtener el mejor lambda
lam_12 = BoxCox.lambda(distrito_12_mes$y, method = "loglik")
distrito_12_mes$y = BoxCox(distrito_12_mes$y, lam_12)

ggplot(distrito_12_mes, aes(x=ds, y=y )) + 
  geom_line(colour="grey")  + 
  geom_point( size=2, shape=20, fill="white", colour="blue") + 
  theme_minimal()


prediccion_12 <- prophet(distrito_12_mes, daily.seasonality=TRUE)
future_12 <- make_future_dataframe(prediccion_12, periods = 730)
forecast_12 <- predict(prediccion_12, future_12)
plot(prediccion_12, forecast_12)
prophet_plot_components(prediccion_12, forecast_12)

forecast_inverso_12 <- forecast_12
forecast_inverso_12$yhat_transformado = InvBoxCox(forecast_12$yhat, lam_12)

dyplot.prophet(prediccion_12, forecast_12)


forecast_df_12 <- as.data.frame(forecast_inverso_12)

distrito_12_forecast <- forecast_df_12 %>% 
  select(ds,yhat_transformado) %>% 
  mutate(distrito= "12")


write.table(distrito_12_forecast, file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_prophet_distritos.csv", sep=",", append=TRUE)


############## DISTRITO 13  ################################################

## selecciono distrito 13 y me quedo solo con las columnas Fecha y price

distrito_13 <- distritos_original %>% 
  filter(cod_distrito=="13") %>% 
  select(Fecha, price_m2, year)

#creo una columna para tener el mes, que sera la agrupacion que utilice para mis datos, junto con el año

distrito_13 <- mutate(distrito_13, mes =month(as.POSIXlt(distrito_13$Fecha, format="%y-%m-%d")))

#ahora creo una columna que una el mes y el a?o

distrito_13 <- unite(distrito_13, mes_year,c(4,3),  sep = "", remove = TRUE)


#ahora voy a realizar una agrupacion de los datos por el campo mes_year

distrito_13_group <- distrito_13 %>% 
  group_by(mes_year) %>% 
  mutate(fecha=Fecha) %>% 
  group_by(Fecha)

#agrupado previamente por mes ahora hago la media del unitario por mes_year
distrito_13_mes <- distrito_13_group %>% summarise(y= mean(price_m2))

#renombramos la columna 1 para que pueda utilizarse en prophet

names(distrito_13_mes)[1] = "ds"

#Realizamos el BoxCox para obtener el mejor lambda
lam_13 = BoxCox.lambda(distrito_13_mes$y, method = "loglik")
distrito_13_mes$y = BoxCox(distrito_13_mes$y, lam_13)

ggplot(distrito_13_mes, aes(x=ds, y=y )) + 
  geom_line(colour="grey")  + 
  geom_point( size=2, shape=20, fill="white", colour="blue") + 
  theme_minimal()


prediccion_13 <- prophet(distrito_13_mes, daily.seasonality=TRUE)
future_13 <- make_future_dataframe(prediccion_13, periods = 730)
forecast_13 <- predict(prediccion_13, future_13)
plot(prediccion_13, forecast_13)
prophet_plot_components(prediccion_13, forecast_13)

forecast_inverso_13 <- forecast_13
forecast_inverso_13$yhat_transformado = InvBoxCox(forecast_13$yhat, lam_13)

dyplot.prophet(prediccion_13, forecast_13)


forecast_df_13 <- as.data.frame(forecast_inverso_13)

distrito_13_forecast <- forecast_df_13 %>% 
  select(ds,yhat_transformado) %>% 
  mutate(distrito= "13")


write.table(distrito_13_forecast, file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_prophet_distritos.csv", sep=",", append=TRUE)

############## DISTRITO 14  ################################################

## selecciono distrito 14 y me quedo solo con las columnas Fecha y price

distrito_14 <- distritos_original %>% 
  filter(cod_distrito=="14") %>% 
  select(Fecha, price_m2, year)

#creo una columna para tener el mes, que sera la agrupacion que utilice para mis datos, junto con el año

distrito_14 <- mutate(distrito_14, mes =month(as.POSIXlt(distrito_14$Fecha, format="%y-%m-%d")))

#ahora creo una columna que una el mes y el a?o

distrito_14 <- unite(distrito_14, mes_year,c(4,3),  sep = "", remove = TRUE)


#ahora voy a realizar una agrupacion de los datos por el campo mes_year

distrito_14_group <- distrito_14 %>% 
  group_by(mes_year) %>% 
  mutate(fecha=Fecha) %>% 
  group_by(Fecha)

#agrupado previamente por mes ahora hago la media del unitario por mes_year
distrito_14_mes <- distrito_14_group %>% summarise(y= mean(price_m2))

#renombramos la columna 1 para que pueda utilizarse en prophet

names(distrito_14_mes)[1] = "ds"

#Realizamos el BoxCox para obtener el mejor lambda
lam_14 = BoxCox.lambda(distrito_14_mes$y, method = "loglik")
distrito_14_mes$y = BoxCox(distrito_14_mes$y, lam_14)

ggplot(distrito_14_mes, aes(x=ds, y=y )) + 
  geom_line(colour="grey")  + 
  geom_point( size=2, shape=20, fill="white", colour="blue") + 
  theme_minimal()


prediccion_14 <- prophet(distrito_14_mes, daily.seasonality=TRUE)
future_14 <- make_future_dataframe(prediccion_14, periods = 730)
forecast_14 <- predict(prediccion_14, future_14)
plot(prediccion_14, forecast_14)
prophet_plot_components(prediccion_14, forecast_14)

forecast_inverso_14 <- forecast_14
forecast_inverso_14$yhat_transformado = InvBoxCox(forecast_14$yhat, lam_14)

dyplot.prophet(prediccion_14, forecast_14)


forecast_df_14 <- as.data.frame(forecast_inverso_14)

distrito_14_forecast <- forecast_df_14 %>% 
  select(ds,yhat_transformado) %>% 
  mutate(distrito= "14")


write.table(distrito_14_forecast, file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_prophet_distritos.csv", sep=",", append=TRUE)

############## DISTRITO 15  ################################################

## selecciono distrito 15 y me quedo solo con las columnas Fecha y price

distrito_15 <- distritos_original %>% 
  filter(cod_distrito=="15") %>% 
  select(Fecha, price_m2, year)

#creo una columna para tener el mes, que sera la agrupacion que utilice para mis datos, junto con el año

distrito_15 <- mutate(distrito_15, mes =month(as.POSIXlt(distrito_15$Fecha, format="%y-%m-%d")))

#ahora creo una columna que una el mes y el a?o

distrito_15 <- unite(distrito_15, mes_year,c(4,3),  sep = "", remove = TRUE)


#ahora voy a realizar una agrupacion de los datos por el campo mes_year

distrito_15_group <- distrito_15 %>% 
  group_by(mes_year) %>% 
  mutate(fecha=Fecha) %>% 
  group_by(Fecha)

#agrupado previamente por mes ahora hago la media del unitario por mes_year
distrito_15_mes <- distrito_15_group %>% summarise(y= mean(price_m2))

#renombramos la columna 1 para que pueda utilizarse en prophet

names(distrito_15_mes)[1] = "ds"

#Realizamos el BoxCox para obtener el mejor lambda
lam_15 = BoxCox.lambda(distrito_15_mes$y, method = "loglik")
distrito_15_mes$y = BoxCox(distrito_15_mes$y, lam_15)

ggplot(distrito_15_mes, aes(x=ds, y=y )) + 
  geom_line(colour="grey")  + 
  geom_point( size=2, shape=20, fill="white", colour="blue") + 
  theme_minimal()


prediccion_15 <- prophet(distrito_15_mes, daily.seasonality=TRUE)
future_15 <- make_future_dataframe(prediccion_15, periods = 730)
forecast_15 <- predict(prediccion_15, future_15)
plot(prediccion_15, forecast_15)
prophet_plot_components(prediccion_15, forecast_15)

forecast_inverso_15 <- forecast_15
forecast_inverso_15$yhat_transformado = InvBoxCox(forecast_15$yhat, lam_15)

dyplot.prophet(prediccion_15, forecast_15)


forecast_df_15 <- as.data.frame(forecast_inverso_15)

distrito_15_forecast <- forecast_df_15 %>% 
  select(ds,yhat_transformado) %>% 
  mutate(distrito= "15")


write.table(distrito_15_forecast, file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_prophet_distritos.csv", sep=",", append=TRUE)

############## DISTRITO 16  ################################################

## selecciono distrito 16 y me quedo solo con las columnas Fecha y price

distrito_16 <- distritos_original %>% 
  filter(cod_distrito=="16") %>% 
  select(Fecha, price_m2, year)

#creo una columna para tener el mes, que sera la agrupacion que utilice para mis datos, junto con el año

distrito_16 <- mutate(distrito_16, mes =month(as.POSIXlt(distrito_16$Fecha, format="%y-%m-%d")))

#ahora creo una columna que una el mes y el a?o

distrito_16 <- unite(distrito_16, mes_year,c(4,3),  sep = "", remove = TRUE)


#ahora voy a realizar una agrupacion de los datos por el campo mes_year

distrito_16_group <- distrito_16 %>% 
  group_by(mes_year) %>% 
  mutate(fecha=Fecha) %>% 
  group_by(Fecha)

#agrupado previamente por mes ahora hago la media del unitario por mes_year
distrito_16_mes <- distrito_16_group %>% summarise(y= mean(price_m2))

#renombramos la columna 1 para que pueda utilizarse en prophet

names(distrito_16_mes)[1] = "ds"

#Realizamos el BoxCox para obtener el mejor lambda
lam_16 = BoxCox.lambda(distrito_16_mes$y, method = "loglik")
distrito_16_mes$y = BoxCox(distrito_16_mes$y, lam_16)

ggplot(distrito_16_mes, aes(x=ds, y=y )) + 
  geom_line(colour="grey")  + 
  geom_point( size=2, shape=20, fill="white", colour="blue") + 
  theme_minimal()


prediccion_16 <- prophet(distrito_16_mes, daily.seasonality=TRUE)
future_16 <- make_future_dataframe(prediccion_16, periods = 730)
forecast_16 <- predict(prediccion_16, future_16)
plot(prediccion_16, forecast_16)
prophet_plot_components(prediccion_16, forecast_16)

forecast_inverso_16 <- forecast_16
forecast_inverso_16$yhat_transformado = InvBoxCox(forecast_16$yhat, lam_16)

dyplot.prophet(prediccion_16, forecast_16)


forecast_df_16 <- as.data.frame(forecast_inverso_16)

distrito_16_forecast <- forecast_df_16 %>% 
  select(ds,yhat_transformado) %>% 
  mutate(distrito= "16")


write.table(distrito_16_forecast, file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_prophet_distritos.csv", sep=",", append=TRUE)

############## DISTRITO 17  ################################################

## selecciono distrito 17 y me quedo solo con las columnas Fecha y price

distrito_17 <- distritos_original %>% 
  filter(cod_distrito=="17") %>% 
  select(Fecha, price_m2, year)

#creo una columna para tener el mes, que sera la agrupacion que utilice para mis datos, junto con el año

distrito_17 <- mutate(distrito_17, mes =month(as.POSIXlt(distrito_17$Fecha, format="%y-%m-%d")))

#ahora creo una columna que una el mes y el a?o

distrito_17 <- unite(distrito_17, mes_year,c(4,3),  sep = "", remove = TRUE)


#ahora voy a realizar una agrupacion de los datos por el campo mes_year

distrito_17_group <- distrito_17 %>% 
  group_by(mes_year) %>% 
  mutate(fecha=Fecha) %>% 
  group_by(Fecha)

#agrupado previamente por mes ahora hago la media del unitario por mes_year
distrito_17_mes <- distrito_17_group %>% summarise(y= mean(price_m2))

#renombramos la columna 1 para que pueda utilizarse en prophet

names(distrito_17_mes)[1] = "ds"

#Realizamos el BoxCox para obtener el mejor lambda
lam_17 = BoxCox.lambda(distrito_17_mes$y, method = "loglik")
distrito_17_mes$y = BoxCox(distrito_17_mes$y, lam_17)

ggplot(distrito_17_mes, aes(x=ds, y=y )) + 
  geom_line(colour="grey")  + 
  geom_point( size=2, shape=20, fill="white", colour="blue") + 
  theme_minimal()


prediccion_17 <- prophet(distrito_17_mes, daily.seasonality=TRUE)
future_17 <- make_future_dataframe(prediccion_17, periods = 730)
forecast_17 <- predict(prediccion_17, future_17)
plot(prediccion_17, forecast_17)
prophet_plot_components(prediccion_17, forecast_17)

forecast_inverso_17 <- forecast_17
forecast_inverso_17$yhat_transformado = InvBoxCox(forecast_17$yhat, lam_17)

dyplot.prophet(prediccion_17, forecast_17)


forecast_df_17 <- as.data.frame(forecast_inverso_17)

distrito_17_forecast <- forecast_df_17 %>% 
  select(ds,yhat_transformado) %>% 
  mutate(distrito= "17")


write.table(distrito_17_forecast, file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_prophet_distritos.csv", sep=",", append=TRUE)


############## DISTRITO 18  ################################################

## selecciono distrito 18 y me quedo solo con las columnas Fecha y price

distrito_18 <- distritos_original %>% 
  filter(cod_distrito=="18") %>% 
  select(Fecha, price_m2, year)

#creo una columna para tener el mes, que sera la agrupacion que utilice para mis datos, junto con el año

distrito_18 <- mutate(distrito_18, mes =month(as.POSIXlt(distrito_18$Fecha, format="%y-%m-%d")))

#ahora creo una columna que una el mes y el a?o

distrito_18 <- unite(distrito_18, mes_year,c(4,3),  sep = "", remove = TRUE)


#ahora voy a realizar una agrupacion de los datos por el campo mes_year

distrito_18_group <- distrito_18 %>% 
  group_by(mes_year) %>% 
  mutate(fecha=Fecha) %>% 
  group_by(Fecha)

#agrupado previamente por mes ahora hago la media del unitario por mes_year
distrito_18_mes <- distrito_18_group %>% summarise(y= mean(price_m2))

#renombramos la columna 1 para que pueda utilizarse en prophet

names(distrito_18_mes)[1] = "ds"

#Realizamos el BoxCox para obtener el mejor lambda
lam_18 = BoxCox.lambda(distrito_18_mes$y, method = "loglik")
distrito_18_mes$y = BoxCox(distrito_18_mes$y, lam_18)

ggplot(distrito_18_mes, aes(x=ds, y=y )) + 
  geom_line(colour="grey")  + 
  geom_point( size=2, shape=20, fill="white", colour="blue") + 
  theme_minimal()


prediccion_18 <- prophet(distrito_18_mes, daily.seasonality=TRUE)
future_18 <- make_future_dataframe(prediccion_18, periods = 730)
forecast_18 <- predict(prediccion_18, future_18)
plot(prediccion_18, forecast_18)
prophet_plot_components(prediccion_18, forecast_18)

forecast_inverso_18 <- forecast_18
forecast_inverso_18$yhat_transformado = InvBoxCox(forecast_18$yhat, lam_18)

dyplot.prophet(prediccion_18, forecast_18)


forecast_df_18 <- as.data.frame(forecast_inverso_18)

distrito_18_forecast <- forecast_df_18 %>% 
  select(ds,yhat_transformado) %>% 
  mutate(distrito= "18")


write.table(distrito_18_forecast, file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_prophet_distritos.csv", sep=",", append=TRUE)

############## DISTRITO 19  ################################################

## selecciono distrito 19 y me quedo solo con las columnas Fecha y price

distrito_19 <- distritos_original %>% 
  filter(cod_distrito=="19") %>% 
  select(Fecha, price_m2, year)

#creo una columna para tener el mes, que sera la agrupacion que utilice para mis datos, junto con el año

distrito_19 <- mutate(distrito_19, mes =month(as.POSIXlt(distrito_19$Fecha, format="%y-%m-%d")))

#ahora creo una columna que una el mes y el a?o

distrito_19 <- unite(distrito_19, mes_year,c(4,3),  sep = "", remove = TRUE)


#ahora voy a realizar una agrupacion de los datos por el campo mes_year

distrito_19_group <- distrito_19 %>% 
  group_by(mes_year) %>% 
  mutate(fecha=Fecha) %>% 
  group_by(Fecha)

#agrupado previamente por mes ahora hago la media del unitario por mes_year
distrito_19_mes <- distrito_19_group %>% summarise(y= mean(price_m2))

#renombramos la columna 1 para que pueda utilizarse en prophet

names(distrito_19_mes)[1] = "ds"

#Realizamos el BoxCox para obtener el mejor lambda
lam_19 = BoxCox.lambda(distrito_19_mes$y, method = "loglik")
distrito_19_mes$y = BoxCox(distrito_19_mes$y, lam_19)

ggplot(distrito_19_mes, aes(x=ds, y=y )) + 
  geom_line(colour="grey")  + 
  geom_point( size=2, shape=20, fill="white", colour="blue") + 
  theme_minimal()


prediccion_19 <- prophet(distrito_19_mes, daily.seasonality=TRUE)
future_19 <- make_future_dataframe(prediccion_19, periods = 730)
forecast_19 <- predict(prediccion_19, future_19)
plot(prediccion_19, forecast_19)
prophet_plot_components(prediccion_19, forecast_19)

forecast_inverso_19 <- forecast_19
forecast_inverso_19$yhat_transformado = InvBoxCox(forecast_19$yhat, lam_19)

dyplot.prophet(prediccion_19, forecast_19)


forecast_df_19 <- as.data.frame(forecast_inverso_19)

distrito_19_forecast <- forecast_df_19 %>% 
  select(ds,yhat_transformado) %>% 
  mutate(distrito= "19")


write.table(distrito_19_forecast, file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_prophet_distritos.csv", sep=",", append=TRUE)

############## DISTRITO 20  ################################################

## selecciono distrito 20 y me quedo solo con las columnas Fecha y price

distrito_20 <- distritos_original %>% 
  filter(cod_distrito=="20") %>% 
  select(Fecha, price_m2, year)

#creo una columna para tener el mes, que sera la agrupacion que utilice para mis datos, junto con el año

distrito_20 <- mutate(distrito_20, mes =month(as.POSIXlt(distrito_20$Fecha, format="%y-%m-%d")))

#ahora creo una columna que una el mes y el a?o

distrito_20 <- unite(distrito_20, mes_year,c(4,3),  sep = "", remove = TRUE)


#ahora voy a realizar una agrupacion de los datos por el campo mes_year

distrito_20_group <- distrito_20 %>% 
  group_by(mes_year) %>% 
  mutate(fecha=Fecha) %>% 
  group_by(Fecha)

#agrupado previamente por mes ahora hago la media del unitario por mes_year
distrito_20_mes <- distrito_20_group %>% summarise(y= mean(price_m2))

#renombramos la columna 1 para que pueda utilizarse en prophet

names(distrito_20_mes)[1] = "ds"

#Realizamos el BoxCox para obtener el mejor lambda
lam_20 = BoxCox.lambda(distrito_20_mes$y, method = "loglik")
distrito_20_mes$y = BoxCox(distrito_20_mes$y, lam_20)

ggplot(distrito_20_mes, aes(x=ds, y=y )) + 
  geom_line(colour="grey")  + 
  geom_point( size=2, shape=20, fill="white", colour="blue") + 
  theme_minimal()


prediccion_20 <- prophet(distrito_20_mes, daily.seasonality=TRUE)
future_20 <- make_future_dataframe(prediccion_20, periods = 730)
forecast_20 <- predict(prediccion_20, future_20)
plot(prediccion_20, forecast_20)
prophet_plot_components(prediccion_20, forecast_20)

forecast_inverso_20 <- forecast_20
forecast_inverso_20$yhat_transformado = InvBoxCox(forecast_20$yhat, lam_20)

dyplot.prophet(prediccion_20, forecast_20)


forecast_df_20 <- as.data.frame(forecast_inverso_20)

distrito_20_forecast <- forecast_df_20 %>% 
  select(ds,yhat_transformado) %>% 
  mutate(distrito= "20")


write.table(distrito_20_forecast, file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_prophet_distritos.csv", sep=",", append=TRUE)

############## DISTRITO 21  ################################################

## selecciono distrito 21 y me quedo solo con las columnas Fecha y price

distrito_21 <- distritos_original %>% 
  filter(cod_distrito=="21") %>% 
  select(Fecha, price_m2, year)

#creo una columna para tener el mes, que sera la agrupacion que utilice para mis datos, junto con el año

distrito_21 <- mutate(distrito_21, mes =month(as.POSIXlt(distrito_21$Fecha, format="%y-%m-%d")))

#ahora creo una columna que una el mes y el a?o

distrito_21 <- unite(distrito_21, mes_year,c(4,3),  sep = "", remove = TRUE)


#ahora voy a realizar una agrupacion de los datos por el campo mes_year

distrito_21_group <- distrito_21 %>% 
  group_by(mes_year) %>% 
  mutate(fecha=Fecha) %>% 
  group_by(Fecha)

#agrupado previamente por mes ahora hago la media del unitario por mes_year
distrito_21_mes <- distrito_21_group %>% summarise(y= mean(price_m2))

#renombramos la columna 1 para que pueda utilizarse en prophet

names(distrito_21_mes)[1] = "ds"

#Realizamos el BoxCox para obtener el mejor lambda
lam_21 = BoxCox.lambda(distrito_21_mes$y, method = "loglik")
distrito_21_mes$y = BoxCox(distrito_21_mes$y, lam_21)

ggplot(distrito_21_mes, aes(x=ds, y=y )) + 
  geom_line(colour="grey")  + 
  geom_point( size=2, shape=20, fill="white", colour="blue") + 
  theme_minimal()


prediccion_21 <- prophet(distrito_21_mes, daily.seasonality=TRUE)
future_21 <- make_future_dataframe(prediccion_21, periods = 730)
forecast_21 <- predict(prediccion_21, future_21)
plot(prediccion_21, forecast_21)
prophet_plot_components(prediccion_21, forecast_21)

forecast_inverso_21 <- forecast_21
forecast_inverso_21$yhat_transformado = InvBoxCox(forecast_21$yhat, lam_21)

dyplot.prophet(prediccion_21, forecast_21)


forecast_df_21 <- as.data.frame(forecast_inverso_21)

distrito_21_forecast <- forecast_df_21 %>% 
  select(ds,yhat_transformado) %>% 
  mutate(distrito= "21")


write.table(distrito_21_forecast, file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_prophet_distritos.csv", sep=",", append=TRUE)

