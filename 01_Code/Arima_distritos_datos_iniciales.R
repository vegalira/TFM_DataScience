install.packages("tidyverse")
install.packages("readxl")
install.packages("tseries")
install.packages("forecast")
install.packages("fUnitRoots")
library(fUnitRoots)
library(tidyverse)
library(readxl)
library(tseries)
library(forecast)

distritos_original <- read_csv("C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\historico_madrid_limpio.csv")
is.data.frame(distritos_original)

#Agrupo por cod_distrito y year

distritos_group <- distritos_original %>% group_by(cod_distrito, year) %>% summarise(mean(price_m2))

## ARIMA DISTRITO 01_CENTRO

#seleccionamos el distrito 01 para hacer el arima sobre él

distrito_01 <- distritos_group %>% 
  filter(cod_distrito == "01")

#ahora nos quedamos solo con el price_m2 para hacer el arima

colnames(distrito_01) <- c("Distrito", "Year", "Price_m2")  


distrito_01_price <- distrito_01 %>% 
  ungroup() %>% 
  select(Price_m2)

#convertimos la serie en timeseries para poder estimar sobre ella

distrito_01_ts <- ts(distrito_01_price, start = c(2005,1), frequency = 1)

plot(distrito_01_ts)

#intamos ver los componentes de la serie temporal(Observed-trend-seasonal-random)
componentes.ts = decompose(distrito_01_ts)

#da un error porque tengo un número insuficiente de años para cubrir ciclos estacionales completos

#hacemos un tsdisplay para analizar la serie temporal con ACF Y PACF

tsdisplay(distrito_01_ts, lag.max=10)

#la visualización no es suficiente para ver si son estacionarias o no. En ACF decrece para volver a subir

#hacemos una visualización de los residuos

urkpssTest(distrito_01_ts, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)


#ejecutamos el kpss test
data_kpss <- kpss.test(distrito_01_ts)

print(data_kpss)

#buscamos el orden de diferenciación para hacer estacionaria la serie temporal.Utilizamos ndiffs


orden_dif <- ndiffs(distrito_01_ts)

#el orden de diferenciación en este caso es cero
#diff_distrito_01 <- diff(distrito_01, orden_dif)
#plot(diff_distrito_01, ylab =" Orden de diferenciación")


#Prueba Dickey-Fuller (ADF)
adf.test(distrito_01_ts, alternative = "stationary")

#Tras realizar la prueba aumentada de Dickey-Fuller (ADF), obtenemos un p-valor = 0.9519. 
#Como el valor es mayor que 0.05 decimos que la serie temporal no es estacionaria y no rechazamos la hipótesis nula

tsdisplay(distrito_01_ts, main="distrito_01", lag=10)

#todo parece indicar que se trata de ruido blanco


# Ahora vamos a realizar el autorima 

arima_fit_01 <- auto.arima(distrito_01_ts, seasonal = FALSE)
print(arima_fit_01)


#dibujamos la serie temporal con su ARIMA
plot(distrito_01_ts, main="ARIMA(0,0,0)")
lines(fitted(arima_fit_01), col="blue")

#visualizamos los residuos para ARIMA (0,0,0)
checkresiduals(arima_fit_01)

#un modelo Arima (0,0,0) con media cero es lo que se denomina ruido blanco. Significa que los errores no están correlacionados en el tiempo

#según consulta a stackoverflow habría que quitar la media
fit1 <- auto.arima(distrito_01_ts, trace=TRUE, test="kpss",  ic="bic",allowmean = F)


#ahora el best model es ARIMA (1,0,0) with zero mean
fitARIMA <- arima(distrito_01_ts, order=c(1,0,0),seasonal = list(order = c(1,0,0), period = 12), method="ML")



#dibujamos la serie temporal con su ARIMA
plot(distrito_01_ts, main="ARIMA(1,0,0)")
lines(fitted(fitARIMA), col="blue")

#vemos que ajusta mucho mejor

#visualizamos los residuos para ARIMA (1,0,0)
checkresiduals(fitARIMA)
#install.packages("lmtest")
library(lmtest)
coeftest(fitARIMA)

confint(fitARIMA)

predict(fitARIMA,n.ahead = 5)

#install.packages("FitAR")
library(FitAR)
boxresult=LjungBoxTest (fitARIMA$residuals,k=2,StartLag=1)
plot(boxresult[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(fitARIMA$residuals)
qqline(fitARIMA$residuals)
#El Acf de los residuos muestra que no hay autocorrelaciones significativas
#Los valores del p-value en el test de Ljung-Box Q están  casi todos por encima de 0.05 por lo que los datos no son dependientes


futurVal <- forecast(fitARIMA,h=5, level=c(99.5))
plot(futurVal)



write.table("distrito_01", file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_distritos.xls", append=TRUE)

write.table(futurVal, file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_distritos.xls", append=TRUE)






################################################################################

## ARIMA DISTRITO 02_ARGANZUELA

#seleccionamos el distrito 02 para hacer el arima sobre él

distrito_02 <- distritos_group %>% 
  filter(cod_distrito == "02")

#ahora nos quedamos solo con el price_m2 para hacer el arima

colnames(distrito_02) <- c("Distrito", "Year", "Price_m2")  


distrito_02_price <- distrito_02 %>% 
  ungroup() %>% 
  select(Price_m2)

#convertimos la serie en timeseries para poder estimar sobre ella

distrito_02_ts <- ts(distrito_02_price, start = c(2003,1), frequency = 1)

plot(distrito_02_ts)

#intamos ver los componentes de la serie temporal(Observed-trend-seasonal-random)
componentes.ts = decompose(distrito_02_ts)

#da un error porque tengo un número insuficiente de años para cubrir ciclos estacionales completos

#hacemos un tsdisplay para analizar la serie temporal con ACF Y PACF

tsdisplay(distrito_02_ts, lag.max=10)

#la visualización no es suficiente para ver si son estacionarias o no. En ACF decrece y parece indicar que la serie no es estacionaria
#hacemos una visualización de los residuos

urkpssTest(distrito_02_ts, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)


#ejecutamos el kpss test
data_kpss <- kpss.test(distrito_02_ts)

print(data_kpss)

#buscamos el orden de diferenciación para hacer estacionaria la serie temporal.Utilizamos ndiffs


orden_dif_02 <- ndiffs(distrito_02_ts)

#el orden de diferenciación en este caso es de 1
diff_distrito_02 <- diff(distrito_02_ts, orden_dif_02)
plot(diff_distrito_02, ylab =" Orden de diferenciación")


tsdisplay(diff_distrito_02, main="Primer orden diferenciación")

#con ACF vemos que ninguna autocorrelación está fuera del límite del 95%

#Prueba Dic5key-Fuller (ADF)
adf.test(distrito_02_ts, alternative = "stationary")

#Tras realizar la prueba aumentada de Dickey-Fuller (ADF), obtenemos un p-valor = 0.99. 
#Como el valor es mayor que 0.05 decimos que la serie temporal no es estacionaria y no rechazamos la hipótesis nula


# Ahora vamos a realizar el autorima 

# Para hacer un modelo Arima la serie debe ser estacionaria y para ello debemos diferenciarla

d1 = diff(distrito_02_ts, differences = 1)
plot(d1)

#para comprobar que la serie es realmente estacionaria tenemos que hacer otra vez el test adf

adf.test(d1, alternative = "stationary")

# El p-value ahora es de 0.01 e inferior a 0.05
#Ahora rechazamos la hipótesis nula y podemos decir que la serie temporal es estacionaria.

Acf(d1, main='ACF para serie diferenciada')
Pacf(d1, main='PACF para serie diferenciada')


arima_fit_02 <- auto.arima(distrito_02_ts, seasonal = FALSE)
print(arima_fit_02)

#devuelve un best model de ARIMA (0,1,0)

fitARIMA_02 <- arima(distrito_02_ts, order=c(0,1,0),seasonal = list(order = c(1,0,0), period = 12),method="ML")


#dibujamos la serie temporal con su ARIMA
plot(distrito_02_ts, main="ARIMA(0,1,0)")
lines(fitted(arima_fit_02), col="blue")
#vemos que ajusta bien

#visualizamos los residuos para ARIMA (0,1,0)
checkresiduals(fitARIMA_02)




coeftest(fitARIMA_02)

confint(fitARIMA_02)

predict(fitARIMA_02,n.ahead = 5)

boxresult=LjungBoxTest (fitARIMA_02$residuals,k=2,StartLag=1)
plot(boxresult[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(fitARIMA_02$residuals)
qqline(fitARIMA_02$residuals)
#El Acf de los residuos muestra que no hay autocorrelaciones significativas
#Los valores del p-value en el test de Ljung-Box Q están  casi todos por encima de 0.05 por lo que los datos no son dependientes


futurVal_02 <- forecast(fitARIMA_02,h=5, level=c(99.5))
plot(futurVal_02)




write.table("distrito_02", file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_distritos.xls", append=TRUE)

write.table(futurVal_02, file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_distritos.xls", append=TRUE)

#################################################################################################3
## ARIMA DISTRITO 02_RETIRO

#seleccionamos el distrito 03 para hacer el arima sobre él

distrito_03 <- distritos_group %>% 
  filter(cod_distrito == "03")

#ahora nos quedamos solo con el price_m2 para hacer el arima

colnames(distrito_03) <- c("Distrito", "Year", "Price_m2")  


distrito_03_price <- distrito_03 %>% 
  ungroup() %>% 
  select(Price_m2)

#convertimos la serie en timeseries para poder estimar sobre ella

distrito_03_ts <- ts(distrito_03_price, start = c(2005,1), frequency = 1)

plot(distrito_03_ts)

#intamos ver los componentes de la serie temporal(Observed-trend-seasonal-random)
componentes.ts = decompose(distrito_03_ts)

#da un error porque tengo un número insuficiente de años para cubrir ciclos estacionales completos

#hacemos un tsdisplay para analizar la serie temporal con ACF Y PACF

tsdisplay(distrito_03_ts, lag.max=10)

#la visualización no es suficiente para ver si son estacionarias o no. 
#hacemos una visualización de los residuos

urkpssTest(distrito_03_ts, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)


#ejecutamos el kpss test
data_kpss <- kpss.test(distrito_03_ts)

print(data_kpss)

#buscamos el orden de diferenciación para hacer estacionaria la serie temporal.Utilizamos ndiffs


orden_dif_03 <- ndiffs(distrito_03_ts)

#el orden de diferenciación en este caso es cero


#Prueba Dickey-Fuller (ADF)
adf.test(distrito_03_ts, alternative = "stationary")

#Tras realizar la prueba aumentada de Dickey-Fuller (ADF), obtenemos un p-valor = 0.99. 
#Como el valor es mayor que 0.05 decimos que la serie temporal no es estacionaria y no rechazamos la hipótesis nula

tsdisplay(distrito_03_ts, main="distrito_01", lag=10)

#todo parece indicar que se trata de ruido blanco


# Ahora vamos a realizar el autorima 

arima_fit_03 <- auto.arima(distrito_03_ts, seasonal = FALSE)
print(arima_fit_03)
#obtenermos un ARIMA (1,0,1)
#dibujamos la serie temporal con su ARIMA
plot(distrito_03_ts, main="ARIMA(1,0,1)")
lines(fitted(arima_fit_03), col="blue")

#visualizamos los residuos para ARIMA (1,0,1)
checkresiduals(arima_fit_03)

#según consulta a stackoverflow habría que quitar la media
#fit3 <- auto.arima(distrito_03_ts, trace=TRUE, test="kpss",  ic="bic",allowmean = F)


#ahora el best model es ARIMA (1,0,0) with zero mean
fitARIMA_03 <- arima(distrito_03_ts, order=c(1,0,1),seasonal = list(order = c(1,0,0), period = 12),method="ML")



#dibujamos la serie temporal con su ARIMA
plot(distrito_03_ts, main="ARIMA(1,0,1)")
lines(fitted(fitARIMA_03), col="blue")

#vemos que ajusta mucho mejor

#visualizamos los residuos para ARIMA (1,0,1)
checkresiduals(fitARIMA_03)

coeftest(fitARIMA_03)

confint(fitARIMA_03)

predict(fitARIMA_03,n.ahead = 5)

boxresult=LjungBoxTest (fitARIMA_03$residuals,k=2,StartLag=1)
plot(boxresult[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(fitARIMA_03$residuals)
qqline(fitARIMA_03$residuals)
#El Acf de los residuos muestra que no hay autocorrelaciones significativas
#Los valores del p-value en el test de Ljung-Box Q están  casi todos por encima de 0.05 por lo que los datos no son dependientes


futurVal_03 <- forecast(fitARIMA_03,h=5, level=c(99.5))
plot(futurVal_03)



write.table("distrito_03", file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_distritos.xls", append=TRUE)

write.table(futurVal_03, file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_distritos.xls", append=TRUE)



############################################################################################33

############################################################################################33

## ARIMA DISTRITO 04_SALAMANCA

#seleccionamos el distrito 04 para hacer el arima sobre él

distrito_04 <- distritos_group %>% 
  filter(cod_distrito == "04")

#ahora nos quedamos solo con el price_m2 para hacer el arima

colnames(distrito_04) <- c("Distrito", "Year", "Price_m2")  


distrito_04_price <- distrito_04 %>% 
  ungroup() %>% 
  select(Price_m2)

#convertimos la serie en timeseries para poder estimar sobre ella

distrito_04_ts <- ts(distrito_04_price, start = c(2004,1), frequency = 1)

plot(distrito_04_ts)

#intamos ver los componentes de la serie temporal(Observed-trend-seasonal-random)
componentes.ts = decompose(distrito_04_ts)

#da un error porque tengo un número insuficiente de años para cubrir ciclos estacionales completos

#hacemos un tsdisplay para analizar la serie temporal con ACF Y PACF

tsdisplay(distrito_04_ts, lag.max=10)

#la visualización no es suficiente para ver si son estacionarias o no. 
#hacemos una visualización de los residuos

urkpssTest(distrito_04_ts, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)


#ejecutamos el kpss test
data_kpss <- kpss.test(distrito_04_ts)

print(data_kpss)

#buscamos el orden de diferenciación para hacer estacionaria la serie temporal.Utilizamos ndiffs


orden_dif_04 <- ndiffs(distrito_04_ts)

#el orden de diferenciación en este caso es cero


#Prueba Dickey-Fuller (ADF)
adf.test(distrito_04_ts, alternative = "stationary")

#Tras realizar la prueba aumentada de Dickey-Fuller (ADF), obtenemos un p-valor = 0.46. 
#Como el valor es mayor que 0.05 decimos que la serie temporal no es estacionaria y no rechazamos la hipótesis nula

tsdisplay(distrito_04_ts, main="distrito_04", lag=10)

#todo parece indicar que se trata de ruido blanco


# Ahora vamos a realizar el autorima 

arima_fit_04 <- auto.arima(distrito_04_ts, seasonal = FALSE)
print(arima_fit_04)

#arroja un arima de (2,0,0)
#dibujamos la serie temporal con su ARIMA
plot(distrito_04_ts, main="ARIMA(2,0,0)")
lines(fitted(arima_fit_04), col="blue")

#visualizamos los residuos para ARIMA (2,0,0)
checkresiduals(arima_fit_04)


#según consulta a stackoverflow habría que quitar la media
#fit4 <- auto.arima(distrito_04_ts, trace=TRUE, test="kpss",  ic="bic",allowmean = F)


#ahora el best model es ARIMA (2,0,0) with zero mean
fitARIMA_04 <- arima(distrito_04_ts, order=c(2,0,0),seasonal = list(order = c(1,0,0), period = 12),method="ML")



#dibujamos la serie temporal con su ARIMA
plot(distrito_04_ts, main="ARIMA(2,0,0)")
lines(fitted(fitARIMA_04), col="blue")

#vemos que ajusta mucho mejor

#visualizamos los residuos para ARIMA (2,0,0)
checkresiduals(fitARIMA_04)
coeftest(fitARIMA_04)

confint(fitARIMA_04)

predict(fitARIMA_04,n.ahead = 5)


boxresult=LjungBoxTest (fitARIMA_04$residuals,k=2,StartLag=1)
plot(boxresult[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(fitARIMA_04$residuals)
qqline(fitARIMA_04$residuals)
#El Acf de los residuos muestra que no hay autocorrelaciones significativas
#Los valores del p-value en el test de Ljung-Box Q están  casi todos por encima de 0.05 por lo que los datos no son dependientes


futurVal_04 <- forecast(fitARIMA_04,h=5, level=c(99.5))
plot(futurVal_04)



write.table("distrito_04", file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_distritos.xls", append=TRUE)

write.table(futurVal_04, file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_distritos.xls", append=TRUE)

###########################################################################################################333

## ARIMA DISTRITO 05_CHAMARTIN

#seleccionamos el distrito 05 para hacer el arima sobre él

distrito_05 <- distritos_group %>% 
  filter(cod_distrito == "05")

#ahora nos quedamos solo con el price_m2 para hacer el arima

colnames(distrito_05) <- c("Distrito", "Year", "Price_m2")  


distrito_05_price <- distrito_05 %>% 
  ungroup() %>% 
  select(Price_m2)

#convertimos la serie en timeseries para poder estimar sobre ella

distrito_05_ts <- ts(distrito_05_price, start = c(2003,1), frequency = 1)

plot(distrito_05_ts)

#intamos ver los componentes de la serie temporal(Observed-trend-seasonal-random)
componentes.ts = decompose(distrito_05_ts)

#da un error porque tengo un número insuficiente de años para cubrir ciclos estacionales completos

#hacemos un tsdisplay para analizar la serie temporal con ACF Y PACF

tsdisplay(distrito_05_ts, lag.max=10)

#la visualización no es suficiente para ver si son estacionarias o no. Aunque al decrecer el ACF podemos deducir que quizá no sea estacionaria
#hacemos una visualización de los residuos

urkpssTest(distrito_05_ts, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)


#ejecutamos el kpss test
data_kpss <- kpss.test(distrito_05_ts)

print(data_kpss)

#buscamos el orden de diferenciación para hacer estacionaria la serie temporal.Utilizamos ndiffs


orden_dif_05 <- ndiffs(distrito_05_ts)

#el orden de diferenciación en este caso es de 1
diff_distrito_05 <- diff(distrito_05_ts, orden_dif_05)
plot(diff_distrito_05, ylab =" Orden de diferenciación")


tsdisplay(diff_distrito_05, main="Primer orden diferenciación")

#con ACF vemos que ninguna autocorrelación está fuera del límite del 95%

#Prueba Dic5key-Fuller (ADF)
adf.test(distrito_05_ts, alternative = "stationary")

#Tras realizar la prueba aumentada de Dickey-Fuller (ADF), obtenemos un p-valor = 0.89. 
#Como el valor es mayor que 0.05 decimos que la serie temporal no es estacionaria y no rechazamos la hipótesis nula


# Ahora vamos a realizar el autorima 

# Para hacer un modelo Arima la serie debe ser estacionaria y para ello debemos diferenciarla

d1 = diff(distrito_05_ts, differences = 1)
plot(d1)

#para comprobar que la serie es realmente estacionaria tenemos que hacer otra vez el test adf

adf.test(d1, alternative = "stationary")

# El p-value ahora es de 0.01 e inferior a 0.05
#Ahora rechazamos la hipótesis nula y podemos decir que la serie temporal es estacionaria.

Acf(d1, main='ACF para serie diferenciada')
Pacf(d1, main='PACF para serie diferenciada')


arima_fit_05 <- auto.arima(distrito_05_ts, seasonal = FALSE)
print(arima_fit_05)

#devuelve un best model de ARIMA (0,1,0)

fitARIMA_05 <- arima(distrito_05_ts, order=c(0,1,0),seasonal = list(order = c(1,0,0), period = 12),method="ML")

#best model ARIMA (0,1,0)
#dibujamos la serie temporal con su ARIMA
plot(distrito_05_ts, main="ARIMA(0,1,0)")
lines(fitted(arima_fit_05), col="blue")
#vemos que ajusta bien

#visualizamos los residuos para ARIMA (0,1,0)
checkresiduals(fitARIMA_05)




coeftest(fitARIMA_05)

confint(fitARIMA_05)

predict(fitARIMA_05,n.ahead = 5)

boxresult=LjungBoxTest (fitARIMA_05$residuals,k=2,StartLag=1)
plot(boxresult[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(fitARIMA_05$residuals)
qqline(fitARIMA_05$residuals)
#El Acf de los residuos muestra que no hay autocorrelaciones significativas
#Los valores del p-value en el test de Ljung-Box Q están  casi todos por encima de 0.05 por lo que los datos no son dependientes


futurVal_05 <- forecast(fitARIMA_05,h=5, level=c(99.5))
plot(futurVal_05)




write.table("distrito_05", file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_distritos.xls", append=TRUE)

write.table(futurVal_05, file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_distritos.xls", append=TRUE)


## ARIMA DISTRITO 06_TETUAN

#seleccionamos el distrito 06 para hacer el arima sobre él

distrito_06 <- distritos_group %>% 
  filter(cod_distrito == "06")

#ahora nos quedamos solo con el price_m2 para hacer el arima

colnames(distrito_06) <- c("Distrito", "Year", "Price_m2")  


distrito_06_price <- distrito_06 %>% 
  ungroup() %>% 
  select(Price_m2)

#convertimos la serie en timeseries para poder estimar sobre ella

distrito_06_ts <- ts(distrito_06_price, start = c(2003,1), frequency = 1)

plot(distrito_06_ts)


#intamos ver los componentes de la serie temporal(Observed-trend-seasonal-random)
componentes.ts = decompose(distrito_06_ts)

#da un error porque tengo un número insuficiente de años para cubrir ciclos estacionales completos

#hacemos un tsdisplay para analizar la serie temporal con ACF Y PACF

tsdisplay(distrito_06_ts, lag.max=10)

#la visualización no es suficiente para ver si son estacionarias o no. Aunque al decrecer el ACF podemos deducir que quizá no sea estacionaria
#hacemos una visualización de los residuos

urkpssTest(distrito_06_ts, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)


#ejecutamos el kpss test
data_kpss <- kpss.test(distrito_06_ts)

print(data_kpss)

#En el KPSS los valores pequeños de p indican que quizá se debería hacer una diferenciación.

#buscamos el orden de diferenciación para hacer estacionaria la serie temporal.Utilizamos ndiffs


orden_dif_06 <- ndiffs(distrito_06_ts)

#el orden de diferenciación en este caso es de 1
diff_distrito_06 <- diff(distrito_06_ts, orden_dif_06)
plot(diff_distrito_06, ylab =" Orden de diferenciación")


tsdisplay(diff_distrito_06, main="Primer orden diferenciación")

#con ACF vemos que ninguna autocorrelación está fuera del límite del 95%

#Prueba Dic5key-Fuller (ADF)
adf.test(distrito_06_ts, alternative = "stationary")

#Tras realizar la prueba aumentada de Dickey-Fuller (ADF), obtenemos un p-valor = 0.82. 
#Como el valor es mayor que 0.05 decimos que la serie temporal no es estacionaria y no rechazamos la hipótesis nula


# Ahora vamos a realizar el autorima 


#para comprobar que la serie es realmente estacionaria tenemos que hacer otra vez el test adf

adf.test(diff_distrito_06, alternative = "stationary")

# El p-value ahora es más alto aún... NO LO PUEDO EXPLICAR

Acf(diff_distrito_06, main='ACF para serie diferenciada')
Pacf(diff_distrito_06, main='PACF para serie diferenciada')


arima_fit_06 <- auto.arima(distrito_06_ts, seasonal = FALSE)
print(arima_fit_06)

#devuelve un best model de ARIMA (0,1,0)

fitARIMA_06 <- arima(distrito_06_ts, order=c(0,1,0),seasonal = list(order = c(1,0,0), period = 12),method="ML")

#best model ARIMA (0,1,0)
#dibujamos la serie temporal con su ARIMA
plot(distrito_06_ts, main="ARIMA(0,1,0)")
lines(fitted(arima_fit_06), col="blue")
#vemos que ajusta más o menos bien

#visualizamos los residuos para ARIMA (0,1,0)
checkresiduals(fitARIMA_06)


coeftest(fitARIMA_06)

confint(fitARIMA_06)

predict(fitARIMA_06,n.ahead = 5)

boxresult=LjungBoxTest (fitARIMA_06$residuals,k=2,StartLag=1)
plot(boxresult[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(fitARIMA_06$residuals)
qqline(fitARIMA_06$residuals)
#El Acf de los residuos muestra que no hay autocorrelaciones significativas
#Los valores del p-value en el test de Ljung-Box Q están  casi todos por encima de 0.05 por lo que los datos no son dependientes


futurVal_06 <- forecast(fitARIMA_06,h=5, level=c(99.5))
plot(futurVal_06)



write.table("distrito_06", file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_distritos.xls", append=TRUE)

write.table(futurVal_06, file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_distritos.xls", append=TRUE)

## ARIMA DISTRITO 07_CHAMBERI

#seleccionamos el distrito 07 para hacer el arima sobre él

distrito_07 <- distritos_group %>% 
  filter(cod_distrito == "07")

#ahora nos quedamos solo con el price_m2 para hacer el arima

colnames(distrito_07) <- c("Distrito", "Year", "Price_m2")  


distrito_07_price <- distrito_07 %>% 
  ungroup() %>% 
  select(Price_m2)

#convertimos la serie en timeseries para poder estimar sobre ella

distrito_07_ts <- ts(distrito_07_price, start = c(2005,1), frequency = 1)

plot(distrito_07_ts)


#intamos ver los componentes de la serie temporal(Observed-trend-seasonal-random)
componentes.ts = decompose(distrito_07_ts)

#da un error porque tengo un número insuficiente de años para cubrir ciclos estacionales completos

#hacemos un tsdisplay para analizar la serie temporal con ACF Y PACF

tsdisplay(distrito_07_ts, lag.max=10)

#la visualización no es suficiente para ver si son estacionarias o no. Aunque al decrecer el ACF podemos deducir que quizá no sea estacionaria
#hacemos una visualización de los residuos

urkpssTest(distrito_07_ts, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)


#ejecutamos el kpss test
data_kpss <- kpss.test(distrito_07_ts)

print(data_kpss)

#En el KPSS los valores pequeños de p indican que quizá se debería hacer una diferenciación.

#buscamos el orden de diferenciación para hacer estacionaria la serie temporal.Utilizamos ndiffs


orden_dif_07 <- ndiffs(distrito_07_ts)


#el orden de diferenciación en este caso es cero

#Prueba Dickey-Fuller (ADF)
adf.test(distrito_07_ts, alternative = "stationary")

#Tras realizar la prueba aumentada de Dickey-Fuller (ADF), obtenemos un p-valor = 0.9519. 
#Como el valor es mayor que 0.05 decimos que la serie temporal no es estacionaria y no rechazamos la hipótesis nula

tsdisplay(distrito_07_ts, main="distrito_01", lag=10)

#todo parece indicar que se trata de ruido blanco


# Ahora vamos a realizar el autorima 

arima_fit_07 <- auto.arima(distrito_07_ts, seasonal = FALSE)
print(arima_fit_07)


#dibujamos la serie temporal con su ARIMA
plot(distrito_07_ts, main="ARIMA(1,0,0)")
lines(fitted(arima_fit_07), col="blue")

#visualizamos los residuos para ARIMA (1,0,0)
checkresiduals(arima_fit_07)



#el best model es ARIMA (1,0,0) with zero mean
fitARIMA_07 <- arima(distrito_07_ts, order=c(1,0,0),seasonal = list(order = c(1,0,0), period = 12), method="ML")



#dibujamos la serie temporal con su ARIMA
plot(distrito_07_ts, main="ARIMA(1,0,0)")
lines(fitted(fitARIMA_07), col="blue")

#vemos que ajusta mucho mejor

#visualizamos los residuos para ARIMA (1,0,0)
checkresiduals(fitARIMA_07)
#install.packages("lmtest")
library(lmtest)
coeftest(fitARIMA_07)

confint(fitARIMA_07)

predict(fitARIMA_07,n.ahead = 5)

#install.packages("FitAR")
library(FitAR)
boxresult=LjungBoxTest (fitARIMA_07$residuals,k=2,StartLag=1)
plot(boxresult[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(fitARIMA_07$residuals)
qqline(fitARIMA_07$residuals)
#El Acf de los residuos muestra que no hay autocorrelaciones significativas
#Los valores del p-value en el test de Ljung-Box Q están  casi todos por encima de 0.05 por lo que los datos no son dependientes


futurVal_07 <- forecast(fitARIMA_07,h=5, level=c(99.5))
plot(futurVal_07)



write.table("distrito_07", file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_distritos.xls", append=TRUE)

write.table(futurVal_07, file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_distritos.xls", append=TRUE)


########################################################################################################3

## ARIMA DISTRITO 08_FUENCARRAL-EL PARDO

#seleccionamos el distrito 08 para hacer el arima sobre él

distrito_08 <- distritos_group %>% 
  filter(cod_distrito == "08")

#ahora nos quedamos solo con el price_m2 para hacer el arima

colnames(distrito_08) <- c("Distrito", "Year", "Price_m2")  


distrito_08_price <- distrito_08 %>% 
  ungroup() %>% 
  select(Price_m2)

#convertimos la serie en timeseries para poder estimar sobre ella

distrito_08_ts <- ts(distrito_08_price, start = c(2003,1), frequency = 1)

plot(distrito_08_ts)


#intamos ver los componentes de la serie temporal(Observed-trend-seasonal-random)
componentes.ts = decompose(distrito_08_ts)

#da un error porque tengo un número insuficiente de años para cubrir ciclos estacionales completos

#hacemos un tsdisplay para analizar la serie temporal con ACF Y PACF

tsdisplay(distrito_08_ts, lag.max=10)

#la visualización no es suficiente para ver si son estacionarias o no. Aunque al decrecer el ACF podemos deducir que quizá no sea estacionaria
#hacemos una visualización de los residuos

urkpssTest(distrito_08_ts, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)


#ejecutamos el kpss test
data_kpss <- kpss.test(distrito_08_ts)

print(data_kpss)

#En el KPSS los valores pequeños de p indican que quizá se debería hacer una diferenciación.

#buscamos el orden de diferenciación para hacer estacionaria la serie temporal.Utilizamos ndiffs


orden_dif_08 <- ndiffs(distrito_08_ts)

#el orden de diferenciación en este caso es de 1
diff_distrito_08 <- diff(distrito_08_ts, orden_dif_08)
plot(diff_distrito_08, ylab =" Orden de diferenciación")


tsdisplay(diff_distrito_08, main="Primer orden diferenciación")

#con ACF vemos que ninguna autocorrelación está fuera del límite del 95%

#Prueba Dic5key-Fuller (ADF)
adf.test(distrito_08_ts, alternative = "stationary")

#Tras realizar la prueba aumentada de Dickey-Fuller (ADF), obtenemos un p-valor = 0.99. 
#Como el valor es mayor que 0.05 decimos que la serie temporal no es estacionaria y no rechazamos la hipótesis nula


# Ahora vamos a realizar el autorima 


#para comprobar que la serie es realmente estacionaria tenemos que hacer otra vez el test adf

adf.test(diff_distrito_08, alternative = "stationary")

# El p-value apenas baja por lo que la serie no es estacionaria aun



Acf(diff_distrito_08, main='ACF para serie diferenciada')
Pacf(diff_distrito_08, main='PACF para serie diferenciada')


arima_fit_08 <- auto.arima(distrito_08_ts, seasonal = FALSE)
print(arima_fit_08)

#devuelve un best model de ARIMA (0,1,0)

fitARIMA_08 <- arima(distrito_08_ts, order=c(0,1,0),seasonal = list(order = c(1,0,0), period = 12),method="ML")

#best model ARIMA (0,1,0)
#dibujamos la serie temporal con su ARIMA
plot(distrito_08_ts, main="ARIMA(0,1,0)")
lines(fitted(arima_fit_08), col="blue")
#vemos que ajusta más o menos bien

#visualizamos los residuos para ARIMA (0,1,0)
checkresiduals(fitARIMA_08)


coeftest(fitARIMA_08)

confint(fitARIMA_08)

predict(fitARIMA_08,n.ahead = 5)

boxresult=LjungBoxTest (fitARIMA_08$residuals,k=2,StartLag=1)
plot(boxresult[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(fitARIMA_08$residuals)
qqline(fitARIMA_08$residuals)
#El Acf de los residuos muestra que no hay autocorrelaciones significativas
#Los valores del p-value en el test de Ljung-Box Q están  casi todos por encima de 0.05 por lo que los datos no son dependientes


futurVal_08 <- forecast(fitARIMA_08,h=5, level=c(99.5))
plot(futurVal_08)



write.table("distrito_08", file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_distritos.xls", append=TRUE)

write.table(futurVal_08, file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_distritos.xls", append=TRUE)


########################################################################################################3

## ARIMA DISTRITO 09_MONCLOA-ARAVACA

#seleccionamos el distrito 09 para hacer el arima sobre él

distrito_09 <- distritos_group %>% 
  filter(cod_distrito == "09")

#ahora nos quedamos solo con el price_m2 para hacer el arima

colnames(distrito_09) <- c("Distrito", "Year", "Price_m2")  


distrito_09_price <- distrito_09 %>% 
  ungroup() %>% 
  select(Price_m2)

#convertimos la serie en timeseries para poder estimar sobre ella

distrito_09_ts <- ts(distrito_09_price, start = c(2003,1), frequency = 1)

plot(distrito_09_ts)


#intamos ver los componentes de la serie temporal(Observed-trend-seasonal-random)
componentes.ts = decompose(distrito_09_ts)

#da un error porque tengo un número insuficiente de años para cubrir ciclos estacionales completos

#hacemos un tsdisplay para analizar la serie temporal con ACF Y PACF

tsdisplay(distrito_09_ts, lag.max=10)

#la visualización no es suficiente para ver si son estacionarias o no. Aunque al decrecer el ACF podemos deducir que quizá no sea estacionaria
#hacemos una visualización de los residuos

urkpssTest(distrito_09_ts, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)


#ejecutamos el kpss test
data_kpss <- kpss.test(distrito_09_ts)

print(data_kpss)

#En el KPSS los valores pequeños de p indican que quizá se debería hacer una diferenciación.

#buscamos el orden de diferenciación para hacer estacionaria la serie temporal.Utilizamos ndiffs


orden_dif_09 <- ndiffs(distrito_09_ts)

#el orden de diferenciación en este caso es de 1
diff_distrito_09 <- diff(distrito_09_ts, orden_dif_09)
plot(diff_distrito_09, ylab =" Orden de diferenciación")


tsdisplay(diff_distrito_09, main="Primer orden diferenciación")

#con ACF vemos que ninguna autocorrelación está fuera del límite del 95%

#Prueba Dic5key-Fuller (ADF)
adf.test(distrito_09_ts, alternative = "stationary")

#Tras realizar la prueba aumentada de Dickey-Fuller (ADF), obtenemos un p-valor = 0.99. 
#Como el valor es mayor que 0.05 decimos que la serie temporal no es estacionaria y no rechazamos la hipótesis nula


# Ahora vamos a realizar el autorima 


#para comprobar que la serie es realmente estacionaria tenemos que hacer otra vez el test adf

adf.test(diff_distrito_09, alternative = "stationary")

# El p-value apenas baja por lo que la serie no es estacionaria aun



Acf(diff_distrito_09, main='ACF para serie diferenciada')
Pacf(diff_distrito_09, main='PACF para serie diferenciada')


arima_fit_09 <- auto.arima(distrito_09_ts, seasonal = FALSE)
print(arima_fit_09)

#devuelve un best model de ARIMA (0,1,0)

fitARIMA_09 <- arima(distrito_09_ts, order=c(0,1,0),seasonal = list(order = c(1,0,0), period = 12),method="ML")

#best model ARIMA (0,1,0)
#dibujamos la serie temporal con su ARIMA
plot(distrito_09_ts, main="ARIMA(0,1,0)")
lines(fitted(arima_fit_09), col="blue")
#vemos que ajusta más o menos bien

#visualizamos los residuos para ARIMA (0,1,0)
checkresiduals(fitARIMA_09)


coeftest(fitARIMA_09)

confint(fitARIMA_09)

predict(fitARIMA_09,n.ahead = 5)

boxresult=LjungBoxTest (fitARIMA_09$residuals,k=2,StartLag=1)
plot(boxresult[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(fitARIMA_09$residuals)
qqline(fitARIMA_09$residuals)
#El Acf de los residuos muestra que no hay autocorrelaciones significativas
#Los valores del p-value en el test de Ljung-Box Q están  casi todos por encima de 0.05 por lo que los datos no son dependientes


futurVal_09 <- forecast(fitARIMA_09,h=5, level=c(99.5))
plot(futurVal_09)



write.table("distrito_09", file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_distritos.xls", append=TRUE)

write.table(futurVal_09, file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_distritos.xls", append=TRUE)


########################################################################################################3

## ARIMA DISTRITO 10_LATINA

#seleccionamos el distrito 10 para hacer el arima sobre él

distrito_10 <- distritos_group %>% 
  filter(cod_distrito == "10")

#ahora nos quedamos solo con el price_m2 para hacer el arima

colnames(distrito_10) <- c("Distrito", "Year", "Price_m2")  


distrito_10_price <- distrito_10 %>% 
  ungroup() %>% 
  select(Price_m2)

#convertimos la serie en timeseries para poder estimar sobre ella

distrito_10_ts <- ts(distrito_10_price, start = c(2003,1), frequency = 1)

plot(distrito_10_ts)


#intamos ver los componentes de la serie temporal(Observed-trend-seasonal-random)
componentes.ts = decompose(distrito_10_ts)

#da un error porque tengo un número insuficiente de años para cubrir ciclos estacionales completos

#hacemos un tsdisplay para analizar la serie temporal con ACF Y PACF

tsdisplay(distrito_10_ts, lag.max=10)

#la visualización no es suficiente para ver si son estacionarias o no. Aunque al decrecer el ACF podemos deducir que quizá no sea estacionaria
#hacemos una visualización de los residuos

urkpssTest(distrito_10_ts, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)


#ejecutamos el kpss test
data_kpss <- kpss.test(distrito_10_ts)

print(data_kpss)

#En el KPSS los valores pequeños de p indican que quizá se debería hacer una diferenciación.

#buscamos el orden de diferenciación para hacer estacionaria la serie temporal.Utilizamos ndiffs


orden_dif_10 <- ndiffs(distrito_10_ts)

#el orden de diferenciación en este caso es de 1
diff_distrito_10 <- diff(distrito_10_ts, orden_dif_10)
plot(diff_distrito_10, ylab =" Orden de diferenciación")


tsdisplay(diff_distrito_10, main="Primer orden diferenciación")

#con ACF vemos que ninguna autocorrelación está fuera del límite del 95%

#Prueba Dic5key-Fuller (ADF)
adf.test(distrito_10_ts, alternative = "stationary")

#Tras realizar la prueba aumentada de Dickey-Fuller (ADF), obtenemos un p-valor = 0.99. 
#Como el valor es mayor que 0.05 decimos que la serie temporal no es estacionaria y no rechazamos la hipótesis nula


# Ahora vamos a realizar el autorima 


#para comprobar que la serie es realmente estacionaria tenemos que hacer otra vez el test adf

adf.test(diff_distrito_10, alternative = "stationary")

# El p-value apenas baja por lo que la serie no es estacionaria aun



Acf(diff_distrito_10, main='ACF para serie diferenciada')
Pacf(diff_distrito_10, main='PACF para serie diferenciada')


arima_fit_10 <- auto.arima(distrito_10_ts, seasonal = FALSE)
print(arima_fit_10)

#devuelve un best model de ARIMA (0,1,0)

fitARIMA_10 <- arima(distrito_10_ts, order=c(0,1,0),seasonal = list(order = c(1,0,0), period = 12),method="ML")

#best model ARIMA (0,1,0)
#dibujamos la serie temporal con su ARIMA
plot(distrito_10_ts, main="ARIMA(0,1,0)")
lines(fitted(arima_fit_10), col="blue")
#vemos que ajusta más o menos bien

#visualizamos los residuos para ARIMA (0,1,0)
checkresiduals(fitARIMA_10)


coeftest(fitARIMA_10)

confint(fitARIMA_10)

predict(fitARIMA_10,n.ahead = 5)

boxresult=LjungBoxTest (fitARIMA_10$residuals,k=2,StartLag=1)
plot(boxresult[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(fitARIMA_10$residuals)
qqline(fitARIMA_10$residuals)
#El Acf de los residuos muestra que no hay autocorrelaciones significativas
#Los valores del p-value en el test de Ljung-Box Q están  casi todos por encima de 0.05 por lo que los datos no son dependientes


futurVal_10 <- forecast(fitARIMA_10,h=5, level=c(99.5))
plot(futurVal_10)



write.table("distrito_10", file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_distritos.xls", append=TRUE)

write.table(futurVal_10, file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_distritos.xls", append=TRUE)

########################################################################################################3

## ARIMA DISTRITO 11_CARABANCHEL

#seleccionamos el distrito 11 para hacer el arima sobre él

distrito_11 <- distritos_group %>% 
  filter(cod_distrito == "11")

#ahora nos quedamos solo con el price_m2 para hacer el arima

colnames(distrito_11) <- c("Distrito", "Year", "Price_m2")  


distrito_11_price <- distrito_11 %>% 
  ungroup() %>% 
  select(Price_m2)

#convertimos la serie en timeseries para poder estimar sobre ella

distrito_11_ts <- ts(distrito_11_price, start = c(2003,1), frequency = 1)

plot(distrito_11_ts)


#intamos ver los componentes de la serie temporal(Observed-trend-seasonal-random)
componentes.ts = decompose(distrito_11_ts)

#da un error porque tengo un número insuficiente de años para cubrir ciclos estacionales completos

#hacemos un tsdisplay para analizar la serie temporal con ACF Y PACF

tsdisplay(distrito_11_ts, lag.max=10)

#la visualización no es suficiente para ver si son estacionarias o no. Aunque al decrecer el ACF podemos deducir que quizá no sea estacionaria
#hacemos una visualización de los residuos

urkpssTest(distrito_11_ts, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)


#ejecutamos el kpss test
data_kpss <- kpss.test(distrito_11_ts)

print(data_kpss)

#En el KPSS los valores pequeños de p indican que quizá se debería hacer una diferenciación.

#buscamos el orden de diferenciación para hacer estacionaria la serie temporal.Utilizamos ndiffs


orden_dif_11 <- ndiffs(distrito_11_ts)

#el orden de diferenciación en este caso es de 1
diff_distrito_11 <- diff(distrito_11_ts, orden_dif_11)
plot(diff_distrito_11, ylab =" Orden de diferenciación")


tsdisplay(diff_distrito_11, main="Primer orden diferenciación")

#con ACF vemos que ninguna autocorrelación está fuera del límite del 95%

#Prueba Dic5key-Fuller (ADF)
adf.test(distrito_11_ts, alternative = "stationary")

#Tras realizar la prueba aumentada de Dickey-Fuller (ADF), obtenemos un p-valor = 0.99. 
#Como el valor es mayor que 0.05 decimos que la serie temporal no es estacionaria y no rechazamos la hipótesis nula


# Ahora vamos a realizar el autorima 


#para comprobar que la serie es realmente estacionaria tenemos que hacer otra vez el test adf

adf.test(diff_distrito_11, alternative = "stationary")

# El p-value apenas baja por lo que la serie no es estacionaria aun



Acf(diff_distrito_11, main='ACF para serie diferenciada')
Pacf(diff_distrito_11, main='PACF para serie diferenciada')


arima_fit_11 <- auto.arima(distrito_11_ts, seasonal = FALSE)
print(arima_fit_11)

#devuelve un best model de ARIMA (0,1,0)

fitARIMA_11 <- arima(distrito_11_ts, order=c(0,1,0),seasonal = list(order = c(1,0,0), period = 12),method="ML")

#best model ARIMA (0,1,0)
#dibujamos la serie temporal con su ARIMA
plot(distrito_11_ts, main="ARIMA(0,1,0)")
lines(fitted(arima_fit_11), col="blue")
#vemos que ajusta más o menos bien

#visualizamos los residuos para ARIMA (0,1,0)
checkresiduals(fitARIMA_11)


coeftest(fitARIMA_11)

confint(fitARIMA_11)

predict(fitARIMA_11,n.ahead = 5)

boxresult=LjungBoxTest (fitARIMA_11$residuals,k=2,StartLag=1)
plot(boxresult[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(fitARIMA_11$residuals)
qqline(fitARIMA_11$residuals)
#El Acf de los residuos muestra que no hay autocorrelaciones significativas
#Los valores del p-value en el test de Ljung-Box Q están  casi todos por encima de 0.05 por lo que los datos no son dependientes


futurVal_11 <- forecast(fitARIMA_11,h=5, level=c(99.5))
plot(futurVal_11)



write.table("distrito_11", file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_distritos.xls", append=TRUE)

write.table(futurVal_11, file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_distritos.xls", append=TRUE)

########################################################################################################3

## ARIMA DISTRITO 12_USERA

#seleccionamos el distrito 12 para hacer el arima sobre él

distrito_12 <- distritos_group %>% 
  filter(cod_distrito == "12")

#ahora nos quedamos solo con el price_m2 para hacer el arima

colnames(distrito_12) <- c("Distrito", "Year", "Price_m2")  


distrito_12_price <- distrito_12 %>% 
  ungroup() %>% 
  select(Price_m2)

#convertimos la serie en timeseries para poder estimar sobre ella

distrito_12_ts <- ts(distrito_12_price, start = c(2003,1), frequency = 1)

plot(distrito_12_ts)


#intamos ver los componentes de la serie temporal(Observed-trend-seasonal-random)
componentes.ts = decompose(distrito_12_ts)

#da un error porque tengo un número insuficiente de años para cubrir ciclos estacionales completos

#hacemos un tsdisplay para analizar la serie temporal con ACF Y PACF

tsdisplay(distrito_12_ts, lag.max=10)

#la visualización no es suficiente para ver si son estacionarias o no. Aunque al decrecer el ACF podemos deducir que quizá no sea estacionaria
#hacemos una visualización de los residuos

urkpssTest(distrito_12_ts, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)


#ejecutamos el kpss test
data_kpss <- kpss.test(distrito_12_ts)

print(data_kpss)

#En el KPSS los valores pequeños de p indican que quizá se debería hacer una diferenciación.

#buscamos el orden de diferenciación para hacer estacionaria la serie temporal.Utilizamos ndiffs


orden_dif_12 <- ndiffs(distrito_12_ts)

#el orden de diferenciación en este caso es de 1
diff_distrito_12 <- diff(distrito_12_ts, orden_dif_12)
plot(diff_distrito_12, ylab =" Orden de diferenciación")


tsdisplay(diff_distrito_12, main="Primer orden diferenciación")

#con ACF vemos que ninguna autocorrelación está fuera del límite del 95%

#Prueba Dic5key-Fuller (ADF)
adf.test(distrito_12_ts, alternative = "stationary")

#Tras realizar la prueba aumentada de Dickey-Fuller (ADF), obtenemos un p-valor = 0.99. 
#Como el valor es mayor que 0.05 decimos que la serie temporal no es estacionaria y no rechazamos la hipótesis nula


# Ahora vamos a realizar el autorima 


#para comprobar que la serie es realmente estacionaria tenemos que hacer otra vez el test adf

adf.test(diff_distrito_12, alternative = "stationary")

# El p-value apenas baja por lo que la serie no es estacionaria aun



Acf(diff_distrito_12, main='ACF para serie diferenciada')
Pacf(diff_distrito_12, main='PACF para serie diferenciada')


arima_fit_12 <- auto.arima(distrito_12_ts, seasonal = FALSE)
print(arima_fit_12)

#devuelve un best model de ARIMA (0,1,0)

fitARIMA_12 <- arima(distrito_12_ts, order=c(0,1,0),seasonal = list(order = c(1,0,0), period = 12),method="ML")

#best model ARIMA (0,1,0)
#dibujamos la serie temporal con su ARIMA
plot(distrito_12_ts, main="ARIMA(0,1,0)")
lines(fitted(arima_fit_12), col="blue")
#vemos que ajusta más o menos bien

#visualizamos los residuos para ARIMA (0,1,0)
checkresiduals(fitARIMA_12)


coeftest(fitARIMA_12)

confint(fitARIMA_12)

predict(fitARIMA_12,n.ahead = 5)

boxresult=LjungBoxTest (fitARIMA_12$residuals,k=2,StartLag=1)
plot(boxresult[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(fitARIMA_12$residuals)
qqline(fitARIMA_12$residuals)
#El Acf de los residuos muestra que no hay autocorrelaciones significativas
#Los valores del p-value en el test de Ljung-Box Q están  casi todos por encima de 0.05 por lo que los datos no son dependientes


futurVal_12 <- forecast(fitARIMA_12,h=5, level=c(99.5))
plot(futurVal_12)



write.table("distrito_12", file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_distritos.xls", append=TRUE)

write.table(futurVal_12, file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_distritos.xls", append=TRUE)


########################################################################################################3

## ARIMA DISTRITO 13_PUENTE DE VALLECAS

#seleccionamos el distrito 13 para hacer el arima sobre él

distrito_13 <- distritos_group %>% 
  filter(cod_distrito == "13")

#ahora nos quedamos solo con el price_m2 para hacer el arima

colnames(distrito_13) <- c("Distrito", "Year", "Price_m2")  


distrito_13_price <- distrito_13 %>% 
  ungroup() %>% 
  select(Price_m2)

#convertimos la serie en timeseries para poder estimar sobre ella

distrito_13_ts <- ts(distrito_13_price, start = c(2003,1), frequency = 1)

plot(distrito_13_ts)


#intamos ver los componentes de la serie temporal(Observed-trend-seasonal-random)
componentes.ts = decompose(distrito_13_ts)

#da un error porque tengo un número insuficiente de años para cubrir ciclos estacionales completos

#hacemos un tsdisplay para analizar la serie temporal con ACF Y PACF

tsdisplay(distrito_13_ts, lag.max=10)

#la visualización no es suficiente para ver si son estacionarias o no. Aunque al decrecer el ACF podemos deducir que quizá no sea estacionaria
#hacemos una visualización de los residuos

urkpssTest(distrito_13_ts, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)


#ejecutamos el kpss test
data_kpss <- kpss.test(distrito_13_ts)

print(data_kpss)

#En el KPSS los valores pequeños de p indican que quizá se debería hacer una diferenciación.

#buscamos el orden de diferenciación para hacer estacionaria la serie temporal.Utilizamos ndiffs


orden_dif_13 <- ndiffs(distrito_13_ts)

#el orden de diferenciación en este caso es de 1
diff_distrito_13 <- diff(distrito_13_ts, orden_dif_13)
plot(diff_distrito_13, ylab =" Orden de diferenciación")


tsdisplay(diff_distrito_13, main="Primer orden diferenciación")

#con ACF vemos que ninguna autocorrelación está fuera del límite del 95%

#Prueba Dic5key-Fuller (ADF)
adf.test(distrito_13_ts, alternative = "stationary")

#Tras realizar la prueba aumentada de Dickey-Fuller (ADF), obtenemos un p-valor = 0.99. 
#Como el valor es mayor que 0.05 decimos que la serie temporal no es estacionaria y no rechazamos la hipótesis nula


# Ahora vamos a realizar el autorima 


#para comprobar que la serie es realmente estacionaria tenemos que hacer otra vez el test adf

adf.test(diff_distrito_13, alternative = "stationary")

# El p-value apenas baja por lo que la serie no es estacionaria aun



Acf(diff_distrito_13, main='ACF para serie diferenciada')
Pacf(diff_distrito_13, main='PACF para serie diferenciada')


arima_fit_13 <- auto.arima(distrito_13_ts, seasonal = FALSE)
print(arima_fit_13)

#devuelve un best model de ARIMA (0,1,0)

fitARIMA_13 <- arima(distrito_13_ts, order=c(0,1,0),seasonal = list(order = c(1,0,0), period = 12),method="ML")

#best model ARIMA (0,1,0)
#dibujamos la serie temporal con su ARIMA
plot(distrito_13_ts, main="ARIMA(0,1,0)")
lines(fitted(arima_fit_13), col="blue")
#vemos que ajusta más o menos bien

#visualizamos los residuos para ARIMA (0,1,0)
checkresiduals(fitARIMA_13)


coeftest(fitARIMA_13)

confint(fitARIMA_13)

predict(fitARIMA_13,n.ahead = 5)

boxresult=LjungBoxTest (fitARIMA_13$residuals,k=2,StartLag=1)
plot(boxresult[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(fitARIMA_13$residuals)
qqline(fitARIMA_13$residuals)
#El Acf de los residuos muestra que no hay autocorrelaciones significativas
#Los valores del p-value en el test de Ljung-Box Q están  casi todos por encima de 0.05 por lo que los datos no son dependientes


futurVal_13 <- forecast(fitARIMA_13,h=5, level=c(99.5))
plot(futurVal_13)



write.table("distrito_13", file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_distritos.xls", append=TRUE)

write.table(futurVal_13, file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_distritos.xls", append=TRUE)


########################################################################################################3

## ARIMA DISTRITO 14_MORATALAZ

#seleccionamos el distrito 14 para hacer el arima sobre él

distrito_14 <- distritos_group %>% 
  filter(cod_distrito == "14")

#ahora nos quedamos solo con el price_m2 para hacer el arima

colnames(distrito_14) <- c("Distrito", "Year", "Price_m2")  


distrito_14_price <- distrito_14 %>% 
  ungroup() %>% 
  select(Price_m2)

#convertimos la serie en timeseries para poder estimar sobre ella

distrito_14_ts <- ts(distrito_14_price, start = c(2003,1), frequency = 1)

plot(distrito_14_ts)


#intamos ver los componentes de la serie temporal(Observed-trend-seasonal-random)
componentes.ts = decompose(distrito_14_ts)

#da un error porque tengo un número insuficiente de años para cubrir ciclos estacionales completos

#hacemos un tsdisplay para analizar la serie temporal con ACF Y PACF

tsdisplay(distrito_14_ts, lag.max=10)

#la visualización no es suficiente para ver si son estacionarias o no. Aunque al decrecer el ACF podemos deducir que quizá no sea estacionaria
#hacemos una visualización de los residuos

urkpssTest(distrito_14_ts, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)


#ejecutamos el kpss test
data_kpss <- kpss.test(distrito_14_ts)

print(data_kpss)

#En el KPSS los valores pequeños de p indican que quizá se debería hacer una diferenciación.

#buscamos el orden de diferenciación para hacer estacionaria la serie temporal.Utilizamos ndiffs


orden_dif_14 <- ndiffs(distrito_14_ts)

#el orden de diferenciación en este caso es de 1
diff_distrito_14 <- diff(distrito_14_ts, orden_dif_14)
plot(diff_distrito_14, ylab =" Orden de diferenciación")


tsdisplay(diff_distrito_14, main="Primer orden diferenciación")

#con ACF vemos que ninguna autocorrelación está fuera del límite del 95%

#Prueba Dic5key-Fuller (ADF)
adf.test(distrito_14_ts, alternative = "stationary")

#Tras realizar la prueba aumentada de Dickey-Fuller (ADF), obtenemos un p-valor = 0.99. 
#Como el valor es mayor que 0.05 decimos que la serie temporal no es estacionaria y no rechazamos la hipótesis nula


# Ahora vamos a realizar el autorima 


#para comprobar que la serie es realmente estacionaria tenemos que hacer otra vez el test adf

adf.test(diff_distrito_14, alternative = "stationary")

# El p-value apenas baja por lo que la serie no es estacionaria aun



Acf(diff_distrito_14, main='ACF para serie diferenciada')
Pacf(diff_distrito_14, main='PACF para serie diferenciada')


arima_fit_14 <- auto.arima(distrito_14_ts, seasonal = FALSE)
print(arima_fit_14)

#devuelve un best model de ARIMA (0,1,0)

fitARIMA_14 <- arima(distrito_14_ts, order=c(0,1,0),seasonal = list(order = c(1,0,0), period = 12),method="ML")

#best model ARIMA (0,1,0)
#dibujamos la serie temporal con su ARIMA
plot(distrito_14_ts, main="ARIMA(0,1,0)")
lines(fitted(arima_fit_14), col="blue")
#vemos que ajusta más o menos bien

#visualizamos los residuos para ARIMA (0,1,0)
checkresiduals(fitARIMA_14)


coeftest(fitARIMA_14)

confint(fitARIMA_14)

predict(fitARIMA_14,n.ahead = 5)

boxresult=LjungBoxTest (fitARIMA_14$residuals,k=2,StartLag=1)
plot(boxresult[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(fitARIMA_14$residuals)
qqline(fitARIMA_14$residuals)
#El Acf de los residuos muestra que no hay autocorrelaciones significativas
#Los valores del p-value en el test de Ljung-Box Q están  casi todos por encima de 0.05 por lo que los datos no son dependientes


futurVal_14 <- forecast(fitARIMA_14,h=5, level=c(99.5))
plot(futurVal_14)



write.table("distrito_14", file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_distritos.xls", append=TRUE)

write.table(futurVal_14, file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_distritos.xls", append=TRUE)

########################################################################################################3

## ARIMA DISTRITO 15_CIUDAD LINEAL

#seleccionamos el distrito 15 para hacer el arima sobre él

distrito_15 <- distritos_group %>% 
  filter(cod_distrito == "15")

#ahora nos quedamos solo con el price_m2 para hacer el arima

colnames(distrito_15) <- c("Distrito", "Year", "Price_m2")  


distrito_15_price <- distrito_15 %>% 
  ungroup() %>% 
  select(Price_m2)

#convertimos la serie en timeseries para poder estimar sobre ella

distrito_15_ts <- ts(distrito_15_price, start = c(2003,1), frequency = 1)

plot(distrito_15_ts)


#intamos ver los componentes de la serie temporal(Observed-trend-seasonal-random)
componentes.ts = decompose(distrito_15_ts)

#da un error porque tengo un número insuficiente de años para cubrir ciclos estacionales completos

#hacemos un tsdisplay para analizar la serie temporal con ACF Y PACF

tsdisplay(distrito_15_ts, lag.max=10)

#la visualización no es suficiente para ver si son estacionarias o no. Aunque al decrecer el ACF podemos deducir que quizá no sea estacionaria
#hacemos una visualización de los residuos

urkpssTest(distrito_15_ts, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)


#ejecutamos el kpss test
data_kpss <- kpss.test(distrito_15_ts)

print(data_kpss)

#En el KPSS los valores pequeños de p indican que quizá se debería hacer una diferenciación.

#buscamos el orden de diferenciación para hacer estacionaria la serie temporal.Utilizamos ndiffs


orden_dif_15 <- ndiffs(distrito_15_ts)

#el orden de diferenciación en este caso es de 1
diff_distrito_15 <- diff(distrito_15_ts, orden_dif_15)
plot(diff_distrito_15, ylab =" Orden de diferenciación")


tsdisplay(diff_distrito_15, main="Primer orden diferenciación")

#con ACF vemos que ninguna autocorrelación está fuera del límite del 95%

#Prueba Dic5key-Fuller (ADF)
adf.test(distrito_15_ts, alternative = "stationary")

#Tras realizar la prueba aumentada de Dickey-Fuller (ADF), obtenemos un p-valor = 0.99. 
#Como el valor es mayor que 0.05 decimos que la serie temporal no es estacionaria y no rechazamos la hipótesis nula


# Ahora vamos a realizar el autorima 


#para comprobar que la serie es realmente estacionaria tenemos que hacer otra vez el test adf

adf.test(diff_distrito_15, alternative = "stationary")

# El p-value apenas baja por lo que la serie no es estacionaria aun



Acf(diff_distrito_15, main='ACF para serie diferenciada')
Pacf(diff_distrito_15, main='PACF para serie diferenciada')


arima_fit_15 <- auto.arima(distrito_15_ts, seasonal = FALSE)
print(arima_fit_15)

#devuelve un best model de ARIMA (0,1,0)

fitARIMA_15 <- arima(distrito_15_ts, order=c(0,1,0),seasonal = list(order = c(1,0,0), period = 12),method="ML")

#best model ARIMA (0,1,0)
#dibujamos la serie temporal con su ARIMA
plot(distrito_15_ts, main="ARIMA(0,1,0)")
lines(fitted(arima_fit_15), col="blue")
#vemos que ajusta más o menos bien

#visualizamos los residuos para ARIMA (0,1,0)
checkresiduals(fitARIMA_15)


coeftest(fitARIMA_15)

confint(fitARIMA_15)

predict(fitARIMA_15,n.ahead = 5)

boxresult=LjungBoxTest (fitARIMA_15$residuals,k=2,StartLag=1)
plot(boxresult[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(fitARIMA_15$residuals)
qqline(fitARIMA_15$residuals)
#El Acf de los residuos muestra que no hay autocorrelaciones significativas
#Los valores del p-value en el test de Ljung-Box Q están  casi todos por encima de 0.05 por lo que los datos no son dependientes


futurVal_15 <- forecast(fitARIMA_15,h=5, level=c(99.5))
plot(futurVal_15)



write.table("distrito_15", file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_distritos.xls", append=TRUE)

write.table(futurVal_15, file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_distritos.xls", append=TRUE)


########################################################################################################3

## ARIMA DISTRITO 16_HORTALEZA

#seleccionamos el distrito 16 para hacer el arima sobre él

distrito_16 <- distritos_group %>% 
  filter(cod_distrito == "16")

#ahora nos quedamos solo con el price_m2 para hacer el arima

colnames(distrito_16) <- c("Distrito", "Year", "Price_m2")  


distrito_16_price <- distrito_16 %>% 
  ungroup() %>% 
  select(Price_m2)

#convertimos la serie en timeseries para poder estimar sobre ella

distrito_16_ts <- ts(distrito_16_price, start = c(2003,1), frequency = 1)

plot(distrito_16_ts)


#intamos ver los componentes de la serie temporal(Observed-trend-seasonal-random)
componentes.ts = decompose(distrito_16_ts)

#da un error porque tengo un número insuficiente de años para cubrir ciclos estacionales completos

#hacemos un tsdisplay para analizar la serie temporal con ACF Y PACF

tsdisplay(distrito_16_ts, lag.max=10)

#la visualización no es suficiente para ver si son estacionarias o no. Aunque al decrecer el ACF podemos deducir que quizá no sea estacionaria
#hacemos una visualización de los residuos

urkpssTest(distrito_16_ts, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)


#ejecutamos el kpss test
data_kpss <- kpss.test(distrito_16_ts)

print(data_kpss)

#En el KPSS los valores pequeños de p indican que quizá se debería hacer una diferenciación.

#buscamos el orden de diferenciación para hacer estacionaria la serie temporal.Utilizamos ndiffs


orden_dif_16 <- ndiffs(distrito_16_ts)

#el orden de diferenciación en este caso es de 1
diff_distrito_16 <- diff(distrito_16_ts, orden_dif_16)
plot(diff_distrito_16, ylab =" Orden de diferenciación")


tsdisplay(diff_distrito_16, main="Primer orden diferenciación")

#con ACF vemos que ninguna autocorrelación está fuera del límite del 95%

#Prueba Dic5key-Fuller (ADF)
adf.test(distrito_16_ts, alternative = "stationary")

#Tras realizar la prueba aumentada de Dickey-Fuller (ADF), obtenemos un p-valor = 0.99. 
#Como el valor es mayor que 0.05 decimos que la serie temporal no es estacionaria y no rechazamos la hipótesis nula


# Ahora vamos a realizar el autorima 


#para comprobar que la serie es realmente estacionaria tenemos que hacer otra vez el test adf

adf.test(diff_distrito_16, alternative = "stationary")

# El p-value apenas baja por lo que la serie no es estacionaria aun



Acf(diff_distrito_16, main='ACF para serie diferenciada')
Pacf(diff_distrito_16, main='PACF para serie diferenciada')


arima_fit_16 <- auto.arima(distrito_16_ts, seasonal = FALSE)
print(arima_fit_16)

#devuelve un best model de ARIMA (0,1,0)

fitARIMA_16 <- arima(distrito_16_ts, order=c(0,1,0),seasonal = list(order = c(1,0,0), period = 12),method="ML")

#best model ARIMA (0,1,0)
#dibujamos la serie temporal con su ARIMA
plot(distrito_16_ts, main="ARIMA(0,1,0)")
lines(fitted(arima_fit_16), col="blue")
#vemos que ajusta más o menos bien

#visualizamos los residuos para ARIMA (0,1,0)
checkresiduals(fitARIMA_16)


coeftest(fitARIMA_16)

confint(fitARIMA_16)

predict(fitARIMA_16,n.ahead = 5)

boxresult=LjungBoxTest (fitARIMA_16$residuals,k=2,StartLag=1)
plot(boxresult[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(fitARIMA_16$residuals)
qqline(fitARIMA_16$residuals)
#El Acf de los residuos muestra que no hay autocorrelaciones significativas
#Los valores del p-value en el test de Ljung-Box Q están  casi todos por encima de 0.05 por lo que los datos no son dependientes


futurVal_16 <- forecast(fitARIMA_16,h=5, level=c(99.5))
plot(futurVal_16)



write.table("distrito_16", file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_distritos.xls", append=TRUE)

write.table(futurVal_16, file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_distritos.xls", append=TRUE)


########################################################################################################3

## ARIMA DISTRITO 17_VILLAVERDE

#seleccionamos el distrito 17 para hacer el arima sobre él

distrito_17 <- distritos_group %>% 
  filter(cod_distrito == "17")

#ahora nos quedamos solo con el price_m2 para hacer el arima

colnames(distrito_17) <- c("Distrito", "Year", "Price_m2")  


distrito_17_price <- distrito_17 %>% 
  ungroup() %>% 
  select(Price_m2)

#convertimos la serie en timeseries para poder estimar sobre ella

distrito_17_ts <- ts(distrito_17_price, start = c(2003,1), frequency = 1)

plot(distrito_17_ts)


#intamos ver los componentes de la serie temporal(Observed-trend-seasonal-random)
componentes.ts = decompose(distrito_17_ts)

#da un error porque tengo un número insuficiente de años para cubrir ciclos estacionales completos

#hacemos un tsdisplay para analizar la serie temporal con ACF Y PACF

tsdisplay(distrito_17_ts, lag.max=10)

#la visualización no es suficiente para ver si son estacionarias o no. Aunque al decrecer el ACF podemos deducir que quizá no sea estacionaria
#hacemos una visualización de los residuos

urkpssTest(distrito_17_ts, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)


#ejecutamos el kpss test
data_kpss <- kpss.test(distrito_17_ts)

print(data_kpss)

#En el KPSS los valores pequeños de p indican que quizá se debería hacer una diferenciación.

#buscamos el orden de diferenciación para hacer estacionaria la serie temporal.Utilizamos ndiffs


orden_dif_17 <- ndiffs(distrito_17_ts)

#el orden de diferenciación en este caso es de 1
diff_distrito_17 <- diff(distrito_17_ts, orden_dif_17)
plot(diff_distrito_17, ylab =" Orden de diferenciación")


tsdisplay(diff_distrito_17, main="Primer orden diferenciación")

#con ACF vemos que ninguna autocorrelación está fuera del límite del 95%

#Prueba Dic5key-Fuller (ADF)
adf.test(distrito_17_ts, alternative = "stationary")

#Tras realizar la prueba aumentada de Dickey-Fuller (ADF), obtenemos un p-valor = 0.99. 
#Como el valor es mayor que 0.05 decimos que la serie temporal no es estacionaria y no rechazamos la hipótesis nula


# Ahora vamos a realizar el autorima 


#para comprobar que la serie es realmente estacionaria tenemos que hacer otra vez el test adf

adf.test(diff_distrito_17, alternative = "stationary")

# El p-value apenas baja por lo que la serie no es estacionaria aun



Acf(diff_distrito_17, main='ACF para serie diferenciada')
Pacf(diff_distrito_17, main='PACF para serie diferenciada')


arima_fit_17 <- auto.arima(distrito_17_ts, seasonal = FALSE)
print(arima_fit_17)

#devuelve un best model de ARIMA (0,1,0)

fitARIMA_17 <- arima(distrito_17_ts, order=c(0,1,0),seasonal = list(order = c(1,0,0), period = 12),method="ML")

#best model ARIMA (0,1,0)
#dibujamos la serie temporal con su ARIMA
plot(distrito_17_ts, main="ARIMA(0,1,0)")
lines(fitted(arima_fit_17), col="blue")
#vemos que ajusta más o menos bien

#visualizamos los residuos para ARIMA (0,1,0)
checkresiduals(fitARIMA_17)


coeftest(fitARIMA_17)

confint(fitARIMA_17)

predict(fitARIMA_17,n.ahead = 5)

boxresult=LjungBoxTest (fitARIMA_17$residuals,k=2,StartLag=1)
plot(boxresult[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(fitARIMA_17$residuals)
qqline(fitARIMA_17$residuals)
#El Acf de los residuos muestra que no hay autocorrelaciones significativas
#Los valores del p-value en el test de Ljung-Box Q están  casi todos por encima de 0.05 por lo que los datos no son dependientes


futurVal_17 <- forecast(fitARIMA_17,h=5, level=c(99.5))
plot(futurVal_17)



write.table("distrito_17", file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_distritos.xls", append=TRUE)

write.table(futurVal_17, file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_distritos.xls", append=TRUE)

########################################################################################################3

## ARIMA DISTRITO 18_VILLA DE VALLECAS

#seleccionamos el distrito 18 para hacer el arima sobre él

distrito_18 <- distritos_group %>% 
  filter(cod_distrito == "18")

#ahora nos quedamos solo con el price_m2 para hacer el arima

colnames(distrito_18) <- c("Distrito", "Year", "Price_m2")  


distrito_18_price <- distrito_18 %>% 
  ungroup() %>% 
  select(Price_m2)

#convertimos la serie en timeseries para poder estimar sobre ella

distrito_18_ts <- ts(distrito_18_price, start = c(2003,1), frequency = 1)

plot(distrito_18_ts)


#intamos ver los componentes de la serie temporal(Observed-trend-seasonal-random)
componentes.ts = decompose(distrito_18_ts)

#da un error porque tengo un número insuficiente de años para cubrir ciclos estacionales completos

#hacemos un tsdisplay para analizar la serie temporal con ACF Y PACF

tsdisplay(distrito_18_ts, lag.max=10)

#la visualización no es suficiente para ver si son estacionarias o no. Aunque al decrecer el ACF podemos deducir que quizá no sea estacionaria
#hacemos una visualización de los residuos

urkpssTest(distrito_18_ts, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)


#ejecutamos el kpss test
data_kpss <- kpss.test(distrito_18_ts)

print(data_kpss)

#En el KPSS los valores pequeños de p indican que quizá se debería hacer una diferenciación.

#buscamos el orden de diferenciación para hacer estacionaria la serie temporal.Utilizamos ndiffs


orden_dif_18 <- ndiffs(distrito_18_ts)

#el orden de diferenciación en este caso es de 1
diff_distrito_18 <- diff(distrito_18_ts, orden_dif_18)
plot(diff_distrito_18, ylab =" Orden de diferenciación")


tsdisplay(diff_distrito_18, main="Primer orden diferenciación")

#con ACF vemos que ninguna autocorrelación está fuera del límite del 95%

#Prueba Dic5key-Fuller (ADF)
adf.test(distrito_18_ts, alternative = "stationary")

#Tras realizar la prueba aumentada de Dickey-Fuller (ADF), obtenemos un p-valor = 0.99. 
#Como el valor es mayor que 0.05 decimos que la serie temporal no es estacionaria y no rechazamos la hipótesis nula


# Ahora vamos a realizar el autorima 


#para comprobar que la serie es realmente estacionaria tenemos que hacer otra vez el test adf

adf.test(diff_distrito_18, alternative = "stationary")

# El p-value apenas baja por lo que la serie no es estacionaria aun



Acf(diff_distrito_18, main='ACF para serie diferenciada')
Pacf(diff_distrito_18, main='PACF para serie diferenciada')


arima_fit_18 <- auto.arima(distrito_18_ts, seasonal = FALSE)
print(arima_fit_18)

#devuelve un best model de ARIMA (0,1,0)

fitARIMA_18 <- arima(distrito_18_ts, order=c(0,1,0),seasonal = list(order = c(1,0,0), period = 12),method="ML")

#best model ARIMA (0,1,0)
#dibujamos la serie temporal con su ARIMA
plot(distrito_18_ts, main="ARIMA(0,1,0)")
lines(fitted(arima_fit_18), col="blue")
#vemos que ajusta más o menos bien

#visualizamos los residuos para ARIMA (0,1,0)
checkresiduals(fitARIMA_18)


coeftest(fitARIMA_18)

confint(fitARIMA_18)

predict(fitARIMA_18,n.ahead = 5)

boxresult=LjungBoxTest (fitARIMA_18$residuals,k=2,StartLag=1)
plot(boxresult[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(fitARIMA_18$residuals)
qqline(fitARIMA_18$residuals)
#El Acf de los residuos muestra que no hay autocorrelaciones significativas
#Los valores del p-value en el test de Ljung-Box Q están  casi todos por encima de 0.05 por lo que los datos no son dependientes


futurVal_18 <- forecast(fitARIMA_18,h=5, level=c(99.5))
plot(futurVal_18)



write.table("distrito_18", file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_distritos.xls", append=TRUE)

write.table(futurVal_18, file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_distritos.xls", append=TRUE)

########################################################################################################3

## ARIMA DISTRITO 19_VICALVARO

#seleccionamos el distrito 19 para hacer el arima sobre él

distrito_19 <- distritos_group %>% 
  filter(cod_distrito == "19")

#ahora nos quedamos solo con el price_m2 para hacer el arima

colnames(distrito_19) <- c("Distrito", "Year", "Price_m2")  


distrito_19_price <- distrito_19 %>% 
  ungroup() %>% 
  select(Price_m2)

#convertimos la serie en timeseries para poder estimar sobre ella

distrito_19_ts <- ts(distrito_19_price, start = c(2003,1), frequency = 1)

plot(distrito_19_ts)


#intamos ver los componentes de la serie temporal(Observed-trend-seasonal-random)
componentes.ts = decompose(distrito_19_ts)

#da un error porque tengo un número insuficiente de años para cubrir ciclos estacionales completos

#hacemos un tsdisplay para analizar la serie temporal con ACF Y PACF

tsdisplay(distrito_19_ts, lag.max=10)

#la visualización no es suficiente para ver si son estacionarias o no. Aunque al decrecer el ACF podemos deducir que quizá no sea estacionaria
#hacemos una visualización de los residuos

urkpssTest(distrito_19_ts, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)


#ejecutamos el kpss test
data_kpss <- kpss.test(distrito_19_ts)

print(data_kpss)

#En el KPSS los valores pequeños de p indican que quizá se debería hacer una diferenciación.

#buscamos el orden de diferenciación para hacer estacionaria la serie temporal.Utilizamos ndiffs


orden_dif_19 <- ndiffs(distrito_19_ts)

#el orden de diferenciación en este caso es de 1
diff_distrito_19 <- diff(distrito_19_ts, orden_dif_19)
plot(diff_distrito_19, ylab =" Orden de diferenciación")


tsdisplay(diff_distrito_19, main="Primer orden diferenciación")

#con ACF vemos que ninguna autocorrelación está fuera del límite del 95%

#Prueba Dic5key-Fuller (ADF)
adf.test(distrito_19_ts, alternative = "stationary")

#Tras realizar la prueba aumentada de Dickey-Fuller (ADF), obtenemos un p-valor = 0.99. 
#Como el valor es mayor que 0.05 decimos que la serie temporal no es estacionaria y no rechazamos la hipótesis nula


# Ahora vamos a realizar el autorima 


#para comprobar que la serie es realmente estacionaria tenemos que hacer otra vez el test adf

adf.test(diff_distrito_19, alternative = "stationary")

# El p-value apenas baja por lo que la serie no es estacionaria aun



Acf(diff_distrito_19, main='ACF para serie diferenciada')
Pacf(diff_distrito_19, main='PACF para serie diferenciada')


arima_fit_19 <- auto.arima(distrito_19_ts, seasonal = FALSE)
print(arima_fit_19)

#devuelve un best model de ARIMA (0,1,0)

fitARIMA_19 <- arima(distrito_19_ts, order=c(0,1,0),seasonal = list(order = c(1,0,0), period = 12),method="ML")

#best model ARIMA (0,1,0)
#dibujamos la serie temporal con su ARIMA
plot(distrito_19_ts, main="ARIMA(0,1,0)")
lines(fitted(arima_fit_19), col="blue")
#vemos que ajusta más o menos bien

#visualizamos los residuos para ARIMA (0,1,0)
checkresiduals(fitARIMA_19)


coeftest(fitARIMA_19)

confint(fitARIMA_19)

predict(fitARIMA_19,n.ahead = 5)

boxresult=LjungBoxTest (fitARIMA_19$residuals,k=2,StartLag=1)
plot(boxresult[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(fitARIMA_19$residuals)
qqline(fitARIMA_19$residuals)
#El Acf de los residuos muestra que no hay autocorrelaciones significativas
#Los valores del p-value en el test de Ljung-Box Q están  casi todos por encima de 0.05 por lo que los datos no son dependientes


futurVal_19 <- forecast(fitARIMA_19,h=5, level=c(99.5))
plot(futurVal_19)



write.table("distrito_19", file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_distritos.xls", append=TRUE)

write.table(futurVal_19, file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_distritos.xls", append=TRUE)

########################################################################################################3

## ARIMA DISTRITO 20_SAN BLAS-CANILLEJAS

#seleccionamos el distrito 20 para hacer el arima sobre él

distrito_20 <- distritos_group %>% 
  filter(cod_distrito == "20")

#ahora nos quedamos solo con el price_m2 para hacer el arima

colnames(distrito_20) <- c("Distrito", "Year", "Price_m2")  


distrito_20_price <- distrito_20 %>% 
  ungroup() %>% 
  select(Price_m2)

#convertimos la serie en timeseries para poder estimar sobre ella

distrito_20_ts <- ts(distrito_20_price, start = c(2003,1), frequency = 1)

plot(distrito_20_ts)


#intamos ver los componentes de la serie temporal(Observed-trend-seasonal-random)
componentes.ts = decompose(distrito_20_ts)

#da un error porque tengo un número insuficiente de años para cubrir ciclos estacionales completos

#hacemos un tsdisplay para analizar la serie temporal con ACF Y PACF

tsdisplay(distrito_20_ts, lag.max=10)

#la visualización no es suficiente para ver si son estacionarias o no. Aunque al decrecer el ACF podemos deducir que quizá no sea estacionaria
#hacemos una visualización de los residuos

urkpssTest(distrito_20_ts, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)


#ejecutamos el kpss test
data_kpss <- kpss.test(distrito_20_ts)

print(data_kpss)

#En el KPSS los valores pequeños de p indican que quizá se debería hacer una diferenciación.

#buscamos el orden de diferenciación para hacer estacionaria la serie temporal.Utilizamos ndiffs


orden_dif_20 <- ndiffs(distrito_20_ts)

#el orden de diferenciación en este caso es de 1
diff_distrito_20 <- diff(distrito_20_ts, orden_dif_20)
plot(diff_distrito_20, ylab =" Orden de diferenciación")


tsdisplay(diff_distrito_20, main="Primer orden diferenciación")

#con ACF vemos que ninguna autocorrelación está fuera del límite del 95%

#Prueba Dic5key-Fuller (ADF)
adf.test(distrito_20_ts, alternative = "stationary")

#Tras realizar la prueba aumentada de Dickey-Fuller (ADF), obtenemos un p-valor = 0.99. 
#Como el valor es mayor que 0.05 decimos que la serie temporal no es estacionaria y no rechazamos la hipótesis nula


# Ahora vamos a realizar el autorima 


#para comprobar que la serie es realmente estacionaria tenemos que hacer otra vez el test adf

adf.test(diff_distrito_20, alternative = "stationary")

# El p-value apenas baja por lo que la serie no es estacionaria aun



Acf(diff_distrito_20, main='ACF para serie diferenciada')
Pacf(diff_distrito_20, main='PACF para serie diferenciada')


arima_fit_20 <- auto.arima(distrito_20_ts, seasonal = FALSE)
print(arima_fit_20)

#devuelve un best model de ARIMA (0,1,0)

fitARIMA_20 <- arima(distrito_20_ts, order=c(0,1,0),seasonal = list(order = c(1,0,0), period = 12),method="ML")

#best model ARIMA (0,1,0)
#dibujamos la serie temporal con su ARIMA
plot(distrito_20_ts, main="ARIMA(0,1,0)")
lines(fitted(arima_fit_20), col="blue")
#vemos que ajusta más o menos bien

#visualizamos los residuos para ARIMA (0,1,0)
checkresiduals(fitARIMA_20)


coeftest(fitARIMA_20)

confint(fitARIMA_20)

predict(fitARIMA_20,n.ahead = 5)

boxresult=LjungBoxTest (fitARIMA_20$residuals,k=2,StartLag=1)
plot(boxresult[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(fitARIMA_20$residuals)
qqline(fitARIMA_20$residuals)
#El Acf de los residuos muestra que no hay autocorrelaciones significativas
#Los valores del p-value en el test de Ljung-Box Q están  casi todos por encima de 0.05 por lo que los datos no son dependientes


futurVal_20 <- forecast(fitARIMA_20,h=5, level=c(99.5))
plot(futurVal_20)



write.table("distrito_20", file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_distritos.xls", append=TRUE)

write.table(futurVal_20, file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_distritos.xls", append=TRUE)

########################################################################################################3

## ARIMA DISTRITO 21_BARAJAS

#seleccionamos el distrito 21 para hacer el arima sobre él

distrito_21 <- distritos_group %>% 
  filter(cod_distrito == "21")

#ahora nos quedamos solo con el price_m2 para hacer el arima

colnames(distrito_21) <- c("Distrito", "Year", "Price_m2")  


distrito_21_price <- distrito_21 %>% 
  ungroup() %>% 
  select(Price_m2)

#convertimos la serie en timeseries para poder estimar sobre ella

distrito_21_ts <- ts(distrito_21_price, start = c(2003,1), frequency = 1)

plot(distrito_21_ts)


#intamos ver los componentes de la serie temporal(Observed-trend-seasonal-random)
componentes.ts = decompose(distrito_21_ts)

#da un error porque tengo un número insuficiente de años para cubrir ciclos estacionales completos

#hacemos un tsdisplay para analizar la serie temporal con ACF Y PACF

tsdisplay(distrito_21_ts, lag.max=10)

#la visualización no es suficiente para ver si son estacionarias o no. Aunque al decrecer el ACF podemos deducir que quizá no sea estacionaria
#hacemos una visualización de los residuos

urkpssTest(distrito_21_ts, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)


#ejecutamos el kpss test
data_kpss <- kpss.test(distrito_21_ts)

print(data_kpss)

#En el KPSS los valores pequeños de p indican que quizá se debería hacer una diferenciación.

#buscamos el orden de diferenciación para hacer estacionaria la serie temporal.Utilizamos ndiffs


orden_dif_21 <- ndiffs(distrito_21_ts)

#el orden de diferenciación en este caso es de 1
diff_distrito_21 <- diff(distrito_21_ts, orden_dif_21)
plot(diff_distrito_21, ylab =" Orden de diferenciación")


tsdisplay(diff_distrito_21, main="Primer orden diferenciación")

#con ACF vemos que ninguna autocorrelación está fuera del límite del 95%

#Prueba Dic5key-Fuller (ADF)
adf.test(distrito_21_ts, alternative = "stationary")

#Tras realizar la prueba aumentada de Dickey-Fuller (ADF), obtenemos un p-valor = 0.99. 
#Como el valor es mayor que 0.05 decimos que la serie temporal no es estacionaria y no rechazamos la hipótesis nula


# Ahora vamos a realizar el autorima 


#para comprobar que la serie es realmente estacionaria tenemos que hacer otra vez el test adf

adf.test(diff_distrito_21, alternative = "stationary")

# El p-value apenas baja por lo que la serie no es estacionaria aun



Acf(diff_distrito_21, main='ACF para serie diferenciada')
Pacf(diff_distrito_21, main='PACF para serie diferenciada')


arima_fit_21 <- auto.arima(distrito_21_ts, seasonal = FALSE)
print(arima_fit_21)

#devuelve un best model de ARIMA (0,1,0)

fitARIMA_21 <- arima(distrito_21_ts, order=c(0,1,0),seasonal = list(order = c(1,0,0), period = 12),method="ML")

#best model ARIMA (0,1,0)
#dibujamos la serie temporal con su ARIMA
plot(distrito_21_ts, main="ARIMA(0,1,0)")
lines(fitted(arima_fit_21), col="blue")
#vemos que ajusta más o menos bien

#visualizamos los residuos para ARIMA (0,1,0)
checkresiduals(fitARIMA_21)


coeftest(fitARIMA_21)

confint(fitARIMA_21)

predict(fitARIMA_21,n.ahead = 5)

boxresult=LjungBoxTest (fitARIMA_21$residuals,k=2,StartLag=1)
plot(boxresult[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(fitARIMA_21$residuals)
qqline(fitARIMA_21$residuals)
#El Acf de los residuos muestra que no hay autocorrelaciones significativas
#Los valores del p-value en el test de Ljung-Box Q están  casi todos por encima de 0.05 por lo que los datos no son dependientes


futurVal_21 <- forecast(fitARIMA_21,h=5, level=c(99.5))
plot(futurVal_21)



write.table("distrito_21", file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_distritos.xls", append=TRUE)

write.table(futurVal_21, file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\prediccion_distritos.xls", append=TRUE)




