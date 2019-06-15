install.packages("tidyverse")
install.packages("readxl")
install.packages("tseries")
install.packages("forecast")
library(tidyverse)
library(readxl)
library(tseries)
library(forecast)


############# RENTA#######################################

#cargamos la serie temporal extraida del Ayuntamiento de Madrid

renta <- read_xlsx("C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\01_data\\Serie_temporal_Renta_Distritos_Ayto.xlsx")
renta_tib <- as_tibble(renta)

cabeceras <- names(renta_tib)

#realizamos bucle para hacer el ARIMA en los 21 distritos de Madrid
#esto devuelve un output con la predicción

for (i in 3:length(cabeceras)){
  
  renta_tib %>% select(cabeceras[i])
  #print(my_data[i])
  renta_ts <- ts(renta_tib[i], start= c(2013,1), frequency=1)
  data_kpss <- kpss.test(renta_ts)
  is.ts(renta_ts)
  #print(data_kpss)
  ndiffs(renta_ts)
  #ggtsdisplay(diff(data_ts))
  fitARIMA <- arima(renta_ts, order=c(1,0,0),seasonal = list(order = c(1,0,0), period = 4),method="ML")
  fitARIMA %>% forecast %>% autoplot
  Box.test(fitARIMA$residuals, type = "Ljung-Box")
  prediccion = forecast(fitARIMA, h=5)
  #print(prediccion)
  #plot(forecast(fitARIMA, h=5), incluide=150)
  write.table(cabeceras[i], file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\02_predicciones_macro\\prediccion_renta.xls", append=TRUE)
  write.table(prediccion, file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\02_predicciones_macro\\prediccion_renta.xls", append=TRUE)
}


############# COMPRAVENTAS #######################################

#cargamos la serie temporal extraida del Ayuntamiento de Madrid

compraventas <- read_xlsx("C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\01_data\\Serie_temporal_Compraventas_Distritos_Ayto.xlsx")

compraventas_tib <- as_tibble(compraventas)

cabeceras <- names(compraventas_tib)

#realizamos bucle para hacer el ARIMA en los 21 distritos de Madrid
#esto devuelve un output con la predicción

for (i in 3:length(cabeceras)){
  
  compraventas_tib %>% select(cabeceras[i])
  #print(my_data[i])
  compraventas_ts <- ts(compraventas_tib[i], start= c(2007,1), frequency=1)
  data_kpss <- kpss.test(compraventas_ts)
  is.ts(compraventas_ts)
  #print(data_kpss)
  ndiffs(compraventas_ts)
  #ggtsdisplay(diff(data_ts))
  fitARIMA <- arima(compraventas_ts, order=c(1,1,1),seasonal = list(order = c(1,0,0), period = 4),method="ML")
  fitARIMA %>% forecast %>% autoplot
  Box.test(fitARIMA$residuals, type = "Ljung-Box")
  prediccion = forecast(fitARIMA, h=5)
  #print(prediccion)
  #plot(forecast(fitARIMA, h=5), incluide=150)
  write.table(cabeceras[i], file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\02_predicciones_macro\\prediccion_compraventas.xls", append=TRUE)
  write.table(prediccion, file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\02_predicciones_macro\\prediccion_compraventas.xls", append=TRUE)
}


############# INMUEBLES #######################################

#cargamos la serie temporal extraida del Ayuntamiento de Madrid

inmuebles <- read_xlsx("C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\01_data\\Serie_temporal_Inmuebles_Distritos_Ayto.xlsx")

inmuebles_tib <- as_tibble(inmuebles)

cabeceras <- names(inmuebles_tib)

#realizamos bucle para hacer el ARIMA en los 21 distritos de Madrid
#esto devuelve un output con la predicción


for (i in 3:length(cabeceras)){
  
  inmuebles_tib %>% select(cabeceras[i])
  #print(my_data[i])
  inmuebles_ts <- ts(inmuebles_tib[i], start= c(2013,1), frequency=1)
  data_kpss <- kpss.test(inmuebles_ts)
  is.ts(inmuebles_ts)
  #print(data_kpss)
  ndiffs(inmuebles_ts)
  #ggtsdisplay(diff(data_ts))
  fitARIMA <- arima(inmuebles_ts, order=c(1,0,0),seasonal = list(order = c(1,0,0), period = 4),method="ML")
  fitARIMA %>% forecast %>% autoplot
  Box.test(fitARIMA$residuals, type = "Ljung-Box")
  prediccion = forecast(fitARIMA, h=5)
  #print(prediccion)
  #plot(forecast(fitARIMA, h=5), incluide=150)
  write.table(cabeceras[i], file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\02_predicciones_macro\\prediccion_inmuebles.xls", append=TRUE)
  write.table(prediccion, file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\02_predicciones_macro\\prediccion_inmuebles.xls", append=TRUE)
}



############# NUMERO DE PARADOS #######################################

#cargamos la serie temporal extraida del Ayuntamiento de Madrid

parados <- read_xlsx("C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\01_data\\Serie_temporal_Parados_Distritos_Ayto.xlsx")

parados_tib <- as_tibble(parados)

cabeceras <- names(parados_tib)

#realizamos bucle para hacer el ARIMA en los 21 distritos de Madrid
#esto devuelve un output con la predicción


for (i in 3:length(cabeceras)){
  
  parados_tib %>% select(cabeceras[i])
  #print(my_data[i])
  parados_ts <- ts(parados_tib[i], start= c(2013,1), frequency=1)
  data_kpss <- kpss.test(parados_ts)
  is.ts(parados_ts)
  #print(data_kpss)
  ndiffs(parados_ts)
  #ggtsdisplay(diff(data_ts))
  fitARIMA <- arima(parados_ts, order=c(1,0,0),seasonal = list(order = c(1,0,0), period = 4),method="ML")
  fitARIMA %>% forecast %>% autoplot
  Box.test(fitARIMA$residuals, type = "Ljung-Box")
  prediccion = forecast(fitARIMA, h=5)
  #print(prediccion)
  #plot(forecast(fitARIMA, h=5), incluide=150)
  write.table(cabeceras[i], file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\02_predicciones_macro\\prediccion_parados.xls", append=TRUE)
  write.table(prediccion, file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\02_predicciones_macro\\prediccion_parados.xls", append=TRUE)
}

