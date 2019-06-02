install.packages("tidyverse")
install.packages("readxl")
install.packages("tseries")
install.packages("forecast")
library(tidyverse)
library(readxl)
library(tseries)
library(forecast)

parados <- read_xlsx("C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\01_data\\01_data_macroeconomia\\estimadas\\Serie_temporal_Parados_Distritos_Ayto.xlsx")
is.data.frame(parados)

parados_tib <- as_tibble(parados)

cabeceras <- names(parados_tib)

for (i in 3:length(cabeceras)){
  
  parados_tib %>% select(cabeceras[i])
  #print(my_data[i])
  parados_ts <- ts(parados_tib[i], start= c(2013,1), frequency=1)
  data_kpss <- kpss.test(parados_ts)
  is.ts(parados_ts)
  #print(data_kpss)
  ndiffs(parados_ts)
  #ggtsdisplay(diff(data_ts))
  fitARIMA <- arima(parados_ts, order=c(1,1,1),seasonal = list(order = c(1,0,0), period = 4),method="ML")
  fitARIMA %>% forecast %>% autoplot
  Box.test(fitARIMA$residuals, type = "Ljung-Box")
  prediccion = forecast(fitARIMA, h=5)
  #print(prediccion)
  #plot(forecast(fitARIMA, h=5), incluide=150)
  write.table(cabeceras[i], file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\02_predicciones_macro\\prediccion_parados.xls", append=TRUE)
  write.table(prediccion, file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\00_Codigo_ok\\03_Ouput\\02_predicciones_macro\\prediccion_parados.xls", append=TRUE)
}
