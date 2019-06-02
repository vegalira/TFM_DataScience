library(tidyverse)

prueba_ts_distritos <- read_xlsx("C:\\Users\\Lor\\Desktop\\Master\\TFM\\01_data\\Serie_temporal_Compraventas_Distritos_Ayto.xlsx")
is.data.frame(prueba_ts_distritos)

my_data <- as_tibble(prueba_ts_distritos)

cabeceras <- names(my_data)

for (i in 3:length(cabeceras)){
  
  my_data %>% select(cabeceras[i])
  #print(my_data[i])
  data_ts <- ts(my_data[i], start= c(2007,1), frequency=1)
  data_kpss <- kpss.test(data_ts)
  is.ts(data_ts)
  #print(data_kpss)
  ndiffs(data_ts)
  #ggtsdisplay(diff(data_ts))
  fitARIMA <- arima(data_ts, order=c(1,1,1),seasonal = list(order = c(1,0,0), period = 4),method="ML")
  fitARIMA %>% forecast %>% autoplot
  Box.test(fitARIMA$residuals, type = "Ljung-Box")
  prediccion = forecast(fitARIMA, h=5)
  #print(prediccion)
  #plot(forecast(fitARIMA, h=5), incluide=150)
  write.table(cabeceras[i], file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\prediccion_compraventas.csv", append=TRUE)
  write.table(prediccion, file = "C:\\Users\\Lor\\Desktop\\Master\\TFM\\prediccion_compraventas.csv", append=TRUE)
}
