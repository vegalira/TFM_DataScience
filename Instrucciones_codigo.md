# Instrucciones para ejecutar el código

## Orden de ejecución


### 1.	Ejecutar el script de R 01_Arima_variables_macro.R

En este Script se realizan las predicciones de las series temporales seleccionadas.
Los ficheros de salida se adjuntan, por seguridad, en prevención de posibles problemas de ejecución en la carpeta 02_Output/01_predicciones_macro del repositorio. 
2.	Ejecutar el script de R 02_Carga_datos_limpieza.R
En este Script se realiza la limpieza de los datos utilizados para el modelo. Se utilizará para este script el fichero historico_madrid_anonimizado.xlsx. Al tratarse de información confidencial de la empresa Gesvalt (con quien se ha firmado NDA) la información se remitirá a los evaluadores por los medios indicados a tal efecto. No encontrará por tanto en Github dicha información. En previsión de que no pueda ejecutarse el código de limpieza de datos se proporcionará a los evaluadores el fichero con el output (“historico_madrid_limpio.csv” ) que este script para poder ejecutar el presente script en caso de algún problema con el fichero.

3.	Ejecutar el script de Python 03_Modelo_no_macro.ipynb
En este Script se ha desarrollo un modelo de predicción de precios de vivienda. Se ha tenido en cuenta para ello el fichero Excel “historico_madrid_limpio.csv” que arroja el script mencionado anteriormente
4.	Ejecutar el script de Python 04_Modelo_macro.ipynb
En este Script se ha desarrollo un modelo de predicción de precios de vivienda con las variables macro seleccionadas. Para este script son necesarios los ficheros “historico_madrid_limpio.csv” y “variables_macro.xlsx” que puede encontrarse en la carpeta 00_Data del repositorio.
5.	Ejecutar el script de R 05_Prophet_distritos.R
Este Script necesita para su ejecución el fichero historico_madrid_limpio.csv que hemos obtenido del script 02_Carga_datos_limpieza.R. El output de dicho script predicción_prophet_distritos.csv puede encontrarse en la carpeta 02_Output, por seguridad.

