# TFM_DataScience
# Impacto del ciclo inmobiliario en el precio de la vivienda. Distritos de Madrid

La idea inicial de este proyecto era evaluar el impacto de las variables macroeconómicas en el precio de la vivienda en los distritos de Madrid.

Se trataba de intentar ver la influencia de esas variables en el precio y poder predicir si el precio de la vivienda iba a subir o bajar en función de ellas. De esta manera nos podríamos anticipar ante un cambio de tendencia al alza o la baja, con lo que ello supone.

Como más adelante desarrollaré, la falta de datos a nivel distrito hizo que tuviera que predecir dischas variables. Analizando los resultados de estas predicciones concluí que no eran suficientemente relevantes como para poder realizar el estudio. 

Por ello, no se han tenido en cuenta estas variables en el análisis final de los distritos de Madrid pero sí en el modelo de predicción de precios de vivienda. Profundizaremos más adelante en ello.

## Código

En fichero anexo a este repositorio se adjuntarán las intrucciones para la ejecución del código y el orden que debe seguirse. 

He utilizado dos lenguajes de programación, R y Python. 

```
R:

Para:
- Limpieza de datos del fichero original
- Predicción de las variables macroeconómicas utilizadas en el modelo con ARIMA.
- Predicción de los precios de los distritos (en global) para 2019 y 2020 con PROPHET

```

```
Python:

Para:
- Modelo de predicción de precios de vivienda con los datos originales.
- Modelo de predicción de precios de vivienda con los datos originales y con las variables macroeconómicas

```
## Desarrollo del estudio

### Paso 1. Selección de variables macroeconómicas y predicción.

Tras un análisis exhaustivo de los datos proporcionados por los organismos oficiales a nivel geográfico de distrito, concluí que hay una clara falta de información al respecto y, en la mayor parte de los casos, desactualizada.

Por tanto, la decisión fue utilizar únicamente como variables macroeconómicas las siguientes (todas ellas a nivel distrito):
* Renta: (https://www.madrid.es/portales/munimadrid/es/Inicio/El-Ayuntamiento/Estadistica/Areas-de-informacion-estadistica/Economia/Renta/Renta-neta-media-de-los-hogares-Urban-Audit) - Fuente
* [Compraventas de inmuebles]: (https://www.madrid.es/portales/munimadrid/es/Inicio/El-Ayuntamiento/Estadistica/Distritos-en-cifras/Distritos-en-cifras-Informacion-de-Distritos) - Fuente
* [Número de inmuebles] (parque de vivienda): (https://www.madrid.es/portales/munimadrid/es/Inicio/El-Ayuntamiento/Estadistica/Distritos-en-cifras/Distritos-en-cifras-Informacion-de-Distritos) - Fuente
* [Número de parados] : (https://www.madrid.es/portales/munimadrid/es/Inicio/El-Ayuntamiento/Estadistica/Distritos-en-cifras/Distritos-en-cifras-Informacion-de-Distritos) - Fuente
* [Población] : (https://www.madrid.es/portales/munimadrid/es/Inicio/El-Ayuntamiento/Estadistica/Distritos-en-cifras/Distritos-en-cifras-Informacion-de-Distritos) - Fuente

Como no se disponía de información actualizada en la mayor parte de las variables, decidí predecir las faltantes con ARIMA. Se puede ver el código en el fichero ``` Arima_variables_macro.R ``` en la carpeta ``` 01_Code ``` del repositorio. Las series de las variables analizadas en el script se encuentran en la carpeta ``` 00_Data ```

Una vez realizada la predicción agrupé todas ellas en un fichero pivotado ``` variables_macro.xlsx ``` para su posterior uso en el modelo de predicción de precios de vivienda.

#### Problemas localizados: 

No existe suficiente información retroactiva para una buena predicción y la información no se encuentra actualizada. 

#### Output:

El output generado de este código se adjunta a este repositorio por si no es posible ejecutar correctamente el script. Los ficheros generados son:

``` prediccion_compraventas.xls. ``` ``` prediccion_inmuebles.xls. ``` ``` prediccion_parados.xls. ```
``` prediccion_renta.xls. ```

### Paso 2. Limpieza del fichero de datos de precio de vivienda

El fichero de precios de vivienda ha sido proporcionado por mi empresa (una Sociedad de Tasación) para su estudio con fines académicos.
Este fichero, al ser privado, no se adjunta a este repositorio por tanto. Se hace llegar por mail a los evaluadores.

Dicho fichero, ``` Historico_Madrid_anonimizado.xlsx ```, dispone de datos desde 2002 hasta marzo de 2019 que corresponde a las tasaciones y testigos elaborados por dicha sociedad en los distritos de Madrid.

El volcado de datos, como suele ser habitual, no es muy eficiente y se ha tenido que realizar una labor de limpieza previa a la realización del modelo. 

Las tareas efectuadas, de modo general en el fichero ``` Carga_datos_limpieza.R ``` ubicado en  la carpeta ``` 01_Code ``` del repositorio, han sido las siguientes:

```
- Eliminar variables del volcado que no aportan ninguna información
- Eliminar datos en función de unas determinadas reglas por el conocimiento del negocio y de los datos (Errores de volcado)
- Análisis de missing values y recodificación
```
#### Output: 
El output generado de este código no puede adjuntarse tampoco al repositorio, por lo que se hará llegar a los evaluadores, en prevención de posibles problemas de ejecución del script en el mismo momento que el fichero original.







