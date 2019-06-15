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

La única serie que no ha sido necesario predecir es la de Población.

Una vez realizada la predicción agrupé todas ellas en un fichero pivotado ``` variables_macro.xlsx ``` para su posterior uso en el modelo de predicción de precios de vivienda.

#### Problemas localizados: 

No existe suficiente información retroactiva para una buena predicción y la información no se encuentra actualizada. 
La variable Renta arroja una predicción, bajo mi punto de vista, erróneo y seguramente sea así porque solo disponemos de información de 3 años, a nivel anual, no son datos suficientes para hacer una estimación correcta, por tanto. Por ello, se ha supuesto para 2016-2019 la renta del 2015. Es práctica habitual en una variable como la de la renta que no se puede actualizar, por su complejidad, de forma más habitual.

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

### Paso 3. Modelo de predicción de precios de vivienda con datos originales y con variables macro

Una vez limpio el fichero con la información histórica de las viviendas procedemos a su análisis y modelado.

  ### 3.1 Modelo sin variables macroeconómicas

Como he comentado anteriormente, se ha realizado con Python en archivo ``` 03_Modelo_no_macro.ipynb ``` en carpeta ``` 01_Code ``` del repositorio.

Se utiliza para ello el fichero ``` historico_madrid_limpio.csv ``` que hemos obtenido del script ``` 02_Carga_datos_limpieza.R ```. Se ha hecho llegar una copia a los evaluadores de este fichero(por su importancia) por si existe algún problema en la ejecución del script.

En este script vamos a analizar los datos, a ver la relación que guardan las variables y hacemos la selección más adecuada de features.
Se descartan outliers. En un primer momento pensé en dejarlos (una vez eliminados los verdaderos errores) porque en algunas zonas de Madrid es muy común que convivan viviendas muy diferentes y muy dispares en precios. Y, a nivel distrito, es normal que esto suceda.
No obstante, estaban haciendo poco interpretable la información y finalmente los descarté del perímetro.

  #### Problemas localizados: 

```
- El volumnen de datos de viviendas utilizado no es elevado y puede que no sea suficiente para la predicción de precios. Este es un problema importante al que me enfrento normalmente en mi trabajo puesto que no tenemos quizá el volumen suficiente como para sacar conclusiones veraces de los datos. Y, sobre todo, para poder hacer predicciones lo más ajustadas posible.
```


  ### 3.2 Modelo con variables macroeconómicas

Igual que el anterior, se ha realizado con Python en archivo ``` 04_Modelo_macro.ipynb ``` en carpeta ``` 01_Code ``` del repositorio.

Se utiliza para ello el fichero ``` historico_madrid_limpio.csv ``` que hemos obtenido del script ``` 02_Carga_datos_limpieza.R ```. Se ha hecho llegar una copia a los evaluadores de este fichero(por su importancia) por si existe algún problema en la ejecución del script.

En este script, como complemento del anterior, se ha analizado la incidencia de las variables macroeconómicas en la predicción.

Como se puede observar, de las features seleccionadas anteriormente, solo se han incluido las featuresmacro(renta y compraventas) por su correlación con el precio.

Se ha vuelto a ejecutar el modelo y podemos observar que la predicción mejora. 

Si tenemos en cuenta además que la renta no hemos podido predecirla y que apenas hemos tenido acceso a este nivel geográfico a otras variables podemos deducir el alto grado de implicación que juegan las variables macroeconómicas en la estimación de precios. 

   #### Problemas localizados: 

```
- Debido a que no existe información a nivel distrito de la mayor parte de variables macroeconómicas tuve que descartar variables y utilizar solo datos desde 2013.
- Como no disponía de datos de todas las variables macro antes de 2013 he tenido que utilizar solo los datos de precios de 2013 en adelante, con lo que eso supone al modelo.
```
  ### CONCLUSIONES PASO 3: 
  
  * Las variables macro ayudan en la predicción de precios
  * Intentaré profundizar, a otros niveles geográficos más amplios, donde se dispone de más información, para ver la incidencia real de las variables macroeconómicas en otro estudio.
  

### Paso 4. Predicción a nivel distrito con series temporales

Lo anterior nos da un modelo para predecir el precio de la vivienda, pero lo que realmente quería estudiar en este proyecto era la predicción a nivel distrito y evaluar si efectivamente van a bajar o subir los precios en el corto/medio plazo.

Y comprobar, ya de paso, si es verdad que estamos de nuevo frente a lo que denominan "burbuja inmobiliaria" en el mercado de la vivienda en Madrid.

Inicialmente consideré realizar un ARIMA y de hecho lo hice pero me encontraba con el mismo problema que en las variables macroeconómicas. No tenía una serie temporal con muchos datos a nivel anual (en la mayor parte contaba desde 2003) y si realizaba el estudio a nivel diario o mensual me faltaban muchos datos. Como comprobé ARIMA no funciona demasiado correctamente bajo esas premisas.

Por ello, decidí realizar la predicción de la serie temporal con PROPHET y con los datos diarios, aunque no fueran únicos y no tuviera en todos los casos porque según he leído es robusto frente a la falta de datos, los atípicos y los cambios de tendencia. Pensé que PROPHET se ajustaba perfectamente a mis requerimientos.

Para la estimación he utilizado R, el script ``` 05_Prophet_distritos ``` se encuentra en carpeta ``` 01_Code ``` del repositorio y, necesita para su ejecución el fichero ``` historico_madrid_limpio.csv ``` que hemos obtenido del script ``` 02_Carga_datos_limpieza.R ```
En este script se genera, por un lado, un fichero con la agrupación por año de los datos de 2018 y la media para cada distrito. Se ha generado así para poder visualizarlo en Tableau después y comparar con el año 2019 y 2020 que predecimos en este script.

Se ha generado una predicción para cada uno de los 21 distritos de Madrid para los años 2019, 2020 y 2021. 
Para obtener el mejor lambda he utilizado la función BoxCox que ha ayudado en la predicción. 

A modo de ejemplo podemos ver 













## BIBLIOGRAFÍA

PROPHET: https://facebook.github.io/prophet/










