# TFM_DataScience
# Impacto del ciclo inmobiliario en el precio de la vivienda. Distritos de Madrid
## Lorena Vega Romero

La idea inicial de este proyecto era evaluar el impacto de las variables macroeconómicas en el precio de la vivienda en los distritos de Madrid.

Se trataba de intentar ver la influencia de esas variables en el precio y poder predicir si el precio de la vivienda iba a subir o bajar en función de ellas. De esta manera nos podríamos anticipar ante un cambio de tendencia al alza o la baja, con lo que ello supone.

Como más adelante desarrollaré, la falta de datos a nivel distrito hizo que tuviera que predecir dichas variables. Analizando los resultados de estas predicciones concluí que no eran suficientemente relevantes como para poder realizar el estudio. 

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
He agruado la información a nivel mensual para facilitar la predicción y para obtener el mejor lambda he utilizado la función BoxCox que ha ayudado en la predicción. 

A modo de ejemplo podemos ver una visualización de la predicción arrojada para el distrito 01_Centro:

### Imagen1. Representación de los datos

![]( https://github.com/vegalira/TFM_DataScience/blob/master/images/distrito_1_1.png)

Podemos observar que hay mucha dispersión en los mismos y eso entiendo va a dificultar la previsión.

### Imagen2. Representación visual de la predicción.

![]( https://github.com/vegalira/TFM_DataScience/blob/master/images/distrito_1_2.png)

Aunque observamos que hay mucha dispersión con respecto a la predicción, si analizamos la línea de predicción en 2019, 2020 y 2021 vemos que se ajusta bastante bien y que sigue en la misma proporción.

### Imagen3. Imagen de gráfico interactivo 

![]( https://github.com/vegalira/TFM_DataScience/blob/master/images/distrito_1_3.png)

He unificado las predicciones de todos los distritos para el 2019 y 2020 en el excel ``` Distritos_madrid_Tableau.xlsx ``` ubicado en carpeta ``` 00_Data ``` junto con las medias de los unitarios a nivel distrito para el 2018 para poder visualizarlo en Tableau.
Además se han incluido en dicho archivo, el valor máximo alcanzado para cada distrito (indicando el año) y la variación del valor actual o predecido con respecto al máximo.

Lo que queremos es comprobar si efectivamente estamos a niveles de máximo (burbuja inmobiliaria) o, si por el contrario, estamos alejados aún.


## Visualización en Tableau

Para la visualización en Tableau necesitaremos los siguientes ficheros:

``` Distritos_madrid_Tableau.xlsx ``` ubicado en carpeta ``` 00_Data ```

De la observación de los datos analizamos lo siguiente:

## ¿Qué distritos verán incrementados sus precios?

Analizando la diferencia entre el valor arrojado en el 2018 para cada distrito y la predicción para el 2019 vemos lo siguiente:

![]( https://github.com/vegalira/TFM_DataScience/blob/master/images/Var_distritos.png)

Como vemos, todos aumentan en un grado medio en la mayor parte de los distritos y en grado máximo en distritos como Villaverde, Puente de Vallecas, Carabanchel y Vicálvaro.

Sólo ve su precio disminuido el distrito de Barajas.

Podríamos pensar que la predicción es errónea, y en cierto modo lo es, en los distritos donde incrementa tanto. Pero, aunque no explica un crecimiento tan exponencial y la realidad del mercado no es tal, es cierto que sí refleja lo que está sucediendo actualmente.

El mercado se encuentra saturado en los distritos más céntricos y más caros tradicionalmente. Esta está provocando un fenómeno de deslocalización hacia distritos e incluso municipios colindantes, provocando un aumento exponencial en los precios de vivienda en estas localizaciones.

Si hacemos el mismo análisis entre 2020-2019

![]( https://github.com/vegalira/TFM_DataScience/blob/master/images/Var_distritos%202020.png)

En este caso ninguno decrece, todos tienen variación positiva y vuelven a repetir en el ranking Villaverde, Puente de Vallecas, Carabanchel y Vicálvaro.

En este caso la predicción parece ser errónea (o eso espero). Un crecimiento de más del 15% de un año para otro no es lo habitual. Si es que la tendencia de estas predicciones se cumplen, todo parece indicar que los precios van a continuar en ascenso pero, previsiblemente, se tendrán que poner en marcha mecanismos de control que consigan regular el mercado o quizá volvamos a sufrir un brusco descenso de los precios que lo equilibren.

No obstante, es cierto que de nuevo, como hemos indicado, los distritos donde hay mayor variación tienen los precios más bajos y tienen, por tanto, mayor recorrido. Quizá, aunque no exacta, no sea tan descabellada esa predicción.


![]( https://github.com/vegalira/TFM_DataScience/blob/master/images/distritos_2018.png)


## ¿Estamos cerca de precios de burbuja inmobiliaria?

Inicalmente las predicciones me parecían demasiado optimistas. Como hemos visto antes, todos los distritos, menos uno en 2019, crecen en precios.

Mi impresión inicial, por mi conocimiento del mercado y porque la percepción generalizada es que los precios están demasiado elevados, era que lo óptimo es que las predicciones indicaran un ajuste a la baja en los precios, en las zonas más saturadas. 

Por ello, decidí analizar como se encuentran los precios en comparación con el precio máximo (normalmente 2006-2008) de la serie histórica de precios de vivienda.


En esta imagen podemos ver que la mayor parte de los distritos se encuentra por debajo de los valores máximos alcanzados. Gran parte de ellos muy alejados aún de los precios de 2006-2008 (-24/-35%). 

Precisamente los distritos más populares y céntricos están por debajo pero a niveles cercanos y, muy destacable y evidente por otro lado, en los distritos de Chamberí y Salamanca que ya sobrepasan los máximos históricos.


![]( https://github.com/vegalira/TFM_DataScience/blob/master/images/2019-Max%20.png)


Si vemos como evolucionarían en 2020, los distritos que estaban próximos a los niveles máximos, ya los superan. Es el caso de Moncloa y Centro. Se mantienen Chamberí y Salamanca y, en general, hay una tendencia alcista como habíamos mencionado.

![]( https://github.com/vegalira/TFM_DataScience/blob/master/images/2020-Max.png)


Esos distritos que tenían unos crecimientos exponenciales en las predicciones de 2019 y 2020 son los que se encuentran aun muy alejados de valores de 2006-2008.

![]( https://github.com/vegalira/TFM_DataScience/blob/master/images/Dashboard%201.png)


 ## CONCLUSIONES
  
 Las predicciones, aunque no exactas por supuesto, reflejan el comportamiento actual del mercado inmobiliario.
 
Tanto la situación actual como las predicciones objeto de este estudio nos indican que de forma general no estamos en niveles de 2006-2008. No obstante, tal y como refleja este análisis, en determinados distritos, sí saltan las alarmas. Hablamos de los distritos más céntricos y más demandados.
 
Tal y como hemos observado, el aumento de los precios se desplaza hacia distritos menos céntricos y menos populares que pueden absorber la demanda creciente, tanto en volumen como en precios.

Tendremos que esperar para ver cómo se comporta el mercado finalmente y si se adoptan medidas para impedir este signo alcista de precios que, al margen de otras consideraciones, ya está a niveles preocupantes en cuanto a esfuerzo teórico de las familias.
 
 







## BIBLIOGRAFÍA

PROPHET: https://facebook.github.io/prophet/
R: http://rpubs.com/
STACKOVERFLOW: https://es.stackoverflow.com/
R y Python: https://www.datascience.com/
.
.
.









