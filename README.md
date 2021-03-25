# clustering_consumer

La motivación de este proyecto se puede encontrar en la siguiente documentación de notion

El objetivo de la segmentación es distinguir la categoría alta LTV (cantidad total pagada por estudiante) en función de las características sociodemográficas y transaccionales del mismo. Espeta perspectiva con centralidad en el cliente permite en principio entender la relevancia de las variables sociodemográficas en cada categoría transaccional del negocio tanto por los productos como por las facilidades de pago.


Data crossing
Las fuentes de datos principales para la info sociodemográfica y transaccional vienen de SalesForce y moodle bdd. Prácticamente se consumieron estas fuentes a partir de la info exhibida en Quicksight. La fuente de datos con la que se trabaja en adelante se puede ver enseguida: https://drive.google.com/file/d/1xNYFB1IzQdoP2CUbGzXX0BDfwVsWwK97/view?usp=sharing

La variable género se construyó a partir del nombre del estudiante usando la siguiente función de python: https://aprende.atlassian.net/wiki/spaces/~982489695/pages/867172416/Gender+Determination+Improvement

Data wrangling
Para traducir a alto nivel la información por estudiante consideramos las siguientes restricciones:

0. Se acotó a los registros de estudiante generado en el año 2020. 
1. El término de pago preponderante por estudiante.
2. El primer diplomado asociado a cada estudiante.
3. Tener bogo o no según el estudiante haya consumido bogos en alguna de las oportunidades a las que está relacionado en su vida en aprende.

Sobre más información sociodemográfica como la siguiente:
1. Está buscando empleo o no
2. Planes de pago telefónico
3. Intereses profesionales
4. Ingresos mensuales
Por la falta de información (menos del 10% había proveido esta el tiempo del análisis) se opotó por incluir la información para fines estadísticos aunque finalmente no fue concluyente el uso de estas variables en los resultados que mostramos.

Data modeling
Para el modelo de datos, con la técnica PCA concluimos que el 90% de la varianza puede explicarse con las primeras 5 variables. Más aún, dado que categorizamos como alta y baja la clase LTV por estar a la izquierda o derecha del valor promedio, las variables relacionadas con esta categoría concluimos que son país, tener bogo y producto. Dentro de las categorías incluímos la interacción con payment terms y la clase género como subcategoría de la clase país. 
Con lo anterior desarrollamos el algoritmo ML de clusterización y árbol de decisión. 

Para tratar las variables son categóricas (todas) usamos la téncica encoder de sklearn priorizando para reducir el número de variables, en país consideramos US, MX, CO y others como el resto, para productos consideramos los más relevantes por número de estudiantes inscritos.

Insights
Los insights se pueden encontrar en la presentación https://docs.google.com/presentation/d/16ItEk09eKvF99wxHDfKVNyF5a8qwi3x6cbGZhFOMFSk/edit#slide=id.gc712988b44_0_12
