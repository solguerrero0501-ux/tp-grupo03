# ¿Importa el barrio? Precios de hipermercados en la Ciudad de Buenos Aires

Trabajo Práctico de Ciencia de Datos para Economía y Negocios — FCE, UBA
Segundo cuatrimestre de 2026. Profesor: Nicolás Sidicaro.

## Integrantes

- Matías Propato
- Jacqueline Quispe
- Sol Trotta Guerrero

## Objetivo e hipótesis

El trabajo mide cuánto cuesta la misma canasta de productos en cada hipermercado
de la Ciudad de Buenos Aires y pregunta **de qué depende esa diferencia**.

Arrancamos con tres hipótesis:

1. **Hay diferencias de precio entre los hipermercados de la Ciudad.** Se
   contrasta comparando el precio de productos idénticos entre sucursales.
2. **Esas diferencias responden a la ubicación:** los barrios de mayor poder
   adquisitivo pagan más caro. Se contrasta cruzando el índice de precios de
   cada sucursal con su barrio y su posición en el eje norte-sur.
3. **Esas diferencias responden a la cadena**, no al lugar. Se contrasta
   comparando la variación del índice entre cadenas contra la variación
   adentro de cada cadena.

**La segunda hipótesis no se confirma y la tercera sí.** Es el resultado que
ordena todo el trabajo, y es un cambio respecto de lo que planteamos en la
instancia 1: entonces la hipótesis geográfica era la principal. La mantuvimos
como hipótesis a contrastar, la testeamos y la rechazamos con los datos, en
lugar de reescribirla después de ver los resultados.

## La base de datos

| | |
|---|---|
| Nombre | Precios Claros — Base SEPA (Sistema Electrónico de Publicidad de Precios Argentinos) |
| Fuente | https://datos.produccion.gob.ar/dataset/sepa-precios |
| Organismo | Subsecretaría de Defensa del Consumidor y Lealtad Comercial |
| Periodicidad | Diaria |
| Período usado | Un día: viernes 11 de septiembre de 2026 |
| Unidad de observación | Un producto ofrecido en una sucursal, ese día |
| Licencia | Creative Commons Attribution 4.0 |

El SEPA obliga a los comercios de cierto tamaño a publicar todos los días los
precios de lista de todos sus productos. El paquete diario completo son unos 12
millones de registros de todo el país.

**Nuestro recorte**: sucursales de la Ciudad de Buenos Aires
(`sucursales_provincia == "AR-C"`, que es el código ISO 3166-2, no el texto
"CABA") y solo del tipo Hipermercado. La categoría no la inventamos nosotros: el
Anexo II de la Resolución 678/2020, que es el documento de especificaciones
técnicas del SEPA y reglamenta la Resolución 12 del 12 de febrero de 2016 de la
ex Secretaría de Comercio, clasifica las sucursales por cantidad de líneas de
caja: hipermercado más de 15 cajas, supermercado entre 4 y 15, autoservicio
entre 1 y 3.

Después del recorte y de validar las coordenadas: **519.126 observaciones, 45
sucursales, 7 banderas, 4 empresas y 56.576 productos**.

### Base auxiliar

- `auxiliar/barrios_caba.geojson`: los polígonos oficiales de los 48 barrios de
  la Ciudad. Se usa porque el campo `sucursales_barrio` del SEPA **viene vacío
  en las 45 sucursales**, así que el barrio hay que construirlo cruzando la
  coordenada de cada local contra los polígonos. El cruce no pierde
  observaciones: las 45 sucursales caen adentro de un barrio.
- `auxiliar/maestro-provincias.xlsx`: el traductor de códigos de provincia que
  publica el propio dataset.
- `auxiliar/canasta_consumo.csv`: los 45 productos con cantidades mensuales que
  componen la canasta de consumo. Lo armamos nosotros eligiendo entre los 1.257
  productos comparables; está documentado en el script 04.

## El análisis

El trabajo construye dos canastas, porque responden preguntas distintas:

- **La canasta comparable**: los **1.257** productos con código de barras real
  que se venden en las siete banderas (el 2,3% de los 54.365 con código de
  barras). No es lo que compra una familia: es el universo de lo que se puede
  comparar entre cadenas, porque la descripción del producto la escribe cada
  cadena como quiere y el código de barras es la única clave que cruza.
- **La canasta de consumo**: **45** productos con cantidades mensuales,
  elegidos de entre esos 1.257. Una compra del mes, que da alrededor de
  $280.000 y que el lector puede juzgar.

Sobre esa base se aplican cuatro métodos además de las descriptivas:

| Método | Unidad | Para qué |
|---|---|---|
| Índice de precios por sucursal | 3 | Convertir 52.732 precios en una medida por sucursal |
| ANOVA, Welch y Kruskal-Wallis | 2 | Testear si las cadenas difieren |
| Tukey | 2 | Ver qué pares de cadenas difieren de verdad |
| HHI y CR | 3 | Medir la concentración y testear si se paga |

## Estructura del repositorio

```
tp-grupo03/
├── raw/              datos crudos del SEPA, no se modifican
├── auxiliar/         bases complementarias y la canasta de consumo
├── input/            datos procesados que generan los scripts
├── output/
│   ├── tablas/       tablas de resultados
│   └── graficos/     visualizaciones
├── script/           los scripts, numerados por orden de ejecución
├── utils/            funciones propias, una por archivo
└── README.md
```

## Cómo reproducir

### Paquetes

`here`, `readr`, `dplyr`, `tidyr`, `stringr`, `stringi`, `ggplot2`, `scales`,
`jsonlite`, `readxl`, `systemfonts`.

Correr una sola vez `script/00_instalar_paquetes.R`, que los instala si faltan.
Está separado del resto a propósito: los scripts 01 a 07 no instalan nada, solo
cargan librerías.

### Orden

Abrir `tp-grupo03.Rproj` y correr los scripts en orden. Cada uno guarda sus
resultados en un archivo y el siguiente los lee de ahí, así que se pueden correr
de a uno y en sesiones distintas.

| Script | Qué hace | Qué genera |
|---|---|---|
| `01_limpieza.R` | Descomprime el paquete del SEPA, lee los csv, aplica el recorte a hipermercados de CABA, valida contra las coordenadas y une las tres tablas | `input/base.rds` |
| `02_descriptivas.R` | Descriptivas antes de limpiar: tamaño, banderas y empresas, distribución del precio, cobertura de productos | `output/tablas/02_*.csv`, 3 gráficos |
| `03_faltantes_outliers.R` | Faltantes por variable, comparación de tres criterios de outlier, decisión y descriptivas antes/después | `input/base_limpia.rds`, `output/tablas/03_*.csv`, 2 gráficos |
| `04_canasta_indice.R` | Las dos canastas, las categorías de producto, el precio de referencia, el índice por sucursal y los barrios | `input/canasta_comparable.rds`, `input/indice_sucursal.rds`, `output/tablas/04_*.csv` |
| `05_inferencia.R` | Supuestos, ANOVA, Welch, Kruskal-Wallis, Tukey y el test de la hipótesis geográfica | `output/tablas/05_*.csv`, 1 gráfico |
| `06_concentracion.R` | HHI y CR de la Ciudad, HHI por barrio, y si la concentración del barrio se paga | `output/tablas/06_*.csv`, 2 gráficos |
| `07_graficos.R` | Las dos visualizaciones de la instancia 3, con las correcciones incorporadas | `output/graficos/grupo_03_exploratorio.jpg` y `grupo_03_comunicativo.jpg` |

### Funciones propias

| Archivo | Función |
|---|---|
| `utils/leer_sepa.R` | Lee un csv del SEPA sorteando sus tres defectos de formato |
| `utils/asignar_barrio.R` | Asigna barrio y comuna a una coordenada, por cruce de rayos contra los polígonos |
| `utils/clasificar_categoria.R` | Clasifica un producto en una de 10 categorías a partir de su descripción |
| `utils/tema_tp.R` | Tema visual común a todos los gráficos y la función que los guarda |
| `utils/formato_numeros.R` | Formato de números en castellano para ejes y tablas |

### Notas

Todas las rutas son relativas, resueltas con `here()`. No hay ningún `setwd()`,
`rm(list = ls())` ni `install.packages()` adentro de los scripts de análisis.
Los csv descomprimidos del SEPA van a una carpeta temporal y no al repositorio:
son 65 MB que se regeneran en segundos.

## Conclusiones principales

**1. Las cadenas difieren y la diferencia es grande.** El índice va de 89,5
(Maxi) a 105,7 (Jumbo). ANOVA F(4, 38) = 3.160,7 con p = 2,3e-47 y un eta
cuadrado de 0,997: el 99,7% de la variabilidad del índice entre sucursales
corresponde a diferencias entre cadenas. Welch y Kruskal-Wallis coinciden, así
que el resultado no depende de los supuestos que fallan. Tukey muestra que
Carrefour, Market y Vea no se distinguen entre sí: son tres banderas con el
mismo precio.

**2. El barrio no explica nada.** El ANOVA del índice por zona da F = 0,57 con
p = 0,57, y la correlación con la latitud es r = 0,02 con p = 0,89. El dato que
lo cierra es otro: Coto tiene 24 sucursales repartidas en 17 barrios distintos y
su índice se mueve 0,18 puntos entre todas ellas. En los 10 barrios donde el
único hipermercado es un Coto, la canasta cuesta exactamente $287.621, y los dos
más alejados de ese grupo están a 13 km uno del otro.

**3. El mercado está muy concentrado y la concentración no se paga.** El HHI por
empresa da 3.847 puntos, bastante arriba del umbral de 2.500 que marca un
mercado altamente concentrado; una sola empresa tiene el 53,3% de las bocas y
dos tienen el 77,8%. En 21 de los 28 barrios con hipermercado hay una sola
empresa. Pero la correlación entre el HHI del barrio y el costo de la canasta
ahí no es significativa (r = 0,23, p = 0,24). Es coherente con lo anterior: si
cada cadena publica una sola lista de precios para toda la Ciudad, el precio de
un barrio no puede responder a la competencia de ese barrio.

**La conclusión del trabajo**: lo que define cuánto gasta un vecino no es cuánta
competencia tiene cerca, sino qué cadena le tocó cerca. La diferencia entre la
canasta más barata y la más cara es del 18,9%, y es una diferencia entre
empresas, no entre barrios.

## Limitaciones

- **Un solo día.** Todo el trabajo es una foto del 11 de septiembre de 2026. No
  se puede decir nada sobre la evolución de los precios.
- **Precios de lista.** La norma no obliga a publicar promociones ni descuentos
  de tarjeta, que son una parte importante de lo que efectivamente paga la
  gente.
- **Ningún producto fresco.** La canasta es de envasados de marca. La carne, la
  fruta y la verdura no tienen código de barras compartido entre cadenas, así
  que no hay forma de compararlas.
- **Comparación por código de barras.** Al cruzar por código de barras se
  compara el mismo producto físico, donde el precio lo pone en buena medida la
  marca. La competencia entre cadenas pasa también por qué marcas tiene cada una
  en góndola, y eso este diseño no lo captura.
- **El HHI se mide en cantidad de sucursales**, no en facturación, que el SEPA
  no publica. Un hipermercado grande y uno chico cuentan igual.
- **Solo hipermercados.** Quedan afuera supermercados y autoservicios, que son
  donde compra buena parte de la gente.
