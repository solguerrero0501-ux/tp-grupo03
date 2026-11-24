# ---------------------------------------------------------------------------
# 02_descriptivas.R
#
# Estadisticas descriptivas de la base ANTES de cualquier limpieza.
#
# El objetivo de este script es que el lector se haga una idea de los datos
# antes de ver ningun resultado: que hay adentro, como se reparte entre
# cadenas, y como es la distribucion del precio, que es la variable central de
# todo el trabajo.
#
# Las descriptivas de DESPUES del tratamiento estan en 03_faltantes_outliers.R,
# para que la comparacion antes/despues quede en el mismo lugar que la decision
# que la produce.
#
# Entrada : input/base.rds
# Salida  : output/tablas/02_*.csv
#           output/graficos/02_*.png
# ---------------------------------------------------------------------------

library(here)
library(readr)
library(dplyr)
library(ggplot2)

here::i_am("script/02_descriptivas.R")

source(here("utils", "tema_tp.R"))
source(here("utils", "formato_numeros.R"))

base <- readRDS(here("input", "base.rds"))

dir.create(here("output", "tablas"),   showWarnings = FALSE, recursive = TRUE)
dir.create(here("output", "graficos"), showWarnings = FALSE, recursive = TRUE)



# ===========================================================================
# 1. QUE HAY EN LA BASE
# ===========================================================================

cat(strrep("=", 72), "\nLA BASE\n", strrep("=", 72), "\n", sep = "")
cat("unidad de observacion: un producto ofrecido en una sucursal, el 11-09-2026\n\n")

tamano <- data.frame(
  concepto = c("observaciones", "variables", "sucursales", "banderas",
               "empresas", "productos distintos"),
  valor    = c(nrow(base), ncol(base),
               n_distinct(paste(base$id_comercio, base$id_sucursal)),
               n_distinct(base$comercio_bandera_nombre),
               n_distinct(base$comercio_razon_social),
               n_distinct(base$id_producto))
)
print(tamano, row.names = FALSE)
write_csv(tamano, here("output", "tablas", "02_tamano_base.csv"))


# ===========================================================================
# 2. LAS SIETE BANDERAS SON CUATRO EMPRESAS
# ===========================================================================
# Este es el primer dato del trabajo que no se ve mirando la base por encima:
# las banderas que compiten en la gondola no son empresas distintas. Carrefour,
# Market y Maxi son todas INC S.A.; Jumbo y Vea son Cencosud. Importa para todo
# lo que viene, porque si tres de las siete banderas son la misma empresa, la
# mediana entre banderas esta sobrerrepresentando a esa empresa.

por_bandera <- base |>
  group_by(comercio_bandera_nombre, comercio_razon_social) |>
  summarise(sucursales  = n_distinct(paste(id_comercio, id_sucursal)),
            productos   = n_distinct(id_producto),
            obs         = n(),
            precio_mediano = median(productos_precio_lista, na.rm = TRUE),
            .groups = "drop") |>
  arrange(desc(sucursales))

cat("\n", strrep("=", 72), "\nBANDERAS Y EMPRESAS\n", strrep("=", 72), "\n", sep = "")
print(as.data.frame(por_bandera), row.names = FALSE)
write_csv(por_bandera, here("output", "tablas", "02_banderas_empresas.csv"))

cat("\nsucursales por empresa:\n")
print(base |>
        group_by(comercio_razon_social) |>
        summarise(sucursales = n_distinct(paste(id_comercio, id_sucursal))) |>
        arrange(desc(sucursales)) |>
        as.data.frame(), row.names = FALSE)


# --- Grafico: cuantas sucursales tiene cada cadena -------------------------
g_suc <- por_bandera |>
  mutate(cadena  = acortar_bandera(comercio_bandera_nombre),
         empresa = sub(" S\\.A\\..*", "", comercio_razon_social)) |>
  ggplot(aes(x = reorder(cadena, sucursales), y = sucursales)) +
  geom_col(fill = AZUL, width = 0.65) +
  geom_text(aes(label = sucursales), hjust = -0.35, size = 3.2, colour = GRIS_TXT) +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title    = "Coto concentra m\u00e1s de la mitad de los hipermercados de la Ciudad",
    subtitle = envolver(paste("Cantidad de sucursales de cada cadena entre los 45",
                     "hipermercados de la Ciudad de Buenos Aires, 11-09-2026."), ANCHO_BAJADA),
    x        = "Cadena",
    y        = "Sucursales (cantidad)",
    caption  = envolver(paste0(PIE_FUENTE,
                      " Hipermercados de la Ciudad de Buenos Aires, 11 de septiembre de 2026."), ANCHO_PIE)) +
  tema_tp() +
  theme(panel.grid.major.y = element_blank())

guardar_grafico(g_suc, "02_sucursales_por_cadena", ancho = 24, alto = 13)


# ===========================================================================
# 3. LA VARIABLE PRINCIPAL: EL PRECIO DE LISTA
# ===========================================================================

p <- base$productos_precio_lista

descriptivas <- data.frame(
  estadistico = c("observaciones", "media", "desvio estandar", "minimo",
                  "percentil 1", "primer cuartil", "mediana", "tercer cuartil",
                  "percentil 99", "maximo", "asimetria"),
  valor = c(
    sum(!is.na(p)),
    mean(p, na.rm = TRUE),
    sd(p,   na.rm = TRUE),
    min(p,  na.rm = TRUE),
    quantile(p, 0.01, na.rm = TRUE),
    quantile(p, 0.25, na.rm = TRUE),
    median(p, na.rm = TRUE),
    quantile(p, 0.75, na.rm = TRUE),
    quantile(p, 0.99, na.rm = TRUE),
    max(p, na.rm = TRUE),
    mean(((p - mean(p, na.rm = TRUE)) / sd(p, na.rm = TRUE))^3, na.rm = TRUE)
  )
)

cat("\n", strrep("=", 72), "\nPRECIO DE LISTA, ANTES DE LIMPIAR\n", strrep("=", 72), "\n", sep = "")
print(transform(descriptivas, valor = formato_num(valor, 2)), row.names = FALSE)
write_csv(descriptivas, here("output", "tablas", "02_descriptivas_precio.csv"))

cat("\nLa media es", round(mean(p, na.rm = TRUE) / median(p, na.rm = TRUE), 1),
    "veces la mediana y la asimetria es fuertemente positiva.\n")
cat("No es un defecto del dato: es que la variable mezcla un chicle con un\n")
cat("televisor. Esto es lo que vuelve inservible cualquier criterio de outlier\n")
cat("aplicado sobre el precio sin mirar de que producto se trata (ver script 03).\n")


# --- Grafico: la distribucion del precio, en escala logaritmica ------------
# Se usa escala logaritmica y se dice en el titulo del eje. En escala lineal el
# grafico es una sola barra pegada al cero: el 99% de los productos entra en el
# primer 2% del ancho.
g_dist <- base |>
  filter(!is.na(productos_precio_lista), productos_precio_lista > 0) |>
  ggplot(aes(x = productos_precio_lista)) +
  geom_histogram(bins = 60, fill = AZUL, colour = NA) +
  scale_x_log10(labels = function(x) formato_pesos(x),
                breaks = c(100, 1000, 10000, 100000, 1e6)) +
  scale_y_continuous(labels = function(x) formato_num(x)) +
  labs(
    title    = "El precio de un producto va de $0,01 a $13 millones",
    subtitle = envolver(paste("Distribuci\u00f3n del precio de lista de las", formato_num(sum(!is.na(p))),
                     "observaciones de la base, antes de cualquier limpieza."), ANCHO_BAJADA),
    x        = "Precio de lista (pesos, escala logar\u00edtmica)",
    y        = "Observaciones (cantidad)",
    caption  = envolver(paste(PIE_FUENTE,
                     "\nEl eje horizontal est\u00e1 en escala logar\u00edtmica: cada marca vale diez veces la anterior."), ANCHO_PIE)) +
  tema_tp()

guardar_grafico(g_dist, "02_distribucion_precio", ancho = 24, alto = 13)


# ===========================================================================
# 4. COBERTURA: QUE PRODUCTOS SE PUEDEN COMPARAR
# ===========================================================================
# El campo productos_ean no es el codigo de barras: es una bandera 0/1 que dice
# si el id_producto ES un codigo de barras real. Solo esos sirven para comparar
# entre cadenas, porque la descripcion del producto la escribe cada cadena como
# quiere y no hay forma de cruzarla.

cat("\n", strrep("=", 72), "\nCOBERTURA\n", strrep("=", 72), "\n", sep = "")
print(table(base$tipo_codigo))

ean <- filter(base, productos_ean == "1")
en_cuantas <- ean |>
  group_by(id_producto) |>
  summarise(banderas = n_distinct(comercio_bandera_nombre), .groups = "drop")

cobertura <- en_cuantas |>
  count(banderas, name = "productos") |>
  mutate(pct = 100 * productos / sum(productos))

cat("\nen cuantas de las 7 banderas aparece cada producto con codigo de barras:\n")
print(transform(as.data.frame(cobertura), pct = round(pct, 1)), row.names = FALSE)
write_csv(cobertura, here("output", "tablas", "02_cobertura_productos.csv"))

cat("\nSolo", cobertura$productos[cobertura$banderas == 7], "productos estan en las 7 banderas.\n")
cat("Ese es el techo de lo comparable y define la canasta del script 04.\n")


# --- Grafico: la cobertura ------------------------------------------------
g_cob <- cobertura |>
  ggplot(aes(x = factor(banderas), y = productos)) +
  geom_col(aes(fill = banderas == 7), width = 0.7) +
  geom_text(aes(label = formato_num(productos)), vjust = -0.6, size = 3,
            colour = GRIS_TXT) +
  scale_fill_manual(values = c("FALSE" = GRIS, "TRUE" = AZUL)) +
  scale_y_continuous(labels = function(x) formato_num(x),
                     expand = expansion(mult = c(0, 0.14))) +
  labs(
    title    = "Casi ning\u00fan producto se vende en las siete cadenas a la vez",
    subtitle = envolver(paste("Productos con c\u00f3digo de barras real, seg\u00fan en cuántas de las siete",
                     "banderas aparecen.\nLa barra azul es la canasta comparable: los \u00fanicos",
                     "productos que permiten comparar precios entre cadenas."), ANCHO_BAJADA),
    x        = "Cantidad de banderas en las que aparece el producto",
    y        = "Productos (cantidad)",
    caption  = envolver(paste0(PIE_FUENTE,
                      " Hipermercados de la Ciudad de Buenos Aires, 11 de septiembre de 2026."), ANCHO_PIE)) +
  tema_tp()

guardar_grafico(g_cob, "02_cobertura_productos", ancho = 24, alto = 13)


cat("\nlisto. Ya se puede correr 03_faltantes_outliers.R\n")
