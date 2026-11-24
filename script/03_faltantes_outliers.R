# ---------------------------------------------------------------------------
# 03_faltantes_outliers.R
#
# Diagnostico de valores faltantes y de valores atipicos, la decision que se
# toma con ellos, y las descriptivas de antes y de despues.
#
# El punto del script, y lo que conviene defender en la oral, es que aca el
# criterio importa mas que el umbral. Los dos criterios de manual (rango
# intercuartilico y z-score) fallan por la misma razon: miden la dispersion del
# SURTIDO, no la del precio. Un chicle y un televisor estan en la misma
# columna. El criterio que si funciona compara cada precio contra el de ese
# mismo producto en las otras sucursales.
#
# Entrada : input/base.rds
# Salida  : input/base_limpia.rds
#           output/tablas/03_*.csv
#           output/graficos/03_*.png
# ---------------------------------------------------------------------------

library(here)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

here::i_am("script/03_faltantes_outliers.R")

source(here("utils", "tema_tp.R"))
source(here("utils", "formato_numeros.R"))

base <- readRDS(here("input", "base.rds"))
p    <- base$productos_precio_lista



# ===========================================================================
# 1. VALORES FALTANTES
# ===========================================================================
# Se cuentan sobre la base ya normalizada en 01: ahi los textos vacios y los
# que traian solo espacios pasaron a NA. Esto no es un detalle: Coto publica la
# marca como un espacio en blanco, no como vacio, y si no se normaliza esas
# filas cuentan como marca cargada y el diagnostico da mal.

faltantes <- base |>
  summarise(across(everything(), ~ sum(is.na(.x)))) |>
  pivot_longer(everything(), names_to = "variable", values_to = "faltantes") |>
  mutate(pct = 100 * faltantes / nrow(base)) |>
  arrange(desc(faltantes))

cat(strrep("=", 72), "\n1. VALORES FALTANTES\n", strrep("=", 72), "\n", sep = "")
print(as.data.frame(filter(faltantes, faltantes > 0)) |>
        transform(pct = round(pct, 1)), row.names = FALSE)
write_csv(faltantes, here("output", "tablas", "03_faltantes.csv"))

cat("\nvariables sin ningun faltante:", sum(faltantes$faltantes == 0), "de", nrow(faltantes), "\n")
cat("La variable del analisis, productos_precio_lista, tiene",
    faltantes$faltantes[faltantes$variable == "productos_precio_lista"], "faltantes.\n")

# --- Grafico de faltantes -------------------------------------------------
g_falt <- faltantes |>
  filter(faltantes > 0) |>
  mutate(variable = gsub("_", " ", variable)) |>
  ggplot(aes(x = reorder(variable, pct), y = pct)) +
  geom_col(fill = AZUL, width = 0.65) +
  geom_text(aes(label = formato_pct(pct / 100)), hjust = -0.25, size = 3,
            colour = GRIS_TXT) +
  coord_flip(clip = "off") +
  scale_y_continuous(labels = function(x) formato_pct(x / 100),
                     expand = expansion(mult = c(0, 0.16))) +
  labs(
    title    = "Los faltantes se concentran en los campos de promociones",
    subtitle = envolver(paste("Porcentaje de observaciones sin dato, sobre",
                     formato_num(nrow(base)), "observaciones.",
                     "Solo se muestran las variables que tienen al menos un faltante."), ANCHO_BAJADA),
    x        = "Variable",
    y        = "Observaciones sin dato (% del total)",
    caption  = envolver(paste0(PIE_FUENTE,
                      " Hipermercados de la Ciudad de Buenos Aires, 11 de septiembre de 2026."), ANCHO_PIE)) +
  tema_tp() +
  theme(panel.grid.major.y = element_blank())

guardar_grafico(g_falt, "03_faltantes", ancho = 24, alto = 14)


# ===========================================================================
# 2A. PRIMER CRITERIO DE MANUAL: RANGO INTERCUARTILICO
# ===========================================================================

q1  <- quantile(p, 0.25, na.rm = TRUE)
q3  <- quantile(p, 0.75, na.rm = TRUE)
lim_iqr <- q3 + 1.5 * (q3 - q1)
marcadas_iqr <- sum(p > lim_iqr, na.rm = TRUE)

cat("\n", strrep("=", 72), "\n2A. CRITERIO IQR (1,5)\n", strrep("=", 72), "\n", sep = "")
cat("Q1:", formato_pesos(q1), " Q3:", formato_pesos(q3),
    " limite:", formato_pesos(lim_iqr), "\n")
cat("observaciones marcadas como atipicas:", formato_num(marcadas_iqr),
    "(", formato_pct(marcadas_iqr / sum(!is.na(p)), 1), ")\n")
cat("-> marcar uno de cada diez datos no es detectar outliers.\n")

marcados_iqr <- filter(base, productos_precio_lista > lim_iqr)

cat("\nlos productos que mas veces aparecen marcados:\n")
print(marcados_iqr |>
        count(productos_descripcion, sort = TRUE) |>
        head(6) |>
        as.data.frame(), row.names = FALSE)

cat("\nlos mas caros de los marcados:\n")
print(marcados_iqr |>
        slice_max(productos_precio_lista, n = 5) |>
        select(productos_descripcion, productos_precio_lista) |>
        as.data.frame(), row.names = FALSE)

cat("\n-> no hay un solo error de carga en la lista: son vinos, quesos\n")
cat("   especiales, mariscos, bebidas blancas y electrodomesticos, todos a\n")
cat("   precios reales. El criterio no esta detectando precios mal cargados,\n")
cat("   esta detectando que el supermercado vende cosas caras y cosas baratas.\n")


# ===========================================================================
# 2B. SEGUNDO CRITERIO DE MANUAL: Z-SCORE
# ===========================================================================
# Da todavia peor que el IQR, y por un motivo que conviene decir explicito: el
# desvio estandar de una variable tan asimetrica es enorme, asi que el umbral
# se va tan arriba que deja de marcar nada util.

media <- mean(p, na.rm = TRUE)
desvio <- sd(p, na.rm = TRUE)
lim_z <- media + 3 * desvio
marcadas_z <- sum(p > lim_z, na.rm = TRUE)

cat("\n", strrep("=", 72), "\n2B. CRITERIO Z-SCORE (3 desvios)\n", strrep("=", 72), "\n", sep = "")
cat("media:", formato_pesos(media), " desvio:", formato_pesos(desvio),
    " limite:", formato_pesos(lim_z), "\n")
cat("observaciones marcadas:", formato_num(marcadas_z),
    "(", formato_pct(marcadas_z / sum(!is.na(p)), 2), ")\n")
cat("-> el umbral queda por encima de", formato_pesos(lim_z),
    ": no marca errores, marca electrodomesticos caros.\n")

comparacion <- data.frame(
  criterio = c("IQR 1,5", "z-score 3", "ratio contra el mismo producto"),
  umbral   = c(formato_pesos(lim_iqr), formato_pesos(lim_z), "ratio fuera de [1/3, 3]"),
  marcadas = c(marcadas_iqr, marcadas_z, NA)
)


# ===========================================================================
# 2C. EL CRITERIO QUE SI SIRVE: CADA PRECIO CONTRA SU PROPIO PRODUCTO
# ===========================================================================
# En lugar de preguntar "es caro este precio", se pregunta "es caro este precio
# PARA ESTE PRODUCTO". Se compara el precio de cada producto en cada sucursal
# contra la mediana de ese mismo producto en todas las sucursales que lo
# venden. Solo tiene sentido en productos con codigo de barras real y presentes
# en varias sucursales: se pide un minimo de 5.

ean <- filter(base, productos_ean == "1")

n_suc <- ean |>
  group_by(id_producto) |>
  summarise(n_suc = n_distinct(paste(id_comercio, id_sucursal)), .groups = "drop")

comp <- ean |>
  inner_join(n_suc, by = "id_producto") |>
  filter(n_suc >= 5) |>
  group_by(id_producto) |>
  mutate(mediana_prod = median(productos_precio_lista, na.rm = TRUE)) |>
  ungroup() |>
  mutate(ratio = productos_precio_lista / mediana_prod)

cat("\n", strrep("=", 72), "\n2C. CRITERIO POR PRODUCTO\n", strrep("=", 72), "\n", sep = "")
cat("observaciones comparables (producto en 5+ sucursales):", formato_num(nrow(comp)), "\n")
cat("\nratio precio / mediana del producto:\n")
print(round(quantile(comp$ratio, c(0.001, 0.01, 0.25, 0.5, 0.75, 0.99, 0.999),
                     na.rm = TRUE), 3))

sospechosas <- filter(comp, ratio > 3 | ratio < 1 / 3)
cat("\nobservaciones con ratio fuera de [1/3, 3]:", nrow(sospechosas),
    "(", formato_pct(nrow(sospechosas) / nrow(comp), 3), ")\n")

cat("\nlas sospechosas, una por una:\n")
print(sospechosas |>
        mutate(cadena = acortar_bandera(comercio_bandera_nombre)) |>
        select(cadena, productos_descripcion, productos_precio_lista,
               mediana_prod, ratio) |>
        arrange(ratio) |>
        as.data.frame() |>
        transform(ratio = round(ratio, 2)), row.names = FALSE)
cat("-> no son errores de precio: son codigos de barras puestos sobre\n")
cat("   presentaciones distintas (el vaso suelto contra el pack de seis,\n")
cat("   el repuesto contra el envase con dispenser).\n")

write_csv(sospechosas |>
            mutate(cadena = acortar_bandera(comercio_bandera_nombre)) |>
            select(cadena, productos_descripcion, productos_precio_lista,
                   mediana_prod, ratio),
          here("output", "tablas", "03_outliers_por_producto.csv"))


# --- Grafico: los tres criterios, uno al lado del otro --------------------
# Es el grafico que resume la decision: cuantas observaciones marca cada
# criterio. La escala es logaritmica porque van de 6 a 50.000.
g_crit <- data.frame(
  criterio = factor(c("IQR 1,5", "z-score 3 desvios", "Ratio contra\nel mismo producto"),
                    levels = c("IQR 1,5", "z-score 3 desvios",
                               "Ratio contra\nel mismo producto")),
  marcadas = c(marcadas_iqr, marcadas_z, nrow(sospechosas))
) |>
  ggplot(aes(x = criterio, y = marcadas)) +
  geom_col(aes(fill = marcadas == min(marcadas)), width = 0.6) +
  geom_text(aes(label = formato_num(marcadas)), vjust = -0.6, size = 3.4,
            colour = GRIS_TXT) +
  scale_fill_manual(values = c("FALSE" = GRIS, "TRUE" = AZUL)) +
  scale_y_log10(labels = function(x) formato_num(x),
                expand = expansion(mult = c(0, 0.18))) +
  labs(
    title    = "Los criterios de manual marcan el surtido, no los errores",
    subtitle = envolver(paste("Observaciones marcadas como at\u00edpicas por cada criterio, sobre",
                     formato_num(sum(!is.na(p))), "precios.\nEl criterio elegido",
                     "es el tercero: comparar cada precio contra el de ese mismo",
                     "producto\nen las demas sucursales."), ANCHO_BAJADA),
    x        = "Criterio de detecci\u00f3n",
    y        = "Observaciones marcadas (escala logar\u00edtmica)",
    caption  = envolver(paste(PIE_FUENTE,
                     "\nEl eje vertical est\u00e1 en escala logar\u00edtmica porque los tres criterios difieren en varios \u00f3rdenes de magnitud."), ANCHO_PIE)) +
  tema_tp()

guardar_grafico(g_crit, "03_criterios_outliers", ancho = 24, alto = 14)


# ===========================================================================
# 3. LA DECISION
# ===========================================================================
# a) Se eliminan los precios por debajo de $100. En 2026 no hay producto de
#    supermercado a ese precio: son errores de carga evidentes.
# b) Los precios muy altos se MANTIENEN. Son reales, y el analisis posterior
#    trabaja con el precio relativo al mismo producto, que los pone a todos en
#    la misma escala. Eliminarlos seria eliminar la mitad cara del surtido.
# c) Las 6 observaciones del punto 2C tambien se mantienen: son un problema de
#    clasificacion del producto, no del precio, y quedan fuera de la canasta
#    del script 04 por otra via.

error_evidente <- !is.na(p) & p < 100

cat("\n", strrep("=", 72), "\n3. DECISION\n", strrep("=", 72), "\n", sep = "")
cat("a) precios menores a $100 (error de carga evidente):",
    sum(error_evidente), "observaciones -> se eliminan\n")
cat("b) precios altos: se mantienen, son reales\n")
cat("c) las", nrow(sospechosas), "del criterio por producto: se mantienen,",
    "son un problema de clasificacion\n")

base_limpia <- base[!error_evidente, ]
cat("\nbase:", formato_num(nrow(base)), "->", formato_num(nrow(base_limpia)),
    "filas (", formato_pct(1 - nrow(base_limpia) / nrow(base), 3), "menos )\n")


# ===========================================================================
# 4. DESCRIPTIVAS ANTES Y DESPUES
# ===========================================================================
# La consigna pide mostrar el efecto de la decision. Como se eliminan 107
# observaciones sobre medio millon, el efecto tiene que ser practicamente nulo,
# y eso es exactamente lo que hay que mostrar: la limpieza no movio el
# analisis.

resumir <- function(x, etiqueta) {
  data.frame(
    momento     = etiqueta,
    n           = sum(!is.na(x)),
    media       = mean(x, na.rm = TRUE),
    desvio      = sd(x, na.rm = TRUE),
    minimo      = min(x, na.rm = TRUE),
    p25         = quantile(x, 0.25, na.rm = TRUE),
    mediana     = median(x, na.rm = TRUE),
    p75         = quantile(x, 0.75, na.rm = TRUE),
    maximo      = max(x, na.rm = TRUE)
  )
}

antes_despues <- bind_rows(
  resumir(p, "antes de limpiar"),
  resumir(base_limpia$productos_precio_lista, "despues de limpiar")
)

cat("\n", strrep("=", 72), "\n4. DESCRIPTIVAS ANTES Y DESPUES\n", strrep("=", 72), "\n", sep = "")
print(antes_despues |>
        mutate(across(c(media, desvio, minimo, p25, mediana, p75, maximo),
                      ~ formato_num(.x, 1))) |>
        as.data.frame(), row.names = FALSE)
write_csv(antes_despues, here("output", "tablas", "03_antes_despues.csv"))

cambio_mediana <- 100 * (antes_despues$mediana[2] / antes_despues$mediana[1] - 1)
cambio_media   <- 100 * (antes_despues$media[2]   / antes_despues$media[1]   - 1)
cat("\nla mediana cambia", formato_num(cambio_mediana, 3),
    "% y la media", formato_num(cambio_media, 3), "%.\n")
cat("La limpieza saca errores sin mover la distribucion: es lo que se buscaba.\n")


# ===========================================================================
# 5. GUARDADO
# ===========================================================================

saveRDS(base_limpia, here("input", "base_limpia.rds"))

cat("\nguardada en input/base_limpia.rds\n")
cat("Ya se puede correr 04_canasta_indice.R\n")
