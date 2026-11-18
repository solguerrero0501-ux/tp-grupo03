# ---------------------------------------------------------------------------
# 04_canasta_indice.R
#
# Construye las dos canastas del trabajo y el indice de precios por sucursal.
#
# Son dos canastas distintas porque responden dos preguntas distintas, y
# conviene no mezclarlas:
#
#   1. La canasta COMPARABLE: los 1.257 productos con codigo de barras que se
#      venden en las siete banderas. No es lo que compra una familia: es el
#      universo de lo que se puede comparar entre cadenas. Sirve para medir
#      si una cadena es cara o barata, producto por producto.
#
#   2. La canasta de CONSUMO: 45 productos con cantidades mensuales, elegidos
#      de entre esos 1.257. Sirve para responder cuanto cuesta la compra del
#      mes en cada cadena y en cada barrio, que es la pregunta que se entiende
#      sin explicacion.
#
# Entrada : input/base_limpia.rds
#           auxiliar/canasta_consumo.csv
#           auxiliar/barrios_caba.geojson
# Salida  : input/canasta_comparable.rds
#           input/indice_sucursal.rds
#           output/tablas/04_*.csv
# ---------------------------------------------------------------------------

library(here)
library(readr)
library(dplyr)
library(tidyr)

here::i_am("script/04_canasta_indice.R")

source(here("utils", "clasificar_categoria.R"))
source(here("utils", "asignar_barrio.R"))
source(here("utils", "tema_tp.R"))
source(here("utils", "formato_numeros.R"))

base <- readRDS(here("input", "base_limpia.rds"))
base$suc <- paste(base$id_comercio, base$id_sucursal, sep = "-")


# ===========================================================================
# 1. LA CANASTA COMPARABLE
# ===========================================================================
# Solo productos con codigo de barras real: la descripcion la escribe cada
# cadena como quiere ("Fideos LUCCHETTI Tallarin N5 x500g" contra "FIDEOS
# LUCCHETTI TALLARIN 500G") y no hay forma de cruzarla. El EAN es la unica
# clave que sirve.

ean <- filter(base, productos_ean == "1")

n_banderas <- ean |>
  group_by(id_producto) |>
  summarise(banderas = n_distinct(comercio_bandera_nombre), .groups = "drop")

ids_canasta <- n_banderas$id_producto[n_banderas$banderas == 7]

cat(strrep("=", 72), "\n1. CANASTA COMPARABLE\n", strrep("=", 72), "\n", sep = "")
cat("productos con codigo de barras real :", formato_num(nrow(n_banderas)), "\n")
cat("presentes en las 7 banderas         :", formato_num(length(ids_canasta)),
    "(", formato_pct(length(ids_canasta) / nrow(n_banderas), 1), ")\n")

canasta <- filter(ean, id_producto %in% ids_canasta)

cat("observaciones de la canasta         :", formato_num(nrow(canasta)), "\n")
cat("sucursales                          :", n_distinct(canasta$suc), "\n")

cobertura_suc <- canasta |>
  group_by(suc) |>
  summarise(productos = n_distinct(id_producto), .groups = "drop")
cat("productos de la canasta por sucursal: min", min(cobertura_suc$productos),
    "| mediana", median(cobertura_suc$productos),
    "| max", max(cobertura_suc$productos), "\n")
cat("\nEs poco: el", formato_pct(length(ids_canasta) / nrow(n_banderas), 1),
    "de los productos con codigo de barras.\n")
cat("Pero es lo unico comparable entre las siete cadenas.\n")


# ===========================================================================
# 2. CATEGORIAS
# ===========================================================================
# La base no trae categoria. Se construye con el diccionario de palabras de
# utils/clasificar_categoria.R. Se imprime el resultado para poder revisarlo.

productos <- canasta |>
  distinct(id_producto, .keep_all = TRUE) |>
  select(id_producto, productos_descripcion, productos_marca) |>
  mutate(categoria = clasificar_categoria(productos_descripcion, productos_marca))

cat("\n", strrep("=", 72), "\n2. CATEGORIAS\n", strrep("=", 72), "\n", sep = "")
tabla_cat <- productos |>
  count(categoria, name = "productos") |>
  mutate(pct = round(100 * productos / sum(productos), 1)) |>
  arrange(desc(productos))
print(as.data.frame(tabla_cat), row.names = FALSE)
write_csv(tabla_cat, here("output", "tablas", "04_categorias.csv"))

sin_clasificar <- filter(productos, categoria == "Otros")
cat("\nquedaron sin clasificar:", nrow(sin_clasificar),
    "(", formato_pct(nrow(sin_clasificar) / nrow(productos), 1), ")\n")
cat("los primeros 15:\n")
cat(paste0("   ", substr(head(sin_clasificar$productos_descripcion, 15), 1, 66),
           collapse = "\n"), "\n")

write_csv(productos, here("output", "tablas", "04_categorias_productos.csv"))

canasta <- left_join(canasta, select(productos, id_producto, categoria),
                     by = "id_producto")


# ===========================================================================
# 3. EL PRECIO DE REFERENCIA
# ===========================================================================
# Para decir si una sucursal es cara o barata hay que comparar cada precio
# contra alguna referencia de ESE producto. Como se calcula esa referencia no
# es un detalle tecnico: cambia el resultado.
#
#   a) Mediana simple entre todas las observaciones. NO sirve: Coto tiene 24 de
#      las 45 sucursales, asi que la mediana es practicamente el precio de Coto
#      y el indice de Coto daria 100 por construccion.
#
#   b) Dos etapas con mediana. Primero el precio de cada producto en cada
#      bandera (mediana entre sus sucursales), despues la mediana de esos 7
#      valores. Mejor, pero tiene un problema propio: tres de las siete
#      banderas (Carrefour, Market y Maxi) son la misma empresa, INC S.A., asi
#      que la mediana de los 7 cae casi siempre sobre un precio de INC.
#
#   c) Dos etapas con media. Igual que (b) pero promediando los 7 valores. Cada
#      bandera pesa lo mismo y ninguna queda clavada en la referencia por
#      construccion. Es la que usamos.
#
# El script imprime las tres para que la decision se pueda discutir, y ademas
# imprime el diagnostico que la cierra: con que frecuencia el precio relativo
# da exactamente 1,000, que es la marca de que la referencia ES el precio de
# esa bandera y no una referencia independiente.

precio_bandera <- canasta |>
  group_by(id_producto, comercio_bandera_nombre) |>
  summarise(precio = median(productos_precio_lista, na.rm = TRUE), .groups = "drop")

referencias <- precio_bandera |>
  group_by(id_producto) |>
  summarise(ref_mediana_7 = median(precio),
            ref_media_7   = mean(precio),
            .groups = "drop") |>
  left_join(canasta |>
              group_by(id_producto) |>
              summarise(ref_simple = median(productos_precio_lista, na.rm = TRUE),
                        .groups = "drop"),
            by = "id_producto")

canasta <- canasta |>
  left_join(referencias, by = "id_producto") |>
  mutate(rel_simple    = productos_precio_lista / ref_simple,
         rel_mediana_7 = productos_precio_lista / ref_mediana_7,
         rel           = productos_precio_lista / ref_media_7)   # la que se usa

cat("\n", strrep("=", 72), "\n3. PRECIO DE REFERENCIA\n", strrep("=", 72), "\n", sep = "")
cat("indice por bandera segun como se calcule la referencia (base 100):\n\n")

comparacion <- canasta |>
  group_by(comercio_bandera_nombre) |>
  summarise(`a) mediana simple`  = 100 * mean(rel_simple,    na.rm = TRUE),
            `b) mediana de 7`    = 100 * mean(rel_mediana_7, na.rm = TRUE),
            `c) media de 7`      = 100 * mean(rel,           na.rm = TRUE),
            .groups = "drop") |>
  arrange(`c) media de 7`)

print(comparacion |>
        mutate(across(where(is.numeric), ~ round(.x, 1))) |>
        as.data.frame(), row.names = FALSE)
write_csv(comparacion, here("output", "tablas", "04_comparacion_referencia.csv"))

cat("\n-> con (a) la bandera con mas sucursales queda pegada a 100 por construccion.\n")
cat("-> con (c) las siete banderas pesan lo mismo. Es la que usamos.\n")

# El diagnostico que decide entre (b) y (c): cuantas veces el precio relativo
# da exactamente 1,000. Que de exactamente 1 significa que la referencia no es
# independiente de la bandera, sino que ES su precio.
pegados <- canasta |>
  group_by(cadena = acortar_bandera(comercio_bandera_nombre)) |>
  summarise(`con mediana de 7` = 100 * mean(abs(rel_mediana_7 - 1) < 1e-9, na.rm = TRUE),
            `con media de 7`   = 100 * mean(abs(rel - 1)           < 1e-9, na.rm = TRUE),
            .groups = "drop") |>
  arrange(desc(`con mediana de 7`))

cat("\nporcentaje de observaciones cuyo precio relativo da EXACTAMENTE 1,000:\n\n")
print(pegados |>
        mutate(across(where(is.numeric), ~ round(.x, 1))) |>
        as.data.frame(), row.names = FALSE)
write_csv(pegados, here("output", "tablas", "04_precio_relativo_exacto.csv"))

cat("\n-> esto es lo que descarta (b): con la mediana de 7, dos de cada tres\n")
cat("   precios de Carrefour y de Market caen exactamente sobre la referencia,\n")
cat("   porque tres de las siete banderas son INC S.A. y la mediana cae sobre\n")
cat("   ellas. No se los estaria comparando contra nada. Con la media de 7 eso\n")
cat("   desaparece.\n")


# ===========================================================================
# 4. INDICE POR SUCURSAL
# ===========================================================================
# El indice de una sucursal es el promedio de sus precios relativos por 100.
# Asi cada sucursal es UNA observacion, y los tests del script 05 corren sobre
# 45 valores en lugar de sobre 52.732 precios, que es lo que haria que
# cualquier diferencia diera significativa solo por el tamano de la muestra.

indice <- canasta |>
  group_by(suc, comercio_bandera_nombre, comercio_razon_social,
           sucursales_nombre, lat, lon) |>
  summarise(indice    = 100 * mean(rel, na.rm = TRUE),
            productos = n_distinct(id_producto),
            .groups   = "drop")

cat("\n", strrep("=", 72), "\n4. INDICE POR SUCURSAL\n", strrep("=", 72), "\n", sep = "")
cat("sucursales con indice:", nrow(indice), "\n\n")
print(indice |>
        group_by(cadena = acortar_bandera(comercio_bandera_nombre)) |>
        summarise(sucursales = n(),
                  indice_medio = round(mean(indice), 1),
                  minimo = round(min(indice), 1),
                  maximo = round(max(indice), 1),
                  rango  = round(max(indice) - min(indice), 2)) |>
        arrange(indice_medio) |>
        as.data.frame(), row.names = FALSE)

cat("\nOjo con la ultima columna: es cuanto se mueve el indice ADENTRO de cada\n")
cat("cadena, entre sus propias sucursales. Es casi cero en todas.\n")


# ===========================================================================
# 5. LA CANASTA DE CONSUMO
# ===========================================================================
# 45 productos con cantidades mensuales, elegidos de entre los 1.257
# comparables. Responde a la devolucion de la instancia 3: una canasta de
# 1.257 productos no es lo que compra una familia, y un total de millones de
# pesos no se parece a ninguna compra real.
#
# Limitacion que hay que decir: no hay ni un producto fresco. Ni carne, ni
# fruta, ni verdura, ni pan de panaderia. No es una eleccion nuestra: los
# frescos no tienen codigo de barras compartido entre cadenas, asi que no hay
# forma de compararlos. La canasta es de envasados de marca.

consumo <- read_csv(here("auxiliar", "canasta_consumo.csv"),
                    col_types = cols(id_producto = col_character(),
                                     cantidad    = col_double(),
                                     .default    = col_character()))

cat("\n", strrep("=", 72), "\n5. CANASTA DE CONSUMO\n", strrep("=", 72), "\n", sep = "")
cat("productos:", nrow(consumo), "| unidades totales:", sum(consumo$cantidad), "\n")

# Control: los 45 tienen que estar entre los 1.257 comparables.
fuera <- setdiff(consumo$id_producto, ids_canasta)
cat("productos que no estan en la canasta comparable:", length(fuera), "\n")
stopifnot(length(fuera) == 0)

# Precio de cada producto en cada sucursal, y el mediano de su cadena para
# imputar lo que una sucursal no publica. Sin la imputacion no se podrian
# comparar canastas: cada sucursal estaria comprando una canasta distinta.
precio_suc <- canasta |>
  filter(id_producto %in% consumo$id_producto) |>
  group_by(suc, id_producto) |>
  summarise(precio = median(productos_precio_lista, na.rm = TRUE), .groups = "drop")

mediano_cadena <- canasta |>
  filter(id_producto %in% consumo$id_producto) |>
  group_by(comercio_bandera_nombre, id_producto) |>
  summarise(precio_cadena = median(productos_precio_lista, na.rm = TRUE),
            .groups = "drop")

costo <- indice |>
  select(suc, comercio_bandera_nombre) |>
  tidyr::crossing(consumo) |>
  left_join(precio_suc, by = c("suc", "id_producto")) |>
  left_join(mediano_cadena, by = c("comercio_bandera_nombre", "id_producto")) |>
  mutate(imputado = is.na(precio),
         precio   = ifelse(imputado, precio_cadena, precio),
         gasto    = precio * cantidad)

cat("lineas imputadas:", formato_pct(mean(costo$imputado), 1),
    "del total (", sum(costo$imputado), "de", nrow(costo), ")\n")

canasta_suc <- costo |>
  group_by(suc) |>
  summarise(costo_canasta = sum(gasto),
            imputados     = sum(imputado),
            .groups = "drop")

indice <- left_join(indice, canasta_suc, by = "suc")

cat("\ncosto de la canasta mensual por cadena:\n")
print(indice |>
        group_by(cadena = acortar_bandera(comercio_bandera_nombre)) |>
        summarise(sucursales = n(),
                  costo_medio = round(mean(costo_canasta)),
                  minimo = round(min(costo_canasta)),
                  maximo = round(max(costo_canasta))) |>
        arrange(costo_medio) |>
        as.data.frame(), row.names = FALSE)

brecha <- max(indice$costo_canasta) / min(indice$costo_canasta) - 1
cat("\nmas barata:", formato_pesos(min(indice$costo_canasta)), "\n")
cat("mas cara  :", formato_pesos(max(indice$costo_canasta)), "\n")
cat("brecha    :", formato_pct(brecha, 1), "\n")

reparto <- costo |>
  group_by(rubro) |>
  summarise(gasto = sum(gasto), .groups = "drop") |>
  mutate(pct = round(100 * gasto / sum(gasto), 1)) |>
  arrange(desc(pct))
cat("\npeso de cada rubro en la canasta:\n")
print(as.data.frame(select(reparto, rubro, pct)), row.names = FALSE)
write_csv(reparto, here("output", "tablas", "04_reparto_canasta.csv"))


# ===========================================================================
# 6. BARRIOS
# ===========================================================================
# El campo sucursales_barrio del SEPA viene vacio en las 45 sucursales, asi que
# el barrio hay que construirlo cruzando la coordenada de cada local contra los
# poligonos oficiales de la Ciudad. Ver utils/asignar_barrio.R.

cat("\n", strrep("=", 72), "\n6. BARRIOS\n", strrep("=", 72), "\n", sep = "")
cat("sucursales con el campo sucursales_barrio cargado:",
    sum(!is.na(base$sucursales_barrio)), "de", nrow(base), "\n")

geo <- asignar_barrio(indice$lon, indice$lat, here("auxiliar", "barrios_caba.geojson"))
indice$barrio <- geo$barrio
indice$comuna <- geo$comuna

# El geojson del Gobierno de la Ciudad publica los nombres de barrio sin
# tildes. Como estos nombres se dibujan en los graficos y se imprimen en las
# tablas, se les devuelve la ortografia correcta. Es solo cosmetico: no cambia
# ninguna asignacion ni ningun numero.
CON_TILDE_BARRIO <- c(
  "San Nicolas"      = "San Nicolás",
  "San Cristobal"    = "San Cristóbal",
  "Villa Ortuzar"    = "Villa Ortúzar",
  "Villa Pueyrredon" = "Villa Pueyrredón"
)
corregido <- unname(CON_TILDE_BARRIO[indice$barrio])
indice$barrio <- ifelse(is.na(corregido), indice$barrio, corregido)

cat("sucursales asignadas a un barrio:", sum(!is.na(indice$barrio)), "de", nrow(indice), "\n")
if (any(is.na(indice$barrio))) {
  cat("ATENCION: hay sucursales sin barrio, revisar sus coordenadas:\n")
  print(as.data.frame(indice[is.na(indice$barrio), c("suc", "sucursales_nombre", "lat", "lon")]),
        row.names = FALSE)
}

cat("barrios con al menos un hipermercado:", n_distinct(indice$barrio, na.rm = TRUE),
    "de los 48 de la Ciudad\n")


# ===========================================================================
# 7. GUARDADO
# ===========================================================================

saveRDS(canasta, here("input", "canasta_comparable.rds"))
saveRDS(indice,  here("input", "indice_sucursal.rds"))

write_csv(indice |>
            mutate(cadena = acortar_bandera(comercio_bandera_nombre)) |>
            select(cadena, sucursal = sucursales_nombre, barrio, comuna,
                   indice, costo_canasta, lat, lon) |>
            arrange(costo_canasta),
          here("output", "tablas", "04_indice_sucursal.csv"))

cat("\nguardados: input/canasta_comparable.rds e input/indice_sucursal.rds\n")
cat("Ya se puede correr 05_inferencia.R\n")
