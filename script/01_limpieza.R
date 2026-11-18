# ---------------------------------------------------------------------------
# 01_limpieza.R
#
# De los archivos crudos del SEPA a la base de trabajo.
#
# Es el unico script que toca raw/. Descomprime el paquete del 11-09-2026, lee
# los csv, aplica el recorte a hipermercados de CABA, valida ese recorte contra
# las coordenadas, une las tres tablas y guarda el resultado.
#
# Entrada : raw/sepa_liviano_11-09.zip
# Salida  : input/base.rds
#           output/tablas/01_recorte.csv
#           output/tablas/01_sucursales_descartadas.csv
# ---------------------------------------------------------------------------

library(here)
library(readr)
library(dplyr)

here::i_am("script/01_limpieza.R")

source(here("utils", "leer_sepa.R"))

dir.create(here("input"),            showWarnings = FALSE, recursive = TRUE)
dir.create(here("output", "tablas"), showWarnings = FALSE, recursive = TRUE)


# ===========================================================================
# 1. DESCOMPRESION
# ===========================================================================
# El paquete del SEPA es un zip de zips: adentro hay un archivo por comercio y
# cada uno trae comercio.csv, sucursales.csv y productos.csv.
#
# Se descomprime a una carpeta temporal y no al repositorio. raw/ no se
# modifica nunca, que es la regla de la carpeta, y los csv descomprimidos son
# 65 MB que no aportan nada versionados: se regeneran en segundos.

zip_sepa <- here("raw", "sepa_liviano_11-09.zip")
stopifnot(file.exists(zip_sepa))

tmp <- file.path(tempdir(), "sepa")
unlink(tmp, recursive = TRUE)
dir.create(tmp, recursive = TRUE)

unzip(zip_sepa, exdir = tmp)
internos <- list.files(tmp, pattern = "\\.zip$", recursive = TRUE, full.names = TRUE)
cat("comercios en el paquete:", length(internos), "\n")

for (z in internos) {
  # sepa_1_comercio-sepa-12_2026-09-11_09-05-10.zip  ->  12
  id <- sub(".*comercio-sepa-([0-9]+).*", "\\1", basename(z))
  unzip(z, exdir = file.path(tmp, "csv", paste0("comercio_", id)))
}

carpetas <- list.dirs(file.path(tmp, "csv"), recursive = FALSE)
cat("carpetas de comercio:", length(carpetas), "\n")


# ===========================================================================
# 2. COMERCIO Y SUCURSALES
# ===========================================================================
# Estas dos tablas son chicas: se leen enteras, de los 16 comercios.

comercios  <- bind_rows(lapply(carpetas, function(d) leer_sepa(file.path(d, "comercio.csv"))))
sucursales <- bind_rows(lapply(carpetas, function(d) leer_sepa(file.path(d, "sucursales.csv"))))

cat("\n", strrep("=", 72), "\nBASE CRUDA\n", strrep("=", 72), "\n", sep = "")
cat("filas de comercio.csv (una por bandera):", nrow(comercios), "\n")
cat("comercios distintos                    :", n_distinct(comercios$id_comercio), "\n")
cat("sucursales en el pais                  :", nrow(sucursales), "\n")


# ===========================================================================
# 3. EL RECORTE: CABA + HIPERMERCADO
# ===========================================================================
# Dos cosas que no son obvias y que estan documentadas aca porque son los dos
# lugares donde este filtro se rompe en silencio:
#
#   - El campo de provincia NO dice "CABA": dice "AR-C", que es el codigo ISO
#     3166-2 de la Ciudad. Filtrar por "CABA" devuelve cero filas y ningun
#     error.
#   - "Hipermercado" no es una categoria que inventamos nosotros. El Anexo II
#     de la Resolucion 678/2020, que es el documento de especificaciones
#     tecnicas del SEPA y reglamenta la Resolucion 12 del 12 de febrero de 2016
#     de la ex Secretaria de Comercio, clasifica las sucursales por cantidad de
#     lineas de caja:
#         Hipermercado  : mas de 15 cajas
#         Supermercado  : entre 4 y 15 cajas
#         Autoservicio  : entre 1 y 3 cajas
#         Tradicional   : mostrador sin linea de caja
#     Es un criterio oficial de tamano, no una opinion nuestra, y por eso el
#     recorte se puede defender.

cat("\n", strrep("=", 72), "\nRECORTE\n", strrep("=", 72), "\n", sep = "")
cat("sucursales en AR-C            :", sum(sucursales$sucursales_provincia == "AR-C"), "\n")

caba <- sucursales |>
  filter(sucursales_provincia == "AR-C",
         sucursales_tipo       == "Hipermercado") |>
  mutate(lat = suppressWarnings(as.numeric(sucursales_latitud)),
         lon = suppressWarnings(as.numeric(sucursales_longitud)))

cat("de esas, hipermercados        :", nrow(caba), "\n")

# Validacion con la geolocalizacion. Antes de creerle al campo de provincia,
# se chequea que la coordenada caiga dentro del rectangulo que encierra a CABA.
caba <- caba |>
  mutate(en_caba = !is.na(lat) & !is.na(lon) &
                   lat > -34.71 & lat < -34.52 &
                   lon > -58.54 & lon < -58.33)

cat("\nla coordenada cae dentro de CABA:\n")
print(table(caba$en_caba))

descartadas <- caba |>
  filter(!en_caba) |>
  select(id_comercio, id_sucursal, sucursales_nombre, sucursales_localidad, lat, lon)

cat("\nlas que dicen AR-C y estan en otro lado:\n")
print(as.data.frame(descartadas), row.names = FALSE)
# Son todas de la misma bandera. No es ruido al azar: es un error sistematico
# de un comercio al cargar la provincia, y el propio campo de localidad lo
# delata. Esta es la razon por la que el recorte se valida en vez de confiarse.

write_csv(descartadas, here("output", "tablas", "01_sucursales_descartadas.csv"))

sucursales_final <- filter(caba, en_caba)
cat("\nsucursales que quedan         :", nrow(sucursales_final), "\n")

claves_ok        <- paste(sucursales_final$id_comercio, sucursales_final$id_sucursal, sep = "_")
comercios_a_leer <- sort(as.integer(unique(sucursales_final$id_comercio)))
cat("comercios a leer              :", paste(comercios_a_leer, collapse = ", "), "\n")
cat("(de", length(carpetas), "archivos vamos a abrir", length(comercios_a_leer), ")\n")


# ===========================================================================
# 4. PRODUCTOS
# ===========================================================================
# productos.csv es el archivo grande. Se leen solo los comercios que tienen
# alguna sucursal en el recorte, y de esos solo las filas de las sucursales que
# sobrevivieron a la validacion de coordenadas.

productos <- bind_rows(lapply(comercios_a_leer, function(c) {
  p <- leer_sepa(file.path(tmp, "csv", paste0("comercio_", c), "productos.csv"))
  p <- p[paste(p$id_comercio, p$id_sucursal, sep = "_") %in% claves_ok, ]
  cat(sprintf("comercio %-3s -> %7d filas\n", c, nrow(p)))
  p
}))

cat("\n", strrep("=", 72), "\nCLAVE PRIMARIA\n", strrep("=", 72), "\n", sep = "")
clave <- c("id_comercio", "id_sucursal", "id_producto")
cat("filas                                      :", nrow(productos), "\n")
cat("duplicados por comercio+sucursal+producto  :", sum(duplicated(productos[clave])), "\n")
cat("sucursales distintas                       :",
    n_distinct(paste(productos$id_comercio, productos$id_sucursal)), "\n")
cat("productos distintos                        :", n_distinct(productos$id_producto), "\n")
# Si los duplicados son cero, la unidad de observacion queda confirmada y no
# supuesta: una fila es un producto ofrecido en una sucursal, ese dia.


# ===========================================================================
# 5. JOINS, CON SUS CONTROLES
# ===========================================================================
# La regla es que la tabla de la DERECHA tenga la clave unica. Si esta
# repetida, el join multiplica filas y no avisa: la base crece y nadie se da
# cuenta hasta que los promedios dan raro.

cat("\n", strrep("=", 72), "\nCONTROL DE JOIN\n", strrep("=", 72), "\n", sep = "")
cat("clave duplicada en sucursales:",
    sum(duplicated(sucursales_final[c("id_comercio", "id_sucursal")])), "\n")
cat("clave duplicada en comercios :",
    sum(duplicated(comercios[c("id_comercio", "id_bandera")])), "\n")

huerfanos <- productos |>
  distinct(id_comercio, id_sucursal) |>
  anti_join(sucursales_final, by = c("id_comercio", "id_sucursal"))
cat("sucursales de productos sin match:", nrow(huerfanos), "\n")

filas_antes <- nrow(productos)

base <- productos |>
  left_join(sucursales_final |>
              select(id_comercio, id_sucursal, sucursales_nombre, sucursales_tipo,
                     sucursales_barrio, sucursales_localidad, lat, lon),
            by = c("id_comercio", "id_sucursal")) |>
  left_join(comercios |>
              select(id_comercio, id_bandera, comercio_bandera_nombre,
                     comercio_razon_social),
            by = c("id_comercio", "id_bandera"))

cat("filas antes del join:", filas_antes, "| despues:", nrow(base),
    "| iguales:", nrow(base) == filas_antes, "\n")
cat("banderas sin nombre tras el join:", sum(is.na(base$comercio_bandera_nombre)), "\n")


# ===========================================================================
# 6. TIPOS DE DATO
# ===========================================================================
# Se comparan los NA de antes y de despues de convertir. Si la conversion
# generara NA nuevos, significaria que hay valores no numericos escondidos
# (comas decimales, textos, guiones) y habria que mirarlos uno por uno.

numericas <- c("productos_precio_lista", "productos_precio_referencia",
               "productos_cantidad_presentacion", "productos_cantidad_referencia",
               "productos_precio_unitario_promo1", "productos_precio_unitario_promo2")

na_antes <- sum(base$productos_precio_lista == "" | is.na(base$productos_precio_lista))

base <- base |>
  mutate(across(all_of(numericas),
                ~ suppressWarnings(as.numeric(na_if(trimws(.x), "")))))

na_despues <- sum(is.na(base$productos_precio_lista))

cat("\n", strrep("=", 72), "\nCONVERSION\n", strrep("=", 72), "\n", sep = "")
cat("NA en precio de lista antes:", na_antes, "| despues:", na_despues, "\n")

# Los campos de texto vacios y los que traen solo espacios son faltantes. Esto
# importa mas de lo que parece: Coto publica la marca como un espacio en blanco
# en lugar de vacia. Si solo reemplazaramos la cadena vacia, esas filas
# contarian como marca cargada y el analisis de faltantes del script 03 daria
# mal.
base <- base |>
  mutate(across(where(is.character), ~ na_if(trimws(.x), "")))

# El campo productos_ean no es el codigo de barras: es una bandera 0/1 que dice
# si el id_producto ES un codigo de barras real o un codigo interno del
# comercio. Es una trampa del diccionario de datos y define que productos se
# pueden comparar entre cadenas.
base <- base |>
  mutate(tipo_codigo = ifelse(productos_ean == "1", "EAN real", "Codigo interno"))

cat("\ntipo de codigo de producto:\n")
print(table(base$tipo_codigo, useNA = "ifany"))


# ===========================================================================
# 7. GUARDADO
# ===========================================================================

saveRDS(base, here("input", "base.rds"))

resumen <- data.frame(
  concepto = c("filas", "columnas", "sucursales", "banderas", "empresas", "productos"),
  valor    = c(nrow(base), ncol(base),
               n_distinct(paste(base$id_comercio, base$id_sucursal)),
               n_distinct(base$comercio_bandera_nombre),
               n_distinct(base$comercio_razon_social),
               n_distinct(base$id_producto))
)
write_csv(resumen, here("output", "tablas", "01_recorte.csv"))

cat("\n", strrep("=", 72), "\nBASE DE TRABAJO\n", strrep("=", 72), "\n", sep = "")
print(resumen, row.names = FALSE)
cat("\nguardada en input/base.rds\n")
cat("Ya se puede correr 02_descriptivas.R\n")
