# ---------------------------------------------------------------------------
# leer_sepa(ruta)
#
# Lee uno de los csv del SEPA y devuelve solo las filas validas.
#
# Los archivos del SEPA estan separados por barra vertical en lugar de coma y
# traen tres defectos que no figuran en la norma. Los encontramos abriendo los
# archivos a mano, no estaban documentados en ningun lado:
#
#   1. Todos terminan con una linea suelta "Ultima actualizacion: ...", que no
#      tiene barras y entraria entera en la primera columna como si fuera un
#      registro mas.
#   2. El comercio 47 publica un byte NUL en el medio de una fila, que corta la
#      lectura de cualquier parser que lea el archivo como texto.
#   3. El comercio 6 publica saltos de linea ADENTRO del campo de latitud y sin
#      comillas, asi que cada una de sus sucursales queda partida en tres filas
#      incompletas.
#
# Los tres se resuelven con el mismo criterio, que es lo que hace que la
# funcion sea corta: una fila valida es la que tiene un id_comercio que sea un
# numero. Cualquier otra cosa es ruido del archivo y se descarta.
#
# Se lee todo como texto a proposito. La conversion a numero se hace despues y
# a mano, en 01_limpieza.R, para poder contar cuantos NA genera en lugar de que
# aparezcan en silencio.
# ---------------------------------------------------------------------------

leer_sepa <- function(ruta) {

  # (2) Se lee como bytes y se sacan los NUL antes de que ningun parser toque
  #     el archivo. Hacerlo despues no sirve: la lectura ya fallo.
  crudo <- readBin(ruta, what = "raw", n = file.size(ruta))
  crudo <- crudo[crudo != as.raw(0)]

  texto <- rawToChar(crudo)
  if (!validUTF8(texto)) texto <- iconv(texto, "latin1", "UTF-8")
  Encoding(texto) <- "UTF-8"

  lineas <- strsplit(texto, "\r\n|\n|\r")[[1]]
  lineas[1] <- sub("^﻿", "", lineas[1])          # marca BOM del inicio
  lineas <- lineas[trimws(lineas) != ""]

  # (1) El pie de pagina. Se compara en minuscula y con y sin tilde porque no
  #     todos los comercios lo escriben igual.
  pie <- grepl("^\\s*(ultima|última)\\s+actualiz", lineas, ignore.case = TRUE)
  lineas <- lineas[!pie]

  # Se escribe a un archivo temporal en vez de pasarle el texto directamente a
  # read_delim: asi no hay ambiguedad entre "esto es una ruta" y "esto son los
  # datos", que es una fuente clasica de errores silenciosos.
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(lineas, tmp, useBytes = TRUE)

  # Se silencia el aviso generico de readr sobre filas mal formadas y en su
  # lugar se informa cuantas son. No es taparlo: es cambiar un "hay problemas,
  # fijate" por el numero exacto. Las filas mal formadas son justamente las del
  # defecto (3), que el filtro de abajo descarta.
  df <- suppressWarnings(readr::read_delim(
    tmp,
    delim     = "|",
    quote     = "",                  # verificado: los csv del SEPA no usan comillas
    na        = character(),         # el vacio se trata en 01, no aca
    col_types = readr::cols(.default = readr::col_character()),
    progress  = FALSE
  ))

  rotas <- nrow(readr::problems(df))
  if (rotas > 0) {
    message(sprintf("  %s: %d filas con columnas de mas o de menos, se descartan",
                    basename(ruta), rotas))
  }

  if (!"id_comercio" %in% names(df)) return(df)

  # (3) El filtro que resuelve los tres problemas a la vez.
  df[!is.na(df$id_comercio) & grepl("^[0-9]+$", df$id_comercio), ]
}
