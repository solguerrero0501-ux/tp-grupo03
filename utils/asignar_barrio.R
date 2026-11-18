# ---------------------------------------------------------------------------
# asignar_barrio(lon, lat, ruta_geojson)
#
# Devuelve el barrio y la comuna de cada punto, cruzando la coordenada de cada
# sucursal contra los poligonos oficiales de los 48 barrios de la Ciudad.
#
# Por que hace falta: el SEPA tiene un campo sucursales_barrio, pero en las 45
# sucursales del recorte viene vacio. El unico dato geografico utilizable es la
# coordenada, asi que el barrio hay que construirlo.
#
# El metodo es el algoritmo de cruce de rayos (ray casting): se traza una
# semirrecta horizontal desde el punto hacia la derecha y se cuentan los lados
# del poligono que cruza. Si el numero de cruces es impar, el punto esta
# adentro; si es par, esta afuera. Esta implementado a mano, con vectores, en
# lugar de usar sf: son 45 puntos contra 48 poligonos, no justifica la
# dependencia y asi queda explicito que hace el cruce.
#
# Devuelve un data.frame con dos columnas, barrio y comuna, en el mismo orden
# que los puntos de entrada. Un punto que no cae en ningun poligono devuelve NA
# a proposito, para que el script que llama lo vea y no lo tape.
# ---------------------------------------------------------------------------

# Test de punto en poligono para un anillo (matriz de n filas x 2 columnas,
# longitud en la primera y latitud en la segunda).
punto_en_anillo <- function(px, py, anillo) {
  n  <- nrow(anillo)
  j  <- c(n, seq_len(n - 1))          # el vertice anterior a cada vertice
  xi <- anillo[, 1]; yi <- anillo[, 2]
  xj <- anillo[j, 1]; yj <- anillo[j, 2]

  # Un lado cruza la semirrecta si sus dos extremos quedan a distinta altura
  # que el punto y la interseccion cae a la derecha. Cuando yi == yj el primer
  # termino ya da FALSE, asi que la division nunca llega a importar.
  cruza <- ((yi > py) != (yj > py)) &
           (px < (xj - xi) * (py - yi) / (yj - yi) + xi)

  sum(cruza, na.rm = TRUE) %% 2 == 1
}


asignar_barrio <- function(lon, lat, ruta_geojson) {

  gj <- jsonlite::fromJSON(ruta_geojson, simplifyVector = FALSE)

  # Cada feature del geojson es un barrio. Nos quedamos con el anillo exterior
  # de cada poligono, que es el primero; los barrios de CABA no tienen huecos.
  barrios <- lapply(gj$features, function(f) {
    anillo <- f$geometry$coordinates[[1]]
    list(
      nombre = f$properties$nombre,
      comuna = f$properties$comuna,
      anillo = matrix(unlist(anillo), ncol = 2, byrow = TRUE)
    )
  })

  nombre <- rep(NA_character_, length(lon))
  comuna <- rep(NA_integer_,   length(lon))

  for (i in seq_along(lon)) {
    if (is.na(lon[i]) || is.na(lat[i])) next
    for (b in barrios) {
      if (punto_en_anillo(lon[i], lat[i], b$anillo)) {
        nombre[i] <- b$nombre
        comuna[i] <- as.integer(b$comuna)
        break
      }
    }
  }

  data.frame(barrio = nombre, comuna = comuna, stringsAsFactors = FALSE)
}
