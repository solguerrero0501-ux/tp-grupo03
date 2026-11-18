# ---------------------------------------------------------------------------
# Preparacion del entorno. Se corre UNA sola vez, antes del primer script.
#
# Esta separado del flujo de analisis a proposito: los scripts 01 a 07 no
# instalan nada, solo cargan librerias. Asi el analisis se puede correr muchas
# veces sin tocar la instalacion, y la instalacion no queda escondida en el
# medio de un script.
#
# Si ya tenes los paquetes, no hace falta correrlo.
# ---------------------------------------------------------------------------

paquetes <- c(
  "here",         # rutas relativas a la raiz del proyecto
  "readr",        # lectura de los csv del SEPA
  "dplyr",        # manipulacion
  "tidyr",        # pivoteos
  "stringr",      # texto, para clasificar productos por su descripcion
  "stringi",      # normalizacion de tildes en la clasificacion
  "ggplot2",      # graficos
  "scales",       # formato de los ejes
  "jsonlite",     # lectura del geojson de barrios
  "readxl",       # maestro de provincias
  "systemfonts"   # deteccion de la fuente disponible
)

faltan <- setdiff(paquetes, rownames(installed.packages()))

if (length(faltan) > 0) {
  message("instalando: ", paste(faltan, collapse = ", "))
  install.packages(faltan, repos = "https://cloud.r-project.org")
} else {
  message("todos los paquetes ya estaban instalados")
}


# Fuentes -------------------------------------------------------------------
# Solo hace falta en Google Colab, que arranca sin ninguna fuente proporcional
# instalada. Si no hay ninguna, R cae en una monoespaciada que es mas ancha y
# desborda los textos de todos los graficos.
#
# En Windows o Mac este bloque no hace nada: ya hay fuentes proporcionales.

if (dir.exists("/content")) {
  message("entorno Colab detectado, instalando fuentes")
  system("apt-get -qq install -y fonts-dejavu-core fonts-liberation")
}

fuentes <- tryCatch(unique(systemfonts::system_fonts()$family),
                    error = function(e) character(0))
proporcionales <- c("DejaVu Sans", "Liberation Sans", "Nimbus Sans",
                    "Arial", "Helvetica")

if (any(proporcionales %in% fuentes)) {
  message("fuente disponible: ",
          proporcionales[proporcionales %in% fuentes][1])
} else {
  warning("no se encontro una fuente proporcional; los graficos pueden salir ",
          "con texto desbordado. Volve a correr este script.")
}

message("listo, ya se puede correr 01_limpieza.R")
