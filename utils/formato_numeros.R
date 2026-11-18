# ---------------------------------------------------------------------------
# Formato de numeros en castellano, para ejes, tablas y anotaciones.
#
# En Argentina el separador de miles es el punto y el decimal es la coma, al
# reves de lo que hace R por defecto. Como la consigna pide numeros bien
# formateados en todos los graficos y tablas, conviene tenerlo en un solo lugar
# y no repetir el formatC() en cada script.
# ---------------------------------------------------------------------------

# 1234567.8 -> "1.234.568"
formato_num <- function(x, decimales = 0) {
  formatC(x, format = "f", digits = decimales,
          big.mark = ".", decimal.mark = ",")
}

# 1234567.8 -> "$1.234.568"
formato_pesos <- function(x, decimales = 0) {
  paste0("$", formato_num(x, decimales))
}

# 7098477 -> "$7,1 M"   (para ejes, donde el numero entero no entra)
formato_millones <- function(x, decimales = 1) {
  paste0("$", formato_num(x / 1e6, decimales), " M")
}

# 0.173 -> "17,3%"      (recibe la proporcion, no el porcentaje ya hecho)
formato_pct <- function(x, decimales = 1) {
  paste0(formato_num(100 * x, decimales), "%")
}
