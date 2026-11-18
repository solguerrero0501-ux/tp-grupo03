# ---------------------------------------------------------------------------
# Tema visual unico para todos los graficos del trabajo, y la funcion que los
# guarda.
#
# Esta en utils/ porque lo usan 02_descriptivas.R, 03_faltantes_outliers.R,
# 05_inferencia.R, 06_concentracion.R y 07_graficos.R. Tener el tema en un solo
# archivo es lo que hace que los doce graficos del trabajo se vean como una
# misma familia y no como doce graficos sueltos.
#
# La paleta es de dos tonos de azul mas tres grises. Es deliberadamente corta:
# el color se usa para distinguir una cosa de otra, no para decorar, y la
# diferencia entre el azul y el gris se mantiene al imprimir en blanco y negro.
# ---------------------------------------------------------------------------

AZUL      <- "#2C4A6B"   # la serie o el valor que se quiere mirar
AZUL_SUAVE<- "#D6DFE8"   # relleno de cajas y barras
GRIS      <- "#BFBFBF"   # el resto de los datos, sin protagonismo
GRIS_TIT  <- "#2B2B2B"   # titulos
GRIS_TXT  <- "#595959"   # bajadas y nombres de ejes
GRIS_SUAVE<- "#8A8A8A"   # notas al pie
GRILLA    <- "#EDEDED"


# Fuente -------------------------------------------------------------------
# Colab arranca sin ninguna fuente proporcional instalada, y en ese caso R cae
# en una monoespaciada que es un 25% mas ancha y desborda todos los textos. Se
# busca una proporcional entre las habituales y, si no hay ninguna, se usa la
# generica. La instalacion de fuentes en Colab esta en 00_instalar_paquetes.R,
# fuera del flujo de analisis.
elegir_fuente <- function() {
  instaladas <- tryCatch(unique(systemfonts::system_fonts()$family),
                         error = function(e) character(0))
  preferidas <- c("DejaVu Sans", "Liberation Sans", "Nimbus Sans",
                  "Arial", "Helvetica")
  hay <- preferidas[preferidas %in% instaladas]
  if (length(hay) > 0) hay[1] else "sans"
}

FUENTE <- elegir_fuente()


# Tema ---------------------------------------------------------------------
tema_tp <- function(base_size = 11) {
  ggplot2::theme_minimal(base_size = base_size, base_family = FUENTE) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(face = "bold", size = base_size * 1.4,
                                               colour = GRIS_TIT, hjust = 0,
                                               margin = ggplot2::margin(b = 6)),
      plot.subtitle    = ggplot2::element_text(size = base_size * 0.95, colour = GRIS_TXT,
                                               hjust = 0, lineheight = 1.25,
                                               margin = ggplot2::margin(b = 12)),
      plot.caption     = ggplot2::element_text(size = base_size * 0.72, colour = GRIS_SUAVE,
                                               hjust = 0, lineheight = 1.3,
                                               margin = ggplot2::margin(t = 12)),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      axis.title       = ggplot2::element_text(size = base_size * 0.9, colour = GRIS_TXT),
      axis.text        = ggplot2::element_text(size = base_size * 0.8, colour = "#4F4F4F"),
      panel.grid.major = ggplot2::element_line(colour = GRILLA, linewidth = 0.35),
      panel.grid.minor = ggplot2::element_blank(),
      strip.text       = ggplot2::element_text(face = "bold", size = base_size * 0.82,
                                               colour = GRIS_TXT, hjust = 0),
      legend.position  = "none",
      plot.margin      = ggplot2::margin(12, 14, 10, 12)
    )
}


# Guardado -----------------------------------------------------------------
# Todos los graficos salen por aca, con ggsave() y en centimetros, para que el
# tamano del texto sea el mismo en todos y para saber de antemano como van a
# entrar en la diapositiva.
guardar_grafico <- function(grafico, archivo, ancho = 22, alto = 13,
                            carpeta = here::here("output", "graficos"),
                            formato = "png") {
  dir.create(carpeta, showWarnings = FALSE, recursive = TRUE)
  ruta <- file.path(carpeta, paste0(archivo, ".", formato))
  ggplot2::ggsave(ruta, grafico, width = ancho, height = alto, units = "cm",
                  dpi = 300, bg = "white")
  message("  grafico guardado: ", ruta)
  invisible(ruta)
}


# Pie de fuente ------------------------------------------------------------
# La consigna pide la fuente del dato al pie de TODOS los graficos. Esta aca
# para escribirla una sola vez y que diga exactamente lo mismo en los diez.
# Las tildes van como escapes \uXXXX y no como caracteres literales para que
# el texto salga bien sin depender de con que encoding lea R el archivo.
PIE_FUENTE <- paste(
  "Fuente: elaboración propia en base al Sistema Electrónico de",
  "Publicidad de Precios Argentinos (SEPA - Precios Claros),",
  "Secretaría de Comercio."
)


# Nombres cortos de las banderas, para que entren en los ejes ---------------
# En la base figuran como razon social o nombre largo. En los graficos se usan
# estos, que son como los conoce cualquiera.
NOMBRE_CORTO <- c(
  "COTO CICSA"             = "Coto",
  "Hipermercado Carrefour" = "Carrefour",
  "HiperChangomas"         = "Changomas",
  "Jumbo"                  = "Jumbo",
  "Market"                 = "Market",
  "Maxi"                   = "Maxi",
  "Vea"                    = "Vea"
)

acortar_bandera <- function(x) {
  corto <- unname(NOMBRE_CORTO[as.character(x)])
  ifelse(is.na(corto), as.character(x), corto)
}
