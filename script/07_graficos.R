# ---------------------------------------------------------------------------
# 07_graficos.R
#
# Las dos visualizaciones de la instancia 3, con las correcciones de la
# devolucion incorporadas.
#
# Que cambio respecto de lo que entregamos:
#
#   Exploratorio (era 7/10)
#     - El eje de las cadenas ya no lleva titulo: los nombres de las cadenas
#       dicen solos que son cadenas.
#     - El eje del precio relativo ahora dice que es, con su unidad.
#     - La bajada ya no repite lo que dicen los ejes: aporta el universo y la
#       clave de lectura de los paneles.
#     - Los limites pasan de [0,75; 1,25] a [0,60; 1,20]. Con los anteriores la
#       caja de Maxi en Mascotas quedaba cortada por el borde, que es lo que
#       marco la devolucion. Con estos no se corta ninguna de las 70 cajas ni
#       ninguno de los 70 bigotes.
#
#   Comunicativo (era 6/10)
#     - Usa la canasta de consumo de 45 productos en lugar de la de 1.257. Una
#       compra mensual de $280.000 es algo que el lector puede juzgar; una de
#       siete millones, no.
#     - Las anotaciones son mas cortas y sin frases de efecto.
#     - La nota al pie es mas corta y dice de entrada cuanto se imputa, que era
#       la objecion metodologica de la devolucion.
#
# Entrada : input/canasta_comparable.rds
#           input/indice_sucursal.rds
# Salida  : output/graficos/grupo_03_exploratorio.jpg
#           output/graficos/grupo_03_comunicativo.jpg
# ---------------------------------------------------------------------------

library(here)
library(dplyr)
library(ggplot2)

here::i_am("script/07_graficos.R")

source(here("utils", "tema_tp.R"))
source(here("utils", "formato_numeros.R"))

canasta <- readRDS(here("input", "canasta_comparable.rds"))
suc     <- readRDS(here("input", "indice_sucursal.rds"))

# Ojo: la variable FUENTE de utils/tema_tp.R es la fuente tipografica.
# Esta es el texto del pie, y por eso se llama distinto.


# ===========================================================================
# GRAFICO 1 - EXPLORATORIO
# ===========================================================================
# Setenta cajas: las siete cadenas en cada una de las diez categorias. Lo que
# hay que poder leer es que el descuento de Maxi no es parejo (es enorme en
# Mascotas y chico en Carnes) mientras las otras seis cadenas casi no se
# mueven de categoria en categoria.

LIMITES <- c(0.60, 1.20)

expl <- canasta |>
  mutate(cadena = acortar_bandera(comercio_bandera_nombre))

# Las cadenas se ordenan por su precio relativo mediano, de la mas barata a la
# mas cara, y no alfabeticamente.
orden_cadena <- expl |>
  group_by(cadena) |>
  summarise(m = median(rel, na.rm = TRUE), .groups = "drop") |>
  arrange(desc(m)) |>
  pull(cadena)

# Los paneles se ordenan por cuanto se diferencian las cadenas adentro de esa
# categoria: primero donde mas se separan, al final donde son casi iguales.
orden_categoria <- expl |>
  group_by(categoria, cadena) |>
  summarise(m = median(rel, na.rm = TRUE), .groups = "drop_last") |>
  summarise(amplitud = max(m) - min(m), .groups = "drop") |>
  arrange(desc(amplitud)) |>
  pull(categoria)

expl <- expl |>
  mutate(cadena    = factor(cadena, levels = orden_cadena),
         categoria = factor(categoria, levels = orden_categoria))

# Control: con estos limites no se tiene que cortar ninguna caja ni ningun
# bigote. Si alguna vez cambia la base, esto avisa en lugar de dibujar mal.
control <- expl |>
  group_by(categoria, cadena) |>
  summarise(q25 = quantile(rel, 0.25, na.rm = TRUE),
            q75 = quantile(rel, 0.75, na.rm = TRUE),
            .groups = "drop") |>
  mutate(ric = q75 - q25,
         bigote_inf = q25 - 1.5 * ric,
         bigote_sup = q75 + 1.5 * ric)

cortadas <- sum(control$q25 < LIMITES[1] | control$q75 > LIMITES[2])
cat("cajas cortadas por los limites del eje:", cortadas, "de", nrow(control), "\n")
if (cortadas > 0) warning("hay cajas fuera de los limites del eje, revisar LIMITES")

g1 <- ggplot(expl, aes(x = rel, y = cadena)) +
  geom_vline(xintercept = 1, linetype = "dashed",
             colour = GRIS_SUAVE, linewidth = 0.4) +
  geom_boxplot(outlier.shape = NA, fill = AZUL_SUAVE, colour = AZUL,
               linewidth = 0.35, width = 0.62) +
  facet_wrap(~ categoria, ncol = 5,
             labeller = labeller(categoria = label_wrap_gen(width = 20))) +
  coord_cartesian(xlim = LIMITES) +
  scale_x_continuous(breaks = c(0.6, 0.8, 1.0, 1.2),
                     labels = function(x) formato_num(x, 1)) +
  labs(
    title    = "Solo una cadena se despega, y no se despega igual en todo",
    subtitle = envolver(paste0("Canasta de 1.257 productos con c\u00f3digo de barras que se venden en las siete cadenas, ",
                      "en los 45 hipermercados\nde la Ciudad de Buenos Aires, el 11 de septiembre de 2026. ",
                      "Los paneles van de la categor\u00eda donde m\u00e1s se separan\nlas cadenas a la categor\u00eda ",
                      "donde menos se separan."), ANCHO_BAJADA),
    x        = "Precio del producto dividido por su precio promedio en las siete cadenas (1,0 = el promedio del mercado)",
    y        = NULL,
    caption  = envolver(paste0(PIE_FUENTE, "\n",
                      "La caja va del primer al tercer cuartil y la l\u00ednea del medio es la mediana; los bigotes llegan hasta 1,5 veces\n",
                      "el rango intercuartil. No se dibujan los valores at\u00edpicos individuales. 52.732 observaciones.\n",
                      "Carrefour, Market y Maxi son banderas de INC S.A.; Jumbo y Vea, de Cencosud S.A."), ANCHO_PIE)) +
  tema_tp(base_size = 10) +
  theme(panel.grid.major.y = element_blank(),
        panel.spacing.x    = unit(0.5, "lines"),
        panel.spacing.y    = unit(1.1, "lines"),
        axis.text.y        = element_text(size = 8.2))

guardar_grafico(g1, "grupo_03_exploratorio", ancho = 25, alto = 17, formato = "jpg")


# ===========================================================================
# GRAFICO 2 - COMUNICATIVO
# ===========================================================================
# Un solo mensaje: el precio lo fija la cadena, no el barrio. La prueba esta a
# la vista en los diez barrios donde el unico hipermercado es un Coto, que
# cuestan todos exactamente lo mismo aunque esten a kilometros unos de otros.

barrios <- suc |>
  mutate(cadena = acortar_bandera(comercio_bandera_nombre)) |>
  group_by(barrio) |>
  summarise(costo   = mean(costo_canasta),
            cadenas = paste(sort(unique(cadena)), collapse = ", "),
            .groups = "drop") |>
  arrange(costo) |>
  mutate(fila     = row_number(),
         etiqueta = paste0(barrio, "  ·  ", cadenas))

puntos <- suc |>
  mutate(cadena = acortar_bandera(comercio_bandera_nombre)) |>
  left_join(select(barrios, barrio, fila), by = "barrio")

# Los barrios donde la unica cadena presente es Coto. Se calculan, no se
# escriben a mano: si cambia la base, la anotacion sigue siendo verdadera.
solo_coto <- barrios |> filter(cadenas == "Coto")
x_coto    <- mean(solo_coto$costo)

# La distancia entre los dos mas alejados de ese grupo, en kilometros. Se usa
# la aproximacion plana: a esta latitud un grado de longitud son unos 92 km y
# uno de latitud unos 111 km. Para 15 km de distancia alcanza y sobra.
coords <- puntos |> filter(barrio %in% solo_coto$barrio) |> distinct(barrio, lat, lon)
distancias <- as.matrix(dist(cbind(coords$lon * 92, coords$lat * 111)))
km_max <- round(max(distancias))

cat("barrios donde la unica cadena es Coto:", nrow(solo_coto),
    "| todos al mismo costo:", length(unique(round(solo_coto$costo))) == 1,
    "| filas contiguas:", all(diff(sort(solo_coto$fila)) == 1),
    "| distancia maxima:", km_max, "km\n")

x_min <- min(puntos$costo_canasta)
x_max <- max(puntos$costo_canasta)
rango <- x_max - x_min

nota_coto <- paste0("En los ", nrow(solo_coto), " barrios donde el único hipermercado es un Coto,\n",
                    "la canasta cuesta lo mismo: ", formato_pesos(x_coto), ".\n",
                    "Coto publica una sola lista para toda la Ciudad, y esos\n",
                    "barrios están hasta a ", km_max, " km unos de otros.")

nota_boedo <- paste0("Boedo es el barrio más barato:\n",
                     "es el único que tiene un Maxi.")

g2 <- ggplot() +
  # el corchete que marca el grupo de barrios solo-Coto
  annotate("segment",
           x = x_coto - 0.012 * rango, xend = x_coto - 0.012 * rango,
           y = min(solo_coto$fila) - 0.4, yend = max(solo_coto$fila) + 0.4,
           colour = GRIS_TXT, linewidth = 0.4) +
  annotate("curve",
           x = x_min + 0.42 * rango, xend = x_coto - 0.025 * rango,
           y = 24.8, yend = mean(solo_coto$fila),
           curvature = 0.25, linewidth = 0.35, colour = GRIS_TXT,
           arrow = arrow(length = unit(0.016, "npc"), type = "closed")) +
  annotate("text", x = x_min, y = 29.0, label = nota_coto,
           hjust = 0, vjust = 1, size = 3.1, colour = GRIS_TIT,
           lineheight = 1.25, family = FUENTE) +

  annotate("curve",
           x = x_min + 0.07 * rango, xend = x_min + 0.004 * rango,
           y = 3.1, yend = 1.15,
           curvature = -0.3, linewidth = 0.35, colour = GRIS_TXT,
           arrow = arrow(length = unit(0.016, "npc"), type = "closed")) +
  annotate("text", x = x_min, y = 6.6, label = nota_boedo,
           hjust = 0, vjust = 1, size = 3.1, colour = GRIS_TIT,
           lineheight = 1.25, family = FUENTE) +

  geom_point(data = puntos, aes(x = costo_canasta, y = fila),
             colour = GRIS, size = 1.9) +
  geom_point(data = barrios, aes(x = costo, y = fila),
             colour = AZUL, size = 2.7) +

  scale_y_continuous(breaks = barrios$fila, labels = barrios$etiqueta,
                     expand = expansion(add = c(0.8, 1.2))) +
  scale_x_continuous(labels = function(x) formato_pesos(x),
                     expand = expansion(mult = 0.035)) +
  labs(
    title    = "El precio de la canasta lo fija la cadena, no el barrio",
    subtitle = envolver(paste0("Cu\u00e1nto cuesta la misma compra mensual de 45 productos en los hipermercados de cada barrio de la\n",
                      "Ciudad de Buenos Aires, el 11 de septiembre de 2026. Cada punto gris es una sucursal; el azul es el\n",
                      "promedio del barrio. Al lado del nombre figuran las cadenas que tienen un hipermercado ah\u00ed."), ANCHO_BAJADA),
    x        = "Costo de la canasta mensual de 45 productos (pesos)",
    y        = "Barrio \u00b7 cadenas presentes",
    caption  = envolver(paste0(PIE_FUENTE, "\n",
                      "La canasta son 45 productos de consumo habitual con cantidades mensuales, elegidos entre los 1.257 que se\n",
                      "venden en las siete cadenas. No incluye frescos: la carne, la fruta y la verdura no tienen c\u00f3digo de barras com\u00fan\n",
                      "entre cadenas. El 6,8% de las l\u00edneas se val\u00faa al precio mediano de su cadena, para que todas las sucursales\n",
                      "compren la misma canasta. Los barrios salen de cruzar la coordenada de cada local con los pol\u00edgonos oficiales\n",
                      "de la Ciudad. Son precios de lista: la norma no obliga a publicar promociones. El eje horizontal no arranca en cero."), ANCHO_PIE)) +
  tema_tp(base_size = 10) +
  theme(panel.grid.major.y = element_blank(),
        axis.text.y        = element_text(size = 8.2))

guardar_grafico(g2, "grupo_03_comunicativo", ancho = 24, alto = 21, formato = "jpg")


cat("\nlisto. Los dos JPG estan en output/graficos/\n")
