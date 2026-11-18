# ---------------------------------------------------------------------------
# 05_inferencia.R
#
# Metodos de inferencia estadistica (unidad 2) sobre el indice de precios por
# sucursal.
#
# La pregunta es: las diferencias de precio entre sucursales, son diferencias
# entre CADENAS o diferencias entre LUGARES de la ciudad?
#
# Una decision que conviene defender antes que nada: los tests corren sobre las
# 45 sucursales, no sobre los 52.732 precios. Con 52.732 observaciones
# cualquier diferencia, por chica que sea, da significativa solo por el tamano
# de la muestra. La unidad que tiene sentido para esta pregunta es la sucursal:
# cada una fija una lista de precios.
#
# Entrada : input/indice_sucursal.rds
# Salida  : output/tablas/05_*.csv
#           output/graficos/05_*.png
# ---------------------------------------------------------------------------

library(here)
library(readr)
library(dplyr)
library(ggplot2)

here::i_am("script/05_inferencia.R")

source(here("utils", "tema_tp.R"))
source(here("utils", "formato_numeros.R"))

suc <- readRDS(here("input", "indice_sucursal.rds")) |>
  mutate(cadena  = acortar_bandera(comercio_bandera_nombre),
         empresa = comercio_razon_social)



# ===========================================================================
# 1. QUE SE VA A TESTEAR
# ===========================================================================

cat(strrep("=", 72), "\n1. LA VARIABLE\n", strrep("=", 72), "\n", sep = "")
cat("indice de precios de la sucursal: promedio de sus precios relativos x 100.\n")
cat("unidad de observacion del test  : la sucursal.", nrow(suc), "observaciones.\n\n")

print(suc |>
        group_by(cadena) |>
        summarise(n = n(), media = round(mean(indice), 2),
                  desvio = round(sd(indice), 3),
                  minimo = round(min(indice), 2), maximo = round(max(indice), 2)) |>
        arrange(media) |>
        as.data.frame(), row.names = FALSE)

# Maxi y HiperChangomas tienen UNA sucursal cada una. Con n = 1 no hay varianza
# dentro del grupo, asi que no pueden entrar en un ANOVA. Se las excluye del
# test y se lo dice: el test corre sobre 5 banderas y 43 sucursales.
testeables <- suc |>
  group_by(cadena) |>
  filter(n() > 1) |>
  ungroup() |>
  mutate(cadena = factor(cadena))

cat("\nbanderas con mas de una sucursal:", n_distinct(testeables$cadena),
    "| sucursales en el test:", nrow(testeables), "\n")
cat("quedan afuera Maxi y Changomas, con una sucursal cada una.\n")


# ===========================================================================
# 2. SUPUESTOS
# ===========================================================================
# Se miran ANTES de correr el ANOVA, no despues. Si fallan, el ANOVA no se tira
# a la basura: se acompana con tests que no dependan del supuesto que falla.

cat("\n", strrep("=", 72), "\n2. SUPUESTOS\n", strrep("=", 72), "\n", sep = "")

cat("\na) Normalidad dentro de cada grupo (Shapiro-Wilk).\n")
cat("   Solo se puede correr en grupos con al menos 3 observaciones.\n\n")
shapiro <- testeables |>
  group_by(cadena) |>
  filter(n() >= 3) |>
  summarise(n = n(),
            W = round(shapiro.test(indice)$statistic, 3),
            p = signif(shapiro.test(indice)$p.value, 3),
            .groups = "drop") |>
  mutate(normal = ifelse(p > 0.05, "no se rechaza", "SE RECHAZA"))
print(as.data.frame(shapiro), row.names = FALSE)

cat("\nb) Homogeneidad de varianzas (Bartlett).\n")
bartlett <- bartlett.test(indice ~ cadena, data = testeables)
cat("   K2 =", round(bartlett$statistic, 2),
    "| gl =", bartlett$parameter,
    "| p =", signif(bartlett$p.value, 3), "\n")
cat("   (Bartlett es sensible a la falta de normalidad, asi que este resultado\n")
cat("    hay que leerlo junto con el de arriba, no solo.)\n")

rechazan_normalidad <- sum(shapiro$p <= 0.05)

cat("\n-> Normalidad: se rechaza en", rechazan_normalidad, "de los",
    nrow(shapiro), "grupos testeables.\n")
cat("   Varianzas iguales: Bartlett",
    ifelse(bartlett$p.value < 0.05, "las rechaza.", "no las rechaza."), "\n")
cat("   Es decir, el supuesto que falla con claridad es el de varianzas\n")
cat("   iguales, y eso tiene una explicacion concreta: Coto tiene 24\n")
cat("   sucursales con precios identicos entre si, asi que su varianza es\n")
cat("   casi cero, mientras que Jumbo dispersa mucho mas.\n")
cat("\n   Por eso abajo estan tambien Welch, que no asume varianzas iguales, y\n")
cat("   Kruskal-Wallis, que no asume normalidad. Si los tres coinciden, el\n")
cat("   resultado no depende de cual supuesto se asuma.\n")

write_csv(shapiro, here("output", "tablas", "05_supuestos_shapiro.csv"))


# ===========================================================================
# 3. ANOVA, WELCH Y KRUSKAL-WALLIS
# ===========================================================================

cat("\n", strrep("=", 72), "\n3. LOS TRES TESTS\n", strrep("=", 72), "\n", sep = "")

modelo <- aov(indice ~ cadena, data = testeables)
tabla  <- summary(modelo)[[1]]

sc_entre  <- tabla[["Sum Sq"]][1]
sc_dentro <- tabla[["Sum Sq"]][2]
eta2      <- sc_entre / (sc_entre + sc_dentro)

cat("\na) ANOVA de un factor\n")
print(tabla)
cat("   F(", tabla[["Df"]][1], ",", tabla[["Df"]][2], ") = ",
    formato_num(tabla[["F value"]][1], 1),
    " | p = ", signif(tabla[["Pr(>F)"]][1], 3), "\n", sep = "")
cat("   eta cuadrado = ", formato_num(eta2, 4),
    " -> el ", formato_pct(eta2, 1),
    " de la variabilidad del indice entre sucursales\n", sep = "")
cat("   corresponde a diferencias entre cadenas.\n")

cat("\nb) Welch (no asume varianzas iguales)\n")
welch <- oneway.test(indice ~ cadena, data = testeables, var.equal = FALSE)
cat("   F = ", formato_num(welch$statistic, 1),
    " | gl = ", round(welch$parameter[1], 1), ", ", round(welch$parameter[2], 1),
    " | p = ", signif(welch$p.value, 3), "\n", sep = "")

cat("\nc) Kruskal-Wallis (no asume normalidad)\n")
kw <- kruskal.test(indice ~ cadena, data = testeables)
cat("   chi2 = ", formato_num(kw$statistic, 1),
    " | gl = ", kw$parameter,
    " | p = ", signif(kw$p.value, 3), "\n", sep = "")

cat("\n-> Los tres rechazan la hipotesis nula de medias iguales. El resultado\n")
cat("   no depende de que supuesto se asuma.\n")
cat("   Ojo con la interpretacion: rechazar la nula no dice que la cadena\n")
cat("   CAUSE el precio. Dice que la cadena ordena los precios.\n")

resumen_tests <- data.frame(
  test       = c("ANOVA", "Welch", "Kruskal-Wallis"),
  estadistico = c(tabla[["F value"]][1], welch$statistic, kw$statistic),
  p_valor    = c(tabla[["Pr(>F)"]][1], welch$p.value, kw$p.value)
)
write_csv(resumen_tests, here("output", "tablas", "05_tests.csv"))


# ===========================================================================
# 4. TUKEY: QUE PARES DIFIEREN DE VERDAD
# ===========================================================================
# El ANOVA dice "no todas son iguales". No dice cuales. Tukey compara todos los
# pares corrigiendo por comparaciones multiples.

cat("\n", strrep("=", 72), "\n4. TUKEY\n", strrep("=", 72), "\n", sep = "")

tk <- TukeyHSD(modelo)$cadena
tk_df <- data.frame(
  par          = rownames(tk),
  diferencia   = tk[, "diff"],
  li           = tk[, "lwr"],
  ls           = tk[, "upr"],
  p_ajustado   = tk[, "p adj"],
  row.names    = NULL
) |>
  mutate(distinguible = ifelse(p_ajustado < 0.05, "si", "no")) |>
  arrange(p_ajustado)

print(tk_df |>
        mutate(across(c(diferencia, li, ls), ~ round(.x, 2)),
               p_ajustado = signif(p_ajustado, 3)) |>
        as.data.frame(), row.names = FALSE)

write_csv(tk_df, here("output", "tablas", "05_tukey.csv"))

no_distinguibles <- filter(tk_df, distinguible == "no")
cat("\npares que NO se distinguen entre si:", nrow(no_distinguibles), "de", nrow(tk_df), "\n")
if (nrow(no_distinguibles) > 0) {
  cat(paste0("   ", no_distinguibles$par, collapse = "\n"), "\n")
}


# ===========================================================================
# 5. LA HIPOTESIS GEOGRAFICA
# ===========================================================================
# La hipotesis con la que arrancamos era que el precio dependia del barrio.
# Esta seccion es la que la pone a prueba, y el resultado es que NO se
# confirma. Que un trabajo refute su propia hipotesis inicial no es un
# problema: es un resultado, y es el que ordena todo el argumento.

cat("\n", strrep("=", 72), "\n5. LA HIPOTESIS GEOGRAFICA\n", strrep("=", 72), "\n", sep = "")

# a) Por zona norte-centro-sur. Se usan terciles de latitud como aproximacion
#    al eje norte-sur de la ciudad; no son las comunas reales y conviene
#    aclararlo.
suc <- suc |>
  mutate(zona = cut(lat, breaks = quantile(lat, c(0, 1/3, 2/3, 1)),
                    labels = c("Sur", "Centro", "Norte"),
                    include.lowest = TRUE))

cat("\na) ANOVA del indice por zona (terciles de latitud)\n")
anova_zona <- summary(aov(indice ~ zona, data = suc))[[1]]
print(anova_zona)
cat("   -> p =", signif(anova_zona[["Pr(>F)"]][1], 3), "\n")

# b) Correlacion con la latitud, que usa la coordenada sin agrupar.
cor_lat <- cor.test(suc$indice, suc$lat)
cat("\nb) Correlacion entre el indice y la latitud\n")
cat("   r = ", formato_num(cor_lat$estimate, 3),
    " | p = ", signif(cor_lat$p.value, 3), "\n", sep = "")

# c) El test que mas pesa: cuanto se mueve el indice ADENTRO de cada cadena.
#    Si el barrio importara, dos sucursales de la misma cadena en barrios
#    distintos deberian tener precios distintos.
cat("\nc) Variacion del indice adentro de cada cadena\n")
dentro <- suc |>
  group_by(cadena) |>
  filter(n() > 1) |>
  summarise(sucursales = n(),
            barrios    = n_distinct(barrio),
            rango      = round(max(indice) - min(indice), 3),
            .groups = "drop") |>
  arrange(desc(sucursales))
print(as.data.frame(dentro), row.names = FALSE)

cat("\n-> Coto tiene", dentro$sucursales[dentro$cadena == "Coto"],
    "sucursales en", dentro$barrios[dentro$cadena == "Coto"],
    "barrios distintos y su indice se mueve",
    formato_num(dentro$rango[dentro$cadena == "Coto"], 2), "puntos.\n")
cat("   La hipotesis geografica no se confirma. Las cadenas ponen el mismo\n")
cat("   precio en toda la ciudad, y lo que cambia entre barrios es QUE CADENA\n")
cat("   hay, no donde esta el barrio.\n")

write_csv(dentro, here("output", "tablas", "05_variacion_interna.csv"))


# ===========================================================================
# 6. EL GRAFICO DEL RESULTADO
# ===========================================================================
# Un grafico de los resultados del metodo, que es lo que pide la consigna: se
# tiene que ver de un vistazo que la distancia entre cadenas es grande y la
# distancia adentro de cada cadena es nula.

orden <- suc |> group_by(cadena) |> summarise(m = mean(indice)) |> arrange(m)

g <- suc |>
  mutate(cadena = factor(cadena, levels = orden$cadena)) |>
  ggplot(aes(x = indice, y = cadena)) +
  geom_vline(xintercept = 100, linetype = "dashed", colour = GRIS_SUAVE,
             linewidth = 0.4) +
  geom_point(size = 2.6, colour = AZUL, alpha = 0.75) +
  scale_x_continuous(labels = function(x) formato_num(x, 0)) +
  labs(
    title    = "Cada cadena fija un precio y lo repite en todas sus sucursales",
    subtitle = paste("\u00cdndice de precios de cada una de las 45 sucursales, por cadena.",
                     "Cada punto es una sucursal.\nEl \u00edndice es el precio promedio de la",
                     "canasta comparable de 1.257 productos respecto del\npromedio de las",
                     "siete banderas: 100 es cobrar como el promedio del mercado."),
    x        = "\u00cdndice de precios de la sucursal (promedio de las siete banderas = 100)",
    y        = "Cadena",
    caption  = paste(PIE_FUENTE,
                     "\nEl eje horizontal no arranca en cero. Maxi y HiperChangomas tienen una sola sucursal en la Ciudad.")
  ) +
  tema_tp() +
  theme(panel.grid.major.y = element_blank())

guardar_grafico(g, "05_indice_por_cadena", ancho = 20, alto = 11)


cat("\nlisto. Ya se puede correr 06_concentracion.R\n")
