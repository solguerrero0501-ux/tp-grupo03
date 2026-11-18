# ---------------------------------------------------------------------------
# 06_concentracion.R
#
# Metodo de analisis cuantitativo (unidad 3): indices de concentracion.
#
# Va despues de la inferencia porque se apoya en ella. El script 05 mostro que
# el precio lo fija la cadena y no el barrio. Lo que falta responder es la
# pregunta economica que sigue naturalmente: si en la mayoria de los barrios
# hay una sola empresa, esa falta de competencia se paga?
#
# Se calculan:
#   - el indice de Herfindahl-Hirschman (HHI) de los hipermercados de la
#     Ciudad, por empresa y por bandera;
#   - las razones de concentracion CR1, CR2 y CR3;
#   - el HHI de cada barrio;
#   - la relacion entre el HHI del barrio y el costo de la canasta ahi.
#
# Entrada : input/indice_sucursal.rds
# Salida  : output/tablas/06_*.csv
#           output/graficos/06_*.png
# ---------------------------------------------------------------------------

library(here)
library(readr)
library(dplyr)
library(ggplot2)

here::i_am("script/06_concentracion.R")

source(here("utils", "tema_tp.R"))
source(here("utils", "formato_numeros.R"))

suc <- readRDS(here("input", "indice_sucursal.rds")) |>
  mutate(cadena  = acortar_bandera(comercio_bandera_nombre),
         empresa = sub(" *(S\\.A\\.|SRL|CENTRO INTEGRAL.*)$", "", comercio_razon_social),
         empresa = trimws(empresa))



# ---------------------------------------------------------------------------
# El HHI es la suma de los cuadrados de las participaciones de mercado, en
# porcentaje. Va de casi 0 (muchisimos competidores chicos) a 10.000 (un solo
# jugador con el 100%). El umbral habitual para hablar de mercado altamente
# concentrado es 2.500.
#
# La participacion se mide en cantidad de sucursales, no en facturacion, que
# es el dato que el SEPA no publica. Es una aproximacion y hay que decirlo: un
# hipermercado grande y uno chico cuentan igual.
# ---------------------------------------------------------------------------
hhi <- function(x) {
  participacion <- 100 * table(x) / length(x)
  sum(participacion^2)
}


# ===========================================================================
# 1. CONCENTRACION EN TODA LA CIUDAD
# ===========================================================================

cat(strrep("=", 72), "\n1. CONCENTRACION EN LA CIUDAD\n", strrep("=", 72), "\n", sep = "")

reparto_empresa <- suc |>
  count(empresa, name = "sucursales") |>
  mutate(participacion = 100 * sucursales / sum(sucursales)) |>
  arrange(desc(participacion))

cat("\nparticipacion por empresa (sobre 45 hipermercados):\n")
print(reparto_empresa |>
        mutate(participacion = round(participacion, 1)) |>
        as.data.frame(), row.names = FALSE)

hhi_empresa <- hhi(suc$empresa)
hhi_bandera <- hhi(suc$cadena)

cat("\nHHI por empresa :", formato_num(hhi_empresa, 0), "puntos\n")
cat("HHI por bandera :", formato_num(hhi_bandera, 0), "puntos\n")
cat("umbral de mercado altamente concentrado: 2.500 puntos\n")

cr <- cumsum(sort(reparto_empresa$participacion, decreasing = TRUE))
cat("\nCR1 =", formato_pct(cr[1] / 100, 1),
    "| CR2 =", formato_pct(cr[2] / 100, 1),
    "| CR3 =", formato_pct(cr[3] / 100, 1), "\n")

cat("\n-> El mercado de hipermercados de la Ciudad esta altamente concentrado:\n")
cat("   una sola empresa tiene mas de la mitad de las bocas y dos tienen mas\n")
cat("   de tres cuartos. Las siete banderas que se ven en la calle son cuatro\n")
cat("   empresas.\n")

write_csv(reparto_empresa, here("output", "tablas", "06_reparto_empresa.csv"))


# --- Grafico: el reparto del mercado --------------------------------------
g_rep <- reparto_empresa |>
  mutate(etiqueta = paste0(formato_num(participacion, 1), "%")) |>
  ggplot(aes(x = reorder(empresa, participacion), y = participacion)) +
  geom_col(fill = AZUL, width = 0.62) +
  geom_text(aes(label = etiqueta), hjust = -0.25, size = 3.2, colour = GRIS_TXT) +
  coord_flip(clip = "off") +
  scale_y_continuous(labels = function(x) paste0(formato_num(x, 0), "%"),
                     expand = expansion(mult = c(0, 0.16))) +
  labs(
    title    = paste0("Cuatro empresas se reparten los 45 hipermercados de la Ciudad"),
    subtitle = paste0("Participaci\u00f3n de cada empresa en la cantidad de hipermercados de la ",
                      "Ciudad de Buenos Aires, 11-09-2026.\nEl \u00edndice de Herfindahl-Hirschman ",
                      "da ", formato_num(hhi_empresa, 0), " puntos; por encima de 2.500 se ",
                      "considera un mercado\naltamente concentrado."),
    x        = "Empresa",
    y        = "Participaci\u00f3n en la cantidad de hipermercados (%)",
    caption  = paste(PIE_FUENTE,
                     "\nLa participaci\u00f3n se mide en cantidad de sucursales, no en facturaci\u00f3n: el SEPA no publica ventas.")
  ) +
  tema_tp() +
  theme(panel.grid.major.y = element_blank())

guardar_grafico(g_rep, "06_reparto_mercado", ancho = 20, alto = 11)


# ===========================================================================
# 2. CONCENTRACION BARRIO POR BARRIO
# ===========================================================================

cat("\n", strrep("=", 72), "\n2. CONCENTRACION POR BARRIO\n", strrep("=", 72), "\n", sep = "")

barrios <- suc |>
  group_by(barrio) |>
  summarise(locales  = n(),
            cadenas  = n_distinct(cadena),
            empresas = n_distinct(empresa),
            hhi      = hhi(empresa),
            costo    = mean(costo_canasta),
            indice   = mean(indice),
            .groups  = "drop") |>
  arrange(hhi, costo)

print(barrios |>
        mutate(hhi = round(hhi), costo = round(costo), indice = round(indice, 1)) |>
        as.data.frame(), row.names = FALSE)
write_csv(barrios, here("output", "tablas", "06_hhi_barrio.csv"))

monopolios <- sum(barrios$empresas == 1)
cat("\nbarrios donde hay una sola empresa:", monopolios, "de", nrow(barrios),
    "(", formato_pct(monopolios / nrow(barrios), 0), ")\n")
cat("En esos barrios, quien quiere comprar en un hipermercado no elige cadena.\n")


# ===========================================================================
# 3. LA CONCENTRACION DEL BARRIO, SE PAGA?
# ===========================================================================
# Esta es la pregunta que cierra el trabajo. La intuicion economica dice que
# si: menos competencia, precios mas altos. Lo medimos.

cat("\n", strrep("=", 72), "\n3. LA CONCENTRACION DEL BARRIO, SE PAGA?\n",
    strrep("=", 72), "\n", sep = "")

pearson  <- cor.test(barrios$hhi, barrios$costo)
spearman <- suppressWarnings(cor.test(barrios$hhi, barrios$costo, method = "spearman"))

cat("\ncorrelacion entre el HHI del barrio y el costo de la canasta ahi:\n")
cat("   Pearson  r   = ", formato_num(pearson$estimate, 3),
    " | p = ", signif(pearson$p.value, 3), "\n", sep = "")
cat("   Spearman rho = ", formato_num(spearman$estimate, 3),
    " | p = ", signif(spearman$p.value, 3), "\n", sep = "")

cat("\ncosto medio de la canasta segun cuantas empresas hay en el barrio:\n")
print(barrios |>
        group_by(empresas) |>
        summarise(barrios = n(),
                  costo_medio = round(mean(costo)),
                  .groups = "drop") |>
        as.data.frame(), row.names = FALSE)

if (pearson$p.value > 0.05) {
  cat("\n-> NO hay relacion significativa. Es un resultado, no una falta de\n")
  cat("   resultado, y es coherente con todo lo anterior: si cada cadena\n")
  cat("   publica una sola lista de precios para toda la Ciudad, el precio de\n")
  cat("   un barrio no puede responder a la competencia DE ESE barrio. La\n")
  cat("   cadena no mira el barrio cuando pone el precio.\n")
  cat("\n   Dicho de otro modo: la concentracion local es alta y no se paga en\n")
  cat("   el precio de lista, porque el precio no se decide localmente. Lo que\n")
  cat("   define cuanto gasta un vecino no es cuanta competencia tiene cerca,\n")
  cat("   sino que cadena le toco cerca.\n")
} else {
  cat("\n-> La relacion es significativa. Revisar la interpretacion del script.\n")
}

resultado <- data.frame(
  medida = c("HHI por empresa (ciudad)", "HHI por bandera (ciudad)",
             "CR1", "CR2", "CR3",
             "barrios con una sola empresa", "barrios analizados",
             "correlacion HHI-costo (Pearson r)", "p-valor Pearson",
             "correlacion HHI-costo (Spearman rho)", "p-valor Spearman"),
  valor  = c(hhi_empresa, hhi_bandera, cr[1], cr[2], cr[3],
             monopolios, nrow(barrios),
             pearson$estimate, pearson$p.value,
             spearman$estimate, spearman$p.value)
)
write_csv(resultado, here("output", "tablas", "06_resultado_concentracion.csv"))


# --- Grafico: concentracion contra precio ---------------------------------
# Se grafica aunque el resultado sea nulo, porque el resultado nulo ES el
# hallazgo. Los puntos se separan un poco en el eje horizontal (jitter) porque
# 21 de los 28 barrios tienen exactamente el mismo HHI y si no quedarian todos
# encimados en una sola linea vertical.
set.seed(2026)

g_hhi <- barrios |>
  ggplot(aes(x = hhi, y = costo)) +
  geom_smooth(method = "lm", se = TRUE, colour = GRIS, fill = "#EFEFEF",
              linewidth = 0.5, formula = y ~ x) +
  geom_jitter(width = 180, height = 0, size = 2.6, colour = AZUL, alpha = 0.8) +
  scale_x_continuous(breaks = c(3750, 5000, 10000),
                     labels = function(x) formato_num(x, 0)) +
  scale_y_continuous(labels = function(x) formato_millones(x, 0)) +
  labs(
    title    = "Donde hay un solo supermercado no se paga m\u00e1s caro",
    subtitle = paste0("Cada punto es uno de los ", nrow(barrios), " barrios de la Ciudad con al ",
                      "menos un hipermercado. El eje horizontal es el \u00edndice de\nHerfindahl-Hirschman ",
                      "del barrio: 10.000 significa que todas sus bocas son de la misma empresa.\n",
                      "La correlacion no es significativa (r = ", formato_num(pearson$estimate, 2),
                      ", p = ", formato_num(pearson$p.value, 2), "): la concentraci\u00f3n del barrio ",
                      "no explica su precio."),
    x        = "\u00cdndice de Herfindahl-Hirschman del barrio (puntos, m\u00e1ximo 10.000)",
    y        = "Costo de la canasta mensual de 45 productos (pesos)",
    caption  = paste(PIE_FUENTE,
                     "\nLos puntos se separan levemente en horizontal para que no se superpongan: 21 de los 28 barrios tienen un HHI de 10.000.",
                     "\nEl eje vertical no arranca en cero. La banda gris es el intervalo de confianza del 95% de la recta de regresion.")
  ) +
  tema_tp()

guardar_grafico(g_hhi, "06_hhi_vs_precio", ancho = 20, alto = 12)


cat("\nlisto. Ya se puede correr 07_graficos.R\n")
