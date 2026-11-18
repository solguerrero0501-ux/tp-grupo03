# ---------------------------------------------------------------------------
# clasificar_categoria(descripcion, marca)
#
# La base del SEPA no trae categoria de producto: solo la descripcion que
# escribio cada cadena y la marca. La categoria la construimos nosotros
# buscando palabras clave en esos dos campos.
#
# Es una decision metodologica, no un dato: otro diccionario de palabras da
# otra clasificacion. Por eso 04_canasta_indice.R imprime los productos que
# quedaron en cada categoria y los que quedaron sin clasificar, para poder
# revisar a ojo si algo quedo mal puesto.
#
# Dos reglas que hacen que funcione:
#   - El orden importa: gana la primera categoria que coincide. Por eso
#     Mascotas va primera (el "alimento para perro" tiene la palabra alimento)
#     y Almacen va ultima (es la mas generica).
#   - Las palabras se buscan como PREFIJO, con limite de palabra solo a la
#     izquierda, asi "galletita" tambien encuentra "galletitas". Las que
#     empiezan con "=" se buscan como palabra COMPLETA, para las cortas y
#     ambiguas: "=sal" no tiene que encontrar "salsa".
#
# Las listas se armaron en varias rondas: se clasificaba, se miraba que habia
# quedado en "Otros", y se agregaban las palabras que faltaban. Las rondas
# estan separadas abajo para que se vea el proceso.
# ---------------------------------------------------------------------------

PALABRAS <- list(
  list("Mascotas", c(
    "perro", "gato", "mascota", "whiskas", "pedigree", "dentastix", "dogui",
    "cat chow", "dog chow", "purina", "sheba", "friskies", "felix", "eukanuba",
    "vitalcan", "old prince", "balanced", "alim dogui", "alim gati", "gatito")),

  list("Bebidas con alcohol", c(
    "vino", "cerveza", "vodka", "fernet", "licor", "whisky", "whiskey", "=gin",
    "aperitivo", "champagne", "espumante", "sidra", "malbec", "cabernet",
    "smirnoff", "baileys", "quilmes", "brahma", "stella", "heineken", "campari",
    "gancia", "branca", "cynar", "amargo", "chandon", "absolut", "johnnie",
    "criadores", "escorihuela", "andes origen", "benjamin nieto", "toro viejo")),

  list("Bebidas sin alcohol", c(
    "gaseosa", "jugo", "saborizada", "energizante", "isotonic", "coca cola",
    "pepsi", "sprite", "fanta", "seven up", "7up", "powerade", "gatorade",
    "aquarius", "cepita", "baggio", "villavicencio", "benedictino",
    "villa del sur", "=ades", "levite", "=tang", "clight", "exprim", "=agua",
    "gasificada", "tonica", "=speed", "red bull", "monster", "soda ")),

  list("Lacteos y huevos", c(
    "leche", "yogur", "=yog", "queso", "crema de leche", "crema original",
    "crema light", "manteca", "dulce de leche", "huevo", "postre", "=flan",
    "ricota", "muzzarella", "mozzarella", "serenisima", "milkaut", "ilolay",
    "sancor", "danonino", "casancrem", "finlandia", "tregar", "ser firme",
    "cindor", "mendicrim", "philadelphia", "=tybo", "port salut", "cremon",
    "las tres ninas", "danone", "yogs")),

  list("Carnes, frescos y congelados", c(
    "pollo", "carne", "milanesa", "medallon", "suprema", "hamburguesa",
    "pescado", "merluza", "cerdo", "=bife", "asado", "salchicha", "chorizo",
    "jamon", "mortadela", "salame", "bondiola", "pechuga", "pata muslo",
    "swift", "=paty", "granja del sol", "lechuga", "=tomate", "cebolla",
    "banana", "manzana", "congelad", "helado", "=hel", "mc cain", "frizzi",
    "tapa de empanada", "tapa empanada", "tapa de tarta", "la saltena",
    "nugget", "bastones", "rebozad", "calabaza", "verduleria", "papas noisette",
    "papas smiles", "papas prefritas", "arrollado", "matambre")),

  list("Panificados y golosinas", c(
    "galleta", "chocolate", "alfajor", "bizcochuelo", "tostada", "=pan",
    "budin", "factura", "snack", "papas fritas", "palito", "caramelo",
    "chupetin", "chicle", "turron", "oblea", "barra de cereal", "cereal",
    "granola", "magdalena", "bizcocho", "criollita", "pastilla", "cerealitas",
    "oreo", "pepitos", "rhodesia", "bon-o-bon", "bon o bon", "aguila", "arcor",
    "cofler", "milka", "=block", "=tita", "rocklets", "terrabusi", "mentho plus",
    "menthoplus", "tic tac", "manon", "boca de dama", "=opera", "=mani",
    "pororo", "chizito", "saladix", "lincoln", "melba", "merengada",
    "sonrisas", "chocolina", "sucarita", "vauquita", "mogul", "flynn",
    "halls", "beldent", "topline", "bombon", "tableta", "kinder", "shot ",
    "fantoche", "criollas", "grisin", "tutuca", "vainilla x")),

  list("Limpieza", c(
    "detergente", "lavandina", "limpiador", "=limp", "suavizante", "=suav",
    "jabon en polvo", "jabon liquido para ropa", "desinfectante", "insecticida",
    "esponja", "=trapo", "bolsa de residuo", "lustramueble", "antigrasa",
    "quitamancha", "prelavado", "ayudin", "poett", "procenex", "=cif", "vanish",
    "magistral", "=skip", "=ala", "=drive", "comfort", "vivere", "=raid",
    "mortimer", "virulana", "rejilla", "=pila", "desodorante de ambiente",
    "aromatizante", "=odex", "=blem", "fosforo", "servilleta", "rollo de cocina",
    "papel de aluminio", "film ", "guantes", "escoba", "secador", "=balde",
    "mr musculo", "cera para pisos", "sapolan", "zorro", "desengrasante")),

  list("Perfumeria e higiene", c(
    "shampoo", "=sham", "acondicionador", "=acond", "jabon", "crema dental",
    "cepillo dental", "desodorante", "antitranspirante", "antitranspi", "panal",
    "toallita", "toalla fem", "toa fem", "papel higienico", "afeitar",
    "maquina desechable", "repuesto gillette", "repuesto venus", "repuesto bic",
    "crema facial", "crema de manos", "crema corporal", "crema para peinar",
    "protector solar", "algodon", "hisopo", "enjuague", "sedal", "=dove",
    "rexona", "nivea", "colgate", "head & shoulders", "=issue", "always",
    "plusbelle", "=elite", "higienol", "=scott", "lady soft", "pantene", "=axe",
    "gillette", "=bic", "coloracion", "=color", "nutrisse", "excellence",
    "fructis", "curita", "aposito", "siempre libre", "siemprelibre", "carefree",
    "johnson", "prot diario", "panuelo", "interdental", "hilo dental", "talco",
    "vaselina", "gel fijador", "tintura", "depilat", "tampon", "veritas",
    "kotex", "nosotras", "balsamo", "desmaquill", "agua mic", "rose care",
    "gel de afeitar", "=gum", "oral b", "listerine", "eudermo")),

  list("Almacen", c(
    "fideo", "arroz", "aceite", "harina", "azucar", "vinagre", "mayonesa",
    "ketchup", "mostaza", "salsa", "pure de tomate", "tomate perita", "=atun",
    "caballa", "arveja", "lenteja", "poroto", "garbanzo", "yerba", "mate cocido",
    "=cafe", "=te", "cacao", "edulcorante", "endulzante", "condimento",
    "oregano", "pimenton", "provenzal", "=caldo", "=sopa", "=pure", "polenta",
    "avena", "semola", "gelatina", "levadura", "premezcla", "aderezo",
    "aceituna", "=miel", "mermelada", "cocinero", "=natura", "lucchetti",
    "matarazzo", "knorr", "hileret", "la campagnola", "alicante", "dos anclas",
    "menoyo", "exquisita", "riera", "polvo para hornear", "=royal", "=sal",
    "chimichurri", "bicarbonato", "almidon", "maicena", "leudante", "ravioles",
    "noqui", "tallarin", "spaghetti", "codito", "mousse", "pan rallado",
    "rebozador", "coco rallado", "=nuez", "pasas", "maizena", "savora",
    "hellmann", "sopa crema", "caldo en", "bizcochuelo exquisita")),

  # --- Segunda ronda: palabras que aparecieron al revisar los "Otros" ------
  list("Bebidas con alcohol", c("vin up", "vermouth", "carpano", "tinto brik")),
  list("Lacteos y huevos", c("blancrem", "s.ignacio", "san ignacio", "coffee-mate",
                             "coffee mate", "q.", "untable")),
  list("Carnes, frescos y congelados", c("leberwur", "paladini", "paleta ", "burger",
                                         "vegetalex", "fiambre", "queso y jamon")),
  list("Panificados y golosinas", c("galle", "copos de maiz", "granix", "vainilla",
                                    "pozo ", "=pep", "rueditas", "lays", "traviata",
                                    "rumba", "hogarena", "=mana", "=rex", "criollita",
                                    "cracker", "kesitas", "barrita", "cereales")),
  list("Limpieza", c("bolsa rollo", "bolsa cf", "asurin", "destapacaneria", "plomero",
                     "alcohol", "bialcohol", "canasta liquida", "=pato", "rollo cocina",
                     "sussex", "bolsa de consorcio", "repelente", "naftalina",
                     "palito de ambiente", "difusor")),
  list("Almacen", c("favorita", "tirabuzon", "=mate", "chamigo", "taragui", "aceto",
                    "=hrb", "la virginia", "boldo", "=tilo", "manzanilla", "infusion",
                    "saquito", "nescafe", "dolca", "cabrales", "playadito",
                    "rosamonte", "union", "tostado", "semillas")),

  # --- Tercera ronda ------------------------------------------------------
  list("Limpieza", c("insectic", "lysoform", "glade", "=echo", "cera suiza", "apresto",
                     "=robin", "finish", "abrillantador", "desinf", "aerosol",
                     "lavavajillas", "ambientador")),
  list("Carnes, frescos y congelados", c("panceta", "=lario", "green life", "berries",
                                         "espinaca", "arandano", "frutilla x")),
  list("Lacteos y huevos", c("ricotta", "garcia semi", "sachet")),
  list("Perfumeria e higiene", c("shamp", "calipso", "doncella", "algabo", "tresemme",
                                 "polvo desod", "pedico", "acondic", "cremoso para",
                                 "higiene", "jab ")),
  list("Bebidas sin alcohol", c("terma", "beb. hierb", "beb.hierb", "hierbas serranas")),

  # --- Cuarta ronda -------------------------------------------------------
  list("Lacteos y huevos", c("actimel", "margarina")),
  list("Almacen", c("dulce de batata", "dulce de membrillo", "esnaola", "pasta menu",
                    "menu box", "fileto")),
  list("Bebidas con alcohol", c("=ron", "havana", "frizante", "frizze")),
  list("Limpieza", c("selton", "mata mosca", "pomada", "wassington", "betun")),
  list("Perfumeria e higiene", c("preservativo", "fijador", "=roby", "crema corp")),
  list("Panificados y golosinas", c("crocante", "=egran", "inflado"))
)


# Las categorias que van a los graficos, con tilde. Las listas de arriba estan
# sin tilde a proposito: el texto contra el que se busca tambien se normaliza
# sin tildes, asi que las palabras clave no las necesitan.
CON_TILDE <- c(
  "Lacteos y huevos"     = "Lácteos y huevos",
  "Perfumeria e higiene" = "Perfumería e higiene",
  "Almacen"              = "Almacén"
)


escapar_regex <- function(x) gsub("([][{}()*+?.\\\\^$|])", "\\\\\\1", x)

armar_patron <- function(palabras) {
  partes <- vapply(palabras, function(w) {
    if (startsWith(w, "=")) paste0(escapar_regex(substring(w, 2)), "\\b")
    else escapar_regex(w)
  }, character(1), USE.NAMES = FALSE)
  paste0("\\b(?:", paste(partes, collapse = "|"), ")")
}

REGLAS <- lapply(PALABRAS, function(r) list(categoria = r[[1]],
                                            patron    = armar_patron(r[[2]])))


# Saca tildes, pasa a minuscula y colapsa los espacios. Se aplica al texto y no
# a las palabras clave: es mas barato normalizar una vez cada producto que
# escribir cada palabra con y sin tilde.
normalizar <- function(x) {
  x <- stringi::stri_trans_general(as.character(x), "Latin-ASCII")
  x <- tolower(x)
  trimws(gsub("\\s+", " ", x))
}


clasificar_categoria <- function(descripcion, marca = NULL) {
  texto <- normalizar(descripcion)
  if (!is.null(marca)) {
    texto <- paste(texto, normalizar(ifelse(is.na(marca), "", marca)))
  }

  categoria <- rep("Otros", length(texto))
  pendiente <- rep(TRUE, length(texto))

  for (r in REGLAS) {
    if (!any(pendiente)) break
    pega <- pendiente & grepl(r$patron, texto, perl = TRUE)
    categoria[pega] <- r$categoria
    pendiente[pega] <- FALSE
  }

  # Se devuelven con tilde, que es como van a los graficos y a las tablas.
  salida <- unname(CON_TILDE[categoria])
  ifelse(is.na(salida), categoria, salida)
}
