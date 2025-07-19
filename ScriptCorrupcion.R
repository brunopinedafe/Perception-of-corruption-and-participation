require(dplyr)
require(stargazer)
require(haven)

# Reducimos las variables útiles

{

variablesutiles <- c("vb2", "exc2", "exc6", "exc20", "exc7", "exc7new", "prot3",
                 "cp8", "cp13", "q1", "q1tb", "q1tc_r", "q2", "ed", "edr", "edre",
                 "q10inc", "q10new", "q10newt", "exc13a", "exc13", "ocup4a", "colocup4a", "wf1",
                 "pol1", "gi0n", "soct2", "idio2", "it1", "b2", "estratosec")

}
  
# Bases por país

{

# Argentina

arg2019 <- read_dta("/Users/brunopineda/Desktop/Elementos de la Tesis/Datos/Argentina2019") %>%
  select(any_of(variablesutiles)) %>%
  mutate(pais = "Argentina", anio = 2019)

arg2021 <- read_dta("/Users/brunopineda/Desktop/Elementos de la Tesis/Datos/Argentina2021") %>%
  select(any_of(variablesutiles)) %>%
  mutate(pais = "Argentina", anio = 2021)

arg2023 <- read_dta("/Users/brunopineda/Desktop/Elementos de la Tesis/Datos/Argentina2023") %>%
  select(any_of(variablesutiles)) %>%
  mutate(pais = "Argentina", anio = 2023)

# Bolivia

bol2019 <- read_dta("/Users/brunopineda/Desktop/Elementos de la Tesis/Datos/Bolivia2019") %>%
  select(any_of(variablesutiles)) %>%
  mutate(pais = "Bolivia", anio = 2019)

bol2021 <- read_dta("/Users/brunopineda/Desktop/Elementos de la Tesis/Datos/Bolivia2021") %>%
  select(any_of(variablesutiles)) %>%
  mutate(pais = "Bolivia", anio = 2021)

bol2023 <- read_dta("/Users/brunopineda/Desktop/Elementos de la Tesis/Datos/Bolivia2023") %>%
  select(any_of(variablesutiles)) %>%
  mutate(pais = "Bolivia", anio = 2023)

# Brasil

bra2019 <- read_dta("/Users/brunopineda/Desktop/Elementos de la Tesis/Datos/Brasil2019") %>%
  select(any_of(variablesutiles)) %>%
  mutate(pais = "Brasil", anio = 2019)

bra2021 <- read_dta("/Users/brunopineda/Desktop/Elementos de la Tesis/Datos/Brasil2021") %>%
  select(any_of(variablesutiles)) %>%
  mutate(pais = "Brasil", anio = 2021)

bra2023 <- read_dta("/Users/brunopineda/Desktop/Elementos de la Tesis/Datos/Brasil2023") %>%
  select(any_of(variablesutiles)) %>%
  mutate(pais = "Brasil", anio = 2023)

# Chile

chi2019 <- read_dta("/Users/brunopineda/Desktop/Elementos de la Tesis/Datos/Chile2019") %>%
  select(any_of(variablesutiles)) %>%
  mutate(pais = "Chile", anio = 2019)

chi2021 <- read_dta("/Users/brunopineda/Desktop/Elementos de la Tesis/Datos/Chile2021") %>%
  select(any_of(variablesutiles)) %>%
  mutate(pais = "Chile", anio = 2021)

chi2023 <- read_dta("/Users/brunopineda/Desktop/Elementos de la Tesis/Datos/Chile2023") %>%
  select(any_of(variablesutiles)) %>%
  mutate(pais = "Chile", anio = 2023)

# Colombia (consultar por el año 2018)
 
col2018 <- read_dta("/Users/brunopineda/Desktop/Elementos de la Tesis/Datos/Colombia2018") %>%
  select(any_of(variablesutiles)) %>%
  mutate(pais = "Colombia", anio = 2018)

col2021 <- read_dta("/Users/brunopineda/Desktop/Elementos de la Tesis/Datos/Colombia2021") %>%
  select(any_of(variablesutiles)) %>%
  mutate(pais = "Colombia", anio = 2021)

col2023 <- read_dta("/Users/brunopineda/Desktop/Elementos de la Tesis/Datos/Colombia2023") %>%
  select(any_of(variablesutiles)) %>%
  mutate(pais = "Colombia", anio = 2023)

# Ecuador

ecu2019 <- read_dta("/Users/brunopineda/Desktop/Elementos de la Tesis/Datos/Ecuador2019") %>%
  select(any_of(variablesutiles)) %>%
  mutate(pais = "Ecuador", anio = 2019)

ecu2021 <- read_dta("/Users/brunopineda/Desktop/Elementos de la Tesis/Datos/Ecuador2021") %>%
  select(any_of(variablesutiles)) %>%
  mutate(pais = "Ecuador", anio = 2021)

ecu2023 <- read_dta("/Users/brunopineda/Desktop/Elementos de la Tesis/Datos/Ecuador2023") %>%
  select(any_of(variablesutiles)) %>%
  mutate(pais = "Ecuador", anio = 2023)

# Mexico

mex2019 <- read_dta("/Users/brunopineda/Desktop/Elementos de la Tesis/Datos/Mexico2019") %>%
  select(any_of(variablesutiles)) %>%
  mutate(pais = "Mexico", anio = 2019)

mex2021 <- read_dta("/Users/brunopineda/Desktop/Elementos de la Tesis/Datos/Mexico2021") %>%
  select(any_of(variablesutiles)) %>%
  mutate(pais = "Mexico", anio = 2021)

mex2023 <- read_dta("/Users/brunopineda/Desktop/Elementos de la Tesis/Datos/Mexico2023") %>%
  select(any_of(variablesutiles)) %>%
  mutate(pais = "Mexico", anio = 2023)

# Paraguay

par2019 <- read_dta("/Users/brunopineda/Desktop/Elementos de la Tesis/Datos/Paraguay2019") %>%
  select(any_of(variablesutiles)) %>%
  mutate(pais = "Paraguay", anio = 2019)

par2021 <- read_dta("/Users/brunopineda/Desktop/Elementos de la Tesis/Datos/Paraguay2021") %>%
  select(any_of(variablesutiles)) %>%
  mutate(pais = "Paraguay", anio = 2021)

par2023 <- read_dta("/Users/brunopineda/Desktop/Elementos de la Tesis/Datos/Paraguay2023") %>%
  select(any_of(variablesutiles)) %>%
  mutate(pais = "Paraguay", anio = 2023)

# Peru

per2019 <- read_dta("/Users/brunopineda/Desktop/Elementos de la Tesis/Datos/Peru2019") %>%
  select(any_of(variablesutiles)) %>%
  mutate(pais = "Peru", anio = 2019)

per2021 <- read_dta("/Users/brunopineda/Desktop/Elementos de la Tesis/Datos/Peru2021") %>%
  select(any_of(variablesutiles)) %>%
  mutate(pais = "Peru", anio = 2021)

per2023 <- read_dta("/Users/brunopineda/Desktop/Elementos de la Tesis/Datos/Peru2023") %>%
  select(any_of(variablesutiles)) %>%
  mutate(pais = "Peru", anio = 2023)

# Uruguay

uru2019 <- read_dta("/Users/brunopineda/Desktop/Elementos de la Tesis/Datos/Uruguay2019") %>%
  select(any_of(variablesutiles)) %>%
  mutate(pais = "Uruguay", anio = 2019)

uru2021 <- read_dta("/Users/brunopineda/Desktop/Elementos de la Tesis/Datos/Uruguay2021") %>%
  select(any_of(variablesutiles)) %>%
  mutate(pais = "Uruguay", anio = 2021)

uru2023 <- read_dta("/Users/brunopineda/Desktop/Elementos de la Tesis/Datos/Uruguay2023") %>%
  select(any_of(variablesutiles)) %>%
  mutate(pais = "Uruguay", anio = 2023)

}

# Vemos la codificación

{

library(purrr)

bases <- list(
  arg2019 = arg2019,
  arg2021 = arg2021,
  arg2023 = arg2023,
  bol2019 = bol2019,
  bol2021 = bol2021,
  bol2023 = bol2023,
  bra2019 = bra2019,
  bra2021 = bra2021,
  bra2023 = bra2023,
  chi2019 = chi2019,
  chi2021 = chi2021,
  chi2023 = chi2023,
  col2018 = col2018,
  col2021 = col2021,
  col2023 = col2023,
  ecu2019 = ecu2019,
  ecu2021 = ecu2021,
  ecu2023 = ecu2023,
  mex2019 = mex2019,
  mex2021 = mex2021,
  mex2023 = mex2023,
  par2019 = par2019,
  par2021 = par2021,
  par2023 = par2023,
  per2019 = per2019,
  per2021 = per2021,
  per2023 = per2023,
  uru2019 = uru2019,
  uru2021 = uru2021,
  uru2023 = uru2023
)

resumen_vars <- map_dfr(names(bases), function(nombre) {
  df <- bases[[nombre]]
  map_dfr(variablesutiles, function(var) {
    if (var %in% names(df)) {
      valores <- unique(na.omit(df[[var]]))
      tibble(base = nombre, variable = var, valores = paste(sort(valores), collapse = ", "))
    } else {
      tibble(base = nombre, variable = var, valores = "NO PRESENTE")
    }
  })
})

}

# Recodificación de variables a nivel individual

{

# vb2 (si la persona votó o no en las últimas elecciones), datos solo disponibles en 2018/19 y 2023

# Argentina

arg2019 <- arg2019 %>%
  mutate(voto = case_when(
    vb2 == 1 ~ 1,
    vb2 == 2 ~ 0,
    TRUE ~ NA_real_
  ))

arg2023 <- arg2023 %>%
  mutate(voto = case_when(
    vb2 == 1 ~ 1,
    vb2 == 2 ~ 0,
    TRUE ~ NA_real_
  ))

# Uruguay

uru2019 <- uru2019 %>%
  mutate(voto = case_when(
    vb2 == 1 ~ 1,
    vb2 == 2 ~ 0,
    TRUE ~ NA_real_
  ))

uru2023 <- uru2023 %>%
  mutate(voto = case_when(
    vb2 == 1 ~ 1,
    vb2 == 2 ~ 0,
    TRUE ~ NA_real_
  ))

# Chile

chi2019 <- chi2019 %>%
  mutate(voto = case_when(
    vb2 == 1 ~ 1,
    vb2 == 2 ~ 0,
    TRUE ~ NA_real_
  ))

chi2023 <- chi2023 %>%
  mutate(voto = case_when(
    vb2 == 1 ~ 1,
    vb2 == 2 ~ 0,
    TRUE ~ NA_real_
  ))

# Paraguay

par2019 <- par2019 %>%
  mutate(voto = case_when(
    vb2 == 1 ~ 1,
    vb2 == 2 ~ 0,
    TRUE ~ NA_real_
  ))

par2023 <- par2023 %>%
  mutate(voto = case_when(
    vb2 == 1 ~ 1,
    vb2 == 2 ~ 0,
    TRUE ~ NA_real_
  ))

# Bolivia

bol2019 <- bol2019 %>%
  mutate(voto = case_when(
    vb2 == 1 ~ 1,
    vb2 == 2 ~ 0,
    TRUE ~ NA_real_
  ))

bol2023 <- bol2023 %>%
  mutate(voto = case_when(
    vb2 == 1 ~ 1,
    vb2 == 2 ~ 0,
    TRUE ~ NA_real_
  ))

# Brasil

bra2019 <- bra2019 %>%
  mutate(voto = case_when(
    vb2 == 1 ~ 1,
    vb2 == 2 ~ 0,
    TRUE ~ NA_real_
  ))

bra2023 <- bra2023 %>%
  mutate(voto = case_when(
    vb2 == 1 ~ 1,
    vb2 == 2 ~ 0,
    TRUE ~ NA_real_
  ))

# Perú

per2019 <- per2019 %>%
  mutate(voto = case_when(
    vb2 == 1 ~ 1,
    vb2 == 2 ~ 0,
    TRUE ~ NA_real_
  ))

per2023 <- per2023 %>%
  mutate(voto = case_when(
    vb2 == 1 ~ 1,
    vb2 == 2 ~ 0,
    TRUE ~ NA_real_
  ))

# Ecuador

ecu2019 <- ecu2019 %>%
  mutate(voto = case_when(
    vb2 == 1 ~ 1,
    vb2 == 2 ~ 0,
    TRUE ~ NA_real_
  ))

ecu2023 <- ecu2023 %>%
  mutate(voto = case_when(
    vb2 == 1 ~ 1,
    vb2 == 2 ~ 0,
    TRUE ~ NA_real_
  ))

# Colombia

col2018 <- col2018 %>%
  mutate(voto = case_when(
    vb2 == 1 ~ 1,
    vb2 == 2 ~ 0,
    TRUE ~ NA_real_
  ))

col2023 <- col2023 %>%
  mutate(voto = case_when(
    vb2 == 1 ~ 1,
    vb2 == 2 ~ 0,
    TRUE ~ NA_real_
  ))

# México

mex2019 <- mex2019 %>%
  mutate(voto = case_when(
    vb2 == 1 ~ 1,
    vb2 == 2 ~ 0,
    TRUE ~ NA_real_
  ))

mex2023 <- mex2023 %>%
  mutate(voto = case_when(
    vb2 == 1 ~ 1,
    vb2 == 2 ~ 0,
    TRUE ~ NA_real_
  ))

# exc2 (si la persona fue víctima de un soborno policial), datos disponibles en todos los años

# Argentina

arg2019 <- arg2019 %>% mutate(sobpol = exc2)
arg2021 <- arg2021 %>% mutate(sobpol = exc2)
arg2023 <- arg2023 %>% mutate(sobpol = exc2)

# Uruguay

uru2019 <- uru2019 %>% mutate(sobpol = exc2)
uru2021 <- uru2021 %>% mutate(sobpol = exc2)
uru2023 <- uru2023 %>% mutate(sobpol = exc2)

# Chile

chi2019 <- chi2019 %>% mutate(sobpol = exc2)
chi2021 <- chi2021 %>% mutate(sobpol = exc2)
chi2023 <- chi2023 %>% mutate(sobpol = exc2)

# Paraguay

par2019 <- par2019 %>% mutate(sobpol = exc2)
par2021 <- par2021 %>% mutate(sobpol = exc2)
par2023 <- par2023 %>% mutate(sobpol = exc2)

# Bolivia

bol2019 <- bol2019 %>% mutate(sobpol = exc2)
bol2021 <- bol2021 %>% mutate(sobpol = exc2)
bol2023 <- bol2023 %>% mutate(sobpol = exc2)

# Brasil

bra2019 <- bra2019 %>% mutate(sobpol = exc2)
bra2021 <- bra2021 %>% mutate(sobpol = exc2)
bra2023 <- bra2023 %>% mutate(sobpol = exc2)

# Perú

per2019 <- per2019 %>% mutate(sobpol = exc2)
per2021 <- per2021 %>% mutate(sobpol = exc2)
per2023 <- per2023 %>% mutate(sobpol = exc2)

# Ecuador

ecu2019 <- ecu2019 %>% mutate(sobpol = exc2)
ecu2021 <- ecu2021 %>% mutate(sobpol = exc2)
ecu2023 <- ecu2023 %>% mutate(sobpol = exc2)

# Colombia

col2018 <- col2018 %>% mutate(sobpol = exc2)
col2021 <- col2021 %>% mutate(sobpol = exc2)
col2023 <- col2023 %>% mutate(sobpol = exc2)

# México

mex2019 <- mex2019 %>% mutate(sobpol = exc2)
mex2021 <- mex2021 %>% mutate(sobpol = exc2)
mex2023 <- mex2023 %>% mutate(sobpol = exc2)

# exc6 (si la persona fue víctima de un soborno por emp. púb.), datos disponibles en todos los años

# Argentina

arg2019 <- arg2019 %>% mutate(sobpub = exc6)
arg2021 <- arg2021 %>% mutate(sobpub = exc6)
arg2023 <- arg2023 %>% mutate(sobpub = exc6)

# Uruguay

uru2019 <- uru2019 %>% mutate(sobpub = exc6)
uru2021 <- uru2021 %>% mutate(sobpub = exc6)
uru2023 <- uru2023 %>% mutate(sobpub = exc6)

# Chile

chi2019 <- chi2019 %>% mutate(sobpub = exc6)
chi2021 <- chi2021 %>% mutate(sobpub = exc6)
chi2023 <- chi2023 %>% mutate(sobpub = exc6)

# Paraguay

par2019 <- par2019 %>% mutate(sobpub = exc6)
par2021 <- par2021 %>% mutate(sobpub = exc6)
par2023 <- par2023 %>% mutate(sobpub = exc6)

# Bolivia

bol2019 <- bol2019 %>% mutate(sobpub = exc6)
bol2021 <- bol2021 %>% mutate(sobpub = exc6)
bol2023 <- bol2023 %>% mutate(sobpub = exc6)

# Brasil

bra2019 <- bra2019 %>% mutate(sobpub = exc6)
bra2021 <- bra2021 %>% mutate(sobpub = exc6)
bra2023 <- bra2023 %>% mutate(sobpub = exc6)

# Perú

per2019 <- per2019 %>% mutate(sobpub = exc6)
per2021 <- per2021 %>% mutate(sobpub = exc6)
per2023 <- per2023 %>% mutate(sobpub = exc6)

# Ecuador

ecu2019 <- ecu2019 %>% mutate(sobpub = exc6)
ecu2021 <- ecu2021 %>% mutate(sobpub = exc6)
ecu2023 <- ecu2023 %>% mutate(sobpub = exc6)

# Colombia

col2018 <- col2018 %>% mutate(sobpub = exc6)
col2021 <- col2021 %>% mutate(sobpub = exc6)
col2023 <- col2023 %>% mutate(sobpub = exc6)

# México

mex2019 <- mex2019 %>% mutate(sobpub = exc6)
mex2021 <- mex2021 %>% mutate(sobpub = exc6)
mex2023 <- mex2023 %>% mutate(sobpub = exc6)

# exc7 (indica cuán generalizada está la corrupción), datos solo disponibles en 2018/19 y 2023

# Argentina

arg2019 <- arg2019 %>% mutate(genperc = ifelse(exc7 %in% 1:4, 5 - exc7, NA))
arg2023 <- arg2023 %>% mutate(genperc = ifelse(exc7 %in% 1:4, 5 - exc7, NA))

# Uruguay

uru2019 <- uru2019 %>% mutate(genperc = ifelse(exc7 %in% 1:4, 5 - exc7, NA))
uru2023 <- uru2023 %>% mutate(genperc = ifelse(exc7 %in% 1:4, 5 - exc7, NA))

# Chile

chi2019 <- chi2019 %>% mutate(genperc = ifelse(exc7 %in% 1:4, 5 - exc7, NA))
chi2023 <- chi2023 %>% mutate(genperc = ifelse(exc7 %in% 1:4, 5 - exc7, NA))

# Paraguay (incluye la pregunta en todos los años)

par2019 <- par2019 %>% mutate(genperc = ifelse(exc7 %in% 1:4, 5 - exc7, NA))
par2021 <- par2021 %>% mutate(genperc = ifelse(exc7 %in% 1:4, 5 - exc7, NA))
par2023 <- par2023 %>% mutate(genperc = ifelse(exc7 %in% 1:4, 5 - exc7, NA))

# Bolivia

bol2019 <- bol2019 %>% mutate(genperc = ifelse(exc7 %in% 1:4, 5 - exc7, NA))
bol2023 <- bol2023 %>% mutate(genperc = ifelse(exc7 %in% 1:4, 5 - exc7, NA))

# Brasil

bra2019 <- bra2019 %>% mutate(genperc = ifelse(exc7 %in% 1:4, 5 - exc7, NA))
bra2023 <- bra2023 %>% mutate(genperc = ifelse(exc7 %in% 1:4, 5 - exc7, NA))

# Perú (incluye la pregunta en todos los años)

per2019 <- per2019 %>% mutate(genperc = ifelse(exc7 %in% 1:4, 5 - exc7, NA))
per2021 <- per2021 %>% mutate(genperc = ifelse(exc7 %in% 1:4, 5 - exc7, NA))
per2023 <- per2023 %>% mutate(genperc = ifelse(exc7 %in% 1:4, 5 - exc7, NA))

# Ecuador

ecu2019 <- ecu2019 %>% mutate(genperc = ifelse(exc7 %in% 1:4, 5 - exc7, NA))
ecu2023 <- ecu2023 %>% mutate(genperc = ifelse(exc7 %in% 1:4, 5 - exc7, NA))

# Colombia

col2018 <- col2018 %>% mutate(genperc = ifelse(exc7 %in% 1:4, 5 - exc7, NA))
col2023 <- col2023 %>% mutate(genperc = ifelse(exc7 %in% 1:4, 5 - exc7, NA))

# México

mex2019 <- mex2019 %>% mutate(genperc = ifelse(exc7 %in% 1:4, 5 - exc7, NA))
mex2023 <- mex2023 %>% mutate(genperc = ifelse(exc7 %in% 1:4, 5 - exc7, NA))

# exc7new (indica cuántos políticos son corruptos), datos disponibles en todos los años

# Argentina

arg2019 <- arg2019 %>% mutate(partperc = exc7new)
arg2021 <- arg2021 %>% mutate(partperc = exc7new)
arg2023 <- arg2023 %>% mutate(partperc = exc7new)

# Uruguay

uru2019 <- uru2019 %>% mutate(partperc = exc7new)
uru2021 <- uru2021 %>% mutate(partperc = exc7new)
uru2023 <- uru2023 %>% mutate(partperc = exc7new)

# Chile

chi2019 <- chi2019 %>% mutate(partperc = exc7new)
chi2021 <- chi2021 %>% mutate(partperc = exc7new)
chi2023 <- chi2023 %>% mutate(partperc = exc7new)

# Paraguay

par2019 <- par2019 %>% mutate(partperc = exc7new)
par2021 <- par2021 %>% mutate(partperc = exc7new)
par2023 <- par2023 %>% mutate(partperc = exc7new)

# Bolivia

bol2019 <- bol2019 %>% mutate(partperc = exc7new)
bol2021 <- bol2021 %>% mutate(partperc = exc7new)
bol2023 <- bol2023 %>% mutate(partperc = exc7new)

# Brasil

bra2019 <- bra2019 %>% mutate(partperc = exc7new)
bra2021 <- bra2021 %>% mutate(partperc = exc7new)
bra2023 <- bra2023 %>% mutate(partperc = exc7new)

# Perú

per2019 <- per2019 %>% mutate(partperc = exc7new)
per2021 <- per2021 %>% mutate(partperc = exc7new)
per2023 <- per2023 %>% mutate(partperc = exc7new)

# Ecuador

ecu2019 <- ecu2019 %>% mutate(partperc = exc7new)
ecu2021 <- ecu2021 %>% mutate(partperc = exc7new)
ecu2023 <- ecu2023 %>% mutate(partperc = exc7new)

# Colombia

col2018 <- col2018 %>% mutate(partperc = exc7new)
col2021 <- col2021 %>% mutate(partperc = exc7new)
col2023 <- col2023 %>% mutate(partperc = exc7new)

# México

mex2019 <- mex2019 %>% mutate(partperc = exc7new)
mex2021 <- mex2021 %>% mutate(partperc = exc7new)
mex2023 <- mex2023 %>% mutate(partperc = exc7new)

# prot3 (participación en protestas), datos irregulares

# Argentina (solo 2019)

arg2019 <- arg2019 %>% mutate(partprot = case_when(
  prot3 == 1 ~ 1,
  prot3 == 2 ~ 0,
  TRUE ~ NA_real_
))

# Uruguay (solo 2019)

uru2019 <- uru2019 %>% mutate(partprot = case_when(
  prot3 == 1 ~ 1,
  prot3 == 2 ~ 0,
  TRUE ~ NA_real_
))

# Chile (2019 y 2021)

chi2019 <- chi2019 %>% mutate(partprot = case_when(
  prot3 == 1 ~ 1,
  prot3 == 2 ~ 0,
  TRUE ~ NA_real_
))

chi2021 <- chi2021 %>% mutate(partprot = case_when(
  prot3 == 1 ~ 1,
  prot3 == 2 ~ 0,
  TRUE ~ NA_real_
))

# Paraguay (solo 2019)

par2019 <- par2019 %>% mutate(partprot = case_when(
  prot3 == 1 ~ 1,
  prot3 == 2 ~ 0,
  TRUE ~ NA_real_
))

# Bolivia (solo 2019)

bol2019 <- bol2019 %>% mutate(partprot = case_when(
  prot3 == 1 ~ 1,
  prot3 == 2 ~ 0,
  TRUE ~ NA_real_
))

# Brasil (solo 2019)

bra2019 <- bra2019 %>% mutate(partprot = case_when(
  prot3 == 1 ~ 1,
  prot3 == 2 ~ 0,
  TRUE ~ NA_real_
))

# Perú (2019 y 2023)

per2019 <- per2019 %>% mutate(partprot = case_when(
  prot3 == 1 ~ 1,
  prot3 == 2 ~ 0,
  TRUE ~ NA_real_
))

per2023 <- per2023 %>% mutate(partprot = case_when(
  prot3 == 1 ~ 1,
  prot3 == 2 ~ 0,
  TRUE ~ NA_real_
))

# Ecuador (solo 2019)

ecu2019 <- ecu2019 %>% mutate(partprot = case_when(
  prot3 == 1 ~ 1,
  prot3 == 2 ~ 0,
  TRUE ~ NA_real_
))

# Colombia (2018 y 2021)

col2018 <- col2018 %>% mutate(partprot = case_when(
  prot3 == 1 ~ 1,
  prot3 == 2 ~ 0,
  TRUE ~ NA_real_
))

col2021 <- col2021 %>% mutate(partprot = case_when(
  prot3 == 1 ~ 1,
  prot3 == 2 ~ 0,
  TRUE ~ NA_real_
))

# México (solo 2019)

mex2019 <- mex2019 %>% mutate(partprot = case_when(
  prot3 == 1 ~ 1,
  prot3 == 2 ~ 0,
  TRUE ~ NA_real_
))

# cp8 (participación en juntas/comités de mejora de la comunidad), datos solo disponibles en
# 2018/19 y 2023 salvo Perú, donde solo hay del año 2019

# Argentina

arg2019 <- arg2019 %>% mutate(juntas = ifelse(cp8 %in% 1:4, 5 - cp8, NA))
arg2023 <- arg2023 %>% mutate(juntas = ifelse(cp8 %in% 1:4, 5 - cp8, NA))

# Uruguay

uru2019 <- uru2019 %>% mutate(juntas = ifelse(cp8 %in% 1:4, 5 - cp8, NA))
uru2023 <- uru2023 %>% mutate(juntas = ifelse(cp8 %in% 1:4, 5 - cp8, NA))

# Chile

chi2019 <- chi2019 %>% mutate(juntas = ifelse(cp8 %in% 1:4, 5 - cp8, NA))
chi2023 <- chi2023 %>% mutate(juntas = ifelse(cp8 %in% 1:4, 5 - cp8, NA))

# Paraguay

par2019 <- par2019 %>% mutate(juntas = ifelse(cp8 %in% 1:4, 5 - cp8, NA))
par2023 <- par2023 %>% mutate(juntas = ifelse(cp8 %in% 1:4, 5 - cp8, NA))

# Bolivia

bol2019 <- bol2019 %>% mutate(juntas = ifelse(cp8 %in% 1:4, 5 - cp8, NA))
bol2023 <- bol2023 %>% mutate(juntas = ifelse(cp8 %in% 1:4, 5 - cp8, NA))

# Brasil

bra2019 <- bra2019 %>% mutate(juntas = ifelse(cp8 %in% 1:4, 5 - cp8, NA))
bra2023 <- bra2023 %>% mutate(juntas = ifelse(cp8 %in% 1:4, 5 - cp8, NA))

# Perú (solo 2019)

per2019 <- per2019 %>% mutate(juntas = ifelse(cp8 %in% 1:4, 5 - cp8, NA))

# Ecuador

ecu2019 <- ecu2019 %>% mutate(juntas = ifelse(cp8 %in% 1:4, 5 - cp8, NA))
ecu2023 <- ecu2023 %>% mutate(juntas = ifelse(cp8 %in% 1:4, 5 - cp8, NA))

# Colombia

col2018 <- col2018 %>% mutate(juntas = ifelse(cp8 %in% 1:4, 5 - cp8, NA))
col2023 <- col2023 %>% mutate(juntas = ifelse(cp8 %in% 1:4, 5 - cp8, NA))

# México

mex2019 <- mex2019 %>% mutate(juntas = ifelse(cp8 %in% 1:4, 5 - cp8, NA))
mex2023 <- mex2023 %>% mutate(juntas = ifelse(cp8 %in% 1:4, 5 - cp8, NA))

# q1, q1tb y q1tc_r (género), datos disponibles en todos los años

# Argentina

arg2019 <- arg2019 %>% mutate(genero = case_when(q1 == 1 ~ 1, q1 == 2 ~ 0, TRUE ~ NA_real_))
arg2021 <- arg2021 %>% mutate(genero = case_when(q1tb == 1 ~ 1, q1tb == 2 ~ 0, TRUE ~ NA_real_))
arg2023 <- arg2023 %>% mutate(genero = case_when(q1tc_r == 1 ~ 1, q1tc_r == 2 ~ 0, TRUE ~ NA_real_))

# Uruguay

uru2019 <- uru2019 %>% mutate(genero = case_when(q1 == 1 ~ 1, q1 == 2 ~ 0, TRUE ~ NA_real_))
uru2021 <- uru2021 %>% mutate(genero = case_when(q1tb == 1 ~ 1, q1tb == 2 ~ 0, TRUE ~ NA_real_))
uru2023 <- uru2023 %>% mutate(genero = case_when(q1tc_r == 1 ~ 1, q1tc_r == 2 ~ 0, TRUE ~ NA_real_))

# Chile

chi2019 <- chi2019 %>% mutate(genero = case_when(q1 == 1 ~ 1, q1 == 2 ~ 0, TRUE ~ NA_real_))
chi2021 <- chi2021 %>% mutate(genero = case_when(q1tb == 1 ~ 1, q1tb == 2 ~ 0, TRUE ~ NA_real_))
chi2023 <- chi2023 %>% mutate(genero = case_when(q1tc_r == 1 ~ 1, q1tc_r == 2 ~ 0, TRUE ~ NA_real_))

# Paraguay

par2019 <- par2019 %>% mutate(genero = case_when(q1 == 1 ~ 1, q1 == 2 ~ 0, TRUE ~ NA_real_))
par2021 <- par2021 %>% mutate(genero = case_when(q1tb == 1 ~ 1, q1tb == 2 ~ 0, TRUE ~ NA_real_))
par2023 <- par2023 %>% mutate(genero = case_when(q1tc_r == 1 ~ 1, q1tc_r == 2 ~ 0, TRUE ~ NA_real_))

# Bolivia

bol2019 <- bol2019 %>% mutate(genero = case_when(q1 == 1 ~ 1, q1 == 2 ~ 0, TRUE ~ NA_real_))
bol2021 <- bol2021 %>% mutate(genero = case_when(q1tb == 1 ~ 1, q1tb == 2 ~ 0, TRUE ~ NA_real_))
bol2023 <- bol2023 %>% mutate(genero = case_when(q1tc_r == 1 ~ 1, q1tc_r == 2 ~ 0, TRUE ~ NA_real_))

# Brasil

bra2019 <- bra2019 %>% mutate(genero = case_when(q1 == 1 ~ 1, q1 == 2 ~ 0, TRUE ~ NA_real_))
bra2021 <- bra2021 %>% mutate(genero = case_when(q1tb == 1 ~ 1, q1tb == 2 ~ 0, TRUE ~ NA_real_))
bra2023 <- bra2023 %>% mutate(genero = case_when(q1tc_r == 1 ~ 1, q1tc_r == 2 ~ 0, TRUE ~ NA_real_))

# Perú

per2019 <- per2019 %>% mutate(genero = case_when(q1 == 1 ~ 1, q1 == 2 ~ 0, TRUE ~ NA_real_))
per2021 <- per2021 %>% mutate(genero = case_when(q1tb == 1 ~ 1, q1tb == 2 ~ 0, TRUE ~ NA_real_))
per2023 <- per2023 %>% mutate(genero = case_when(q1tc_r == 1 ~ 1, q1tc_r == 2 ~ 0, TRUE ~ NA_real_))

# Ecuador

ecu2019 <- ecu2019 %>% mutate(genero = case_when(q1 == 1 ~ 1, q1 == 2 ~ 0, TRUE ~ NA_real_))
ecu2021 <- ecu2021 %>% mutate(genero = case_when(q1tb == 1 ~ 1, q1tb == 2 ~ 0, TRUE ~ NA_real_))
ecu2023 <- ecu2023 %>% mutate(genero = case_when(q1tc_r == 1 ~ 1, q1tc_r == 2 ~ 0, TRUE ~ NA_real_))

# Colombia

col2018 <- col2018 %>% mutate(genero = case_when(q1 == 1 ~ 1, q1 == 2 ~ 0, TRUE ~ NA_real_))
col2021 <- col2021 %>% mutate(genero = case_when(q1tb == 1 ~ 1, q1tb == 2 ~ 0, TRUE ~ NA_real_))
col2023 <- col2023 %>% mutate(genero = case_when(q1tc_r == 1 ~ 1, q1tc_r == 2 ~ 0, TRUE ~ NA_real_))

# México

mex2019 <- mex2019 %>% mutate(genero = case_when(q1 == 1 ~ 1, q1 == 2 ~ 0, TRUE ~ NA_real_))
mex2021 <- mex2021 %>% mutate(genero = case_when(q1tb == 1 ~ 1, q1tb == 2 ~ 0, TRUE ~ NA_real_))
mex2023 <- mex2023 %>% mutate(genero = case_when(q1tc_r == 1 ~ 1, q1tc_r == 2 ~ 0, TRUE ~ NA_real_))

# q2 (edad), datos disponibles en todos los años

# Argentina

arg2019 <- arg2019 %>% mutate(edad = q2)
arg2021 <- arg2021 %>% mutate(edad = q2)
arg2023 <- arg2023 %>% mutate(edad = q2)

# Uruguay

uru2019 <- uru2019 %>% mutate(edad = q2)
uru2021 <- uru2021 %>% mutate(edad = q2)
uru2023 <- uru2023 %>% mutate(edad = q2)

# Chile

chi2019 <- chi2019 %>% mutate(edad = q2)
chi2021 <- chi2021 %>% mutate(edad = q2)
chi2023 <- chi2023 %>% mutate(edad = q2)

# Paraguay

par2019 <- par2019 %>% mutate(edad = q2)
par2021 <- par2021 %>% mutate(edad = q2)
par2023 <- par2023 %>% mutate(edad = q2)

# Bolivia

bol2019 <- bol2019 %>% mutate(edad = q2)
bol2021 <- bol2021 %>% mutate(edad = q2)
bol2023 <- bol2023 %>% mutate(edad = q2)

# Brasil

bra2019 <- bra2019 %>% mutate(edad = q2)
bra2021 <- bra2021 %>% mutate(edad = q2)
bra2023 <- bra2023 %>% mutate(edad = q2)

# Perú

per2019 <- per2019 %>% mutate(edad = q2)
per2021 <- per2021 %>% mutate(edad = q2)
per2023 <- per2023 %>% mutate(edad = q2)

# Ecuador

ecu2019 <- ecu2019 %>% mutate(edad = q2)
ecu2021 <- ecu2021 %>% mutate(edad = q2)
ecu2023 <- ecu2023 %>% mutate(edad = q2)

# Colombia

col2018 <- col2018 %>% mutate(edad = q2)
col2021 <- col2021 %>% mutate(edad = q2)
col2023 <- col2023 %>% mutate(edad = q2)

# México

mex2019 <- mex2019 %>% mutate(edad = q2)
mex2021 <- mex2021 %>% mutate(edad = q2)
mex2023 <- mex2023 %>% mutate(edad = q2)

# ed, edr, edre (educación), datos disponibles para todos los años con re-codificación

# Años 2018/19 — Variable original: ed

recodificar_ed <- function(df) {
  df %>% mutate(educ = case_when(
    ed == 0 ~ 0,
    ed >= 1 & ed <= 6 ~ 1,
    ed >= 7 & ed <= 12 ~ 2,
    ed >= 13 ~ 3,
    TRUE ~ NA_real_
  ))
}

arg2019 <- recodificar_ed(arg2019)
uru2019 <- recodificar_ed(uru2019)
chi2019 <- recodificar_ed(chi2019)
par2019 <- recodificar_ed(par2019)
bol2019 <- recodificar_ed(bol2019)
bra2019 <- recodificar_ed(bra2019)
per2019 <- recodificar_ed(per2019)
ecu2019 <- recodificar_ed(ecu2019)
col2018 <- recodificar_ed(col2018)
mex2019 <- recodificar_ed(mex2019)

# Año 2021 — Variable original: edr (ya compatible)

recodificar_edr <- function(df) {
  df %>% mutate(educ = case_when(
    edr %in% 0:3 ~ edr,
    TRUE ~ NA_real_
  ))
}

arg2021 <- recodificar_edr(arg2021)
uru2021 <- recodificar_edr(uru2021)
chi2021 <- recodificar_edr(chi2021)
par2021 <- recodificar_edr(par2021)
bol2021 <- recodificar_edr(bol2021)
bra2021 <- recodificar_edr(bra2021)
per2021 <- recodificar_edr(per2021)
ecu2021 <- recodificar_edr(ecu2021)
col2021 <- recodificar_edr(col2021)
mex2021 <- recodificar_edr(mex2021)

# Año 2023 — Variable original: edre (escala 0 a 6)

recodificar_edre <- function(df) {
  df %>% mutate(educ = case_when(
    edre == 0 ~ 0,
    edre %in% 1:2 ~ 1,
    edre %in% 3:4 ~ 2,
    edre %in% 5:6 ~ 3,
    TRUE ~ NA_real_
  ))
}

arg2023 <- recodificar_edre(arg2023)
uru2023 <- recodificar_edre(uru2023)
chi2023 <- recodificar_edre(chi2023)
par2023 <- recodificar_edre(par2023)
bol2023 <- recodificar_edre(bol2023)
bra2023 <- recodificar_edre(bra2023)
per2023 <- recodificar_edre(per2023)
ecu2023 <- recodificar_edre(ecu2023)
col2023 <- recodificar_edre(col2023)
mex2023 <- recodificar_edre(mex2023)

# q10new, q10newt, q10inc (ingresos), datos disponibles para todos los años con re-codificación

# Año 2018/19: copiamos directamente q10new

recodificar_q10new <- function(df) {
  df %>% mutate(income = case_when(
    q10new %in% 0:16 ~ q10new,
    TRUE ~ NA_real_
  ))
}

# Año 2021: mapeamos q10newt a escala 0–16

recodificar_income_2021 <- function(df, cod_min) {
  df %>% mutate(income = case_when(
    q10newt == cod_min     ~ 1,
    q10newt == cod_min + 1 ~ 3,
    q10newt == cod_min + 2 ~ 5,
    q10newt == cod_min + 3 ~ 8,
    q10newt == cod_min + 4 ~ 13,
    TRUE ~ NA_real_
  ))
}

# Aplicamos a cada país y año

# 2018/19

arg2019 <- recodificar_q10new(arg2019)
uru2019 <- recodificar_q10new(uru2019)
chi2019 <- recodificar_q10new(chi2019)
par2019 <- recodificar_q10new(par2019)
bol2019 <- recodificar_q10new(bol2019)
bra2019 <- recodificar_q10new(bra2019)
per2019 <- recodificar_q10new(per2019)
ecu2019 <- recodificar_q10new(ecu2019)
col2018 <- recodificar_q10new(col2018)
mex2019 <- recodificar_q10new(mex2019)

# 2021

arg2021 <- recodificar_income_2021(arg2021, 171)
bol2021 <- recodificar_income_2021(bol2021, 101)
bra2021 <- recodificar_income_2021(bra2021, 151)
chi2021 <- recodificar_income_2021(chi2021, 131)
col2021 <- recodificar_income_2021(col2021, 81)
ecu2021 <- recodificar_income_2021(ecu2021, 91)
par2021 <- recodificar_income_2021(par2021, 121)
per2021 <- recodificar_income_2021(per2021, 111)
mex2021 <- recodificar_income_2021(mex2021, 11)
uru2021 <- recodificar_income_2021(uru2021, 141)

# Año 2023: q10inc ya es ordinal, llevamos escala de 1–15 a 1–16

# Argentina 2023

arg2023 <- arg2023 %>% mutate(income = case_when(
  q10inc %in% 1701:1715 ~ q10inc - 1700 + 1,
  TRUE ~ NA_real_
))

# Bolivia 2023

bol2023 <- bol2023 %>% mutate(income = case_when(
  q10inc %in% 1001:1015 ~ q10inc - 1000 + 1,
  TRUE ~ NA_real_
))

# Brasil 2023

bra2023 <- bra2023 %>% mutate(income = case_when(
  q10inc %in% 1501:1515 ~ q10inc - 1500 + 1,
  TRUE ~ NA_real_
))

# Chile 2023

chi2023 <- chi2023 %>% mutate(income = case_when(
  q10inc %in% 1301:1315 ~ q10inc - 1300 + 1,
  TRUE ~ NA_real_
))

# Colombia 2023

col2023 <- col2023 %>% mutate(income = case_when(
  q10inc %in% 801:815 ~ q10inc - 800 + 1,
  TRUE ~ NA_real_
))

# Ecuador 2023

ecu2023 <- ecu2023 %>% mutate(income = case_when(
  q10inc %in% 901:915 ~ q10inc - 900 + 1,
  TRUE ~ NA_real_
))

# México 2023

mex2023 <- mex2023 %>% mutate(income = case_when(
  q10inc %in% 101:115 ~ q10inc - 100 + 1,
  TRUE ~ NA_real_
))

# Paraguay 2023

par2023 <- par2023 %>% mutate(income = case_when(
  q10inc %in% 1201:1215 ~ q10inc - 1200 + 1,
  TRUE ~ NA_real_
))

# Perú 2023

per2023 <- per2023 %>% mutate(income = case_when(
  q10inc %in% 1101:1115 ~ q10inc - 1100 + 1,
  TRUE ~ NA_real_
))

# Uruguay 2023

uru2023 <- uru2023 %>% mutate(income = case_when(
  q10inc %in% 1401:1415 ~ q10inc - 1400 + 1,
  TRUE ~ NA_real_
))

# wf1 (ayuda gubernamental), datos disponibles en 2018/19 y 2023 (salvo México con nada)

# Año 2018/19

arg2019 <- arg2019 %>% mutate(govhelp = case_when(
  wf1 == 1 ~ 1,
  wf1 == 2 ~ 0,
  TRUE ~ NA_real_
))

uru2019 <- uru2019 %>% mutate(govhelp = case_when(
  wf1 == 1 ~ 1,
  wf1 == 2 ~ 0,
  TRUE ~ NA_real_
))

chi2019 <- chi2019 %>% mutate(govhelp = case_when(
  wf1 == 1 ~ 1,
  wf1 == 2 ~ 0,
  TRUE ~ NA_real_
))

par2019 <- par2019 %>% mutate(govhelp = case_when(
  wf1 == 1 ~ 1,
  wf1 == 2 ~ 0,
  TRUE ~ NA_real_
))

bol2019 <- bol2019 %>% mutate(govhelp = case_when(
  wf1 == 1 ~ 1,
  wf1 == 2 ~ 0,
  TRUE ~ NA_real_
))

bra2019 <- bra2019 %>% mutate(govhelp = case_when(
  wf1 == 1 ~ 1,
  wf1 == 2 ~ 0,
  TRUE ~ NA_real_
))

per2019 <- per2019 %>% mutate(govhelp = case_when(
  wf1 == 1 ~ 1,
  wf1 == 2 ~ 0,
  TRUE ~ NA_real_
))

ecu2019 <- ecu2019 %>% mutate(govhelp = case_when(
  wf1 == 1 ~ 1,
  wf1 == 2 ~ 0,
  TRUE ~ NA_real_
))

col2018 <- col2018 %>% mutate(govhelp = case_when(
  wf1 == 1 ~ 1,
  wf1 == 2 ~ 0,
  TRUE ~ NA_real_
))

# Año 2023

arg2023 <- arg2023 %>% mutate(govhelp = case_when(
  wf1 == 1 ~ 1,
  wf1 == 2 ~ 0,
  TRUE ~ NA_real_
))

uru2023 <- uru2023 %>% mutate(govhelp = case_when(
  wf1 == 1 ~ 1,
  wf1 == 2 ~ 0,
  TRUE ~ NA_real_
))

chi2023 <- chi2023 %>% mutate(govhelp = case_when(
  wf1 == 1 ~ 1,
  wf1 == 2 ~ 0,
  TRUE ~ NA_real_
))

par2023 <- par2023 %>% mutate(govhelp = case_when(
  wf1 == 1 ~ 1,
  wf1 == 2 ~ 0,
  TRUE ~ NA_real_
))

bol2023 <- bol2023 %>% mutate(govhelp = case_when(
  wf1 == 1 ~ 1,
  wf1 == 2 ~ 0,
  TRUE ~ NA_real_
))

bra2023 <- bra2023 %>% mutate(govhelp = case_when(
  wf1 == 1 ~ 1,
  wf1 == 2 ~ 0,
  TRUE ~ NA_real_
))

per2023 <- per2023 %>% mutate(govhelp = case_when(
  wf1 == 1 ~ 1,
  wf1 == 2 ~ 0,
  TRUE ~ NA_real_
))

ecu2023 <- ecu2023 %>% mutate(govhelp = case_when(
  wf1 == 1 ~ 1,
  wf1 == 2 ~ 0,
  TRUE ~ NA_real_
))

col2023 <- col2023 %>% mutate(govhelp = case_when(
  wf1 == 1 ~ 1,
  wf1 == 2 ~ 0,
  TRUE ~ NA_real_
))

# pol1 (interés en política), datos disponibles en 2018/19 y 2023 para todos los países

# 2018/19

arg2019 <- arg2019 %>% mutate(intpol = ifelse(pol1 %in% 1:4, 5 - pol1, NA))
uru2019 <- uru2019 %>% mutate(intpol = ifelse(pol1 %in% 1:4, 5 - pol1, NA))
chi2019 <- chi2019 %>% mutate(intpol = ifelse(pol1 %in% 1:4, 5 - pol1, NA))
par2019 <- par2019 %>% mutate(intpol = ifelse(pol1 %in% 1:4, 5 - pol1, NA))
bol2019 <- bol2019 %>% mutate(intpol = ifelse(pol1 %in% 1:4, 5 - pol1, NA))
bra2019 <- bra2019 %>% mutate(intpol = ifelse(pol1 %in% 1:4, 5 - pol1, NA))
per2019 <- per2019 %>% mutate(intpol = ifelse(pol1 %in% 1:4, 5 - pol1, NA))
ecu2019 <- ecu2019 %>% mutate(intpol = ifelse(pol1 %in% 1:4, 5 - pol1, NA))
col2018 <- col2018 %>% mutate(intpol = ifelse(pol1 %in% 1:4, 5 - pol1, NA))
mex2019 <- mex2019 %>% mutate(intpol = ifelse(pol1 %in% 1:4, 5 - pol1, NA))

# 2023

arg2023 <- arg2023 %>% mutate(intpol = ifelse(pol1 %in% 1:4, 5 - pol1, NA))
uru2023 <- uru2023 %>% mutate(intpol = ifelse(pol1 %in% 1:4, 5 - pol1, NA))
chi2023 <- chi2023 %>% mutate(intpol = ifelse(pol1 %in% 1:4, 5 - pol1, NA))
par2023 <- par2023 %>% mutate(intpol = ifelse(pol1 %in% 1:4, 5 - pol1, NA))
bol2023 <- bol2023 %>% mutate(intpol = ifelse(pol1 %in% 1:4, 5 - pol1, NA))
bra2023 <- bra2023 %>% mutate(intpol = ifelse(pol1 %in% 1:4, 5 - pol1, NA))
per2023 <- per2023 %>% mutate(intpol = ifelse(pol1 %in% 1:4, 5 - pol1, NA))
ecu2023 <- ecu2023 %>% mutate(intpol = ifelse(pol1 %in% 1:4, 5 - pol1, NA))
col2023 <- col2023 %>% mutate(intpol = ifelse(pol1 %in% 1:4, 5 - pol1, NA))
mex2023 <- mex2023 %>% mutate(intpol = ifelse(pol1 %in% 1:4, 5 - pol1, NA))

# gi0n (seguimiento de noticias), datos disponibles en todos los años

# 2018/19

arg2019 <- arg2019 %>% mutate(news = ifelse(gi0n %in% 1:5, 6 - gi0n, NA))
uru2019 <- uru2019 %>% mutate(news = ifelse(gi0n %in% 1:5, 6 - gi0n, NA))
chi2019 <- chi2019 %>% mutate(news = ifelse(gi0n %in% 1:5, 6 - gi0n, NA))
par2019 <- par2019 %>% mutate(news = ifelse(gi0n %in% 1:5, 6 - gi0n, NA))
bol2019 <- bol2019 %>% mutate(news = ifelse(gi0n %in% 1:5, 6 - gi0n, NA))
bra2019 <- bra2019 %>% mutate(news = ifelse(gi0n %in% 1:5, 6 - gi0n, NA))
per2019 <- per2019 %>% mutate(news = ifelse(gi0n %in% 1:5, 6 - gi0n, NA))
ecu2019 <- ecu2019 %>% mutate(news = ifelse(gi0n %in% 1:5, 6 - gi0n, NA))
col2018 <- col2018 %>% mutate(news = ifelse(gi0n %in% 1:5, 6 - gi0n, NA))
mex2019 <- mex2019 %>% mutate(news = ifelse(gi0n %in% 1:5, 6 - gi0n, NA))

# 2021

arg2021 <- arg2021 %>% mutate(news = ifelse(gi0n %in% 1:5, 6 - gi0n, NA))
uru2021 <- uru2021 %>% mutate(news = ifelse(gi0n %in% 1:5, 6 - gi0n, NA))
chi2021 <- chi2021 %>% mutate(news = ifelse(gi0n %in% 1:5, 6 - gi0n, NA))
par2021 <- par2021 %>% mutate(news = ifelse(gi0n %in% 1:5, 6 - gi0n, NA))
bol2021 <- bol2021 %>% mutate(news = ifelse(gi0n %in% 1:5, 6 - gi0n, NA))
bra2021 <- bra2021 %>% mutate(news = ifelse(gi0n %in% 1:5, 6 - gi0n, NA))
per2021 <- per2021 %>% mutate(news = ifelse(gi0n %in% 1:5, 6 - gi0n, NA))
ecu2021 <- ecu2021 %>% mutate(news = ifelse(gi0n %in% 1:5, 6 - gi0n, NA))
col2021 <- col2021 %>% mutate(news = ifelse(gi0n %in% 1:5, 6 - gi0n, NA))
mex2021 <- mex2021 %>% mutate(news = ifelse(gi0n %in% 1:5, 6 - gi0n, NA))

# 2023

arg2023 <- arg2023 %>% mutate(news = ifelse(gi0n %in% 1:5, 6 - gi0n, NA))
uru2023 <- uru2023 %>% mutate(news = ifelse(gi0n %in% 1:5, 6 - gi0n, NA))
chi2023 <- chi2023 %>% mutate(news = ifelse(gi0n %in% 1:5, 6 - gi0n, NA))
par2023 <- par2023 %>% mutate(news = ifelse(gi0n %in% 1:5, 6 - gi0n, NA))
bol2023 <- bol2023 %>% mutate(news = ifelse(gi0n %in% 1:5, 6 - gi0n, NA))
bra2023 <- bra2023 %>% mutate(news = ifelse(gi0n %in% 1:5, 6 - gi0n, NA))
per2023 <- per2023 %>% mutate(news = ifelse(gi0n %in% 1:5, 6 - gi0n, NA))
ecu2023 <- ecu2023 %>% mutate(news = ifelse(gi0n %in% 1:5, 6 - gi0n, NA))
col2023 <- col2023 %>% mutate(news = ifelse(gi0n %in% 1:5, 6 - gi0n, NA))
mex2023 <- mex2023 %>% mutate(news = ifelse(gi0n %in% 1:5, 6 - gi0n, NA))

# soct2 (situación económica del país), datos disponibles en 2018/19 y 2023 para todos los países

# 2018/19

arg2019 <- arg2019 %>% mutate(ecoperc = ifelse(soct2 %in% 1:3, 4 - soct2, NA))
uru2019 <- uru2019 %>% mutate(ecoperc = ifelse(soct2 %in% 1:3, 4 - soct2, NA))
chi2019 <- chi2019 %>% mutate(ecoperc = ifelse(soct2 %in% 1:3, 4 - soct2, NA))
par2019 <- par2019 %>% mutate(ecoperc = ifelse(soct2 %in% 1:3, 4 - soct2, NA))
bol2019 <- bol2019 %>% mutate(ecoperc = ifelse(soct2 %in% 1:3, 4 - soct2, NA))
bra2019 <- bra2019 %>% mutate(ecoperc = ifelse(soct2 %in% 1:3, 4 - soct2, NA))
per2019 <- per2019 %>% mutate(ecoperc = ifelse(soct2 %in% 1:3, 4 - soct2, NA))
ecu2019 <- ecu2019 %>% mutate(ecoperc = ifelse(soct2 %in% 1:3, 4 - soct2, NA))
col2018 <- col2018 %>% mutate(ecoperc = ifelse(soct2 %in% 1:3, 4 - soct2, NA))
mex2019 <- mex2019 %>% mutate(ecoperc = ifelse(soct2 %in% 1:3, 4 - soct2, NA))

# 2023

arg2023 <- arg2023 %>% mutate(ecoperc = ifelse(soct2 %in% 1:3, 4 - soct2, NA))
uru2023 <- uru2023 %>% mutate(ecoperc = ifelse(soct2 %in% 1:3, 4 - soct2, NA))
chi2023 <- chi2023 %>% mutate(ecoperc = ifelse(soct2 %in% 1:3, 4 - soct2, NA))
par2023 <- par2023 %>% mutate(ecoperc = ifelse(soct2 %in% 1:3, 4 - soct2, NA))
bol2023 <- bol2023 %>% mutate(ecoperc = ifelse(soct2 %in% 1:3, 4 - soct2, NA))
bra2023 <- bra2023 %>% mutate(ecoperc = ifelse(soct2 %in% 1:3, 4 - soct2, NA))
per2023 <- per2023 %>% mutate(ecoperc = ifelse(soct2 %in% 1:3, 4 - soct2, NA))
ecu2023 <- ecu2023 %>% mutate(ecoperc = ifelse(soct2 %in% 1:3, 4 - soct2, NA))
col2023 <- col2023 %>% mutate(ecoperc = ifelse(soct2 %in% 1:3, 4 - soct2, NA))
mex2023 <- mex2023 %>% mutate(ecoperc = ifelse(soct2 %in% 1:3, 4 - soct2, NA))

# idio2 (situación económica personal), datos disponibles en todos los años

# 2018/19

arg2019 <- arg2019 %>% mutate(ecopersperc = ifelse(idio2 %in% 1:3, 4 - idio2, NA))
uru2019 <- uru2019 %>% mutate(ecopersperc = ifelse(idio2 %in% 1:3, 4 - idio2, NA))
chi2019 <- chi2019 %>% mutate(ecopersperc = ifelse(idio2 %in% 1:3, 4 - idio2, NA))
par2019 <- par2019 %>% mutate(ecopersperc = ifelse(idio2 %in% 1:3, 4 - idio2, NA))
bol2019 <- bol2019 %>% mutate(ecopersperc = ifelse(idio2 %in% 1:3, 4 - idio2, NA))
bra2019 <- bra2019 %>% mutate(ecopersperc = ifelse(idio2 %in% 1:3, 4 - idio2, NA))
per2019 <- per2019 %>% mutate(ecopersperc = ifelse(idio2 %in% 1:3, 4 - idio2, NA))
ecu2019 <- ecu2019 %>% mutate(ecopersperc = ifelse(idio2 %in% 1:3, 4 - idio2, NA))
col2018 <- col2018 %>% mutate(ecopersperc = ifelse(idio2 %in% 1:3, 4 - idio2, NA))
mex2019 <- mex2019 %>% mutate(ecopersperc = ifelse(idio2 %in% 1:3, 4 - idio2, NA))

# 2021

arg2021 <- arg2021 %>% mutate(ecopersperc = ifelse(idio2 %in% 1:3, 4 - idio2, NA))
uru2021 <- uru2021 %>% mutate(ecopersperc = ifelse(idio2 %in% 1:3, 4 - idio2, NA))
chi2021 <- chi2021 %>% mutate(ecopersperc = ifelse(idio2 %in% 1:3, 4 - idio2, NA))
par2021 <- par2021 %>% mutate(ecopersperc = ifelse(idio2 %in% 1:3, 4 - idio2, NA))
bol2021 <- bol2021 %>% mutate(ecopersperc = ifelse(idio2 %in% 1:3, 4 - idio2, NA))
bra2021 <- bra2021 %>% mutate(ecopersperc = ifelse(idio2 %in% 1:3, 4 - idio2, NA))
per2021 <- per2021 %>% mutate(ecopersperc = ifelse(idio2 %in% 1:3, 4 - idio2, NA))
ecu2021 <- ecu2021 %>% mutate(ecopersperc = ifelse(idio2 %in% 1:3, 4 - idio2, NA))
col2021 <- col2021 %>% mutate(ecopersperc = ifelse(idio2 %in% 1:3, 4 - idio2, NA))
mex2021 <- mex2021 %>% mutate(ecopersperc = ifelse(idio2 %in% 1:3, 4 - idio2, NA))

# 2023

arg2023 <- arg2023 %>% mutate(ecopersperc = ifelse(idio2 %in% 1:3, 4 - idio2, NA))
uru2023 <- uru2023 %>% mutate(ecopersperc = ifelse(idio2 %in% 1:3, 4 - idio2, NA))
chi2023 <- chi2023 %>% mutate(ecopersperc = ifelse(idio2 %in% 1:3, 4 - idio2, NA))
par2023 <- par2023 %>% mutate(ecopersperc = ifelse(idio2 %in% 1:3, 4 - idio2, NA))
bol2023 <- bol2023 %>% mutate(ecopersperc = ifelse(idio2 %in% 1:3, 4 - idio2, NA))
bra2023 <- bra2023 %>% mutate(ecopersperc = ifelse(idio2 %in% 1:3, 4 - idio2, NA))
per2023 <- per2023 %>% mutate(ecopersperc = ifelse(idio2 %in% 1:3, 4 - idio2, NA))
ecu2023 <- ecu2023 %>% mutate(ecopersperc = ifelse(idio2 %in% 1:3, 4 - idio2, NA))
col2023 <- col2023 %>% mutate(ecopersperc = ifelse(idio2 %in% 1:3, 4 - idio2, NA))
mex2023 <- mex2023 %>% mutate(ecopersperc = ifelse(idio2 %in% 1:3, 4 - idio2, NA))

# it1 (confianza interpersonal), datos disponibles en todos los años

# 2018/19

arg2019 <- arg2019 %>% mutate(confint = ifelse(it1 %in% 1:4, 5 - it1, NA))
uru2019 <- uru2019 %>% mutate(confint = ifelse(it1 %in% 1:4, 5 - it1, NA))
chi2019 <- chi2019 %>% mutate(confint = ifelse(it1 %in% 1:4, 5 - it1, NA))
par2019 <- par2019 %>% mutate(confint = ifelse(it1 %in% 1:4, 5 - it1, NA))
bol2019 <- bol2019 %>% mutate(confint = ifelse(it1 %in% 1:4, 5 - it1, NA))
bra2019 <- bra2019 %>% mutate(confint = ifelse(it1 %in% 1:4, 5 - it1, NA))
per2019 <- per2019 %>% mutate(confint = ifelse(it1 %in% 1:4, 5 - it1, NA))
ecu2019 <- ecu2019 %>% mutate(confint = ifelse(it1 %in% 1:4, 5 - it1, NA))
col2018 <- col2018 %>% mutate(confint = ifelse(it1 %in% 1:4, 5 - it1, NA))
mex2019 <- mex2019 %>% mutate(confint = ifelse(it1 %in% 1:4, 5 - it1, NA))

# 2021

arg2021 <- arg2021 %>% mutate(confint = ifelse(it1 %in% 1:4, 5 - it1, NA))
uru2021 <- uru2021 %>% mutate(confint = ifelse(it1 %in% 1:4, 5 - it1, NA))
chi2021 <- chi2021 %>% mutate(confint = ifelse(it1 %in% 1:4, 5 - it1, NA))
par2021 <- par2021 %>% mutate(confint = ifelse(it1 %in% 1:4, 5 - it1, NA))
bol2021 <- bol2021 %>% mutate(confint = ifelse(it1 %in% 1:4, 5 - it1, NA))
bra2021 <- bra2021 %>% mutate(confint = ifelse(it1 %in% 1:4, 5 - it1, NA))
per2021 <- per2021 %>% mutate(confint = ifelse(it1 %in% 1:4, 5 - it1, NA))
ecu2021 <- ecu2021 %>% mutate(confint = ifelse(it1 %in% 1:4, 5 - it1, NA))
col2021 <- col2021 %>% mutate(confint = ifelse(it1 %in% 1:4, 5 - it1, NA))
mex2021 <- mex2021 %>% mutate(confint = ifelse(it1 %in% 1:4, 5 - it1, NA))

# 2023

arg2023 <- arg2023 %>% mutate(confint = ifelse(it1 %in% 1:4, 5 - it1, NA))
uru2023 <- uru2023 %>% mutate(confint = ifelse(it1 %in% 1:4, 5 - it1, NA))
chi2023 <- chi2023 %>% mutate(confint = ifelse(it1 %in% 1:4, 5 - it1, NA))
par2023 <- par2023 %>% mutate(confint = ifelse(it1 %in% 1:4, 5 - it1, NA))
bol2023 <- bol2023 %>% mutate(confint = ifelse(it1 %in% 1:4, 5 - it1, NA))
bra2023 <- bra2023 %>% mutate(confint = ifelse(it1 %in% 1:4, 5 - it1, NA))
per2023 <- per2023 %>% mutate(confint = ifelse(it1 %in% 1:4, 5 - it1, NA))
ecu2023 <- ecu2023 %>% mutate(confint = ifelse(it1 %in% 1:4, 5 - it1, NA))
col2023 <- col2023 %>% mutate(confint = ifelse(it1 %in% 1:4, 5 - it1, NA))
mex2023 <- mex2023 %>% mutate(confint = ifelse(it1 %in% 1:4, 5 - it1, NA))

# b2 (respeto por las instituciones), datos disponibles en todos los años 

# 2018/19

arg2019 <- arg2019 %>% mutate(respinst = b2)
uru2019 <- uru2019 %>% mutate(respinst = b2)
chi2019 <- chi2019 %>% mutate(respinst = b2)
par2019 <- par2019 %>% mutate(respinst = b2)
bol2019 <- bol2019 %>% mutate(respinst = b2)
bra2019 <- bra2019 %>% mutate(respinst = b2)
per2019 <- per2019 %>% mutate(respinst = b2)
ecu2019 <- ecu2019 %>% mutate(respinst = b2)
col2018 <- col2018 %>% mutate(respinst = b2)
mex2019 <- mex2019 %>% mutate(respinst = b2)

# 2021

arg2021 <- arg2021 %>% mutate(respinst = b2)
uru2021 <- uru2021 %>% mutate(respinst = b2)
chi2021 <- chi2021 %>% mutate(respinst = b2)
par2021 <- par2021 %>% mutate(respinst = b2)
bol2021 <- bol2021 %>% mutate(respinst = b2)
bra2021 <- bra2021 %>% mutate(respinst = b2)
per2021 <- per2021 %>% mutate(respinst = b2)
ecu2021 <- ecu2021 %>% mutate(respinst = b2)
col2021 <- col2021 %>% mutate(respinst = b2)
mex2021 <- mex2021 %>% mutate(respinst = b2)

# 2023

arg2023 <- arg2023 %>% mutate(respinst = b2)
uru2023 <- uru2023 %>% mutate(respinst = b2)
chi2023 <- chi2023 %>% mutate(respinst = b2)
par2023 <- par2023 %>% mutate(respinst = b2)
bol2023 <- bol2023 %>% mutate(respinst = b2)
bra2023 <- bra2023 %>% mutate(respinst = b2)
per2023 <- per2023 %>% mutate(respinst = b2)
ecu2023 <- ecu2023 %>% mutate(respinst = b2)
col2023 <- col2023 %>% mutate(respinst = b2)
mex2023 <- mex2023 %>% mutate(respinst = b2)

# ocup4a y colocup4a (si la persona trabaja o no), datos disponibles para todos los años

# 2018/19

arg2019 <- arg2019 %>% mutate(job = case_when(ocup4a == 1 ~ 1, ocup4a %in% 2:7 ~ 0, TRUE ~ NA_real_))
uru2019 <- uru2019 %>% mutate(job = case_when(ocup4a == 1 ~ 1, ocup4a %in% 2:7 ~ 0, TRUE ~ NA_real_))
chi2019 <- chi2019 %>% mutate(job = case_when(ocup4a == 1 ~ 1, ocup4a %in% 2:7 ~ 0, TRUE ~ NA_real_))
par2019 <- par2019 %>% mutate(job = case_when(ocup4a == 1 ~ 1, ocup4a %in% 2:7 ~ 0, TRUE ~ NA_real_))
bol2019 <- bol2019 %>% mutate(job = case_when(ocup4a == 1 ~ 1, ocup4a %in% 2:7 ~ 0, TRUE ~ NA_real_))
bra2019 <- bra2019 %>% mutate(job = case_when(ocup4a == 1 ~ 1, ocup4a %in% 2:7 ~ 0, TRUE ~ NA_real_))
per2019 <- per2019 %>% mutate(job = case_when(ocup4a == 1 ~ 1, ocup4a %in% 2:7 ~ 0, TRUE ~ NA_real_))
ecu2019 <- ecu2019 %>% mutate(job = case_when(ocup4a == 1 ~ 1, ocup4a %in% 2:7 ~ 0, TRUE ~ NA_real_))
mex2019 <- mex2019 %>% mutate(job = case_when(ocup4a == 1 ~ 1, ocup4a %in% 2:7 ~ 0, TRUE ~ NA_real_))
col2018 <- col2018 %>% mutate(job = case_when(colocup4a == 1 ~ 1, colocup4a %in% 2:8 ~ 0, TRUE ~ NA_real_))

# 2021

arg2021 <- arg2021 %>% mutate(job = case_when(ocup4a == 1 ~ 1, ocup4a %in% 2:7 ~ 0, TRUE ~ NA_real_))
uru2021 <- uru2021 %>% mutate(job = case_when(ocup4a == 1 ~ 1, ocup4a %in% 2:7 ~ 0, TRUE ~ NA_real_))
chi2021 <- chi2021 %>% mutate(job = case_when(ocup4a == 1 ~ 1, ocup4a %in% 2:7 ~ 0, TRUE ~ NA_real_))
par2021 <- par2021 %>% mutate(job = case_when(ocup4a == 1 ~ 1, ocup4a %in% 2:7 ~ 0, TRUE ~ NA_real_))
bol2021 <- bol2021 %>% mutate(job = case_when(ocup4a == 1 ~ 1, ocup4a %in% 2:7 ~ 0, TRUE ~ NA_real_))
bra2021 <- bra2021 %>% mutate(job = case_when(ocup4a == 1 ~ 1, ocup4a %in% 2:7 ~ 0, TRUE ~ NA_real_))
per2021 <- per2021 %>% mutate(job = case_when(ocup4a == 1 ~ 1, ocup4a %in% 2:7 ~ 0, TRUE ~ NA_real_))
ecu2021 <- ecu2021 %>% mutate(job = case_when(ocup4a == 1 ~ 1, ocup4a %in% 2:7 ~ 0, TRUE ~ NA_real_))
col2021 <- col2021 %>% mutate(job = case_when(ocup4a == 1 ~ 1, ocup4a %in% 2:7 ~ 0, TRUE ~ NA_real_))
mex2021 <- mex2021 %>% mutate(job = case_when(ocup4a == 1 ~ 1, ocup4a %in% 2:7 ~ 0, TRUE ~ NA_real_))

# 2023

arg2023 <- arg2023 %>% mutate(job = case_when(ocup4a == 1 ~ 1, ocup4a %in% 2:7 ~ 0, TRUE ~ NA_real_))
uru2023 <- uru2023 %>% mutate(job = case_when(ocup4a == 1 ~ 1, ocup4a %in% 2:7 ~ 0, TRUE ~ NA_real_))
chi2023 <- chi2023 %>% mutate(job = case_when(ocup4a == 1 ~ 1, ocup4a %in% 2:7 ~ 0, TRUE ~ NA_real_))
par2023 <- par2023 %>% mutate(job = case_when(ocup4a == 1 ~ 1, ocup4a %in% 2:7 ~ 0, TRUE ~ NA_real_))
bol2023 <- bol2023 %>% mutate(job = case_when(ocup4a == 1 ~ 1, ocup4a %in% 2:7 ~ 0, TRUE ~ NA_real_))
bra2023 <- bra2023 %>% mutate(job = case_when(ocup4a == 1 ~ 1, ocup4a %in% 2:7 ~ 0, TRUE ~ NA_real_))
per2023 <- per2023 %>% mutate(job = case_when(ocup4a == 1 ~ 1, ocup4a %in% 2:7 ~ 0, TRUE ~ NA_real_))
ecu2023 <- ecu2023 %>% mutate(job = case_when(ocup4a == 1 ~ 1, ocup4a %in% 2:7 ~ 0, TRUE ~ NA_real_))
col2023 <- col2023 %>% mutate(job = case_when(ocup4a == 1 ~ 1, ocup4a %in% 2:7 ~ 0, TRUE ~ NA_real_))
mex2023 <- mex2023 %>% mutate(job = case_when(ocup4a == 1 ~ 1, ocup4a %in% 2:7 ~ 0, TRUE ~ NA_real_))


}

# Integración de variables a nivel país

{

# PIB/per cápita

gdp_pc <- tibble::tibble(
  pais = c(
    "Argentina", "Argentina", "Argentina",
    "Bolivia", "Bolivia", "Bolivia",
    "Brasil", "Brasil", "Brasil",
    "Chile", "Chile", "Chile",
    "Colombia", "Colombia", "Colombia",
    "Ecuador", "Ecuador", "Ecuador",
    "Mexico", "Mexico", "Mexico",
    "Paraguay", "Paraguay", "Paraguay",
    "Peru", "Peru", "Peru",
    "Uruguay", "Uruguay", "Uruguay"
  ),
  anio = c(
    2019, 2021, 2023,
    2019, 2021, 2023,
    2019, 2021, 2023,
    2019, 2021, 2023,
    2018, 2021, 2023,
    2019, 2021, 2023,
    2019, 2021, 2023,
    2019, 2021, 2023,
    2019, 2021, 2023,
    2019, 2021, 2023
  ),
  gdp_pc = c(
    9955.97, 10738.02, 14187.48,
    3503.90, 3384.84, 3686.28,
    9029.83, 7972.54, 10294.87,
    14495.72, 16216.57, 17067.81,
    6816.97, 6222.62, 6947.36,
    6205.06, 6075.80, 6609.80,
    10369.56, 10314.05, 13790.02,
    5821.18, 5976.93, 6276.35,
    7037.01, 6826.97, 7906.59,
    18315.74, 17888.21, 22797.81
  )
)

# 2018/19
arg2019 <- left_join(arg2019, gdp_pc, by = c("pais", "anio"))
uru2019 <- left_join(uru2019, gdp_pc, by = c("pais", "anio"))
chi2019 <- left_join(chi2019, gdp_pc, by = c("pais", "anio"))
par2019 <- left_join(par2019, gdp_pc, by = c("pais", "anio"))
bol2019 <- left_join(bol2019, gdp_pc, by = c("pais", "anio"))
bra2019 <- left_join(bra2019, gdp_pc, by = c("pais", "anio"))
per2019 <- left_join(per2019, gdp_pc, by = c("pais", "anio"))
ecu2019 <- left_join(ecu2019, gdp_pc, by = c("pais", "anio"))
col2018 <- left_join(col2018, gdp_pc, by = c("pais", "anio"))
mex2019 <- left_join(mex2019, gdp_pc, by = c("pais", "anio"))

# 2021
arg2021 <- left_join(arg2021, gdp_pc, by = c("pais", "anio"))
uru2021 <- left_join(uru2021, gdp_pc, by = c("pais", "anio"))
chi2021 <- left_join(chi2021, gdp_pc, by = c("pais", "anio"))
par2021 <- left_join(par2021, gdp_pc, by = c("pais", "anio"))
bol2021 <- left_join(bol2021, gdp_pc, by = c("pais", "anio"))
bra2021 <- left_join(bra2021, gdp_pc, by = c("pais", "anio"))
per2021 <- left_join(per2021, gdp_pc, by = c("pais", "anio"))
ecu2021 <- left_join(ecu2021, gdp_pc, by = c("pais", "anio"))
col2021 <- left_join(col2021, gdp_pc, by = c("pais", "anio"))
mex2021 <- left_join(mex2021, gdp_pc, by = c("pais", "anio"))

# 2023
arg2023 <- left_join(arg2023, gdp_pc, by = c("pais", "anio"))
uru2023 <- left_join(uru2023, gdp_pc, by = c("pais", "anio"))
chi2023 <- left_join(chi2023, gdp_pc, by = c("pais", "anio"))
par2023 <- left_join(par2023, gdp_pc, by = c("pais", "anio"))
bol2023 <- left_join(bol2023, gdp_pc, by = c("pais", "anio"))
bra2023 <- left_join(bra2023, gdp_pc, by = c("pais", "anio"))
per2023 <- left_join(per2023, gdp_pc, by = c("pais", "anio"))
ecu2023 <- left_join(ecu2023, gdp_pc, by = c("pais", "anio"))
col2023 <- left_join(col2023, gdp_pc, by = c("pais", "anio"))
mex2023 <- left_join(mex2023, gdp_pc, by = c("pais", "anio"))

# Desempleo

desempleo <- tibble::tibble(
  pais = c(
    "Argentina", "Argentina", "Argentina",
    "Bolivia", "Bolivia", "Bolivia",
    "Brasil", "Brasil", "Brasil",
    "Chile", "Chile", "Chile",
    "Colombia", "Colombia", "Colombia",
    "Ecuador", "Ecuador", "Ecuador",
    "Mexico", "Mexico", "Mexico",
    "Paraguay", "Paraguay", "Paraguay",
    "Peru", "Peru", "Peru",
    "Uruguay", "Uruguay", "Uruguay"
  ),
  anio = c(
    2019, 2021, 2023,
    2019, 2021, 2023,
    2019, 2021, 2023,
    2019, 2021, 2023,
    2018, 2021, 2023,
    2019, 2021, 2023,
    2019, 2021, 2023,
    2019, 2021, 2023,
    2019, 2021, 2023,
    2019, 2021, 2023
  ),
  desempleo = c(
    9.843, 8.736, 6.139,
    3.682, 5.089, 3.024,
    11.936, 13.158, 7.947,
    7.493, 9.281, 9.013,
    9.36, 13.898, 9.594,
    3.812, 4.552, 3.51,
    3.477, 4.019, 2.765,
    6.595, 7.311, 5.794,
    3.379, 5.097, 4.899,
    8.836, 9.328, 8.355
  )
)

# 2018/19
arg2019 <- left_join(arg2019, desempleo, by = c("pais", "anio"))
uru2019 <- left_join(uru2019, desempleo, by = c("pais", "anio"))
chi2019 <- left_join(chi2019, desempleo, by = c("pais", "anio"))
par2019 <- left_join(par2019, desempleo, by = c("pais", "anio"))
bol2019 <- left_join(bol2019, desempleo, by = c("pais", "anio"))
bra2019 <- left_join(bra2019, desempleo, by = c("pais", "anio"))
per2019 <- left_join(per2019, desempleo, by = c("pais", "anio"))
ecu2019 <- left_join(ecu2019, desempleo, by = c("pais", "anio"))
col2018 <- left_join(col2018, desempleo, by = c("pais", "anio"))
mex2019 <- left_join(mex2019, desempleo, by = c("pais", "anio"))

# 2021
arg2021 <- left_join(arg2021, desempleo, by = c("pais", "anio"))
uru2021 <- left_join(uru2021, desempleo, by = c("pais", "anio"))
chi2021 <- left_join(chi2021, desempleo, by = c("pais", "anio"))
par2021 <- left_join(par2021, desempleo, by = c("pais", "anio"))
bol2021 <- left_join(bol2021, desempleo, by = c("pais", "anio"))
bra2021 <- left_join(bra2021, desempleo, by = c("pais", "anio"))
per2021 <- left_join(per2021, desempleo, by = c("pais", "anio"))
ecu2021 <- left_join(ecu2021, desempleo, by = c("pais", "anio"))
col2021 <- left_join(col2021, desempleo, by = c("pais", "anio"))
mex2021 <- left_join(mex2021, desempleo, by = c("pais", "anio"))

# 2023
arg2023 <- left_join(arg2023, desempleo, by = c("pais", "anio"))
uru2023 <- left_join(uru2023, desempleo, by = c("pais", "anio"))
chi2023 <- left_join(chi2023, desempleo, by = c("pais", "anio"))
par2023 <- left_join(par2023, desempleo, by = c("pais", "anio"))
bol2023 <- left_join(bol2023, desempleo, by = c("pais", "anio"))
bra2023 <- left_join(bra2023, desempleo, by = c("pais", "anio"))
per2023 <- left_join(per2023, desempleo, by = c("pais", "anio"))
ecu2023 <- left_join(ecu2023, desempleo, by = c("pais", "anio"))
col2023 <- left_join(col2023, desempleo, by = c("pais", "anio"))
mex2023 <- left_join(mex2023, desempleo, by = c("pais", "anio"))

# IDH

idh <- read.csv("/Users/brunopineda/Desktop/Elementos de la Tesis/Datos/idh excel.csv")

idh_filtrado <- idh %>%
  filter(
    Entity %in% c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", 
                  "Mexico", "Paraguay", "Peru", "Uruguay"),
    Year %in% c(2018, 2019, 2021, 2023)
  ) %>%
  select(pais = Entity, anio = Year, idh = `Human.Development.Index`)

idh_filtrado <- idh_filtrado %>%
  mutate(pais = ifelse(pais == "Brazil", "Brasil", pais))

# 2018/19
arg2019 <- left_join(arg2019, idh_filtrado, by = c("pais", "anio"))
uru2019 <- left_join(uru2019, idh_filtrado, by = c("pais", "anio"))
chi2019 <- left_join(chi2019, idh_filtrado, by = c("pais", "anio"))
par2019 <- left_join(par2019, idh_filtrado, by = c("pais", "anio"))
bol2019 <- left_join(bol2019, idh_filtrado, by = c("pais", "anio"))
bra2019 <- left_join(bra2019, idh_filtrado, by = c("pais", "anio"))
per2019 <- left_join(per2019, idh_filtrado, by = c("pais", "anio"))
ecu2019 <- left_join(ecu2019, idh_filtrado, by = c("pais", "anio"))
col2018 <- left_join(col2018, idh_filtrado, by = c("pais", "anio"))
mex2019 <- left_join(mex2019, idh_filtrado, by = c("pais", "anio"))

# 2021
arg2021 <- left_join(arg2021, idh_filtrado, by = c("pais", "anio"))
uru2021 <- left_join(uru2021, idh_filtrado, by = c("pais", "anio"))
chi2021 <- left_join(chi2021, idh_filtrado, by = c("pais", "anio"))
par2021 <- left_join(par2021, idh_filtrado, by = c("pais", "anio"))
bol2021 <- left_join(bol2021, idh_filtrado, by = c("pais", "anio"))
bra2021 <- left_join(bra2021, idh_filtrado, by = c("pais", "anio"))
per2021 <- left_join(per2021, idh_filtrado, by = c("pais", "anio"))
ecu2021 <- left_join(ecu2021, idh_filtrado, by = c("pais", "anio"))
col2021 <- left_join(col2021, idh_filtrado, by = c("pais", "anio"))
mex2021 <- left_join(mex2021, idh_filtrado, by = c("pais", "anio"))

# 2023
arg2023 <- left_join(arg2023, idh_filtrado, by = c("pais", "anio"))
uru2023 <- left_join(uru2023, idh_filtrado, by = c("pais", "anio"))
chi2023 <- left_join(chi2023, idh_filtrado, by = c("pais", "anio"))
par2023 <- left_join(par2023, idh_filtrado, by = c("pais", "anio"))
bol2023 <- left_join(bol2023, idh_filtrado, by = c("pais", "anio"))
bra2023 <- left_join(bra2023, idh_filtrado, by = c("pais", "anio"))
per2023 <- left_join(per2023, idh_filtrado, by = c("pais", "anio"))
ecu2023 <- left_join(ecu2023, idh_filtrado, by = c("pais", "anio"))
col2023 <- left_join(col2023, idh_filtrado, by = c("pais", "anio"))
mex2023 <- left_join(mex2023, idh_filtrado, by = c("pais", "anio"))

# Índice de democracia

di <- read.csv("/Users/brunopineda/Desktop/Elementos de la Tesis/Datos/indicedemocracia.csv")

di_filtrado <- di %>%
  filter(
    Entity %in% c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", 
                  "Mexico", "Paraguay", "Peru", "Uruguay"),
    Year %in% c(2018, 2019, 2021, 2023)
  ) %>%
  select(pais = Entity, anio = Year, democracia = `Democracy.score`)

di_filtrado <- di_filtrado %>%
  mutate(pais = ifelse(pais == "Brazil", "Brasil", pais))

# 2018/19
arg2019 <- left_join(arg2019, di_filtrado, by = c("pais", "anio"))
uru2019 <- left_join(uru2019, di_filtrado, by = c("pais", "anio"))
chi2019 <- left_join(chi2019, di_filtrado, by = c("pais", "anio"))
par2019 <- left_join(par2019, di_filtrado, by = c("pais", "anio"))
bol2019 <- left_join(bol2019, di_filtrado, by = c("pais", "anio"))
bra2019 <- left_join(bra2019, di_filtrado, by = c("pais", "anio"))
per2019 <- left_join(per2019, di_filtrado, by = c("pais", "anio"))
ecu2019 <- left_join(ecu2019, di_filtrado, by = c("pais", "anio"))
col2018 <- left_join(col2018, di_filtrado, by = c("pais", "anio"))
mex2019 <- left_join(mex2019, di_filtrado, by = c("pais", "anio"))

# 2021
arg2021 <- left_join(arg2021, di_filtrado, by = c("pais", "anio"))
uru2021 <- left_join(uru2021, di_filtrado, by = c("pais", "anio"))
chi2021 <- left_join(chi2021, di_filtrado, by = c("pais", "anio"))
par2021 <- left_join(par2021, di_filtrado, by = c("pais", "anio"))
bol2021 <- left_join(bol2021, di_filtrado, by = c("pais", "anio"))
bra2021 <- left_join(bra2021, di_filtrado, by = c("pais", "anio"))
per2021 <- left_join(per2021, di_filtrado, by = c("pais", "anio"))
ecu2021 <- left_join(ecu2021, di_filtrado, by = c("pais", "anio"))
col2021 <- left_join(col2021, di_filtrado, by = c("pais", "anio"))
mex2021 <- left_join(mex2021, di_filtrado, by = c("pais", "anio"))

# 2023
arg2023 <- left_join(arg2023, di_filtrado, by = c("pais", "anio"))
uru2023 <- left_join(uru2023, di_filtrado, by = c("pais", "anio"))
chi2023 <- left_join(chi2023, di_filtrado, by = c("pais", "anio"))
par2023 <- left_join(par2023, di_filtrado, by = c("pais", "anio"))
bol2023 <- left_join(bol2023, di_filtrado, by = c("pais", "anio"))
bra2023 <- left_join(bra2023, di_filtrado, by = c("pais", "anio"))
per2023 <- left_join(per2023, di_filtrado, by = c("pais", "anio"))
ecu2023 <- left_join(ecu2023, di_filtrado, by = c("pais", "anio"))
col2023 <- left_join(col2023, di_filtrado, by = c("pais", "anio"))
mex2023 <- left_join(mex2023, di_filtrado, by = c("pais", "anio"))

# Enforcement, basado en los trabajos de Singh (2011) y Barnes & Rangel (2013)

enforcement <- tibble::tibble(
  pais = c(
    "Argentina", "Argentina", "Argentina",
    "Bolivia",   "Bolivia",   "Bolivia",
    "Brasil",    "Brasil",    "Brasil",
    "Chile",     "Chile",     "Chile",
    "Colombia",  "Colombia",  "Colombia",
    "Ecuador",   "Ecuador",   "Ecuador",
    "Mexico",    "Mexico",    "Mexico",
    "Paraguay",  "Paraguay",  "Paraguay",
    "Peru",      "Peru",      "Peru",
    "Uruguay",   "Uruguay",   "Uruguay"
  ),
  anio = c(
    2019, 2021, 2023,
    2019, 2021, 2023,
    2019, 2021, 2023,
    2019, 2021, 2023,
    2018, 2021, 2023,
    2019, 2021, 2023,
    2019, 2021, 2023,
    2019, 2021, 2023,
    2019, 2021, 2023,
    2019, 2021, 2023
  ),
  enforcement = c(
    3, 3, 3,   # Argentina
    3, 3, 3,   # Bolivia
    2, 2, 2,   # Brasil
    0, 0, 1,   # Chile (voto obligatorio reinstalado en 2022 pero sin sanción clara)
    0, 0, 0,   # Colombia
    2, 2, 2,   # Ecuador
    0, 0, 0,   # México
    2, 2, 2,   # Paraguay
    2, 2, 2,   # Perú
    4, 4, 4    # Uruguay
  )
)

# 2018/19
arg2019 <- left_join(arg2019, enforcement, by = c("pais", "anio"))
uru2019 <- left_join(uru2019, enforcement, by = c("pais", "anio"))
chi2019 <- left_join(chi2019, enforcement, by = c("pais", "anio"))
par2019 <- left_join(par2019, enforcement, by = c("pais", "anio"))
bol2019 <- left_join(bol2019, enforcement, by = c("pais", "anio"))
bra2019 <- left_join(bra2019, enforcement, by = c("pais", "anio"))
per2019 <- left_join(per2019, enforcement, by = c("pais", "anio"))
ecu2019 <- left_join(ecu2019, enforcement, by = c("pais", "anio"))
col2018 <- left_join(col2018, enforcement, by = c("pais", "anio"))
mex2019 <- left_join(mex2019, enforcement, by = c("pais", "anio"))

# 2021
arg2021 <- left_join(arg2021, enforcement, by = c("pais", "anio"))
uru2021 <- left_join(uru2021, enforcement, by = c("pais", "anio"))
chi2021 <- left_join(chi2021, enforcement, by = c("pais", "anio"))
par2021 <- left_join(par2021, enforcement, by = c("pais", "anio"))
bol2021 <- left_join(bol2021, enforcement, by = c("pais", "anio"))
bra2021 <- left_join(bra2021, enforcement, by = c("pais", "anio"))
per2021 <- left_join(per2021, enforcement, by = c("pais", "anio"))
ecu2021 <- left_join(ecu2021, enforcement, by = c("pais", "anio"))
col2021 <- left_join(col2021, enforcement, by = c("pais", "anio"))
mex2021 <- left_join(mex2021, enforcement, by = c("pais", "anio"))

# 2023
arg2023 <- left_join(arg2023, enforcement, by = c("pais", "anio"))
uru2023 <- left_join(uru2023, enforcement, by = c("pais", "anio"))
chi2023 <- left_join(chi2023, enforcement, by = c("pais", "anio"))
par2023 <- left_join(par2023, enforcement, by = c("pais", "anio"))
bol2023 <- left_join(bol2023, enforcement, by = c("pais", "anio"))
bra2023 <- left_join(bra2023, enforcement, by = c("pais", "anio"))
per2023 <- left_join(per2023, enforcement, by = c("pais", "anio"))
ecu2023 <- left_join(ecu2023, enforcement, by = c("pais", "anio"))
col2023 <- left_join(col2023, enforcement, by = c("pais", "anio"))
mex2023 <- left_join(mex2023, enforcement, by = c("pais", "anio"))

}

# Integración de todas las bases de datos

{

all <- bind_rows(
  arg2019, arg2021, arg2023,
  uru2019, uru2021, uru2023,
  chi2019, chi2021, chi2023,
  par2019, par2021, par2023,
  bol2019, bol2021, bol2023,
  bra2019, bra2021, bra2023,
  per2019, per2021, per2023,
  ecu2019, ecu2021, ecu2023,
  col2018, col2021, col2023,
  mex2019, mex2021, mex2023
)

}

# Estadístico descriptivo

{

all %>%
  select(voto, sobpol, sobpub, genperc, partperc, partprot, juntas, genero, edad,
         educ, income, govhelp, intpol, news, ecoperc, ecopersperc, confint, respinst, job,
         gdp_pc, desempleo, idh, democracia, enforcement) %>%
  data.frame() %>%
  stargazer::stargazer(type = "text")

}

# Guardado de all

saveRDS(all, file = "allclean.RDS")
