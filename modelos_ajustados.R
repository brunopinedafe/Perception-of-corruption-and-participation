require(dplyr)
require(stargazer)
require(haven)
require(lme4)
library(stargazer)
library(fixest)

# Abro la base de datos

all <- readRDS("allclean.RDS")

# Creo columna con país año por las dudas pese a que ya hablamos que conviene usar ambos por separado

all <- all %>%
  mutate(pais_anio = paste(pais, as.character(anio), sep = "_"))

all$pais_anio <- as.factor(all$pais_anio)

# Chequeo las variables de interés

all %>%
  select(voto, sobpol, sobpub, genperc, partperc, partprot, juntas, genero, edad,
         educ, income, govhelp, intpol, news, ecoperc, ecopersperc, confint, respinst, job,
         gdp_pc, desempleo, idh, democracia, enforcement, pais, anio, pais_anio) %>%
  as_tibble() %>% print(all, width = Inf)

# Conversión de variables a nivel individual

all <- all %>%
  mutate(
    # genperc (percepción general de corrupción, 4 alternativas originales reducidas a 2)
    genperc_baja = ifelse(genperc %in% c(1, 2), 1, 0),
    genperc_alta = ifelse(genperc %in% c(3, 4), 1, 0),
    
    # partperc (percepción particular sobre cuántos politicos hay involucrados en corrupción,
    # 5 alternativas originales reducidas a 3)
    partperc_baja = ifelse(partperc %in% c(1, 2), 1, 0),
    partperc_media = ifelse(partperc == 3, 1, 0),
    partperc_alta = ifelse(partperc %in% c(4, 5), 1, 0),
    
    # juntas (participación en juntas, 4 alternativas originales reducidas a 2)
    juntas_baja = ifelse(juntas %in% c(1, 2), 1, 0),
    juntas_alto = ifelse(juntas %in% c(3, 4), 1, 0),
    
    # educ (nivel educativo alcanzado)
    educ_ninguna = ifelse(educ == 0, 1, 0),
    educ_primaria = ifelse(educ == 1, 1, 0),
    educ_secundaria = ifelse(educ == 2, 1, 0),
    educ_univoterc = ifelse(educ == 3, 1, 0),
    
    # intpol (interés en política, 4 alternativas originales reducidas a 2)
    intpol_bajo = ifelse(intpol %in% c(1, 2), 1, 0),
    intpol_alto = ifelse(intpol %in% c(3, 4), 1, 0),
    
    # news (seguimiento de noticias, 5 alternativas originales reducidas a 3)
    news_poco  = ifelse(news %in% c(1, 2), 1, 0),
    news_medio = ifelse(news == 3, 1, 0),
    news_alto  = ifelse(news %in% c(4, 5), 1, 0),
    
    # ecoperc (percepción personal de la economía a nivel país)
    ecoperc_peor = ifelse(ecoperc == 1, 1, 0),
    ecoperc_igual = ifelse(ecoperc == 2, 1, 0),
    ecoperc_mejor = ifelse(ecoperc == 3, 1, 0),
    
    # ecopersperc (percpeción personal de la economía a nivel propio/individual)
    ecopersperc_peor = ifelse(ecopersperc == 1, 1, 0),
    ecopersperc_igual = ifelse(ecopersperc == 2, 1, 0),
    ecopersperc_mejor = ifelse(ecopersperc == 3, 1, 0),
    
    # confint (confianza interpersonal, 4 alternativas originales reducidas a 2)
    confint_baja = ifelse(confint %in% c(1, 2), 1, 0),
    confint_alta = ifelse(confint %in% c(3, 4), 1, 0),
    
    # income (nivel de ingresos, 17 alternativas originales, agrupado en 5 rangos)
    income_bajo = ifelse(income %in% 1:4, 1, 0),
    income_medio_bajo = ifelse(income %in% 5:8, 1, 0),
    income_medio = ifelse(income %in% 9:12, 1, 0),
    income_medio_alto = ifelse(income %in% 13:15, 1, 0),
    income_alto = ifelse(income %in% 16:17, 1, 0),
    
    # respinst (nivel de respeto por las instituciones, rango de 7 alternativas originales reducidas a 3)
    respinst_bajo = ifelse(respinst %in% 1:2, 1, 0),
    respinst_medio = ifelse(respinst %in% 3:5, 1, 0),
    respinst_alto = ifelse(respinst %in% 6:7, 1, 0)
  )

# Conversión de variables a nivel país

# gdp_p_k (ajuste de gdp_pc para mejor interpetación)

all <- all %>%
  mutate(gdp_pc_k = gdp_pc / 1000)

# enforcement (5 niveles)

all <- all %>%
  mutate(
    enforcement_bajo = ifelse(enforcement == 0, 1, 0),
    enforcement_medio_bajo = ifelse(enforcement == 1, 1, 0),
    enforcement_medio = ifelse(enforcement == 2, 1, 0),
    enforcement_medio_alto = ifelse(enforcement == 3, 1, 0),
    enforcement_alto = ifelse(enforcement == 4, 1, 0)
  )

# Modelo jerárquico exploratorio 1 - VD: Voto, pocas VIs. Efectos fijos separados.

modelo_jerarquico1 <- glmer(voto ~ 
                             genperc_alta + edad + genero +
                             educ_primaria + educ_secundaria + educ_univoterc +
                             income_medio_bajo + income_medio + income_medio_alto + income_alto +
                             (1 | pais) + (1 | anio),
                           data = all,
                           family = binomial(link = "logit"),
                           control = glmerControl(optimizer = "bobyqa"))

summary(modelo_jerarquico1)

# Modelo jerárquico exploratorio 2 - VD: Voto, pocas VIs. Efectos fijos juntos.

modelo_jerarquico2 <- glmer(voto ~ 
                              genperc_alta + edad + genero +
                              educ_primaria + educ_secundaria + educ_univoterc +
                              income_medio_bajo + income_medio + income_medio_alto + income_alto +
                              (1 | pais_anio),
                            data = all,
                            family = binomial(link = "logit"),
                            control = glmerControl(optimizer = "bobyqa"))

summary(modelo_jerarquico2)

# Modelo jerárquico exploratorio 3 - VD: Voto, pocas VIs. Efectos fijos solo para país.

modelo_jerarquico3 <- glmer(voto ~ 
                              genperc_alta + edad + genero +
                              educ_primaria + educ_secundaria + educ_univoterc +
                              income_medio_bajo + income_medio + income_medio_alto + income_alto +
                              (1 | pais),
                            data = all,
                            family = binomial(link = "logit"),
                            control = glmerControl(optimizer = "bobyqa"))

summary(modelo_jerarquico3)

# Modelo jerárquico completo 1 - VD: Voto, VIs todas. Efectos fijos separados.

modelo_voto_final1 <- glmer(
  voto ~ 
    # Percepción de corrupción (base: genperc_baja; partperc_baja)
    genperc_alta +
    partperc_media + partperc_alta +
    
    # Experiencia de corrupción
    sobpol + sobpub +
    
    # Controles individuales
    edad + genero +
    educ_primaria + educ_secundaria + educ_univoterc +     # base: educ_ninguna
    income_medio_bajo + income_medio + income_medio_alto + income_alto +  # base: income_bajo
    govhelp +
    intpol_alto +                                           # base: intpol_bajo
    news_medio + news_alto +                                # base: news_poco
    ecoperc_igual + ecoperc_mejor +                         # base: ecoperc_peor
    ecopersperc_igual + ecopersperc_mejor +                 # base: ecopersperc_peor
    confint_alta +                                          # base: confint_baja
    respinst_medio + respinst_alto +                        # base: respinst_bajo
    job +
    
    # Variables de contexto
    gdp_pc_k + desempleo + idh + democracia +              # sacamos enforcement
    
    # Efecto aleatorio
    (1 | pais) + (1 | anio),
  
  data = all,
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa")
)

summary(modelo_voto_final1)

# Modelo jerárquico completo 2 - VD: Voto, VIs todas. Efectos fijos juntos.

modelo_voto_final2 <- glmer(
  voto ~ 
    # Percepción de corrupción (base: genperc_baja; partperc_baja)
    genperc_alta +
    partperc_media + partperc_alta +
    
    # Experiencia de corrupción
    sobpol + sobpub +
    
    # Controles individuales
    edad + genero +
    educ_primaria + educ_secundaria + educ_univoterc +     # base: educ_ninguna
    income_medio_bajo + income_medio + income_medio_alto + income_alto +  # base: income_bajo
    govhelp +
    intpol_alto +                                           # base: intpol_bajo
    news_medio + news_alto +                                # base: news_poco
    ecoperc_igual + ecoperc_mejor +                         # base: ecoperc_peor
    ecopersperc_igual + ecopersperc_mejor +                 # base: ecopersperc_peor
    confint_alta +                                          # base: confint_baja
    respinst_medio + respinst_alto +                        # base: respinst_bajo
    job +
    
    # Variables de contexto
    gdp_pc_k + desempleo + idh + democracia +              # sacamos enforcement
    
    # Efecto aleatorio
    (1 | pais_anio),
  
  data = all,
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa")
)

summary(modelo_voto_final2)

# Modelo jerárquico completo 3 - VD: Voto, VIs todas. Efectos fijos solo para país.

modelo_voto_final3 <- glmer(
  voto ~ 
    # Percepción de corrupción (base: genperc_baja; partperc_baja)
    genperc_alta +
    partperc_media + partperc_alta +
    
    # Experiencia de corrupción
    sobpol + sobpub +
    
    # Controles individuales
    edad + genero +
    educ_primaria + educ_secundaria + educ_univoterc +     # base: educ_ninguna
    income_medio_bajo + income_medio + income_medio_alto + income_alto +  # base: income_bajo
    govhelp +
    intpol_alto +                                           # base: intpol_bajo
    news_medio + news_alto +                                # base: news_poco
    ecoperc_igual + ecoperc_mejor +                         # base: ecoperc_peor
    ecopersperc_igual + ecopersperc_mejor +                 # base: ecopersperc_peor
    confint_alta +                                          # base: confint_baja
    respinst_medio + respinst_alto +                        # base: respinst_bajo
    job +
    
    # Variables de contexto
    gdp_pc_k + desempleo + idh + democracia +              # sacamos enforcement
    
    # Efecto aleatorio
    (1 | pais),
  
  data = all,
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa")
)

summary(modelo_voto_final3)
