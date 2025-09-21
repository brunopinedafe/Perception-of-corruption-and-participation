require(dplyr)
require(stargazer)
require(haven)
require(lme4)
library(performance)
library(fixest)
library(broom.mixed)

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

#Modelos para #voto

{
  
  # Modelo jerárquico completo 1 - VD: Voto, VIs todas. Efectos aleatorios separados.
  
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
    control = glmerControl(optimizer = "Nelder_Mead")
  )
  
  summary(modelo_voto_final1)
  
  # Modelo jerárquico completo 2 - VD: Voto, VIs todas. Efectos aleatorios juntos.
  
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
    control = glmerControl(optimizer = "Nelder_Mead")
  )
  
  summary(modelo_voto_final2)
  
  # Modelo jerárquico completo 3 - VD: Voto, VIs todas. Efectos aleatorios solo para país.
  
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
    control = glmerControl(optimizer = "Nelder_Mead")
  )
  
  summary(modelo_voto_final3)
  
  
  }

#Modelos para #protestas

{
  
  # Modelo jerárquico completo 1 - VD: Protestas, VIs todas. Efectos aleatorios separados.
  
  modelo_protestas_final1 <- glmer(
    partprot ~ 
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
    control = glmerControl(optimizer = "Nelder_Mead")
  )
  
  summary(modelo_protestas_final1)
  
  # Modelo jerárquico completo 2 - VD: Protestas, VIs todas. Efectos aleatorios juntos.
  
  modelo_protestas_final2 <- glmer(
    partprot ~ 
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
    control = glmerControl(optimizer = "Nelder_Mead")
  )
  
  summary(modelo_protestas_final2)
  
  # Modelo jerárquico completo 3 - VD: Protestas, VIs todas. Efectos aleatorios solo para país.
  
  modelo_protestas_final3 <- glmer(
    partprot ~ 
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
    control = glmerControl(optimizer="Nelder_Mead")
  )
  
  summary(modelo_protestas_final3)
  
  
}

#Modelos para #juntas

{
  
  # Modelo jerárquico completo 1 - VD: Juntas, VIs todas. Efectos aleatorios separados.
  
  modelo_juntas_final1 <- glmer(
    juntas_alto ~ 
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
    control = glmerControl(optimizer = "Nelder_Mead")
  )
  
  summary(modelo_juntas_final1)
  
  # Modelo jerárquico completo 2 - VD: Juntas, VIs todas. Efectos aleatorios juntos.
  
  modelo_juntas_final2 <- glmer(
    juntas_alto ~ 
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
    control = glmerControl(optimizer = "Nelder_Mead")
  )
  
  summary(modelo_juntas_final2)
  
  # Modelo jerárquico completo 3 - VD: Juntas, VIs todas. Efectos aleatorios solo para país.
  
  modelo_juntas_final3 <- glmer(
    juntas_alto ~ 
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
    control = glmerControl(optimizer = "Nelder_Mead")
  )
  
  summary(modelo_juntas_final3)
  
  
}

# Ajustes

# 1) Mira rangos/escala de los numéricos continuos

all %>%
  select(edad, gdp_pc_k, desempleo, idh, democracia) %>%
  summary()

# 2) Matriz de correlaciones de los contextuales (sospechosos)

all %>%
  select(gdp_pc_k, desempleo, idh, democracia) %>%
  cor(use = "pairwise.complete.obs")

# 3) VIF sobre un glm "sin efectos aleatorios" para ver colinealidad

m_vif <- glm(
  voto ~ genperc_alta + partperc_media + partperc_alta + sobpol + sobpub +
    edad + genero + educ_primaria + educ_secundaria + educ_univoterc +
    income_medio_bajo + income_medio + income_medio_alto + income_alto +
    govhelp + intpol_alto + news_medio + news_alto +
    ecoperc_igual + ecoperc_mejor + ecopersperc_igual + ecopersperc_mejor +
    confint_alta + respinst_medio + respinst_alto + job +
    gdp_pc_k + desempleo + idh + democracia,
  data = all, family = binomial()
)
performance::check_collinearity(m_vif)   # mira VIF>5 (o >10) como señal roja

all_z <- all %>%
  mutate(
    edad_z         = as.numeric(scale(edad)),
    gdp_pc_k_z     = as.numeric(scale(gdp_pc_k)),
    desempleo_z    = as.numeric(scale(desempleo)),
    idh_z          = as.numeric(scale(idh)),
    democracia_z   = as.numeric(scale(democracia))
  )

# Modelos para #voto ajustados

{
  
  # Modelo voto 1 ajustado
  
  modelo_voto_1z <- glmer(
    voto ~
      # Percepción de corrupción (base: *_baja)
      genperc_alta + partperc_media + partperc_alta +
      
      # Experiencia de corrupción
      sobpol + sobpub +
      
      # Controles individuales
      edad_z + genero +
      educ_primaria + educ_secundaria + educ_univoterc +
      income_medio_bajo + income_medio + income_medio_alto + income_alto +
      govhelp + intpol_alto + news_medio + news_alto +
      ecoperc_igual + ecoperc_mejor +
      ecopersperc_igual + ecopersperc_mejor +
      confint_alta + respinst_medio + respinst_alto + job +
      
      # Contexto (escalados)
      gdp_pc_k_z + desempleo_z + idh_z + democracia_z +
      
      # Efectos aleatorios separados
      (1 | pais) + (1 | anio),
    
    data = all_z, family = binomial(link = "logit"),
    control = glmerControl(optimizer = "bobyqa",
                           optCtrl = list(maxfun = 2e5))
  )
  
  summary(modelo_voto_1z)
  
  # Modelo voto 2 ajustado
  
  modelo_voto_2z <- glmer(
    voto ~
      # Percepción de corrupción
      genperc_alta + partperc_media + partperc_alta +
      
      # Experiencia de corrupción
      sobpol + sobpub +
      
      # Controles individuales
      edad_z + genero +
      educ_primaria + educ_secundaria + educ_univoterc +
      income_medio_bajo + income_medio + income_medio_alto + income_alto +
      govhelp + intpol_alto + news_medio + news_alto +
      ecoperc_igual + ecoperc_mejor + ecopersperc_igual + ecopersperc_mejor +
      confint_alta + respinst_medio + respinst_alto + job +
      
      # Contextuales escalados
      gdp_pc_k_z + desempleo_z + idh_z + democracia_z +
      
      # Efecto aleatorio conjunto
      (1 | pais_anio),
    
    data = all_z, family = binomial(link = "logit"),
    control = glmerControl(optimizer = "bobyqa",
                           optCtrl = list(maxfun = 2e5))
  )
  
  summary(modelo_voto_2z)
  
  # Modelo voto 3 ajustado
  
  modelo_voto_3z <- glmer(
    voto ~
      # Percepción de corrupción
      genperc_alta + partperc_media + partperc_alta +
      # Experiencia
      sobpol + sobpub +
      # Controles individuales
      edad_z + genero +
      educ_primaria + educ_secundaria + educ_univoterc +
      income_medio_bajo + income_medio + income_medio_alto + income_alto +
      govhelp + intpol_alto + news_medio + news_alto +
      ecoperc_igual + ecoperc_mejor +
      ecopersperc_igual + ecopersperc_mejor +
      confint_alta + respinst_medio + respinst_alto + job +
      # Contextuales (escalados)
      gdp_pc_k_z + desempleo_z + idh_z + democracia_z +
      # Efecto aleatorio solo país
      (1 | pais),
    
    data = all_z, family = binomial(link = "logit"),
    control = glmerControl(optimizer = "bobyqa",
                           optCtrl = list(maxfun = 2e5))
  )
  
  summary(modelo_voto_3z)
  
  }

# Modelos para #protestas ajustados

{
  
  # Modelo protesta 1 ajustado
  
  modelo_protestas_1z <- glmer(
    partprot ~ 
      genperc_alta + partperc_media + partperc_alta +
      sobpol + sobpub +
      edad_z + genero +
      educ_primaria + educ_secundaria + educ_univoterc +
      income_medio_bajo + income_medio + income_medio_alto + income_alto +
      govhelp + intpol_alto +
      news_medio + news_alto +
      ecoperc_igual + ecoperc_mejor +
      ecopersperc_igual + ecopersperc_mejor +
      confint_alta +
      respinst_medio + respinst_alto +
      job +
      gdp_pc_k_z + desempleo_z + idh_z + democracia_z +
      (1 | pais) + (1 | anio),
    data = all_z,
    family = binomial(link = "logit"),
    control = glmerControl(optimizer = "bobyqa",
                           optCtrl = list(maxfun = 2e5))
  )
  
  summary(modelo_protestas_1z)
  
  # Modelo protesta 2 ajustado
  
  modelo_protestas_2z <- glmer(
    partprot ~ 
      genperc_alta + partperc_media + partperc_alta +
      sobpol + sobpub +
      edad_z + genero +
      educ_primaria + educ_secundaria + educ_univoterc +
      income_medio_bajo + income_medio + income_medio_alto + income_alto +
      govhelp + intpol_alto +
      news_medio + news_alto +
      ecoperc_igual + ecoperc_mejor +
      ecopersperc_igual + ecopersperc_mejor +
      confint_alta +
      respinst_medio + respinst_alto +
      job +
      gdp_pc_k_z + desempleo_z + idh_z + democracia_z +
      (1 | pais_anio),
    data = all_z,
    family = binomial(link = "logit"),
    control = glmerControl(optimizer = "bobyqa",
                           optCtrl = list(maxfun = 2e5))
  )
  
  summary(modelo_protestas_2z)
  
  # Modelo protesta 3 ajustado
  
  modelo_protestas_3z <- glmer(
    partprot ~ 
      # Percepción de corrupción
      genperc_alta + partperc_media + partperc_alta +
      # Experiencia
      sobpol + sobpub +
      # Controles individuales
      edad_z + genero +
      educ_primaria + educ_secundaria + educ_univoterc +
      income_medio_bajo + income_medio + income_medio_alto + income_alto +
      govhelp + intpol_alto +
      news_medio + news_alto +
      ecoperc_igual + ecoperc_mejor +
      ecopersperc_igual + ecopersperc_mejor +
      confint_alta +
      respinst_medio + respinst_alto +
      job +
      # Contextuales (escalados)
      gdp_pc_k_z + desempleo_z + idh_z + democracia_z +
      # Efecto aleatorio
      (1 | pais),
    data = all_z,
    family = binomial(link = "logit"),
    control = glmerControl(optimizer = "bobyqa",
                           optCtrl = list(maxfun = 2e5))
  )
  
  summary(modelo_protestas_3z)
  
}

# Modelos para #juntas ajustados

{
  
  # Modelo juntas 1 ajustado
  
  modelo_juntas_1z <- glmer(
    juntas_alto ~ 
      # Percepción corrupción
      genperc_alta + partperc_media + partperc_alta +
      # Experiencia corrupción
      sobpol + sobpub +
      # Controles individuales
      edad_z + genero +
      educ_primaria + educ_secundaria + educ_univoterc +
      income_medio_bajo + income_medio + income_medio_alto + income_alto +
      govhelp + intpol_alto +
      news_medio + news_alto +
      ecoperc_igual + ecoperc_mejor +
      ecopersperc_igual + ecopersperc_mejor +
      confint_alta +
      respinst_medio + respinst_alto +
      job +
      # Contextuales escalados
      gdp_pc_k_z + desempleo_z + idh_z + democracia_z +
      # Efectos aleatorios
      (1 | pais) + (1 | anio),
    data = all_z,
    family = binomial(link = "logit"),
    control = glmerControl(optimizer = "bobyqa",
                           optCtrl = list(maxfun = 2e5))
  )
  
  summary(modelo_juntas_1z)  
  
  # Modelo juntas 2 ajustado
  
  modelo_juntas_2z <- glmer(
    juntas_alto ~ 
      # Percepción de corrupción
      genperc_alta + partperc_media + partperc_alta +
      # Experiencia
      sobpol + sobpub +
      # Controles individuales
      edad_z + genero +
      educ_primaria + educ_secundaria + educ_univoterc +
      income_medio_bajo + income_medio + income_medio_alto + income_alto +
      govhelp + intpol_alto +
      news_medio + news_alto +
      ecoperc_igual + ecoperc_mejor +
      ecopersperc_igual + ecopersperc_mejor +
      confint_alta +
      respinst_medio + respinst_alto +
      job +
      # Contextuales escalados
      gdp_pc_k_z + desempleo_z + idh_z + democracia_z +
      # Efecto aleatorio conjunto
      (1 | pais_anio),
    data = all_z,
    family = binomial(link = "logit"),
    control = glmerControl(optimizer = "bobyqa",
                           optCtrl = list(maxfun = 2e5))
  )
  
  summary(modelo_juntas_2z)
  
  # Modelo juntas 3 ajustado
  
  modelo_juntas_3z <- glmer(
    juntas_alto ~
      # Percepción de corrupción
      genperc_alta + partperc_media + partperc_alta +
      # Experiencia de corrupción
      sobpol + sobpub +
      # Controles individuales
      edad_z + genero +
      educ_primaria + educ_secundaria + educ_univoterc +
      income_medio_bajo + income_medio + income_medio_alto + income_alto +
      govhelp + intpol_alto +
      news_medio + news_alto +
      ecoperc_igual + ecoperc_mejor +
      ecopersperc_igual + ecopersperc_mejor +
      confint_alta + respinst_medio + respinst_alto + job +
      # Variables contextuales escaladas
      gdp_pc_k_z + desempleo_z + idh_z + democracia_z +
      # Efecto aleatorio
      (1 | pais),
    data = all_z,
    family = binomial(link = "logit"),
    control = glmerControl(optimizer = "bobyqa",
                           optCtrl = list(maxfun = 2e5))
  )
  
  summary(modelo_juntas_3z)
  
}

# Predicción percepción/voto

{
  
  ## ===== Predicción marginal (voto) con el template del mentor =====
  ## Requisitos: objeto del modelo en memoria: modelo_voto_2z
  
  require(lme4)
  require(arm)
  
  set.seed(123)
  
  ## 1) Tu modelo
  m <- modelo_voto_2z   # (glmer binomial, RE: 1 | pais_anio)
  
  ## 2) Simulaciones de betas (arm::sim)
  mod_sim <- arm::sim(m, n.sims = 5000)
  
  ## 3) Marco de predicción: variar SOLO la variable de interés (genperc_alta)
  ##    El resto de predictores se fija en la categoría base / 0
  coef_nms <- names(lme4::fixef(m))
  coef_nms_no_int <- setdiff(coef_nms, "(Intercept)")
  
  pred_frame <- as.data.frame(matrix(0, nrow = 2, ncol = length(coef_nms_no_int)))
  names(pred_frame) <- coef_nms_no_int
  pred_frame$genperc_alta <- c(0, 1)  # 0 = baja, 1 = alta
  
  ## 4) Matriz de diseño en el ORDEN del modelo
  X <- cbind("(Intercept)" = 1, as.matrix(pred_frame))
  X <- X[, coef_nms, drop = FALSE]
  
  ## 5) Predicción simulada (link logit -> prob)
  invlogit <- function(z) 1 / (1 + exp(-z))
  
  ## Ahora (matriz sims x p con los efectos fijos simulados):
  B <- as.matrix(mod_sim@fixef)
  
  XB <- X %*% t(B)                # n_pred x sims
  P  <- invlogit(XB)
  
  ## 6) Resumen (media e IC95%)
  fit <- apply(P, 1, mean)
  lwr <- apply(P, 1, quantile, 0.025)
  upr <- apply(P, 1, quantile, 0.975)
  
  ## ===== Apartado 7 (versión boxplot) =====
  library(ggplot2)
  library(tidyr)
  
  # P es una matriz de 2 x sims (filas = "Baja","Alta"; cols = simulaciones)
  # La pasamos a formato largo: una fila por simulación y categoría
  P_df <- as.data.frame(t(P))                  # sims x 2
  colnames(P_df) <- c("Baja","Alta")
  
  pred_long <- pivot_longer(
    P_df, 
    cols = everything(), 
    names_to = "x", 
    values_to = "p"
  )
  pred_long$x <- factor(pred_long$x, levels = c("Baja","Alta"))
  
  g <- ggplot(pred_long, aes(x = x, y = p)) +
    geom_boxplot(fill = "gray85", color = "black", alpha = 0.8, width = 0.55, outlier.alpha = 0.25) +
    stat_summary(fun = mean, geom = "point", color = "firebrick", size = 4) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(
      x = "Percepción general de corrupción",
      y = "Probabilidad de votar",
      title = "Probabilidad de votar según percepción de corrupción"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(b = 10)), 
      axis.title.x = element_text(margin = margin(t = 15), size = 14),
      axis.title.y = element_text(margin = margin(r = 15), size = 14),
      axis.text = element_text(size = 12),
      panel.grid.minor = element_blank()
    )
  
  print(g)
  
}

# Predicción sobpol/voto

{
  
  ## ===== Predicción marginal (voto) con el template del mentor =====
  ## Requisitos: objeto del modelo en memoria: modelo_voto_2z
  
  require(lme4)
  require(arm)
  
  set.seed(123)
  
  ## 1) Tu modelo
  m <- modelo_voto_2z   # (glmer binomial, RE: 1 | pais_anio)
  
  ## 2) Simulaciones de betas (arm::sim)
  mod_sim <- arm::sim(m, n.sims = 5000)
  
  ## 3) Marco de predicción: variar SOLO la variable de interés (sobpol)
  ##    El resto de predictores se fija en la categoría base / 0
  coef_nms        <- names(lme4::fixef(m))
  coef_nms_no_int <- setdiff(coef_nms, "(Intercept)")
  
  pred_frame <- as.data.frame(matrix(0, nrow = 2, ncol = length(coef_nms_no_int)))
  names(pred_frame) <- coef_nms_no_int
  pred_frame$sobpol <- c(0, 1)   # 0 = no, 1 = sí
  
  ## 4) Matriz de diseño en el ORDEN del modelo
  X <- cbind("(Intercept)" = 1, as.matrix(pred_frame))
  X <- X[, coef_nms, drop = FALSE]
  
  ## 5) Predicción simulada (link logit => prob)
  invlogit <- function(z) 1 / (1 + exp(-z))
  
  ## Ahora (matriz sims x p con los efectos fijos simulados):
  B  <- as.matrix(mod_sim@fixef)
  XB <- X %*% t(B)                 # n_pred x sims
  P  <- invlogit(XB)
  
  ## 6) Resumen (media e IC95%)
  fit <- apply(P, 1, mean)
  lwr <- apply(P, 1, quantile, 0.025)
  upr <- apply(P, 1, quantile, 0.975)
  
  ## ===== Apartado 7 (versión boxplot definitiva) =====
  library(ggplot2)
  library(tidyr)
  
  # P es una matriz de 2 x sims (filas = "No","Sí"; cols = simulaciones)
  # La pasamos a formato largo: una fila por simulación y categoría
  P_df <- as.data.frame(t(P))     # sims x 2
  colnames(P_df) <- c("No", "Sí")
  
  pred_long <- pivot_longer(
    P_df,
    cols = everything(),
    names_to = "x",
    values_to = "p"
  )
  
  pred_long$x <- factor(pred_long$x, levels = c("No", "Sí"))
  
  g <- ggplot(pred_long, aes(x = x, y = p)) +
    geom_boxplot(fill = "gray85", color = "black", alpha = 0.8, width = 0.55, outlier.alpha = 0.25) +
    stat_summary(fun = mean, geom = "point", color = "firebrick", size = 4) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(
      x = "Soborno por parte de policía (experiencia personal)",
      y = "Probabilidad de votar",
      title = "Probabilidad de votar según experiencia de soborno policial"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(b = 10)),
      axis.title.x = element_text(margin = margin(t = 15), size = 14),
      axis.title.y = element_text(margin = margin(r = 15), size = 14),
      axis.text = element_text(size = 12),
      panel.grid.minor = element_blank()
    )
  
  print(g)
  
}

# Predicción sobpub/voto

{
  
  ## ===== Predicción marginal (voto) con el template del mentor =====
  ## Requisitos: objeto del modelo en memoria: modelo_voto_2z
  
  require(lme4)
  require(arm)
  
  set.seed(123)
  
  ## 1) Tu modelo
  m <- modelo_voto_2z   # (glmer binomial, RE: 1 | pais_anio)
  
  ## 2) Simulaciones de betas (arm::sim)
  mod_sim <- arm::sim(m, n.sims = 5000)
  
  ## 3) Marco de predicción: variar SOLO la variable de interés (sobpub)
  ##    El resto de predictores se fija en la categoría base / 0
  coef_nms        <- names(lme4::fixef(m))
  coef_nms_no_int <- setdiff(coef_nms, "(Intercept)")
  
  pred_frame <- as.data.frame(matrix(0, nrow = 2, ncol = length(coef_nms_no_int)))
  names(pred_frame) <- coef_nms_no_int
  pred_frame$sobpub <- c(0, 1)   # 0 = no, 1 = sí
  
  ## 4) Matriz de diseño en el ORDEN del modelo
  X <- cbind("(Intercept)" = 1, as.matrix(pred_frame))
  X <- X[, coef_nms, drop = FALSE]
  
  ## 5) Predicción simulada (link logit => prob)
  invlogit <- function(z) 1 / (1 + exp(-z))
  
  ## Ahora (matriz sims x p con los efectos fijos simulados):
  B  <- as.matrix(mod_sim@fixef)
  XB <- X %*% t(B)                 # n_pred x sims
  P  <- invlogit(XB)
  
  ## 6) Resumen (media e IC95%)
  fit <- apply(P, 1, mean)
  lwr <- apply(P, 1, quantile, 0.025)
  upr <- apply(P, 1, quantile, 0.975)
  
  ## ===== Apartado 7 (versión boxplot definitiva) =====
  library(ggplot2)
  library(tidyr)
  
  # P es una matriz de 2 x sims (filas = "No","Sí"; cols = simulaciones)
  # La pasamos a formato largo: una fila por simulación y categoría
  P_df <- as.data.frame(t(P))     # sims x 2
  colnames(P_df) <- c("No", "Sí")
  
  pred_long <- pivot_longer(
    P_df,
    cols = everything(),
    names_to = "x",
    values_to = "p"
  )
  
  pred_long$x <- factor(pred_long$x, levels = c("No", "Sí"))
  
  g <- ggplot(pred_long, aes(x = x, y = p)) +
    geom_boxplot(fill = "gray85", color = "black", alpha = 0.8, width = 0.55, outlier.alpha = 0.25) +
    stat_summary(fun = mean, geom = "point", color = "firebrick", size = 4) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(
      x = "Soborno por parte de empleado/s público/s (experiencia personal)",
      y = "Probabilidad de votar",
      title = "Probabilidad de votar según experiencia de soborno emp. púb."
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(b = 10)),
      axis.title.x = element_text(margin = margin(t = 15), size = 14),
      axis.title.y = element_text(margin = margin(r = 15), size = 14),
      axis.text = element_text(size = 12),
      panel.grid.minor = element_blank()
    )
  
  print(g)
  
}

# Predicción percepción/protestas

{
  
  ## ===== Predicción marginal (protestas) con el template del mentor =====
  ## Requisitos: objeto del modelo en memoria: modelo_protestas_3z
  
  require(lme4)
  require(arm)
  
  set.seed(123)
  
  ## 1) Tu modelo
  m <- modelo_protestas_3z   # (glmer binomial, RE: 1 | pais)
  
  ## 2) Simulaciones de betas (arm::sim)
  mod_sim <- arm::sim(m, n.sims = 5000)
  
  ## 3) Marco de predicción: variar SOLO la variable de interés (genperc_alta)
  ##    El resto de predictores se fija en la categoría base / 0
  coef_nms        <- names(lme4::fixef(m))
  coef_nms_no_int <- setdiff(coef_nms, "(Intercept)")
  
  pred_frame <- as.data.frame(matrix(0, nrow = 2, ncol = length(coef_nms_no_int)))
  names(pred_frame) <- coef_nms_no_int
  pred_frame$genperc_alta <- c(0, 1)   # 0 = baja, 1 = alta
  
  ## 4) Matriz de diseño en el ORDEN del modelo
  X <- cbind("(Intercept)" = 1, as.matrix(pred_frame))
  X <- X[, coef_nms, drop = FALSE]
  
  ## 5) Predicción simulada (link logit => prob)
  invlogit <- function(z) 1 / (1 + exp(-z))
  
  ## Ahora (matriz sims x p con los efectos fijos simulados):
  B  <- as.matrix(mod_sim@fixef)
  XB <- X %*% t(B)                 # n_pred x sims
  P  <- invlogit(XB)
  
  ## 6) Resumen (media e IC95%)
  fit <- apply(P, 1, mean)
  lwr <- apply(P, 1, quantile, 0.025)
  upr <- apply(P, 1, quantile, 0.975)
  
  ## ===== Apartado 7 (versión boxplot definitiva) =====
  library(ggplot2)
  library(tidyr)
  
  # P es una matriz de 2 x sims (filas = "Baja","Alta"; cols = simulaciones)
  # La pasamos a formato largo: una fila por simulación y categoría
  P_df <- as.data.frame(t(P))       # sims x 2
  colnames(P_df) <- c("Baja", "Alta")
  
  pred_long <- pivot_longer(
    P_df,
    cols = everything(),
    names_to = "x",
    values_to = "p"
  )
  
  pred_long$x <- factor(pred_long$x, levels = c("Baja", "Alta"))
  
  g <- ggplot(pred_long, aes(x = x, y = p)) +
    geom_boxplot(fill = "gray85", color = "black", alpha = 0.8,
                 width = 0.55, outlier.alpha = 0.25) +
    stat_summary(fun = mean, geom = "point", color = "firebrick", size = 4) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(
      x = "Percepción general de corrupción",
      y = "Probabilidad de participar en protestas",
      title = "Probabilidad de protestar según percepción de corrupción"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(b = 10)),
      axis.title.x = element_text(margin = margin(t = 15), size = 14),
      axis.title.y = element_text(margin = margin(r = 15), size = 14),
      axis.text = element_text(size = 12),
      panel.grid.minor = element_blank()
    )
  
  print(g)
  
}

# Predicción sobpol/protestas

{
  
  ## ===== Predicción marginal (protestas) con el template del mentor =====
  ## Requisitos: objeto del modelo en memoria: modelo_protestas_3z
  
  require(lme4)
  require(arm)
  
  set.seed(123)
  
  ## 1) Tu modelo
  m <- modelo_protestas_3z   # (glmer binomial, RE: 1 | pais)
  
  ## 2) Simulaciones de betas (arm::sim)
  mod_sim <- arm::sim(m, n.sims = 5000)
  
  ## 3) Marco de predicción: variar SOLO la variable de interés (sobpol)
  ##    El resto de predictores se fija en la categoría base / 0
  coef_nms        <- names(lme4::fixef(m))
  coef_nms_no_int <- setdiff(coef_nms, "(Intercept)")
  
  pred_frame <- as.data.frame(matrix(0, nrow = 2, ncol = length(coef_nms_no_int)))
  names(pred_frame) <- coef_nms_no_int
  pred_frame$sobpol <- c(0, 1)   # 0 = no, 1 = sí
  
  ## 4) Matriz de diseño en el ORDEN del modelo
  X <- cbind("(Intercept)" = 1, as.matrix(pred_frame))
  X <- X[, coef_nms, drop = FALSE]
  
  ## 5) Predicción simulada (link logit => prob)
  invlogit <- function(z) 1 / (1 + exp(-z))
  
  ## Ahora (matriz sims x p con los efectos fijos simulados):
  B  <- as.matrix(mod_sim@fixef)
  XB <- X %*% t(B)                 # n_pred x sims
  P  <- invlogit(XB)
  
  ## 6) Resumen (media e IC95%)
  fit <- apply(P, 1, mean)
  lwr <- apply(P, 1, quantile, 0.025)
  upr <- apply(P, 1, quantile, 0.975)
  
  ## ===== Apartado 7 (versión boxplot definitiva) =====
  library(ggplot2)
  library(tidyr)
  
  # P es una matriz de 2 x sims (filas = "No","Sí"; cols = simulaciones)
  P_df <- as.data.frame(t(P))          # sims x 2
  colnames(P_df) <- c("No", "Sí")
  
  pred_long <- pivot_longer(
    P_df,
    cols = everything(),
    names_to = "x",
    values_to = "p"
  )
  
  pred_long$x <- factor(pred_long$x, levels = c("No", "Sí"))
  
  g <- ggplot(pred_long, aes(x = x, y = p)) +
    geom_boxplot(fill = "gray85", color = "black", alpha = 0.8,
                 width = 0.55, outlier.alpha = 0.25) +
    stat_summary(fun = mean, geom = "point", color = "firebrick", size = 4) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(
      x = "Soborno por parte de policía (experiencia personal)",
      y = "Probabilidad de participar en protestas",
      title = "Probabilidad de protestar según experiencia de soborno policial"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(b = 10)),
      axis.title.x = element_text(margin = margin(t = 15), size = 14),
      axis.title.y = element_text(margin = margin(r = 15), size = 14),
      axis.text = element_text(size = 12),
      panel.grid.minor = element_blank()
    )
  
  print(g)
  
}

# Predicción sobpub/protestas

{
  
  ## ===== Predicción marginal (protestas) con el template del mentor =====
  ## Requisitos: objeto del modelo en memoria: modelo_protestas_3z
  
  require(lme4)
  require(arm)
  
  set.seed(123)
  
  ## 1) Tu modelo
  m <- modelo_protestas_3z   # (glmer binomial, RE: 1 | pais)
  
  ## 2) Simulaciones de betas (arm::sim)
  mod_sim <- arm::sim(m, n.sims = 5000)
  
  ## 3) Marco de predicción: variar SOLO la variable de interés (sobpub)
  ##    El resto de predictores se fija en la categoría base / 0
  coef_nms        <- names(lme4::fixef(m))
  coef_nms_no_int <- setdiff(coef_nms, "(Intercept)")
  
  pred_frame <- as.data.frame(matrix(0, nrow = 2, ncol = length(coef_nms_no_int)))
  names(pred_frame) <- coef_nms_no_int
  pred_frame$sobpub <- c(0, 1)   # 0 = no, 1 = sí
  
  ## 4) Matriz de diseño en el ORDEN del modelo
  X <- cbind("(Intercept)" = 1, as.matrix(pred_frame))
  X <- X[, coef_nms, drop = FALSE]
  
  ## 5) Predicción simulada (link logit => prob)
  invlogit <- function(z) 1 / (1 + exp(-z))
  
  ## Ahora (matriz sims x p con los efectos fijos simulados):
  B  <- as.matrix(mod_sim@fixef)
  XB <- X %*% t(B)                 # n_pred x sims
  P  <- invlogit(XB)
  
  ## 6) Resumen (media e IC95%)
  fit <- apply(P, 1, mean)
  lwr <- apply(P, 1, quantile, 0.025)
  upr <- apply(P, 1, quantile, 0.975)
  
  ## ===== Apartado 7 (versión boxplot definitiva) =====
  library(ggplot2)
  library(tidyr)
  
  # P es una matriz de 2 x sims (filas = "No","Sí"; cols = simulaciones)
  P_df <- as.data.frame(t(P))     # sims x 2
  colnames(P_df) <- c("No", "Sí")
  
  pred_long <- pivot_longer(
    P_df,
    cols = everything(),
    names_to = "x",
    values_to = "p"
  )
  
  pred_long$x <- factor(pred_long$x, levels = c("No", "Sí"))
  
  g <- ggplot(pred_long, aes(x = x, y = p)) +
    geom_boxplot(fill = "gray85", color = "black", alpha = 0.8,
                 width = 0.55, outlier.alpha = 0.25) +
    stat_summary(fun = mean, geom = "point", color = "firebrick", size = 4) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(
      x = "Soborno por parte de empleado/s público/s (experiencia personal)",
      y = "Probabilidad de participar en protestas",
      title = "Probabilidad de protestar según experiencia de soborno emp. púb."
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(b = 10)),
      axis.title.x = element_text(margin = margin(t = 15), size = 14),
      axis.title.y = element_text(margin = margin(r = 15), size = 14),
      axis.text = element_text(size = 12),
      panel.grid.minor = element_blank()
    )
  
  print(g)
  
  
}

# Predicción percepción/juntas

{
  
  ## ===== Predicción marginal (juntas) con el template del mentor =====
  ## Requisitos: objeto del modelo en memoria: modelo_juntas_2z
  
  require(lme4)
  require(arm)
  
  set.seed(123)
  
  ## 1) Tu modelo
  m <- modelo_juntas_2z   # (glmer binomial, RE: 1 | pais_anio)
  
  ## 2) Simulaciones de betas (arm::sim)
  mod_sim <- arm::sim(m, n.sims = 5000)
  
  ## 3) Marco de predicción: variar SOLO la variable de interés (genperc_alta)
  ##    El resto de predictores se fija en la categoría base / 0
  coef_nms        <- names(lme4::fixef(m))
  coef_nms_no_int <- setdiff(coef_nms, "(Intercept)")
  
  pred_frame <- as.data.frame(matrix(0, nrow = 2, ncol = length(coef_nms_no_int)))
  names(pred_frame) <- coef_nms_no_int
  pred_frame$genperc_alta <- c(0, 1)   # 0 = baja, 1 = alta
  
  ## 4) Matriz de diseño en el ORDEN del modelo
  X <- cbind("(Intercept)" = 1, as.matrix(pred_frame))
  X <- X[, coef_nms, drop = FALSE]
  
  ## 5) Predicción simulada (link logit => prob)
  invlogit <- function(z) 1 / (1 + exp(-z))
  B  <- as.matrix(mod_sim@fixef)
  XB <- X %*% t(B)                 # n_pred x sims
  P  <- invlogit(XB)
  
  ## 6) Resumen (media e IC95%) — opcional para tabla
  fit <- apply(P, 1, mean)
  lwr <- apply(P, 1, quantile, 0.025)
  upr <- apply(P, 1, quantile, 0.975)
  
  ## ===== Apartado 7 (versión boxplot definitiva) =====
  library(ggplot2)
  library(tidyr)
  
  # P es una matriz de 2 x sims (filas = "Baja","Alta"; cols = simulaciones)
  P_df <- as.data.frame(t(P))      # sims x 2
  colnames(P_df) <- c("Baja", "Alta")
  
  pred_long <- pivot_longer(
    P_df,
    cols = everything(),
    names_to = "x",
    values_to = "p"
  )
  
  pred_long$x <- factor(pred_long$x, levels = c("Baja", "Alta"))
  
  g <- ggplot(pred_long, aes(x = x, y = p)) +
    geom_boxplot(fill = "gray85", color = "black", alpha = 0.8,
                 width = 0.55, outlier.alpha = 0.25) +
    stat_summary(fun = mean, geom = "point", color = "firebrick", size = 4) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(
      x = "Percepción general de corrupción",
      y = "Probabilidad de participar en juntas barriales",
      title = "Probabilidad de participar en juntas según percepción de corrupción"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(b = 10)),
      axis.title.x = element_text(margin = margin(t = 15), size = 14),
      axis.title.y = element_text(margin = margin(r = 15), size = 14),
      axis.text = element_text(size = 12),
      panel.grid.minor = element_blank()
    )
  
  print(g)
  
}

# Predicción sobpol/juntas

{
  
  ## ===== Predicción marginal (juntas) con el template del mentor =====
  ## Requisitos: objeto del modelo en memoria: modelo_juntas_2z
  
  require(lme4)
  require(arm)
  
  set.seed(123)
  
  ## 1) Tu modelo
  m <- modelo_juntas_2z   # (glmer binomial, RE: 1 | pais_anio)
  
  ## 2) Simulaciones de betas (arm::sim)
  mod_sim <- arm::sim(m, n.sims = 5000)
  
  ## 3) Marco de predicción: variar SOLO la variable de interés (sobpol)
  ##    El resto de predictores se fija en la categoría base / 0
  coef_nms        <- names(lme4::fixef(m))
  coef_nms_no_int <- setdiff(coef_nms, "(Intercept)")
  
  pred_frame <- as.data.frame(matrix(0, nrow = 2, ncol = length(coef_nms_no_int)))
  names(pred_frame) <- coef_nms_no_int
  pred_frame$sobpol <- c(0, 1)   # 0 = no, 1 = sí
  
  ## 4) Matriz de diseño en el ORDEN del modelo
  X <- cbind("(Intercept)" = 1, as.matrix(pred_frame))
  X <- X[, coef_nms, drop = FALSE]
  
  ## 5) Predicción simulada (link logit => prob)
  invlogit <- function(z) 1 / (1 + exp(-z))
  B  <- as.matrix(mod_sim@fixef)
  XB <- X %*% t(B)                 # n_pred x sims
  P  <- invlogit(XB)
  
  ## 6) Resumen (media e IC95%) — opcional para tabla
  fit <- apply(P, 1, mean)
  lwr <- apply(P, 1, quantile, 0.025)
  upr <- apply(P, 1, quantile, 0.975)
  
  ## ===== Apartado 7 (versión boxplot definitiva) =====
  library(ggplot2)
  library(tidyr)
  
  # P: matriz 2 x sims (filas = "No","Sí"; columnas = simulaciones)
  P_df <- as.data.frame(t(P))     # sims x 2
  colnames(P_df) <- c("No", "Sí")
  
  pred_long <- pivot_longer(
    P_df,
    cols = everything(),
    names_to = "x",
    values_to = "p"
  )
  
  pred_long$x <- factor(pred_long$x, levels = c("No", "Sí"))
  
  g <- ggplot(pred_long, aes(x = x, y = p)) +
    geom_boxplot(fill = "gray85", color = "black", alpha = 0.8,
                 width = 0.55, outlier.alpha = 0.25) +
    stat_summary(fun = mean, geom = "point", color = "firebrick", size = 4) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(
      x = "Soborno por parte de policía (experiencia personal)",
      y = "Probabilidad de participar en juntas barriales",
      title = "Probabilidad de asistir a juntas según experiencia de soborno policial"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(b = 10)),
      axis.title.x = element_text(margin = margin(t = 15), size = 14),
      axis.title.y = element_text(margin = margin(r = 15), size = 14),
      axis.text   = element_text(size = 12),
      panel.grid.minor = element_blank()
    )
  
  print(g)
  
}

# Predicción sobpub/juntas

{
  
  ## ===== Predicción marginal (juntas) con el template del mentor =====
  ## Requisitos: objeto del modelo en memoria: modelo_juntas_2z
  
  require(lme4)
  require(arm)
  
  set.seed(123)
  
  ## 1) Tu modelo
  m <- modelo_juntas_2z   # (glmer binomial, RE: 1 | pais_anio)
  
  ## 2) Simulaciones de betas (arm::sim)
  mod_sim <- arm::sim(m, n.sims = 5000)
  
  ## 3) Marco de predicción: variar SOLO la variable de interés (sobpub)
  ##    El resto de predictores se fija en la categoría base / 0
  coef_nms        <- names(lme4::fixef(m))
  coef_nms_no_int <- setdiff(coef_nms, "(Intercept)")
  
  pred_frame <- as.data.frame(matrix(0, nrow = 2, ncol = length(coef_nms_no_int)))
  names(pred_frame) <- coef_nms_no_int
  pred_frame$sobpub <- c(0, 1)   # 0 = no, 1 = sí
  
  ## 4) Matriz de diseño en el ORDEN del modelo
  X <- cbind("(Intercept)" = 1, as.matrix(pred_frame))
  X <- X[, coef_nms, drop = FALSE]
  
  ## 5) Predicción simulada (link logit => prob)
  invlogit <- function(z) 1 / (1 + exp(-z))
  B  <- as.matrix(mod_sim@fixef)
  XB <- X %*% t(B)                 # n_pred x sims
  P  <- invlogit(XB)
  
  ## 6) Resumen (media e IC95%) — opcional para tabla
  fit <- apply(P, 1, mean)
  lwr <- apply(P, 1, quantile, 0.025)
  upr <- apply(P, 1, quantile, 0.975)
  
  ## ===== Apartado 7 (versión boxplot definitiva) =====
  library(ggplot2)
  library(tidyr)
  
  # P: matriz 2 x sims (filas = "No","Sí"; columnas = simulaciones)
  P_df <- as.data.frame(t(P))   # sims x 2
  colnames(P_df) <- c("No", "Sí")
  
  pred_long <- pivot_longer(
    P_df,
    cols = everything(),
    names_to = "x",
    values_to = "p"
  )
  
  pred_long$x <- factor(pred_long$x, levels = c("No", "Sí"))
  
  g <- ggplot(pred_long, aes(x = x, y = p)) +
    geom_boxplot(fill = "gray85", color = "black", alpha = 0.8,
                 width = 0.55, outlier.alpha = 0.25) +
    stat_summary(fun = mean, geom = "point", color = "firebrick", size = 4) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(
      x = "Soborno por parte de empleado/s público/s (experiencia personal)",
      y = "Probabilidad de participar en juntas barriales",
      title = "Probabilidad de asistir a juntas según experiencia de soborno emp. púb."
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(b = 10)),
      axis.title.x = element_text(margin = margin(t = 15), size = 14),
      axis.title.y = element_text(margin = margin(r = 15), size = 14),
      axis.text = element_text(size = 12),
      panel.grid.minor = element_blank()
    )
  
  print(g)
  
}

# Tabla de cuerpo

{
  
# === Variables que sí van en la tabla principal ===
vars_clave <- c(
  "genperc_alta", "partperc_media", "partperc_alta",
  "sobpol", "sobpub",
  "edad_z", "genero",
  "educ_primaria", "educ_secundaria", "educ_univotec",
  "income_medio_bajo", "income_medio", "income_medio_alto", "income_alto",
  "gdp_pc_z", "democracia_z"
)

# Etiquetas bonitas SOLO para las que mostramos
coef_map_corto <- c(
  "(Intercept)"       = "Intercept",
  "genperc_alta"      = "Percepción de corrupción (alta)",
  "partperc_media"    = "Percepción particular (media)",
  "partperc_alta"     = "Percepción particular (alta)",
  "sobpol"            = "Soborno policía (sí)",
  "sobpub"            = "Soborno empleado público (sí)",
  "edad_z"            = "Edad (z)",
  "genero"            = "Género",
  "educ_primaria"     = "Educación primaria",
  "educ_secundaria"   = "Educación secundaria",
  "educ_univoterc"     = "Educación universitaria",
  "income_medio_bajo" = "Ingreso medio-bajo",
  "income_medio"      = "Ingreso medio",
  "income_medio_alto" = "Ingreso medio-alto",
  "income_alto"       = "Ingreso alto",
  "gdp_pc_k_z"        = "PIB per cápita (z)",
  "democracia_z"      = "Democracia (z)"
)

gt_tbl_corto <- modelsummary(
  list("Voto" = modelo_voto_2z,
       "Protestas" = modelo_protestas_3z,
       "Juntas barriales" = modelo_juntas_2z),
  output   = "gt",
  intercept = TRUE,
  coef_map  = coef_map_corto,
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  stars     = c("***"=0.001, "**"=0.01, "*"=0.05, "."=0.10),  # <-- esquema R
  gof_omit  = "IC|Log|Adj|Pseudo|Deviance",
  fmt       = 3
) |>
  gt::tab_source_note(
    source_note = gt::md(
      "*Errores estándar entre paréntesis. ***p*<0.001; **p*<0.01; *p*<0.05; .*p*<0.10. Especificaciones completas en el Apéndice.*"
    )
  ) |>
  gt::tab_header(
    title    = gt::md("**Modelos jerárquicos (variables clave)**"),
    subtitle = gt::md("Años 2018/19, 2021 y 2023")
  )


gtsave(gt_tbl_corto, "modelos1.html")

}


# Tabla Anexo A - Modelo voto 1

{

  library(broom.mixed)
  library(modelsummary)
  library(gt)
  
  ## ---- (1) Diccionario actualizado de rótulos ----
  coef_rename <- c(
    # tus variables de interés
    "genperc_alta"        = "Percepción de corrupción (alta)",
    "partperc_media"      = "Percepción particular (media)",
    "partperc_alta"       = "Percepción particular (alta)",
    "sobpol"              = "Soborno empleado público",
    "sobpub"              = "Soborno político",
    
    # intercepto
    "(Intercept)"         = "(Intercepto)",
    
    # controles individuales
    "edad_z"              = "Edad (z)",
    "genero"              = "Género",
    "educ_primaria"       = "Educación primaria",
    "educ_secundaria"     = "Educación secundaria",
    "educ_univoterc"      = "Educación universitaria/terciaria",  # <- cambio 2
    
    "income_medio_bajo"   = "Ingreso medio-bajo",
    "income_medio"        = "Ingreso medio",
    "income_medio_alto"   = "Ingreso medio-alto",
    "income_alto"         = "Ingreso alto",
    "govhelp"             = "Ayuda gubernamental",
    
    "intpol_alto"         = "Interés en política (alto)",         # <- cambio 3
    "news_medio"          = "Seguimiento de noticias (medio)",    # <- cambio 4
    "news_alto"           = "Seguimiento de noticias (alto)",
    
    "ecoperc_igual"       = "Igual percepción económica",         # <- cambio 5
    "ecoperc"             = "Peor percepción económica",
    "ecoperc_mejor"       = "Mejor percepción económica",
    
    "ecopersperc_igual"   = "Igual percepción económica personal", # <- cambio 6
    "ecopersperc_mejor"   = "Mejor percepción económica personal", # <- cambio 7
    
    # por si el nombre viene como confina_alta o confint_alta
    "confina_alta"        = "Confianza interpersonal (alta)",     # <- cambio 8
    "confint_alta"        = "Confianza interpersonal (alta)",
    
    "respinst_medio"      = "Respeto institucional medio",
    "respinst_alto"       = "Respeto institucional alto",
    
    "job"                 = "Empleo",                              # <- cambio 9
    
    # controles contextuales
    "gdp_pc_k_z"          = "PIB per cápita (z)",
    "desempleo_z"         = "Desempleo (z)",
    "idh_z"               = "IDH (z)",
    "democracia_z"        = "Democracia (z)"
  )
  
  ## ---- (2) Orden con el INTERCEPTO primero ----
  # tomamos los términos presentes según el modelo
  present_terms <- broom.mixed::tidy(modelo_voto_1z, effects = "fixed")$term
  
  # prioridad: intercepto al tope, luego tus variables sustantivas
  orden_prioridad <- c(
    "(Intercept)", "genperc_alta", "partperc_media", "partperc_alta", "sobpol", "sobpub"
  )
  
  all_terms   <- unique(present_terms)
  coef_order  <- c(orden_prioridad, setdiff(all_terms, orden_prioridad))
  ordered_terms <- coef_order[coef_order %in% present_terms]
  
  ## ---- (3) Partición A/B (igual que venías haciendo) ----
  half <- ceiling(length(ordered_terms)/2)
  left_terms  <- ordered_terms[1:half]
  right_terms <- ordered_terms[(half+1):length(ordered_terms)]
  
  build_map <- function(terms, dict) {
    lbl <- ifelse(terms %in% names(dict), dict[terms], terms)
    stats::setNames(as.character(lbl), terms)
  }
  coef_map_left  <- build_map(left_terms,  coef_rename)
  coef_map_right <- build_map(right_terms, coef_rename)
  
  ## ---- (4) Tablas Partes A y B con los rótulos corregidos ----
  gt_left_v1 <- modelsummary(
    list("Voto — modelo 1 (z)" = modelo_voto_1z),
    output     = "gt",
    title      = "Modelo jerárquico (voto) — Especificación 1 (Parte A)",
    estimate   = "{estimate}{stars}",
    statistic  = "({std.error})",
    stars      = c("*"=.05,"**"=.01,"***"=.001),
    coef_map   = coef_map_left,
    gof_omit   = "IC|Log|Adj|Pseudo|Deviance"
  ) |>
    gt::tab_options(table.width = gt::pct(100))
  
  gt_right_v1 <- modelsummary(
    list("Voto — modelo 1 (z)" = modelo_voto_1z),
    output     = "gt",
    title      = "Modelo jerárquico (voto) — Especificación 1 (Parte B)",
    estimate   = "{estimate}{stars}",
    statistic  = "({std.error})",
    stars      = c("*"=.05,"**"=.01,"***"=.001),
    coef_map   = coef_map_right,
    gof_omit   = "IC|Log|Adj|Pseudo|Deviance"
  ) |>
    gt::tab_options(table.width = gt::pct(100))
  
  gtsave(gt_left_v1,  "anexo_modelo_voto_1z_parteA.png", vwidth = 800)
  gtsave(gt_right_v1, "anexo_modelo_voto_1z_parteB.png", vwidth = 800)
  

}

# Tabla Anexo B - Modelo voto 2

{

  # ====== MODELO: VOTO_2z — Partes A y B ======
  library(broom.mixed)
  library(modelsummary)
  library(gt)
  
  ## (1) Diccionario de rótulos (mismo que para voto_1z)
  coef_rename <- c(
    "genperc_alta"        = "Percepción de corrupción (alta)",
    "partperc_media"      = "Percepción particular (media)",
    "partperc_alta"       = "Percepción particular (alta)",
    "sobpol"              = "Soborno empleado público",
    "sobpub"              = "Soborno político",
    "(Intercept)"         = "(Intercepto)",
    "edad_z"              = "Edad (z)",
    "genero"              = "Género",
    "educ_primaria"       = "Educación primaria",
    "educ_secundaria"     = "Educación secundaria",
    "educ_univoterc"      = "Educación universitaria/terciaria",
    "income_medio_bajo"   = "Ingreso medio-bajo",
    "income_medio"        = "Ingreso medio",
    "income_medio_alto"   = "Ingreso medio-alto",
    "income_alto"         = "Ingreso alto",
    "govhelp"             = "Ayuda gubernamental",
    "intpol_alto"         = "Interés en política (alto)",
    "news_medio"          = "Seguimiento de noticias (medio)",
    "news_alto"           = "Seguimiento de noticias (alto)",
    "ecoperc_igual"       = "Igual percepción económica",
    "ecoperc"             = "Peor percepción económica",
    "ecoperc_mejor"       = "Mejor percepción económica",
    "ecopersperc_igual"   = "Igual percepción económica personal",
    "ecopersperc_mejor"   = "Mejor percepción económica personal",
    "confina_alta"        = "Confianza interpersonal (alta)",
    "confint_alta"        = "Confianza interpersonal (alta)",
    "respinst_medio"      = "Respeto institucional medio",
    "respinst_alto"       = "Respeto institucional alto",
    "job"                 = "Empleo",
    "gdp_pc_k_z"          = "PIB per cápita (z)",
    "desempleo_z"         = "Desempleo (z)",
    "idh_z"               = "IDH (z)",
    "democracia_z"        = "Democracia (z)"
  )
  
  ## (2) Orden con (Intercepto) primero y luego tus claves sustantivas
  present_terms <- broom.mixed::tidy(modelo_voto_2z, effects = "fixed")$term
  orden_prioridad <- c("(Intercept)", "genperc_alta","partperc_media","partperc_alta","sobpol","sobpub")
  all_terms   <- unique(present_terms)
  coef_order  <- c(orden_prioridad, setdiff(all_terms, orden_prioridad))
  ordered_terms <- coef_order[coef_order %in% present_terms]
  
  ## (3) Partición A/B conservando el orden
  half <- ceiling(length(ordered_terms)/2)
  left_terms  <- ordered_terms[1:half]
  right_terms <- ordered_terms[(half+1):length(ordered_terms)]
  
  build_map <- function(terms, dict) {
    lbl <- ifelse(terms %in% names(dict), dict[terms], terms)
    stats::setNames(as.character(lbl), terms)
  }
  coef_map_left  <- build_map(left_terms,  coef_rename)
  coef_map_right <- build_map(right_terms, coef_rename)
  
  ## (4) Tablas
  gt_left_v2 <- modelsummary(
    list("Voto — modelo 2 (z)" = modelo_voto_2z),
    output     = "gt",
    title      = "Modelo jerárquico (voto) — Especificación 2 (Parte A)",
    estimate   = "{estimate}{stars}",
    statistic  = "({std.error})",
    stars      = c("*"=.05,"**"=.01,"***"=.001),
    coef_map   = coef_map_left,
    gof_omit   = "IC|Log|Adj|Pseudo|Deviance"
  ) |>
    gt::tab_options(table.width = gt::pct(100))
  
  gt_right_v2 <- modelsummary(
    list("Voto — modelo 2 (z)" = modelo_voto_2z),
    output     = "gt",
    title      = "Modelo jerárquico (voto) — Especificación 2 (Parte B)",
    estimate   = "{estimate}{stars}",
    statistic  = "({std.error})",
    stars      = c("*"=.05,"**"=.01,"***"=.001),
    coef_map   = coef_map_right,
    gof_omit   = "IC|Log|Adj|Pseudo|Deviance"
  ) |>
    gt::tab_options(table.width = gt::pct(100))
  
  gtsave(gt_left_v2,  "anexo_modelo_voto_2z_parteA.png", vwidth = 800)
  gtsave(gt_right_v2, "anexo_modelo_voto_2z_parteB.png", vwidth = 800)
  

}

# Tabla Anexo C - Modelo voto 3

{

  # ====== MODELO: VOTO_3z — Partes A y B ======
  library(broom.mixed)
  library(modelsummary)
  library(gt)
  
  ## (1) Diccionario de rótulos (mismo que para voto_1z/voto_2z)
  coef_rename <- c(
    "genperc_alta"        = "Percepción de corrupción (alta)",
    "partperc_media"      = "Percepción particular (media)",
    "partperc_alta"       = "Percepción particular (alta)",
    "sobpol"              = "Soborno empleado público",
    "sobpub"              = "Soborno político",
    "(Intercept)"         = "(Intercepto)",
    "edad_z"              = "Edad (z)",
    "genero"              = "Género",
    "educ_primaria"       = "Educación primaria",
    "educ_secundaria"     = "Educación secundaria",
    "educ_univoterc"      = "Educación universitaria/terciaria",
    "income_medio_bajo"   = "Ingreso medio-bajo",
    "income_medio"        = "Ingreso medio",
    "income_medio_alto"   = "Ingreso medio-alto",
    "income_alto"         = "Ingreso alto",
    "govhelp"             = "Ayuda gubernamental",
    "intpol_alto"         = "Interés en política (alto)",
    "news_medio"          = "Seguimiento de noticias (medio)",
    "news_alto"           = "Seguimiento de noticias (alto)",
    "ecoperc_igual"       = "Igual percepción económica",
    "ecoperc"             = "Peor percepción económica",
    "ecoperc_mejor"       = "Mejor percepción económica",
    "ecopersperc_igual"   = "Igual percepción económica personal",
    "ecopersperc_mejor"   = "Mejor percepción económica personal",
    "confina_alta"        = "Confianza interpersonal (alta)",
    "confint_alta"        = "Confianza interpersonal (alta)",
    "respinst_medio"      = "Respeto institucional medio",
    "respinst_alto"       = "Respeto institucional alto",
    "job"                 = "Empleo",
    "gdp_pc_k_z"          = "PIB per cápita (z)",
    "desempleo_z"         = "Desempleo (z)",
    "idh_z"               = "IDH (z)",
    "democracia_z"        = "Democracia (z)"
  )
  
  ## (2) Orden con (Intercepto) primero y luego claves sustantivas
  present_terms <- broom.mixed::tidy(modelo_voto_3z, effects = "fixed")$term
  orden_prioridad <- c("(Intercept)", "genperc_alta","partperc_media","partperc_alta","sobpol","sobpub")
  all_terms   <- unique(present_terms)
  coef_order  <- c(orden_prioridad, setdiff(all_terms, orden_prioridad))
  ordered_terms <- coef_order[coef_order %in% present_terms]
  
  ## (3) Partición A/B conservando el orden
  half <- ceiling(length(ordered_terms)/2)
  left_terms  <- ordered_terms[1:half]
  right_terms <- ordered_terms[(half+1):length(ordered_terms)]
  
  build_map <- function(terms, dict) {
    lbl <- ifelse(terms %in% names(dict), dict[terms], terms)
    stats::setNames(as.character(lbl), terms)
  }
  coef_map_left  <- build_map(left_terms,  coef_rename)
  coef_map_right <- build_map(right_terms, coef_rename)
  
  ## (4) Tablas
  gt_left_v3 <- modelsummary(
    list("Voto — modelo 3 (z)" = modelo_voto_3z),
    output     = "gt",
    title      = "Modelo jerárquico (voto) — Especificación 3 (Parte A)",
    estimate   = "{estimate}{stars}",
    statistic  = "({std.error})",
    stars      = c("*"=.05,"**"=.01,"***"=.001),
    coef_map   = coef_map_left,
    gof_omit   = "IC|Log|Adj|Pseudo|Deviance"
  ) |>
    gt::tab_options(table.width = gt::pct(100))
  
  gt_right_v3 <- modelsummary(
    list("Voto — modelo 3 (z)" = modelo_voto_3z),
    output     = "gt",
    title      = "Modelo jerárquico (voto) — Especificación 3 (Parte B)",
    estimate   = "{estimate}{stars}",
    statistic  = "({std.error})",
    stars      = c("*"=.05,"**"=.01,"***"=.001),
    coef_map   = coef_map_right,
    gof_omit   = "IC|Log|Adj|Pseudo|Deviance"
  ) |>
    gt::tab_options(table.width = gt::pct(100))
  
  gtsave(gt_left_v3,  "anexo_modelo_voto_3z_parteA.png", vwidth = 800)
  gtsave(gt_right_v3, "anexo_modelo_voto_3z_parteB.png", vwidth = 800)
  

}

# Tabla Anexo D - Modelo protestas 1

{

# ====== MODELO: PROTESTAS_1z — Partes A y B ======
library(broom.mixed)
library(modelsummary)
library(gt)

## (1) Diccionario de rótulos (mismo esquema que venimos usando)
coef_rename <- c(
  "genperc_alta"        = "Percepción de corrupción (alta)",
  "partperc_media"      = "Percepción particular (media)",
  "partperc_alta"       = "Percepción particular (alta)",
  "sobpol"              = "Soborno empleado público",
  "sobpub"              = "Soborno político",
  "(Intercept)"         = "(Intercepto)",
  "edad_z"              = "Edad (z)",
  "genero"              = "Género",
  "educ_primaria"       = "Educación primaria",
  "educ_secundaria"     = "Educación secundaria",
  "educ_univoterc"      = "Educación universitaria/terciaria",
  "income_medio_bajo"   = "Ingreso medio-bajo",
  "income_medio"        = "Ingreso medio",
  "income_medio_alto"   = "Ingreso medio-alto",
  "income_alto"         = "Ingreso alto",
  "govhelp"             = "Ayuda gubernamental",
  "intpol_alto"         = "Interés en política (alto)",
  "news_medio"          = "Seguimiento de noticias (medio)",
  "news_alto"           = "Seguimiento de noticias (alto)",
  "ecoperc_igual"       = "Igual percepción económica",
  "ecoperc"             = "Peor percepción económica",
  "ecoperc_mejor"       = "Mejor percepción económica",
  "ecopersperc_igual"   = "Igual percepción económica personal",
  "ecopersperc_mejor"   = "Mejor percepción económica personal",
  "confina_alta"        = "Confianza interpersonal (alta)",
  "confint_alta"        = "Confianza interpersonal (alta)",
  "respinst_medio"      = "Respeto institucional medio",
  "respinst_alto"       = "Respeto institucional alto",
  "job"                 = "Empleo",
  "gdp_pc_k_z"          = "PIB per cápita (z)",
  "desempleo_z"         = "Desempleo (z)",
  "idh_z"               = "IDH (z)",
  "democracia_z"        = "Democracia (z)"
)

## (2) Orden: (Intercepto) primero y luego claves sustantivas
present_terms <- broom.mixed::tidy(modelo_protestas_1z, effects = "fixed")$term
orden_prioridad <- c("(Intercept)", "genperc_alta","partperc_media","partperc_alta","sobpol","sobpub")
all_terms   <- unique(present_terms)
coef_order  <- c(orden_prioridad, setdiff(all_terms, orden_prioridad))
ordered_terms <- coef_order[coef_order %in% present_terms]

## (3) Partición A/B conservando el orden
half <- ceiling(length(ordered_terms)/2)
left_terms  <- ordered_terms[1:half]
right_terms <- ordered_terms[(half+1):length(ordered_terms)]

build_map <- function(terms, dict) {
  lbl <- ifelse(terms %in% names(dict), dict[terms], terms)
  stats::setNames(as.character(lbl), terms)
}
coef_map_left  <- build_map(left_terms,  coef_rename)
coef_map_right <- build_map(right_terms, coef_rename)

## (4) Tablas
gt_left_p1 <- modelsummary(
  list("Protestas — modelo 1 (z)" = modelo_protestas_1z),
  output     = "gt",
  title      = "Modelo jerárquico (protestas) — Especificación 1 (Parte A)",
  estimate   = "{estimate}{stars}",
  statistic  = "({std.error})",
  stars      = c("*"=.05,"**"=.01,"***"=.001),
  coef_map   = coef_map_left,
  gof_omit   = "IC|Log|Adj|Pseudo|Deviance"
) |>
  gt::tab_options(table.width = gt::pct(100))

gt_right_p1 <- modelsummary(
  list("Protestas — modelo 1 (z)" = modelo_protestas_1z),
  output     = "gt",
  title      = "Modelo jerárquico (protestas) — Especificación 1 (Parte B)",
  estimate   = "{estimate}{stars}",
  statistic  = "({std.error})",
  stars      = c("*"=.05,"**"=.01,"***"=.001),
  coef_map   = coef_map_right,
  gof_omit   = "IC|Log|Adj|Pseudo|Deviance"
) |>
  gt::tab_options(table.width = gt::pct(100))

gtsave(gt_left_p1,  "anexo_modelo_protestas_1z_parteA.png", vwidth = 800)
gtsave(gt_right_p1, "anexo_modelo_protestas_1z_parteB.png", vwidth = 800)

}

# Tabla Anexo E - Modelo protestas 2

{

# ====== MODELO: PROTESTAS_2z — Partes A y B ======
library(broom.mixed)
library(modelsummary)
library(gt)

## (1) Diccionario de rótulos (igual al usado antes)
coef_rename <- c(
  "genperc_alta"        = "Percepción de corrupción (alta)",
  "partperc_media"      = "Percepción particular (media)",
  "partperc_alta"       = "Percepción particular (alta)",
  "sobpol"              = "Soborno empleado público",
  "sobpub"              = "Soborno político",
  "(Intercept)"         = "(Intercepto)",
  "edad_z"              = "Edad (z)",
  "genero"              = "Género",
  "educ_primaria"       = "Educación primaria",
  "educ_secundaria"     = "Educación secundaria",
  "educ_univoterc"      = "Educación universitaria/terciaria",
  "income_medio_bajo"   = "Ingreso medio-bajo",
  "income_medio"        = "Ingreso medio",
  "income_medio_alto"   = "Ingreso medio-alto",
  "income_alto"         = "Ingreso alto",
  "govhelp"             = "Ayuda gubernamental",
  "intpol_alto"         = "Interés en política (alto)",
  "news_medio"          = "Seguimiento de noticias (medio)",
  "news_alto"           = "Seguimiento de noticias (alto)",
  "ecoperc_igual"       = "Igual percepción económica",
  "ecoperc"             = "Peor percepción económica",
  "ecoperc_mejor"       = "Mejor percepción económica",
  "ecopersperc_igual"   = "Igual percepción económica personal",
  "ecopersperc_mejor"   = "Mejor percepción económica personal",
  "confina_alta"        = "Confianza interpersonal (alta)",
  "confint_alta"        = "Confianza interpersonal (alta)",
  "respinst_medio"      = "Respeto institucional medio",
  "respinst_alto"       = "Respeto institucional alto",
  "job"                 = "Empleo",
  "gdp_pc_k_z"          = "PIB per cápita (z)",
  "desempleo_z"         = "Desempleo (z)",
  "idh_z"               = "IDH (z)",
  "democracia_z"        = "Democracia (z)"
)

## (2) Orden con (Intercepto) primero y luego variables sustantivas
present_terms <- broom.mixed::tidy(modelo_protestas_2z, effects = "fixed")$term
orden_prioridad <- c("(Intercept)", "genperc_alta","partperc_media","partperc_alta","sobpol","sobpub")
all_terms   <- unique(present_terms)
coef_order  <- c(orden_prioridad, setdiff(all_terms, orden_prioridad))
ordered_terms <- coef_order[coef_order %in% present_terms]

## (3) Partición A/B conservando el orden
half <- ceiling(length(ordered_terms)/2)
left_terms  <- ordered_terms[1:half]
right_terms <- ordered_terms[(half+1):length(ordered_terms)]

build_map <- function(terms, dict) {
  lbl <- ifelse(terms %in% names(dict), dict[terms], terms)
  stats::setNames(as.character(lbl), terms)
}
coef_map_left  <- build_map(left_terms,  coef_rename)
coef_map_right <- build_map(right_terms, coef_rename)

## (4) Tablas
gt_left_p2 <- modelsummary(
  list("Protestas — modelo 2 (z)" = modelo_protestas_2z),
  output     = "gt",
  title      = "Modelo jerárquico (protestas) — Especificación 2 (Parte A)",
  estimate   = "{estimate}{stars}",
  statistic  = "({std.error})",
  stars      = c("*"=.05,"**"=.01,"***"=.001),
  coef_map   = coef_map_left,
  gof_omit   = "IC|Log|Adj|Pseudo|Deviance"
) |>
  gt::tab_options(table.width = gt::pct(100))

gt_right_p2 <- modelsummary(
  list("Protestas — modelo 2 (z)" = modelo_protestas_2z),
  output     = "gt",
  title      = "Modelo jerárquico (protestas) — Especificación 2 (Parte B)",
  estimate   = "{estimate}{stars}",
  statistic  = "({std.error})",
  stars      = c("*"=.05,"**"=.01,"***"=.001),
  coef_map   = coef_map_right,
  gof_omit   = "IC|Log|Adj|Pseudo|Deviance"
) |>
  gt::tab_options(table.width = gt::pct(100))

gtsave(gt_left_p2,  "anexo_modelo_protestas_2z_parteA.png", vwidth = 800)
gtsave(gt_right_p2, "anexo_modelo_protestas_2z_parteB.png", vwidth = 800)

}

# Tabla Anexo F - Modelo protestas 3

{

# ====== MODELO: PROTESTAS_3z — Partes A y B ======
library(broom.mixed)
library(modelsummary)
library(gt)

## (1) Diccionario de rótulos (idéntico al usado en los anteriores)
coef_rename <- c(
  "genperc_alta"        = "Percepción de corrupción (alta)",
  "partperc_media"      = "Percepción particular (media)",
  "partperc_alta"       = "Percepción particular (alta)",
  "sobpol"              = "Soborno empleado público",
  "sobpub"              = "Soborno político",
  "(Intercept)"         = "(Intercepto)",
  "edad_z"              = "Edad (z)",
  "genero"              = "Género",
  "educ_primaria"       = "Educación primaria",
  "educ_secundaria"     = "Educación secundaria",
  "educ_univoterc"      = "Educación universitaria/terciaria",
  "income_medio_bajo"   = "Ingreso medio-bajo",
  "income_medio"        = "Ingreso medio",
  "income_medio_alto"   = "Ingreso medio-alto",
  "income_alto"         = "Ingreso alto",
  "govhelp"             = "Ayuda gubernamental",
  "intpol_alto"         = "Interés en política (alto)",
  "news_medio"          = "Seguimiento de noticias (medio)",
  "news_alto"           = "Seguimiento de noticias (alto)",
  "ecoperc_igual"       = "Igual percepción económica",
  "ecoperc"             = "Peor percepción económica",
  "ecoperc_mejor"       = "Mejor percepción económica",
  "ecopersperc_igual"   = "Igual percepción económica personal",
  "ecopersperc_mejor"   = "Mejor percepción económica personal",
  "confina_alta"        = "Confianza interpersonal (alta)",
  "confint_alta"        = "Confianza interpersonal (alta)",
  "respinst_medio"      = "Respeto institucional medio",
  "respinst_alto"       = "Respeto institucional alto",
  "job"                 = "Empleo",
  "gdp_pc_k_z"          = "PIB per cápita (z)",
  "desempleo_z"         = "Desempleo (z)",
  "idh_z"               = "IDH (z)",
  "democracia_z"        = "Democracia (z)"
)

## (2) Orden: (Intercepto) primero y luego las claves sustantivas
present_terms <- broom.mixed::tidy(modelo_protestas_3z, effects = "fixed")$term
orden_prioridad <- c("(Intercept)", "genperc_alta","partperc_media","partperc_alta","sobpol","sobpub")
all_terms   <- unique(present_terms)
coef_order  <- c(orden_prioridad, setdiff(all_terms, orden_prioridad))
ordered_terms <- coef_order[coef_order %in% present_terms]

## (3) Partición A/B conservando el orden
half <- ceiling(length(ordered_terms)/2)
left_terms  <- ordered_terms[1:half]
right_terms <- ordered_terms[(half+1):length(ordered_terms)]

build_map <- function(terms, dict) {
  lbl <- ifelse(terms %in% names(dict), dict[terms], terms)
  stats::setNames(as.character(lbl), terms)
}
coef_map_left  <- build_map(left_terms,  coef_rename)
coef_map_right <- build_map(right_terms, coef_rename)

## (4) Tablas
gt_left_p3 <- modelsummary(
  list("Protestas — modelo 3 (z)" = modelo_protestas_3z),
  output     = "gt",
  title      = "Modelo jerárquico (protestas) — Especificación 3 (Parte A)",
  estimate   = "{estimate}{stars}",
  statistic  = "({std.error})",
  stars      = c("*"=.05,"**"=.01,"***"=.001),
  coef_map   = coef_map_left,
  gof_omit   = "IC|Log|Adj|Pseudo|Deviance"
) |>
  gt::tab_options(table.width = gt::pct(100))

gt_right_p3 <- modelsummary(
  list("Protestas — modelo 3 (z)" = modelo_protestas_3z),
  output     = "gt",
  title      = "Modelo jerárquico (protestas) — Especificación 3 (Parte B)",
  estimate   = "{estimate}{stars}",
  statistic  = "({std.error})",
  stars      = c("*"=.05,"**"=.01,"***"=.001),
  coef_map   = coef_map_right,
  gof_omit   = "IC|Log|Adj|Pseudo|Deviance"
) |>
  gt::tab_options(table.width = gt::pct(100))

gtsave(gt_left_p3,  "anexo_modelo_protestas_3z_parteA.png", vwidth = 800)
gtsave(gt_right_p3, "anexo_modelo_protestas_3z_parteB.png", vwidth = 800)

}

# Tabla Anexo G - Modelo juntas 1

{

# ====== MODELO: JUNTAS_1z — Partes A y B ======
library(broom.mixed)
library(modelsummary)
library(gt)

## (1) Diccionario de rótulos (mismo que venimos usando)
coef_rename <- c(
  "genperc_alta"        = "Percepción de corrupción (alta)",
  "partperc_media"      = "Percepción particular (media)",
  "partperc_alta"       = "Percepción particular (alta)",
  "sobpol"              = "Soborno empleado público",
  "sobpub"              = "Soborno político",
  "(Intercept)"         = "(Intercepto)",
  "edad_z"              = "Edad (z)",
  "genero"              = "Género",
  "educ_primaria"       = "Educación primaria",
  "educ_secundaria"     = "Educación secundaria",
  "educ_univoterc"      = "Educación universitaria/terciaria",
  "income_medio_bajo"   = "Ingreso medio-bajo",
  "income_medio"        = "Ingreso medio",
  "income_medio_alto"   = "Ingreso medio-alto",
  "income_alto"         = "Ingreso alto",
  "govhelp"             = "Ayuda gubernamental",
  "intpol_alto"         = "Interés en política (alto)",
  "news_medio"          = "Seguimiento de noticias (medio)",
  "news_alto"           = "Seguimiento de noticias (alto)",
  "ecoperc_igual"       = "Igual percepción económica",
  "ecoperc"             = "Peor percepción económica",
  "ecoperc_mejor"       = "Mejor percepción económica",
  "ecopersperc_igual"   = "Igual percepción económica personal",
  "ecopersperc_mejor"   = "Mejor percepción económica personal",
  "confina_alta"        = "Confianza interpersonal (alta)",
  "confint_alta"        = "Confianza interpersonal (alta)",
  "respinst_medio"      = "Respeto institucional medio",
  "respinst_alto"       = "Respeto institucional alto",
  "job"                 = "Empleo",
  "gdp_pc_k_z"          = "PIB per cápita (z)",
  "desempleo_z"         = "Desempleo (z)",
  "idh_z"               = "IDH (z)",
  "democracia_z"        = "Democracia (z)"
)

## (2) Orden: (Intercepto) primero y luego claves sustantivas
present_terms <- broom.mixed::tidy(modelo_juntas_1z, effects = "fixed")$term
orden_prioridad <- c("(Intercept)", "genperc_alta","partperc_media","partperc_alta","sobpol","sobpub")
all_terms   <- unique(present_terms)
coef_order  <- c(orden_prioridad, setdiff(all_terms, orden_prioridad))
ordered_terms <- coef_order[coef_order %in% present_terms]

## (3) Partición A/B conservando el orden
half <- ceiling(length(ordered_terms)/2)
left_terms  <- ordered_terms[1:half]
right_terms <- ordered_terms[(half+1):length(ordered_terms)]

build_map <- function(terms, dict) {
  lbl <- ifelse(terms %in% names(dict), dict[terms], terms)
  stats::setNames(as.character(lbl), terms)
}
coef_map_left  <- build_map(left_terms,  coef_rename)
coef_map_right <- build_map(right_terms, coef_rename)

## (4) Tablas
gt_left_j1 <- modelsummary(
  list("Juntas barriales — modelo 1 (z)" = modelo_juntas_1z),
  output     = "gt",
  title      = "Modelo jerárquico (juntas barriales) — Especificación 1 (Parte A)",
  estimate   = "{estimate}{stars}",
  statistic  = "({std.error})",
  stars      = c("*"=.05,"**"=.01,"***"=.001),
  coef_map   = coef_map_left,
  gof_omit   = "IC|Log|Adj|Pseudo|Deviance"
) |>
  gt::tab_options(table.width = gt::pct(100))

gt_right_j1 <- modelsummary(
  list("Juntas barriales — modelo 1 (z)" = modelo_juntas_1z),
  output     = "gt",
  title      = "Modelo jerárquico (juntas barriales) — Especificación 1 (Parte B)",
  estimate   = "{estimate}{stars}",
  statistic  = "({std.error})",
  stars      = c("*"=.05,"**"=.01,"***"=.001),
  coef_map   = coef_map_right,
  gof_omit   = "IC|Log|Adj|Pseudo|Deviance"
) |>
  gt::tab_options(table.width = gt::pct(100))

gtsave(gt_left_j1,  "anexo_modelo_juntas_1z_parteA.png", vwidth = 800)
gtsave(gt_right_j1, "anexo_modelo_juntas_1z_parteB.png", vwidth = 800)

}

# Tabla Anexo H - Modelo juntas 2

{

# ====== MODELO: JUNTAS_2z — Partes A y B ======
library(broom.mixed)
library(modelsummary)
library(gt)

## (1) Diccionario de rótulos (mismo que venimos usando)
coef_rename <- c(
  "genperc_alta"        = "Percepción de corrupción (alta)",
  "partperc_media"      = "Percepción particular (media)",
  "partperc_alta"       = "Percepción particular (alta)",
  "sobpol"              = "Soborno empleado público",
  "sobpub"              = "Soborno político",
  "(Intercept)"         = "(Intercepto)",
  "edad_z"              = "Edad (z)",
  "genero"              = "Género",
  "educ_primaria"       = "Educación primaria",
  "educ_secundaria"     = "Educación secundaria",
  "educ_univoterc"      = "Educación universitaria/terciaria",
  "income_medio_bajo"   = "Ingreso medio-bajo",
  "income_medio"        = "Ingreso medio",
  "income_medio_alto"   = "Ingreso medio-alto",
  "income_alto"         = "Ingreso alto",
  "govhelp"             = "Ayuda gubernamental",
  "intpol_alto"         = "Interés en política (alto)",
  "news_medio"          = "Seguimiento de noticias (medio)",
  "news_alto"           = "Seguimiento de noticias (alto)",
  "ecoperc_igual"       = "Igual percepción económica",
  "ecoperc"             = "Peor percepción económica",
  "ecoperc_mejor"       = "Mejor percepción económica",
  "ecopersperc_igual"   = "Igual percepción económica personal",
  "ecopersperc_mejor"   = "Mejor percepción económica personal",
  "confina_alta"        = "Confianza interpersonal (alta)",
  "confint_alta"        = "Confianza interpersonal (alta)",
  "respinst_medio"      = "Respeto institucional medio",
  "respinst_alto"       = "Respeto institucional alto",
  "job"                 = "Empleo",
  "gdp_pc_k_z"          = "PIB per cápita (z)",
  "desempleo_z"         = "Desempleo (z)",
  "idh_z"               = "IDH (z)",
  "democracia_z"        = "Democracia (z)"
)

## (2) Orden: (Intercepto) primero y luego claves sustantivas
present_terms <- broom.mixed::tidy(modelo_juntas_2z, effects = "fixed")$term
orden_prioridad <- c("(Intercept)", "genperc_alta","partperc_media","partperc_alta","sobpol","sobpub")
all_terms   <- unique(present_terms)
coef_order  <- c(orden_prioridad, setdiff(all_terms, orden_prioridad))
ordered_terms <- coef_order[coef_order %in% present_terms]

## (3) Partición A/B conservando el orden
half <- ceiling(length(ordered_terms)/2)
left_terms  <- ordered_terms[1:half]
right_terms <- ordered_terms[(half+1):length(ordered_terms)]

build_map <- function(terms, dict) {
  lbl <- ifelse(terms %in% names(dict), dict[terms], terms)
  stats::setNames(as.character(lbl), terms)
}
coef_map_left  <- build_map(left_terms,  coef_rename)
coef_map_right <- build_map(right_terms, coef_rename)

## (4) Tablas
gt_left_j2 <- modelsummary(
  list("Juntas barriales — modelo 2 (z)" = modelo_juntas_2z),
  output     = "gt",
  title      = "Modelo jerárquico (juntas barriales) — Especificación 2 (Parte A)",
  estimate   = "{estimate}{stars}",
  statistic  = "({std.error})",
  stars      = c("*"=.05,"**"=.01,"***"=.001),
  coef_map   = coef_map_left,
  gof_omit   = "IC|Log|Adj|Pseudo|Deviance"
) |>
  gt::tab_options(table.width = gt::pct(100))

gt_right_j2 <- modelsummary(
  list("Juntas barriales — modelo 2 (z)" = modelo_juntas_2z),
  output     = "gt",
  title      = "Modelo jerárquico (juntas barriales) — Especificación 2 (Parte B)",
  estimate   = "{estimate}{stars}",
  statistic  = "({std.error})",
  stars      = c("*"=.05,"**"=.01,"***"=.001),
  coef_map   = coef_map_right,
  gof_omit   = "IC|Log|Adj|Pseudo|Deviance"
) |>
  gt::tab_options(table.width = gt::pct(100))

gtsave(gt_left_j2,  "anexo_modelo_juntas_2z_parteA.png", vwidth = 800)
gtsave(gt_right_j2, "anexo_modelo_juntas_2z_parteB.png", vwidth = 800)

}

# Tabla Anexo I - Modelo juntas 3

{

# ====== MODELO: JUNTAS_3z — Partes A y B ======
library(broom.mixed)
library(modelsummary)
library(gt)

## (1) Diccionario de rótulos (idéntico a los anteriores)
coef_rename <- c(
  "genperc_alta"        = "Percepción de corrupción (alta)",
  "partperc_media"      = "Percepción particular (media)",
  "partperc_alta"       = "Percepción particular (alta)",
  "sobpol"              = "Soborno empleado público",
  "sobpub"              = "Soborno político",
  "(Intercept)"         = "(Intercepto)",
  "edad_z"              = "Edad (z)",
  "genero"              = "Género",
  "educ_primaria"       = "Educación primaria",
  "educ_secundaria"     = "Educación secundaria",
  "educ_univoterc"      = "Educación universitaria/terciaria",
  "income_medio_bajo"   = "Ingreso medio-bajo",
  "income_medio"        = "Ingreso medio",
  "income_medio_alto"   = "Ingreso medio-alto",
  "income_alto"         = "Ingreso alto",
  "govhelp"             = "Ayuda gubernamental",
  "intpol_alto"         = "Interés en política (alto)",
  "news_medio"          = "Seguimiento de noticias (medio)",
  "news_alto"           = "Seguimiento de noticias (alto)",
  "ecoperc_igual"       = "Igual percepción económica",
  "ecoperc"             = "Peor percepción económica",
  "ecoperc_mejor"       = "Mejor percepción económica",
  "ecopersperc_igual"   = "Igual percepción económica personal",
  "ecopersperc_mejor"   = "Mejor percepción económica personal",
  "confina_alta"        = "Confianza interpersonal (alta)",
  "confint_alta"        = "Confianza interpersonal (alta)",
  "respinst_medio"      = "Respeto institucional medio",
  "respinst_alto"       = "Respeto institucional alto",
  "job"                 = "Empleo",
  "gdp_pc_k_z"          = "PIB per cápita (z)",
  "desempleo_z"         = "Desempleo (z)",
  "idh_z"               = "IDH (z)",
  "democracia_z"        = "Democracia (z)"
)

## (2) Orden: (Intercepto) primero y luego claves sustantivas
present_terms <- broom.mixed::tidy(modelo_juntas_3z, effects = "fixed")$term
orden_prioridad <- c("(Intercept)", "genperc_alta","partperc_media","partperc_alta","sobpol","sobpub")
all_terms   <- unique(present_terms)
coef_order  <- c(orden_prioridad, setdiff(all_terms, orden_prioridad))
ordered_terms <- coef_order[coef_order %in% present_terms]

## (3) Partición A/B conservando el orden
half <- ceiling(length(ordered_terms)/2)
left_terms  <- ordered_terms[1:half]
right_terms <- ordered_terms[(half+1):length(ordered_terms)]

build_map <- function(terms, dict) {
  lbl <- ifelse(terms %in% names(dict), dict[terms], terms)
  stats::setNames(as.character(lbl), terms)
}
coef_map_left  <- build_map(left_terms,  coef_rename)
coef_map_right <- build_map(right_terms, coef_rename)

## (4) Tablas
gt_left_j3 <- modelsummary(
  list("Juntas barriales — modelo 3 (z)" = modelo_juntas_3z),
  output     = "gt",
  title      = "Modelo jerárquico (juntas barriales) — Especificación 3 (Parte A)",
  estimate   = "{estimate}{stars}",
  statistic  = "({std.error})",
  stars      = c("*"=.05,"**"=.01,"***"=.001),
  coef_map   = coef_map_left,
  gof_omit   = "IC|Log|Adj|Pseudo|Deviance"
) |>
  gt::tab_options(table.width = gt::pct(100))

gt_right_j3 <- modelsummary(
  list("Juntas barriales — modelo 3 (z)" = modelo_juntas_3z),
  output     = "gt",
  title      = "Modelo jerárquico (juntas barriales) — Especificación 3 (Parte B)",
  estimate   = "{estimate}{stars}",
  statistic  = "({std.error})",
  stars      = c("*"=.05,"**"=.01,"***"=.001),
  coef_map   = coef_map_right,
  gof_omit   = "IC|Log|Adj|Pseudo|Deviance"
) |>
  gt::tab_options(table.width = gt::pct(100))

gtsave(gt_left_j3,  "anexo_modelo_juntas_3z_parteA.png", vwidth = 800)
gtsave(gt_right_j3, "anexo_modelo_juntas_3z_parteB.png", vwidth = 800)

}