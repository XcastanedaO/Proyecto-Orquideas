library(dplyr)
library(tidyr)
library(rstan)


data_model <- read.csv("data_model.csv")


data <- data_model %>% select(c("ac_mental",
                                "edad", "mujer_cabf","def_naturaleza",
                                "area_", "sexo_agre", "parentezco_agresor", 
                                "conv_agre", "pac_hos_" , "escenario"))

data <- data %>% mutate(escenario = case_when(
  escenario %in% c("Otro", "Área deportiva y recreativa", 
                   "Comercio y áreas de servicios", "Espacios abiertos", 
                   "Establecimiento educativo", "Institución de salud", "Lugar de trabajo",
                   "Lugares de esparcimiento con expendio de alcohol", "Vía Pública") ~ "Espacio público y social",
  TRUE ~ escenario
))

data <- data %>% mutate(parentezco_agresor = case_when(
  parentezco_agresor %in% c("Padre", "Madre") ~ "Familiar",
  parentezco_agresor == "Amigo(a)" ~ "Conocido(a)",
  parentezco_agresor == "Otro" ~ "Desconocido(a)",
  TRUE ~ parentezco_agresor
))

data <- data[!apply(is.na(data), 1, any), ]

data <- data %>%
  mutate(
    mujer_cabf = factor(mujer_cabf, levels = c(2, 1)),
    def_naturaleza = factor(def_naturaleza, levels = c("Negligencia y abandono", "Violencia física", "Violencia psicológica", "Violencia sexual")),
    
    area_ = factor(area_, levels = c("Cabecera municipal", "Centro poblado", "Rural disperso")),
    
    sexo_agre = factor(sexo_agre, levels = c("I","F", "M")),
    
    parentezco_agresor = factor(parentezco_agresor, levels = c("Desconocido(a)", "Pareja", "Familiar", "Ex_Pareja","Conocido(a)")),
    
    escenario = factor(escenario, levels = c("Espacio público y social","Vivienda")),
    conv_agre = factor(conv_agre, levels = c(2, 1)),
    pac_hos_ = factor(pac_hos_, levels = c(2, 1)), 
  )

##### Modelo 4: con menos categorías en parentesco y escenario y sin tipo de seguridad social

X_4 <- model.matrix(ac_mental ~ 
                      edad + mujer_cabf + def_naturaleza +
                      area_ + sexo_agre + 
                      parentezco_agresor + conv_agre  + 
                      pac_hos_ + escenario,
                    data = data)[, -1]

# Preparar datos para stan
datos_stan_4 <- list(
  N = nrow(data),
  K = ncol(X_4), ## Efectos fijos
  X = X_4,
  y = as.integer(data$ac_mental)
)

modelo_logistico_4 <- stan(
  file = "logistic_model.stan",
  data = datos_stan_4,
  iter = 5000,
  chains = 4,
  warmup = 1000,
  cores = 30
)

saveRDS(modelo_logistico_4, file = "model_4.rds")
posterior_sample <- extract(modelo_logistico_4)
saveRDS(posterior_sample, file = "posterior_sample_model_4.rds")

