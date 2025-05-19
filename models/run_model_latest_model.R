library(dplyr)
library(tidyr)
library(rstan)


data_model <- read.csv("data_model.csv")

data <- data_model %>% select(c("ac_mental",
                                "edad", "mujer_cabf","def_naturaleza",
                                "area_", "sexo_agre", "parentezco_agresor", 
                                "conv_agre", "pac_hos_" , "escenario", 
                                 "tip_ss_"))

data <- data[!apply(is.na(data), 1, any), ]

data <- data %>%
  mutate(
    mujer_cabf = factor(mujer_cabf, levels = c(2, 1)),
    def_naturaleza = factor(def_naturaleza, levels = c("Negligencia y abandono", "Violencia física", "Violencia psicológica", "Violencia sexual")),
    
    area_ = factor(area_, levels = c("Cabecera municipal", "Centro poblado", "Rural disperso")),
    
    sexo_agre = factor(sexo_agre, levels = c("I","F", "M")),
    
    parentezco_agresor = factor(parentezco_agresor, levels = c("Otro", "Pareja", "Familiar", "Ex_Pareja", "Padre", "Madre", "Conocido(a)", "Amigo(a)", "Desconocido(a)")),
    
    escenario = factor(escenario, levels = c("Otro", "Área deportiva y recreativa", "Comercio y áreas de servicios", "Espacios abiertos", "Establecimiento educativo", "Institución de salud", "Lugar de trabajo", "Lugares de esparcimiento con expendio de alcohol", "Vía Pública", "Vivienda")),
    tip_ss_ =factor(tip_ss_, levels = c("I","C","E", "N", "P", "S")),
    conv_agre = factor(conv_agre, levels = c(2, 1)),
    pac_hos_ = factor(pac_hos_, levels = c(2, 1)), 
  )

##### Modelo 3

X_3 <- model.matrix(ac_mental ~ 
                      edad + mujer_cabf + def_naturaleza +
                      area_ + sexo_agre + 
                      parentezco_agresor + conv_agre  + 
                      pac_hos_ + escenario + tip_ss_,
                    data = data)[, -1]

# Preparar datos para stan
datos_stan_3 <- list(
  N = nrow(data),
  K = ncol(X_3), ## Efectos fijos
  X = X_3,
  y = as.integer(data$ac_mental)
)

modelo_logistico_3 <- stan(
  file = "logistic_model.stan",
  data = datos_stan_3,
  iter = 5000,
  chains = 4,
  warmup = 1000,
  cores = 30
)

saveRDS(modelo_logistico_3, file = "model_3.rds")
posterior_sample <- extract(modelo_logistico_3)
saveRDS(posterior_sample, file = "posterior_sample_model_3.rds")

