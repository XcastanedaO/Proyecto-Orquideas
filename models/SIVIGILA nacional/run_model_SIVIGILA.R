# Este archivo permite ajustar un modelo logistico donde la variable de respuesta es salud mental.
# Se consideran todos los registros de la base de datos

# Librerías necesarias
library(dplyr)
library(tidyr)
library(rstan)

# Leer base de datos
data_model_SIVIGILA <- read.csv("data_model_SIVIGILA.csv")

data_model_SIVIGILA <- read.csv(file.choose())

# Construir matriz de diseño
X <- model.matrix(ac_mental ~ 
                      edad_ + mujer_cabf + def_naturaleza +
                      area + sexo_agre + conv_agre  + 
                      pac_hos + escenario,
                    data = data_model_SIVIGILA)[, -1]

# Preparar datos para stan
data_stan <- list(
  N = nrow(data_model_SIVIGILA),
  K = ncol(X), ## Efectos fijos
  X = X,
  y = as.integer(data_model_SIVIGILA$ac_mental)
)

SIVIGILA_model <- stan(
  file = file.choose(),#"logistic_model.stan",
  data = data_model_SIVIGILA,
  iter = 50,
  chains = 4,
  warmup = 1,
  cores = 30
)

saveRDS(SIVIGILA_model, file = "SIVIGILA_model.rds")
posterior_sample <- extract(SIVIGILA_model)
saveRDS(posterior_sample_SIVIGILA, file = "posterior_sample_SIVIGIL.rds")
