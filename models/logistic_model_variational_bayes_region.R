

library(dplyr)
library(ggplot2)
source("utils/load_data.R")
library(here)
library(hms)
library(forcats)
library(lubridate)
library(stringr)
library(tidyr)
library(stringi)
library(knitr)
library(readr)
library(sf)
library(gmodels)   
library(DescTools)
library(kableExtra)
library(explore)
library(caret)
library(brms)
library(posterior)
library(bayesplot)
library(cmdstanr)

data_model_SIVIGILA <-readRDS(paste0(here(),"/data/processed/data_model_SIVIGILA_geo.rds"))

# Asegurar variable respuesta como 0/1
data_model_SIVIGILA$ac_mental <- as.integer(data_model_SIVIGILA$ac_mental)

# Factorizar explícitamente con niveles fijos (MUY IMPORTANTE)
data_model_SIVIGILA$mujer_cabf <- factor(
  data_model_SIVIGILA$mujer_cabf,
  levels = c("No", "Sí")
)

data_model_SIVIGILA$def_naturaleza <- factor(
  data_model_SIVIGILA$def_naturaleza,
  levels = c("Física", "Psicológica", "Sexual", "Negligencia y abandono")
)

data_model_SIVIGILA$sexo_agre <- factor(
  data_model_SIVIGILA$sexo_agre,
  levels = c("M", "F", "I")
)

data_model_SIVIGILA$conv_agre <- factor(
  data_model_SIVIGILA$conv_agre,
  levels = c("No", "Sí")
)

data_model_SIVIGILA$ciclo_vital <- factor(
  data_model_SIVIGILA$ciclo_vital,
  levels = c("Adultez", "Juventud", "Niñez y adolescencia")
)

data_model_SIVIGILA$area <- factor(
  data_model_SIVIGILA$area,
  levels = c("Cabecera municipal", "Centro poblado", "Rural disperso")
)

data_model_SIVIGILA$escenario <- factor(
  data_model_SIVIGILA$escenario,
  levels = c("Espacio público y social", "Vivienda")
)

data_model_SIVIGILA$pac_hos <- factor(
  data_model_SIVIGILA$pac_hos,
  levels = c("No", "Sí")
)

data_model_SIVIGILA$region <- factor(
  data_model_SIVIGILA$region,
  levels = c("Andina", "Amazonía", "Caribe", "Orinoquía","Pacífico" )
)

data_model_SIVIGILA$departamento_ocurrencia <- factor(
  data_model_SIVIGILA$departamento_ocurrencia,
  levels = c(
    "Antioquia", "Amazonas", "Arauca",
    "Archipiélago De San Andrés, Providencia Y Santa Catalina",
    "Atlántico", "Bogotá, D.C.", "Bolívar", "Boyacá", "Caldas",
    "Caquetá", "Casanare", "Cauca", "Cesar", "Chocó", "Córdoba",
    "Cundinamarca", "Guainía", "Guaviare", "Huila", "La Guajira",
    "Magdalena", "Meta", "Nariño", "Norte De Santander", "Putumayo",
    "Quindío", "Risaralda", "Santander", "Sucre", "Tolima",
    "Valle Del Cauca", "Vaupés", "Vichada"
  )
)

train_index <- createDataPartition(data_model_SIVIGILA$ac_mental, p = 0.8, list = FALSE)
train_data <- data_model_SIVIGILA[train_index, ]
test_data <- data_model_SIVIGILA[-train_index, ]

log_model_region <- cmdstan_model("models/logistic_model_stan_region.stan")

N <- nrow(train_data)

stan_data <- list(
  N = N,
  
  # Variable respuesta
  ac_mental = train_data$ac_mental,
  
  # Índices categóricos (1-based, como exige Stan)
  mujer_cabf     = as.integer(train_data$mujer_cabf),        # 1..2
  def_naturaleza = as.integer(train_data$def_naturaleza),    # 1..4
  sexo_agre      = as.integer(train_data$sexo_agre),         # 1..3
  conv_agre      = as.integer(train_data$conv_agre),         # 1..2
  area      = as.integer(train_data$area),         # 1..2
  escenario      = as.integer(train_data$escenario),         # 1..2
  pac_hos      = as.integer(train_data$pac_hos),         # 1..2
  ciclo_vital    = as.integer(train_data$ciclo_vital),       # 1..3
  region   = as.integer(train_data$region) # 1..33
)

X <- model.matrix(ac_mental ~
                    mujer_cabf + def_naturaleza +
                    sexo_agre + conv_agre  +
                    ciclo_vital + departamento_ocurrencia,
                  data = train_data)

fit_vb_2 <- log_model$variational(
  data = stan_data,
  algorithm = "fullrank",
  output_samples = 10000,
  seed = 123
)

## Validación 

library(posterior)

# saveRDS(fit_vb_2, "models/fit_vb_fullrank_2.rds")

draws <- readRDS("models/draws_fullrank_2.rds")

results_full_2 <- summary(draws)

fit_vb_2$cmdstan_diagnose()

mcmc_hist(draws, pars = c("beta_mujer[1]",  "beta_mujer[2]", "beta_naturaleza[1]")   )



fit_vb$metadata()$elbo
# Subset correcto de betas
beta_draws <- subset_draws(
  draws,
  variable = paste0("beta[", 1:K, "]")
)

save_data(results, "results_meanfield", type = "interim", format = "xlsx")
save_data(results_full, "results_fullrank", type = "interim", format = "xlsx")
save_data(results_full_2, "results_fullrank2", type = "interim", format = "xlsx")

# Resumen posterior
summary_beta <- summarise_draws(
  beta_draws,
  mean,
  sd,
  ~quantile2(.x, probs = c(0.025, 0.975))
)

beta_dep <- grep("^beta_departamento", names(draws), value = TRUE)

rowMeans(as.matrix(draws[, beta_dep])) %>% sum()


#### Curva

# Calcular la media de los coeficientes estimados
alpha_hat <- mean(draws$alpha)
beta_hat <- colMeans(draws[, 4:50])

# Calcular Odds Ratios
odds_ratios <- exp(beta_hat)
intercepto_OR <- exp(alpha_hat)

X_mujer <- model.matrix(~ mujer_cabf - 1, data = test_data)
colnames(X_mujer) <- paste0("beta_mujer[", seq_len(ncol(X_mujer)), "]")

X_naturaleza <- model.matrix(~ def_naturaleza - 1, data = test_data)
colnames(X_naturaleza) <- paste0("beta_naturaleza[", seq_len(ncol(X_naturaleza)), "]")

X_sexo_agre <- model.matrix(~ sexo_agre - 1, data = test_data)
colnames(X_sexo_agre) <- paste0("beta_sexo_agre[", seq_len(ncol(X_sexo_agre)), "]")

X_conv_agre <- model.matrix(~ conv_agre - 1, data = test_data)
colnames(X_conv_agre) <- paste0("beta_conv_agre[", seq_len(ncol(X_conv_agre)), "]")

X_ciclo_vital <- model.matrix(~ ciclo_vital - 1, data = test_data)
colnames(X_ciclo_vital) <- paste0("beta_ciclo_vital[", seq_len(ncol(X_ciclo_vital)), "]")

X_departamento <- model.matrix(~ departamento_ocurrencia - 1, data = test_data)
colnames(X_departamento) <- paste0(
  "beta_departamento[",
  seq_len(ncol(X_departamento)),
  "]"
)

X_test <- cbind(
  X_mujer,
  X_naturaleza,
  X_sexo_agre,
  X_conv_agre,
  X_ciclo_vital,
  X_departamento
)

# Calcular probabilidades predichas
beta_draws <- draws[, colnames(X_test)]

alpha_hat <- mean(draws$alpha) 
eta_hat <- as.numeric(
  alpha_hat + X_test %*% colMeans(beta_draws)
)

p_hat <- plogis(eta_hat)

# library(pROC)
# 
# roc_obj <- roc(
#   response  = test_data$ac_mental,
#   predictor = p_hat,
#   levels = c(0, 1),
#   direction = "<"
# )
# 
# auc_value <- auc(roc_obj)
# auc_value
# 
# plot(
#   roc_obj,
#   col = "blue",
#   lwd = 2,
#   main = paste0("ROC Curve (AUC = ", round(auc_value, 3), ")")
# )
# 
# abline(a = 0, b = 1, lty = 2, col = "gray")

library(PRROC)
PRROC_obj <- roc.curve(scores.class0 = p_hat,
                       weights.class0=test_data$ac_mental,
                       curve=TRUE)
plot(PRROC_obj)
