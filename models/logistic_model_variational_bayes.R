

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



## Ajuste de modelo
log_model <- cmdstan_model("models/logistic_model_stan.stan")

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
  departamento   = as.integer(train_data$departamento_ocurrencia) # 1..33
)


fit_vb_dep <- log_model$variational(
  data = stan_data,
  algorithm = "fullrank",
  output_samples = 10000,
  seed = 123
)

## Validación 

library(posterior)

# 
# draws <- as_draws_df(fit_vb_dep$draws())
# saveRDS(draws, "models/draws_dep.rds")

# draws <- readRDS("models/draws_fullrank_2.rds")

draws <- readRDS("models/draws_dep.rds")

results_full_2 <- summary(draws)

# fit_vb_2$cmdstan_diagnose()

mcmc_hist(draws, pars = c("beta_mujer[1]",  "beta_mujer[2]", "beta_naturaleza[1]")   )


# save_data(results, "results_meanfield", type = "interim", format = "xlsx")
# save_data(results_full, "results_fullrank", type = "interim", format = "xlsx")
# save_data(results_full_2, "results_fullrank2", type = "interim", format = "xlsx")
# save_data(significant_params, "signif_dep", type = "interim", format = "xlsx")
# rowMeans(as.matrix(draws[, beta_dep])) %>% sum()

## Parámetros significativos 
check_significance <- function(x, level = 0.95) {
  alpha <- 1 - level
  ci <- quantile(x, probs = c(alpha / 2, 1 - alpha / 2))
  
  tibble(
    mean = mean(x),
    sd   = sd(x),
    q2.5 = ci[1],
    q97.5 = ci[2],
    significant = !(ci[1] <= 0 & ci[2] >= 0)
  )
}
summary_params <- draws %>%
  select(starts_with("beta")) %>%
  pivot_longer(
    cols = everything(),
    names_to = "parameter",
    values_to = "value"
  ) %>%
  group_by(parameter) %>%
  summarise(check_significance(value), .groups = "drop")

significant_params <- summary_params %>%
  filter(significant)

significant_params$odds_mean <- exp(significant_params$mean)
significant_params$odds_025 <- exp(significant_params$q2.5)
significant_params$odds_075<- exp(significant_params$q97.5)

#### Curva

# Calcular la media de los coeficientes estimados
alpha_hat <- mean(draws$alpha)
beta_hat <- colMeans(draws[, 4:57])

# Calcular Odds Ratios
odds_ratios_dep <- exp(beta_hat)
intercepto_OR <- exp(alpha_hat)

X_mujer <- model.matrix(~ mujer_cabf - 1, data = test_data)
colnames(X_mujer) <- paste0("beta_mujer[", seq_len(ncol(X_mujer)), "]")

X_naturaleza <- model.matrix(~ def_naturaleza - 1, data = test_data)
colnames(X_naturaleza) <- paste0("beta_naturaleza[", seq_len(ncol(X_naturaleza)), "]")

X_sexo_agre <- model.matrix(~ sexo_agre - 1, data = test_data)
colnames(X_sexo_agre) <- paste0("beta_sexo_agre[", seq_len(ncol(X_sexo_agre)), "]")

X_conv_agre <- model.matrix(~ conv_agre - 1, data = test_data)
colnames(X_conv_agre) <- paste0("beta_conv_agre[", seq_len(ncol(X_conv_agre)), "]")

X_area <- model.matrix(~ area - 1, data = test_data)
colnames(X_area) <- paste0("beta_area[", seq_len(ncol(X_area)), "]")

X_escenario <- model.matrix(~ escenario - 1, data = test_data)
colnames(X_escenario) <- paste0("beta_escenario[", seq_len(ncol(X_escenario)), "]")

X_pac_hos <- model.matrix(~ pac_hos - 1, data = test_data)
colnames(X_pac_hos) <- paste0("beta_pac_hos[", seq_len(ncol(X_pac_hos)), "]")

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
  X_area,
  X_escenario,
  X_pac_hos,
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


library(PRROC)
PRROC_obj <- roc.curve(scores.class0 = p_hat,
                       weights.class0=test_data$ac_mental,
                       curve=TRUE)
plot(PRROC_obj)

### ANOVA
commune_sim <- c(fit_vb_dep_anova@sim[[1]][[1]]$,fit_models12@sim[[1]][[2]]$S_commune ,fit_models12@sim[[1]][[3]]$S_commune)
scheme_sim <- c(fit_models12@sim[[1]][[1]]$S_scheme,fit_models12@sim[[1]][[2]]$S_scheme,fit_models12@sim[[1]][[3]]$S_scheme)
development_sim <- c(fit_models12@sim[[1]][[1]]$S_development,fit_models12@sim[[1]][[2]]$S_development,fit_models12@sim[[1]][[3]]$S_development)
security_sim <- c(fit_models12@sim[[1]][[1]]$S_security,fit_models12@sim[[1]][[2]]$S_security,fit_models12@sim[[1]][[3]]$S_security)
gender_sim <- c(fit_models12@sim[[1]][[1]]$S_gender,fit_models12@sim[[1]][[2]]$S_gender,fit_models12@sim[[1]][[3]]$S_gender)
period_sim <- c(fit_models12@sim[[1]][[1]]$S_period,fit_models12@sim[[1]][[2]]$S_period,fit_models12@sim[[1]][[3]]$S_period)