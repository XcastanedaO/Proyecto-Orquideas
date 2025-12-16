

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

data_model_SIVIGILA <-readRDS(paste0(here(),"/data/processed/data_model_SIVIGILA_geo.rds"))

train_index <- createDataPartition(data_model_SIVIGILA$ac_mental, p = 0.8, list = FALSE)
train_data <- data_model_SIVIGILA[train_index, ]
test_data <- data_model_SIVIGILA[-train_index, ]

train_data %>% colnames()
data <- model.matrix(~ ciclo_vital + mujer_cabf + def_naturaleza + area + sexo_agre + conv_agre + pac_hos +
                       escenario + region, train_data)
library(cmdstanr)

# Set cmdstanr as backend
# set_cmdstan_path()
# 
# model <- brm(
#   formula = ac_mental ~ mujer_cabf+def_naturaleza+area+sexo_agre+ conv_agre+ ciclo_vital + departamento_ocurrencia ,
#   family = bernoulli(link = "logit"),
#   data = train_data,
#   prior = set_prior("normal(0, 5)", class = "b"),
#   chains = 4,
#   iter = 2000,
#   warmup = 1000,
#   seed = 123
# )
# 
# saveRDS(model, "logistic_model_ref.rds")

log_model <- cmdstan_model("models/logistic_model_stan.stan")

# Asegurar variable respuesta como 0/1
train_data$ac_mental <- as.integer(train_data$ac_mental)

# Factorizar explícitamente con niveles fijos (MUY IMPORTANTE)
train_data$mujer_cabf <- factor(
  train_data$mujer_cabf,
  levels = c("No", "Sí")
)

train_data$def_naturaleza <- factor(
  train_data$def_naturaleza,
  levels = c("Física", "Psicológica", "Sexual", "Negligencia y abandono")
)

train_data$sexo_agre <- factor(
  train_data$sexo_agre,
  levels = c("M", "F", "I")
)

train_data$conv_agre <- factor(
  train_data$conv_agre,
  levels = c("No", "Sí")
)

train_data$ciclo_vital <- factor(
  train_data$ciclo_vital,
  levels = c("Adultez", "Juventud", "Niñez y adolescencia")
)

train_data$departamento_ocurrencia <- factor(
  train_data$departamento_ocurrencia,
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
  ciclo_vital    = as.integer(train_data$ciclo_vital),       # 1..3
  departamento   = as.integer(train_data$departamento_ocurrencia) # 1..33
)



fit_vb_2 <- log_model$variational(
  data = stan_data,
  algorithm = "fullrank",
  output_samples = 10000,
  seed = 123
)


library(posterior)
saveRDS(fit_vb_2, "models/fit_vb_fullrank_2.rds")
draws <- as_draws_df(fit_vb$draws())

draws <- as_draws_df(fit_vb_2$draws())

results_full_2 <- summary(draws)

fit_vb$cmdstan_diagnose()

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

rowMeans(as.matrix(draws[, beta_dep]))

#######

fit <- log_model$sample(
  data = stan_data,
  chains = 4,
  iter_warmup = 1000,
  iter_sampling = 5000,
  seed = 123,

)
