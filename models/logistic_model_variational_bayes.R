

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
# saveRDS(draws, "models/draws_dep_anova.rds")

# draws <- readRDS("models/draws_fullrank_2.rds")

draws <- readRDS("models/draws_dep_anova.rds")

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
## ANOVA
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.025, .975), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

attach(draws)



# Remove sample values that are significantly below or above the 5% and 95% quantiles,
# respectively, based on the interquartile range (IQR) and save results as data frames


sample_data <- lapply(c(draws$S_mujer, draws$S_naturaleza, draws$S_sexo_agre, draws$S_area, draws$S_conv_agre,
                        draws$S_escenario, draws$S_pac_hos, draws$S_pac_hos, draws$S_ciclo_vital, draws$S_departamento), 
                      function(f) {
                        data <- remove_outliers(f)
                        data <- as.data.frame(data)
                        colnames(data) <- "V1"
                        data
                      })

# Merge data frame with final samples
S_alphas <- do.call(rbind, sample_data)
# Create data frame with the names of the qualitative predictors
groups <- data.frame(rep(c("Mujer","Naturaleza","sexo_agre","S_conv_agre","S_pac_hos", "S_ciclo_vital", "S_departamento"), each = 10000))
# Combine S_alphas and groups by columns, obtaining a new data frame
S_alphas_grup <- cbind(S_alphas,groups) 
# Assign column names to the new data frame "S_alphas_grup"
colnames(S_alphas_grup) <- c("S_alpha","Grupo") 

# Plot Bayesian ANOVA
f <- function(x) {
  r <- quantile(x, probs = c(0, 0.05, 0.5, 0.95, 1)) 
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}


Anova <- ggplot(S_alphas_grup, aes(x=Grupo, y=S_alpha)) + 
  stat_summary(fun.data = f, geom="boxplot",
               fill='steelblue',width = 0.03,position = position_dodge(width=0.8))+
  stat_summary(fun=median, geom="point", shape=21, size=3, col = "black",bg="cadetblue2")+
  theme(aspect.ratio = .6)+
  labs(y = expression(S[alpha]), x = "", title = "Bayesian ANOVA")+
  coord_flip()
