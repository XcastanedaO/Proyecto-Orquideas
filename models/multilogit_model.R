library(dplyr)
library(tibble)
source("utils/load_data.R")
library(UPG)
library(caret)
library(BayesMultiLogit)
library(explore)
y# library(explore)
# model 1
##########
nf_data <- load_data("INMLCF_no_fatales_debugged_2015_2024_v2.csv", type = "interim")

nf_data <- nf_data %>% mutate(
  agresor_group = case_when(
  agresor_ag == "Delincuencia común" ~ "Actores del conflicto",
  agresor_ag == "Miembro de grupos alzados al margen de la ley" ~ "Actores del conflicto",
  agresor_ag == "Miembro de un grupo de la delincuencia organizada" ~  "Actores del conflicto",
    agresor_ag == "Miembros de las fuerzas armadas, de policía, policía judicial y servicios de inteligencia" ~ "Actores del conflicto",
  agresor_ag == "Encargado del cuidado" ~ "Conocido",
  agresor_ag == "Amigo(a)" ~ "Conocido",
  TRUE ~ agresor_ag
))

nf_data <- nf_data %>% filter(agresor_group %in% c("Pareja","Familiar","Ex-pareja","Conocido", "Actores del conflicto")) 

nf_data <- nf_data %>% filter(circunstancia_del_hecho != "Riña") #No corresponden a casos de VBG
nf_data <- nf_data %>% filter(circunstancia_del_hecho != "Bala perdida")

nf_data <- nf_data %>% filter(def_naturaleza %in% c("Violencia sexual","Violencia interpersonal","Violencia intrafamiliar","Violencia sociopolítica")) 

nf_data <- nf_data %>% filter(pais_de_nacimiento_de_la_victima =="Colombia") 


nf_data <- nf_data %>% mutate(
  escolaridad = case_when(
    escolaridad == "Educación básica primaria" ~ "Educación básica primaria y secundaria",
    escolaridad == "Educación básica secundaria o secundaria baja" ~ "Educación básica primaria y secundaria",
    escolaridad == "Doctorado o equivalente" ~ "Educación superior",
    escolaridad == "Educación técnica profesional y tecnológica" ~ "Educación superior",
    escolaridad == "Especialización, maestría o equivalente" ~ "Educación superior",
    escolaridad == "Universitario" ~ "Educación superior",
    escolaridad == "Educación media o secundaria alta" ~ "Educación superior",
    TRUE ~ escolaridad
  )
)

nf_data <- nf_data %>% mutate(
  escenario_del_hecho = case_when(
    escenario_del_hecho != "vivienda" ~ "Espacios sociales",
    TRUE ~ escenario_del_hecho
  )
)


nf_data <- nf_data %>% mutate(
  ciclo_vital_ = case_when(
    grupo_de_edad_de_la_victima %in% c("00-04", "05-09","10-14","15-17","18-19") ~ "Niñas y adolescentes",
    grupo_de_edad_de_la_victima %in% c("20-24", "25-29","30-34") ~ "Jovenes",
    grupo_de_edad_de_la_victima %in% c("35-39", "40-44","45-49","50-54","55-59") ~ "Adultez",
    TRUE ~ grupo_de_edad_de_la_victima
  )
)

nf_data <- nf_data %>% filter(ciclo_vital_ %in% c("Niñas y adolescentes", "Jovenes", "Adultez"))

nf_data <- nf_data %>% mutate(
  rango_de_hora_del_hecho_x_3_horas = case_when(
    rango_de_hora_del_hecho_x_3_horas == "Sin información" ~ "Sin información",
    rango_de_hora_del_hecho_x_3_horas %in% c("00:00 - 02:59", "03:00 - 05:59") ~ "Madrugada",
    rango_de_hora_del_hecho_x_3_horas %in% c("06:00 - 08:59", "09:00 - 11:59", "12:00 - 14:59", "15:00 - 17:59") ~ "Dia",
    TRUE ~ "Noche"
  )
)

nf_data <- nf_data %>% mutate(
  dia_del_hecho = case_when(
    dia_del_hecho %in% c("Sábado", "Domingo") ~ "Fin de semana",
    TRUE ~ "semana"
  )
)

nf_data <- nf_data %>% dplyr::select(def_naturaleza, ciclo_vital_, escolaridad, dia_del_hecho, rango_de_hora_del_hecho_x_3_horas, escenario_del_hecho, zona_del_hecho, agresor_group)
# save_data(nf_data, "data_nf_modelo")
#################
nf_data <- load_data("data_nf_modelo.csv", type = "processed")

train_index <- createDataPartition(nf_data$def_naturaleza, p = 0.7, list = FALSE)
train_data <- nf_data[train_index, ]
test_data <- nf_data[-train_index, ]

data <- model.matrix(~ ciclo_vital_ + escolaridad + rango_de_hora_del_hecho_x_3_horas + dia_del_hecho + escenario_del_hecho + zona_del_hecho + agresor_group, train_data)

levels(as.factor(nf_data$def_naturaleza))

# results.mnl <- UPG(y = train_data[,1], X = data, model = 'mnl', verbose = FALSE, baseline = "Violencia interpersonal")

nf_data[,-1]

nf_data %>% explore()
###############

prop.table(table(nf_data$def_naturaleza))



replace_missing_info <- function(df) {
  df[] <- lapply(df, function(col) {
    if (is.character(col) | is.factor(col)) {  # Apply only to text columns
      col[col %in% c("Sin información", "Sin Información")] <- NA_character_
    }
    return(col)
  })
  return(df)
}

test <- replace_missing_info(nf_data)

test %>% is.na() %>% sum()

na_empty <- data.frame(
  Column = names(test),
  total_na = sapply(test, function(col)
    sum(is.na(col))),
  total_empty = sapply(test, function(col)
    sum(col == "", na.rm = TRUE)),
  percent_na = sapply(test, function(col)
    round(sum(is.na(
      col
    )) / nrow(test) * 100, 2)),
  row.names = NULL
)

na_empty <- na_empty %>%
  filter(total_na > 0 | total_empty > 0) %>% arrange(desc(percent_na))

na_empty

test <- na.omit(test)
prop.table(table(test$def_naturaleza))

########## BayesMultilogit
#########################

library(BayesMultiLogit)

data <- model.matrix(~ ciclo_vital_ + rango_de_hora_del_hecho_x_3_horas, train_data)

model <- multilogit(response_matrix, data, n_sample = 3000, n_burn = 1500, prior = "normal")
############ BRMS
############################ brms
############
library(brms)
library(cmdstanr)

# Set cmdstanr as backend
set_cmdstan_path()

# set.seed(1)
# # randomly sample 300 values from uniform dist'n ranging from 0.5 - 3
# x <- runif(n = 300, min = 0.5, max = 3)
# # randomly sample "a" and "b" 300 times with replacement
# g <- sample(c("a", "b"), size = 300, replace = TRUE)
# 
# lp2 <- 3 + -2*x + -0.7*(g == "b")
# lp3 <- -2 + 1.2*x + -0.3*(g == "b")
# den <- (1 + exp(lp2) + exp(lp3))
# p1 <- 1/den
# p2 <- exp(lp2)/den
# p3 <- exp(lp3)/den
# P <- cbind(p1, p2, p3)
# 
# set.seed(3)
# y <- apply(P, MARGIN = 1, function(x)sample(x = 1:3, size = 1, prob = x))
# d <- data.frame(y = factor(y), x, g)
# 
# # Model (response 'y' must be a factor with 4 levels)
# fit <- brm(
#   y ~ x + g,
#   data = d,
#   family = categorical(),
#   chains = 4,
#   cores = 4,
#   backend = "cmdstanr",
#   threads = threading(8)  # Within-chain parallelization
# )
# 
# summary(fit)
# plot(fit, pars = "^b_")
library(stringi)

test_data <- test_data %>% rename(ciclo_vital = ciclo_vital_)

test_data$def_naturaleza <- as.factor(as.numeric(as.factor(test_data$def_naturaleza)))

# train_data %>%
#   mutate(def_naturaleza = def_naturaleza %>%
#            stri_trans_general("Latin-ASCII") %>%   # Elimina tildes
#            tolower() %>%                           # Convierte a minúsculas
#            gsub(" ", "_", .)                       # Reemplaza espacios por guión bajo
#   )

# train_data %>%
#   mutate(across(where(is.character), 
#                 ~ sapply(.x, function(x) {
#                   x <- stri_trans_general(x, "Latin-ASCII")  # quitar tildes
#                   x <- tolower(x)                             # a minúsculas
#                   x <- gsub(" ", "_", x)                      # reemplazar espacios
#                   return(x)
                # })))
# train_data <- train_data %>% rename(ciclo_vital = ciclo_vital_)
# 
# test_data$def_naturaleza <- as.numeric(as.factor(test_data$def_naturaleza))
# levels(train_data$def_naturaleza) <- levels(train_data$def_naturaleza) %>%
#   stringi::stri_trans_general("Latin-ASCII") %>%   # Quita tildes y ñ -> n
#   gsub(" ", "_", .) %>%                           # Espacio a _
#   gsub("[^A-Za-z0-9_]", "", .)

train_data <- train_data %>% rename(ciclo_vital = ciclo_vital_)

train_data$def_naturaleza <- as.factor(as.numeric(as.factor(train_data$def_naturaleza)))

fit_fr_t <- brm(
  def_naturaleza ~ ciclo_vital + agresor_group +dia_del_hecho + escolaridad + rango_de_hora_del_hecho_x_3_horas +escenario_del_hecho +zona_del_hecho ,
  data = train_data,
  family = categorical(),
  algorithm ="pathfinder",
  iter = 
  chains = 4,
  cores = 5,
  backend = "cmdstanr",
  threads = threading(8)  # Within-chain parallelization
)
saveRDS(fit_fr_t , "brms_total_path.rds")
# modelo 1 ciclo_vital + agresor_group
# modelo 2 ciclo_vital + agresor_group + dia_del_hecho + rango_de_hora_del_hecho_x_3_horas + escenario_del_hecho
summary(fit)
saveRDS(fit, "brms_2_cov.rds")
library(bayesplot)
library(posterior)
library(pROC)

# Energy diagnostic (should look like a normal distribution)
mcmc_neff(neff_ratio(fit))  

# Tree depth diagnostic (should rarely hit max_treedepth)
mcmc_treedepth(nuts_params)  

# Pairs plot to check for funnel-shaped pathologies
mcmc_pairs(fit, pars = c("b_Intercept[1]", "b_x1[1]"))  # Example parameters

pp_check(fit, type = "bars", nsamples = 100)

summary(fit)

# 2. Extraer las muestras para diagnóstico con posterior
posterior_samples <- as_draws_df(fit)

# 3. Diagnóstico Rhat y effective sample size (ESS)
rhat_vals <- rhat(fit)
neff_vals <- neff_ratio(fit) * posterior::niterations(fit)  # neff en número absoluto

print(rhat_vals)
print(neff_vals)

# Ideal: Rhat cerca de 1 ( < 1.01) y neff alto (cuanto más, mejor)

# 4. Traceplots para parámetros seleccionados (p.ej, coeficientes)
mcmc_trace(as_draws_array(fit), pars = c("b_mu2_ciclo_vitalJovenes", "b_mu2_agresor_groupConocido"))  # reemplaza con tus nombres de coef


# 5. Autocorrelación de las cadenas
mcmc_acf(as_draws_array(fit), pars = c("b_mu2_ciclo_vitalJovenes", "b_mu2_agresor_groupConocido"))

# 6. Densidades de las cadenas para ver mezcla
mcmc_dens_overlay(as_draws_array(fit), pars = c("b_mu2_ciclo_vitalJovenes", "b_mu2_agresor_groupConocido"))

# 7. Diagnóstico global con bayesplot: mezcla y problemas de muestreo
mcmc_rhat_hist(rhat_vals)
mcmc_neff_hist(neff_vals / posterior::niterations(fit)) # neff en proporción

# 8. Comprobar divergencias y advertencias
summary(fit)$warnings  # Ver advertencias de muestreo

# 9. Diagnóstico divergencias: extraer info y graficarlas
sampler_diagnostics <- nuts_params(fit)

library(ggplot2)
ggplot(sampler_diagnostics, aes(iteration, div)) + 
  geom_point(alpha=0.3) + 
  labs(title = "Divergent transitions over iterations")

# 10. Monitorear la energía y treedepth para problemas potenciales
ggplot(sampler_diagnostics, aes(iteration, treedepth)) + geom_point(alpha=0.3)
ggplot(sampler_diagnostics, aes(iteration, energy)) + geom_line()

########################
library(nimble)
