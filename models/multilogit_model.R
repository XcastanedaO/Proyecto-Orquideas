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

fit <- brm(
  def_naturaleza ~ ciclo_vital + agresor_group +dia_del_hecho + escolaridad + rango_de_hora_del_hecho_x_3_horas +escenario_del_hecho +zona_del_hecho ,
  data = train_data,
  family = categorical(),
  algorithm ="fullrank",
  chains = 4,
  cores = 5,
  backend = "cmdstanr",
  threads = threading(8)  # Within-chain parallelization
)
saveRDS(fit_fr_t , "brms_total_fullrank.rds")
fit <- readRDS(file = "brms_total_fullrank.rds")
# modelo 1 ciclo_vital + agresor_group
# modelo 2 ciclo_vital + agresor_group + dia_del_hecho + rango_de_hora_del_hecho_x_3_horas + escenario_del_hecho
summary(fit) 

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
cat("\n--- Análisis de Convergencia ---\n")

# 3.1. Resumen del modelo de brms
# Verifica Rhat (factor de escala de reducción potencial: < 1.01 es ideal)
# y ESS (Effective Sample Size: > 400 es bueno, más es mejor).
# ESS/iteración también es útil.
print(fit_fr_t)

# 3.2. Diagnósticos visuales con bayesplot
# Gráficos de traza: Muestran el comportamiento de las cadenas. Deben parecer "orugas peludas" y bien mezcladas.
# Gráficos de densidad: Las densidades de las cadenas deben superponerse bien.
cat("\nGenerando gráficos de traza y densidad de las distribuciones posteriores...\n")


# Para todos los parámetros fijos (pueden ser muchos, considera seleccionar algunos)
plot(fit, N =4, ask = TRUE, )+ # Muestra los primeros 5 parámetros por defecto
legend(legend = "")
# Alternativa: Usar mcmc_plot de bayesplot para mayor control
# Puedes seleccionar parámetros específicos si hay muchos
m_plot <- mcmc_plot(fit_fr_t, type = "acf") # Autocorrelación
m_plot <- mcmc_plot(fit_fr_t, type = "rhat_hist") # Rhat distribution
m_plot <- mcmc_plot(fit, type = "neff_hist") # ESS distribution
print(m_plot)

# O extraer las muestras y usar bayesplot directamente
posterior_samples <- as_draws_df(brm_model)

# Ej. Para un parámetro específico (ajusta los nombres según tu output)
mcmc_trace(posterior_samples, pars = c("b_mu2_ciclo_vitalJovenes",
                                       "b_mu2_agresor_groupExMpareja"))
mcmc_dens_overlay(posterior_samples, c("b_mu2_ciclo_vitalJovenes",
                                       "b_mu2_agresor_groupExMpareja"))
mcmc_acf(posterior_samples, pars = c("b_mu2_ciclo_vitalJovenes",
                                     "b_mu2_agresor_groupExMpareja"))

# 3.3. Chequeo de divergencias (Problemas de muestreo)
# Las divergencias indican que el muestreador no pudo explorar bien el espacio.
# Un número alto es una bandera roja. Ajustar `adapt_delta` (ej. 0.9, 0.95, 0.99) o `max_treedepth`.
cat("\nRevisando diagnósticos de divergencias y transiciones de Stan:\n")
diagnostic_summary <- brm_model$fit$diagnostic_summary()
print(diagnostic_summary)
if (diagnostic_summary$num_divergences > 0) {
  cat(paste0("¡ADVERTENCIA! Se detectaron ", diagnostic_summary$num_divergences, " divergencias.\n"))
  cat("Considere aumentar `adapt_delta` (ej., 0.95 o 0.99) en `control` al ajustar el modelo.\n")
} else {
  cat("No se detectaron divergencias. ¡Buena señal!\n")
}

# 3.4. Posterior Predictive Checks (PPC) - ¿El modelo captura los patrones observados?
# Compara los datos reales con los datos predichos por el modelo.
# Para modelos categóricos, pp_check(type = "bars") o pp_check(type = "freq") son útiles.
cat("\nRealizando Posterior Predictive Checks (PPC)...\n")
pp_check(brm_model, type = "bars", ndraws = 50) # ndraws = número de replicas posteriores a dibujar
pp_check(brm_model, type = "freq", ndraws = 50) # ndraws = número de replicas posteriores a dibujar
cat("Los PPCs deben mostrar que las distribuciones de los datos simulados desde el modelo son similares a los datos observados.\n")

# 3.5. R-cuadrado bayesiano (Goodness of Fit)
cat("\nCalculando R-cuadrado bayesiano:\n")
bayes_R2(brm_model)

# --- 4. Curva ROC y AUC ---
test_index <- createDataPartition(test_data$def_naturaleza, p = 0.7, list = FALSE)

test_data_roc <- test_data[-test_index, ]



cat("\n--- Cálculo de Curva ROC y AUC (Uno-vs-Resto) ---\n")

# Obtener las probabilidades predichas para cada categoría y cada observación
# ¡IMPORTANTE! Usamos newdata = df_test para predecir en el conjunto de prueba
cat("Generando predicciones en el conjunto de PRUEBA (esto puede tardar)...\n")
tic_pred <- Sys.time()
test_data_roc$def_naturaleza <- as.factor(as.numeric(as.factor(test_data_roc$def_naturaleza)))
test_data_roc <- test_data_roc %>% rename(ciclo_vital = ciclo_vital_)

pred_probs_test <- fitted(fit, newdata = test_data_roc, summary = FALSE, prob_method = "softmax_for_cat")
toc_pred <- Sys.time()
cat(paste("Tiempo para generar predicciones en el test set:", round(difftime(toc_pred, tic_pred, units = "secs"), 2), "segundos\n"))

# Las dimensiones son (observaciones_test x iteraciones_MCMC x categorías_respuesta)
n_draws <- dim(pred_probs_test)[2] # Número de muestras posteriores
n_obs_test <- dim(pred_probs_test)[1] # Número de observaciones en el conjunto de prueba
response_levels <- levels(as.factor(train_data$def_naturaleza)) # Usar los niveles originales, ya que df_test podría no tenerlos todos si es muy pequeño
n_response_levels <- length(response_levels)

# Inicializar lista para guardar los AUCs por cada muestra y categoría
auc_per_draw_and_category <- matrix(NA, nrow = n_draws, ncol = n_response_levels)
colnames(auc_per_draw_and_category) <- response_levels

# Obtener las etiquetas verdaderas del conjunto de prueba
true_labels_test <- test_data$def_naturaleza

cat("Calculando AUC para cada muestra posterior para robustez (esto puede tardar)...\n")

for (i_draw in 1:n_draws) {
  for (j_level in 1:n_response_levels) {
    current_level <- response_levels[j_level]
    # Etiquetas binarias: 1 si es la clase actual, 0 si no (del test set)
    true_labels_binary <- as.numeric(true_labels_test == current_level)
    
    # Probabilidades predichas para la clase actual desde esta muestra MCMC (del test set)
    predicted_probs_for_level <- pred_probs_test[, i_draw, j_level]
    
    # Calcular ROC. Añadir un check si solo hay una clase presente
    if (length(unique(true_labels_binary)) > 1) {
      roc_obj <- roc(response = true_labels_binary,
                     predictor = predicted_probs_for_level,
                     quiet = TRUE,
                     direction = "<") # Asegúrate de que las probabilidades más altas corresponden a "positivos"
      auc_per_draw_and_category[i_draw, j_level] <- auc(roc_obj)
    } else {
      auc_per_draw_and_category[i_draw, j_level] <- NA # No se puede calcular AUC si solo hay una clase
    }
  }
}

# Calcular la media y CI del AUC para cada categoría
mean_aucs <- apply(auc_per_draw_and_category, 2, mean, na.rm = TRUE)
lower_ci_aucs <- apply(auc_per_draw_and_category, 2, quantile, probs = 0.025, na.rm = TRUE)
upper_ci_aucs <- apply(auc_per_draw_and_category, 2, quantile, probs = 0.975, na.rm = TRUE)

cat("\nAUC para cada categoría en el conjunto de PRUEBA (Uno-vs-Resto):\n")
auc_summary_test <- data.frame(
  Category = response_levels,
  Mean_AUC = round(mean_aucs, 3),
  CI_2.5 = round(lower_ci_aucs, 3),
  CI_97.5 = round(upper_ci_aucs, 3)
)
print(auc_summary_test)

# AUC promedio (Macro-average) en el conjunto de PRUEBA
overall_mean_auc_test <- mean(mean_aucs, na.rm = TRUE)
cat(paste("\nAUC promedio general en el conjunto de PRUEBA (Macro-average):", round(overall_mean_auc_test, 3), "\n"))

# Para una visualización de ejemplo de una curva ROC (solo para la media de las predicciones)
# Podemos tomar la media de las probabilidades predichas sobre las muestras MCMC
mean_pred_probs_test <- apply(pred_probs_test, c(1, 3), mean) # promedio sobre las muestras
colnames(mean_pred_probs_test) <- response_levels

par(mfrow = c(1, n_response_levels))
for (j_level in 1:n_response_levels) {
  current_level <- response_levels[j_level]
  true_labels_binary <- as.numeric(true_labels_test == current_level)
  if (length(unique(true_labels_binary)) > 1) {
    roc_obj_mean <- roc(response = true_labels_binary,
                        predictor = mean_pred_probs_test[, j_level],
                        quiet = TRUE,
                        direction = "<")
    plot(roc_obj_mean, main = paste("ROC for", current_level, "(Test Set)"),
         print.auc = TRUE, col = "blue", print.thres = "best", cex.axis = 0.8, print.auc.cex = 0.8)
  } else {
    plot(1, type="n", main=paste("ROC for", current_level, "(Test Set)\nNo variance in class"), xlab="", ylab="", axes=F)
  }
}
par(mfrow = c(1,1))

# --- 5. Interpretación de los Coeficientes y Razones de Odds ---

cat("\n--- Interpretación de los Coeficientes y Razones de Odds (ORs) ---\n")

# Obtener un resumen de los efectos fijos (coeficientes)
fixed_effects_summary <- fixef(brm_model, summary = TRUE, probs = c(0.025, 0.975))
print(fixed_effects_summary)

# Extraer las muestras posteriores de los coeficientes para calcular los ORs y sus intervalos de credibilidad
posterior_fixed_effects <- as_draws_df(brm_model, variable = "^b_") %>%
  select(-.chain, -.iteration, -.draw) # Excluir columnas de muestreo

# Nivel de referencia de la variable de respuesta
baseline_response <- levels(df_original$def_naturaleza)[1]
cat(paste0("\nCategoría de referencia para def_naturaleza: '", baseline_response, "'.\n"))
cat("Todos los Odds Ratios (ORs) corresponden a: 'Categoría X' vs. '", baseline_response, "'.\n\n")

# Mostrar y explicar los Odds Ratios
cat("Odds Ratios y sus intervalos de credibilidad:\n")
or_summary_list <- list()

for (param_name in colnames(posterior_fixed_effects)) {
  # Asegurarse de que no sea un intercepto general si hubiere, aunque con categorical() son específicos
  if (startsWith(param_name, "b_")) { # Coeficientes de covariables
    # Extraer los nombres de la categoría de respuesta y de la covariable/nivel
    # La nomenclatura de brms es b_{covariable_level}_{response_level}
    # Por ejemplo: b_contexto_tipoUrbano_def_naturalezaFisica
    parts <- strsplit(param_name, "_")[[1]]
    covariate_part <- parts[2]
    response_part <- parts[3] # Esto es la categoría de respuesta no-referencia
    
    # Para entender el nivel de la covariable si es dummy codificada
    if (length(parts) > 3) { # Es un nivel de covariable categórica
      covariate_original_name <- sub(covariate_part, "", param_name) # Extract original covariate name
      covariate_level <- parts[2]
      # brms usa treatment coding (o dummy coding) por defecto. El primer nivel alfabético es el referente.
      # Por ejemplo, para contexto_tipo -> Rural, Urbano, SubUrbano. Si Rural es ref, b_contexto_tipoUrbano significa Urbano vs Rural.
    } else {
      covariate_level <- "Intercepto" # Caso de los interceptos directamente relacionados con la categoría de respuesta
    }
    
    
    or_samples <- exp(posterior_fixed_effects[[param_name]])
    or_mean <- mean(or_samples)
    or_ci <- quantile(or_samples, c(0.025, 0.975))
    
    or_summary_list[[param_name]] <- data.frame(
      Parameter = param_name,
      Response_Category = response_part,
      Mean_OR = round(or_mean, 3),
      CI_2.5_OR = round(or_ci[1], 3),
      CI_97.5_OR = round(or_ci[2], 3)
    )
    
    cat(paste0("Parámetro: '", param_name, "' (efecto sobre '", response_part, "' vs '", baseline_response, "')\n"))
    cat(paste0("  Odds Ratio Medio: ", round(or_mean, 3), " (95% CI: [", round(or_ci[1], 3), ", ", round(or_ci[2], 3), "])\n"))
    
    # Interpretación detallada
    if (grepl("^b_Intercept", param_name)) {
      cat(paste0("  Interpretación: Cuando todas las covariables están en sus niveles de referencia (o en cero para numéricas), las odds de que la naturaleza de la violencia sea '", response_part, "' en lugar de '", baseline_response, "' son ", round(or_mean, 2), " veces mayores.\n\n"))
    } else {
      # Asumiendo que brms hace dummy coding (el primer nivel alfabético de la covariable es la referencia)
      # Esto requiere saber cuál es el nivel de referencia de la covariable
      # Por ejemplo, para contexto_tipo (Rural, Urbano, SubUrbano), si 'Rural' es el primer alfabético, es el referente.
      # Entonces b_contexto_tipourbano_def_naturalezaFisica representa Urbano vs Rural
      
      # Podemos intentar deducir el nivel de referencia de la covariable
      covariate_original_name <- sub(paste0("^b_", covariate_part, "_def_naturaleza", response_part), covariate_part, param_name)
      # Obtener el nombre de la covariable y el nivel codificado
      parts_cov <- strsplit(covariate_original_name, "_")
      base_cov_name <- parts_cov[[1]][1] # e.g., contexto
      
      # Buscar el nombre completo de la covariable en el dataset para inferir su tipo
      full_cov_name <- names(df_original)[grepl(base_cov_name, names(df_original))][1]
      if (!is.null(full_cov_name) && is.factor(df_original[[full_cov_name]])) {
        ref_cov_level <- levels(df_original[[full_cov_name]])[1]
        cat_level <- sub(base_cov_name, "", covariate_original_name)
        cat_level <- sub("^_", "", cat_level) # remove leading underscore if any
        
        cat(paste0("  Interpretación: Cambiando la covariable '", full_cov_name, "' de su nivel de referencia ('", ref_cov_level, "') a '", cat_level, "', las odds de que la naturaleza de la violencia sea '", response_part, "' en lugar de '", baseline_response, "' son ", round(or_mean, 2), " veces mayores (manteniendo otras covariables constantes).\n"))
        if (or_mean > 1 && or_ci[1] > 1) {
          cat(paste0("    Esto indica que el nivel '", cat_level, "' está FUERTEMENTE asociado con una mayor probabilidad de que la violencia sea '", response_part, "' sobre '", baseline_response, "'.\n\n"))
        } else if (or_mean < 1 && or_ci[2] < 1) {
          cat(paste0("    Esto indica que el nivel '", cat_level, "' está FUERTEMENTE asociado con una menor probabilidad de que la violencia sea '", response_part, "' sobre '", baseline_response, "'.\n\n"))
        } else {
          cat("    El intervalo de credibilidad incluye 1, sugiriendo que no hay una fuerte evidencia de un efecto significativo o direccional claro.\n\n")
        }
      } else {
        # Para covariables numéricas, la interpretación sería por unidad de cambio
        cat(paste0("  Interpretación: Por cada UNIDAD de aumento en la covariable '", covariate_part, "', las odds de que la naturaleza de la violencia sea '", response_part, "' en lugar de '", baseline_response, "' son ", round(or_mean, 2), " veces mayores (manteniendo otras covariables constantes).\n"))
        if (or_mean > 1 && or_ci[1] > 1) {
          cat(paste0("    Esto indica que un aumento en '", covariate_part, "' está FUERTEMENTE asociado con una mayor probabilidad de que la violencia sea '", response_part, "' sobre '", baseline_response, "'.\n\n"))
        } else if (or_mean < 1 && or_ci[2] < 1) {
          cat(paste0("    Esto indica que un aumento en '", covariate_part, "' está FUERTEMENTE asociado con una menor probabilidad de que la violencia sea '", response_part, "' sobre '", baseline_response, "'.\n\n"))
        } else {
          cat("    El intervalo de credibilidad incluye 1, sugiriendo que no hay una fuerte evidencia de un efecto significativo o direccional claro.\n\n")
        }
      }
    }
  }
}

# Opcional: Visualización de los efectos condicionale
# Muestra cómo las probabilidades predichas para cada categoría de respuesta cambian
# a medida que una covariable varía, manteniendo otras en su media/o referente.
cat("\nVisualizando efectos condicionales de algunas covariables...\n")
# plot(conditional_effects(brm_model, "agresor_relacion"), points = TRUE)
# plot(conditional_effects(brm_model, "victima_ingresos"), points = TRUE)

# Puedes generar plots para cada covariable
# for (cov_name in names(df_original)[!names(df_original) %in% c("id_caso", "def_naturaleza")]) {
#   print(plot(conditional_effects(brm_model, cov_name), points = FALSE)) # points=FALSE para datasets grandes
# }

