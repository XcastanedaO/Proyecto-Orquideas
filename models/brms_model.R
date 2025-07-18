library(dplyr)
library(tibble)
source("utils/load_data.R")
library(UPG)
library(caret)

nf_data <- load_data("data_nf_modelo.csv", type = "processed")

train_index <- createDataPartition(nf_data$def_naturaleza, p = 0.7, list = FALSE)
train_data <- nf_data[train_index, ]
test_data <- nf_data[-train_index, ]


brm_model <- readr::read_rds(file = "brms_total_cov.rds")
getwd()
#####
# Instalar y cargar librerías necesarias
# install.packages("brms")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("forcats")
# install.packages("pROC")
# install.packages("bayesplot") # Para análisis visual de convergencia
# install.packages("cmdstanr") # Opcional, pero muy recomendado para rendimiento

library(brms)
library(dplyr)
library(tidyr)
library(forcats) # Para manejo de factores (fct_relevel)
library(pROC)    # Para curva ROC y AUC
library(bayesplot) # Para diagnósticos de MCMC

# Configurar cmdstanr como backend (opcional, pero recomendado para grandes datasets)
# Retirar el comentario si tienes cmdstanr instalado y configurado
set_cmdstan_path() # Asegúrate de que cmdstanr esté configurado correctamente
# options(brms.backend = "cmdstanr")

# --- 2. Ajuste del Modelo Bayesiano de Regresión Logística Multinomial con brms ---
##################
# Definir la fórmula del modelo
# La sintaxis es simple: respuesta ~ predictores
# brms detecta automáticamente que def_naturaleza es categórica y ajusta un modelo multinomial
# (K-1) ecuaciones logísticas, donde K es el número de categorías.
# Los coeficientes para la categoría de referencia (el primer nivel de def_naturaleza) se fijan a 0.
model_formula <- bf(def_naturaleza ~ ciclo_vital_ + 
                      agresor_group +dia_del_hecho + 
                      escolaridad + rango_de_hora_del_hecho_x_3_horas +
                      escenario_del_hecho +zona_del_hecho)

# # Definir priors (opcional pero recomendado para modelos bayesianos)
# # Priors weakly informative son un buen punto de partida.
# # Por defecto, brms usa priors razonables, pero especificarlas da más control.
# # `brms::get_prior(model_formula, data = df_original, family = categorical())` te dice los priors por defecto.
# model_priors <- c(
#   prior(normal(0, 2), class = b), # Para los coeficientes de las covariables
#   prior(normal(0, 5), class = Intercept) # Para los interceptos (éstos pueden ser más dispersos)
# )
# 
# cat("\nIniciando el ajuste del modelo brms...\n")
# tic <- Sys.time()
# brm_model <- brm(
#   formula = model_formula,
#   data = df_original,
#   family = categorical(), # Especifica el modelo logit multinomial
#   prior = model_priors,
#   chains = 4,             # Número de cadenas MCMC
#   iter = 2000,            # Total de iteraciones por cadena
#   warmup = 1000,          # Warmup (burn-in) por cadena
#   thin = 1,               # Thinning (tomar cada 'thin' muestra después de warmup)
#   cores = 4,              # Número de cores / hilos a usar (ajusta según tu CPU)
#   seed = 42,              # Semilla para reproducibilidad
#   control = list(adapt_delta = 0.95), # Aumentar si ves muchas divergencias
#   backend = "rstan"       # Puedes usar "cmdstanr" si lo configuraste para mejor rendimiento
#   # file = "brm_multinomial_model_large_data", # Guardar el modelo en disco mientras se ejecuta
#   # save_warmup = FALSE # No guardar las muestras de warmup para ahorrar memoria
# )
# toc <- Sys.time()
# cat(paste("Tiempo de ajuste del modelo:", round(difftime(toc, tic, units = "mins"), 2), "minutos\n"))

# Guardar el modelo ajustado
# saveRDS(brm_model, "brm_multinomial_model.rds")
# Para cargar: brm_model <- readRDS("brm_multinomial_model.rds")

# --- 3. Análisis Exhaustivo de Convergencia ---
#####################
cat("\n--- Análisis de Convergencia ---\n")

# 3.1. Resumen del modelo de brms
# Verifica Rhat (factor de escala de reducción potencial: < 1.01 es ideal)
# y ESS (Effective Sample Size: > 400 es bueno, más es mejor).
# ESS/iteración también es útil.
print(brm_model)

# 3.2. Diagnósticos visuales con bayesplot
# Gráficos de traza: Muestran el comportamiento de las cadenas. Deben parecer "orugas peludas" y bien mezcladas.
# Gráficos de densidad: Las densidades de las cadenas deben superponerse bien.
cat("\nGenerando gráficos de traza y densidad de las distribuciones posteriores...\n")

# Para todos los parámetros fijos (pueden ser muchos, considera seleccionar algunos)
plot(brm_model, N = 5, ask = FALSE) # Muestra los primeros 5 parámetros por defecto

# Alternativa: Usar mcmc_plot de bayesplot para mayor control
# Puedes seleccionar parámetros específicos si hay muchos
m_plot <- mcmc_plot(brm_model, type = "acf") # Autocorrelación
m_plot <- mcmc_plot(brm_model, type = "rhat_hist") # Rhat distribution
m_plot <- mcmc_plot(brm_model, type = "neff_hist") # ESS distribution
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

cat("\n--- Cálculo de Curva ROC y AUC (Uno-vs-Resto) ---\n")

# Obtener las probabilidades predichas para cada categoría y cada observación
# ¡IMPORTANTE! Usamos newdata = df_test para predecir en el conjunto de prueba
cat("Generando predicciones en el conjunto de PRUEBA (esto puede tardar)...\n")
tic_pred <- Sys.time()
test_data$def_naturaleza <- as.factor(as.numeric(as.factor(test_data$def_naturaleza)))

pred_probs_test <- fitted(brm_model, newdata = test_data, summary = FALSE, prob_method = "softmax_for_cat")
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


cat("\n--- Fin de la Interpretación --- \n")

#### pathfinder
cat("\nIniciando el ajuste del modelo brms con Pathfinder...\n")
tic <- Sys.time()
brm_model_pathfinder <- brm(
  formula = model_formula,
  data = df_original,
  family = categorical(),
  prior = model_priors, # Usa los priors definidos previamente
  # Aquí especificamos el método:
  method = "pathfinder",
  # Opciones específicas para pathfinder (pueden ser ajustadas)
  elarg_iter: #número de iteraciones de optimización para encontrar la moda (sensibilidad)
  n_paths: # número de caminos para explorar (robustez)
  num_draws: 5000# número de muestras a obtener de la aproximación
  pathfinder = pathfinder_args(iter = 10000, num_paths = 4, num_draws = 4000), # Ajusta estos según tus necesidades
  seed = 42,
  cores = 8 # Ajusta según tu CPU
)
toc <- Sys.time()
cat(paste("Tiempo de ajuste del modelo con Pathfinder:", round(difftime(toc, tic, units = "mins"), 2), "minutos\n"))

# Puedes usar summary() o print() para ver los resultados iniciales
print(brm_model_pathfinder)

# Para los diagnósticos, pathfinder no tiene Rhat o ESS como MCMC.
# Te dará advertencias si la aproximación no fue buena. Revisa el output cuidadosamente.
# Las funciones `pp_check()` siguen siendo muy útiles para evaluar el ajuste del modelo.
pp_check(brm_model_pathfinder, type = "bars", ndraws = 50)
