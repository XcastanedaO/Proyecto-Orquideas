library(INLA)
model_data <- train_data

model_data$def_naturaleza <- relevel(as.factor(model_data$def_naturaleza), ref = "Violencia interpersonal")
model_data$dia_del_hecho <- relevel(as.factor(model_data$dia_del_hecho), ref = "semana")


# Prepare the data for INLA's multinomial model
# INLA requires the response to be in a matrix format with one column per category
response_matrix <- model.matrix(~ def_naturaleza - 1, data = model_data)
colnames(response_matrix) <- levels(model_data$def_naturaleza)

# Create indices for the different categories
n_categories <- ncol(response_matrix)
n_obs <- nrow(model_data)
category_indices <- rep(1:n_categories, each = n_obs)

# Create the expanded dataset for INLA
inla_data <- data.frame(
  y = as.vector(response_matrix),
  category = category_indices,
  id = rep(1:n_obs, times = n_categories)
)

# Add the predictors to the expanded dataset
predictors <- model_data %>% 
  select(-def_naturaleza) %>% 
  slice(rep(1:n(), each = n_categories))

inla_data <- cbind(inla_data, predictors)

# Define the formula for the model
# We'll include all available predictors
formula <- y ~ -1 + 
  f(category, model = "iid", hyper = list(prec = list(initial = log(0.001), fixed = TRUE))) +
  f(id, model = "iid", hyper = list(prec = list(initial = log(0.001), fixed = TRUE))) +

  ciclo_vital_ + escolaridad 
# Fit the INLA model
inla_result <- inla(
  formula,
  family = "Poisson", # Using Poisson trick for multinomial
  data = inla_data,
  control.predictor = list(compute = TRUE),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE, config = TRUE),
  verbose = TRUE
)
save_data(train_data, "train_data")
saveRDS(inla_result, "inla_model.rds")
# Summary of the model
summary(inla_result)
inla_result$summary.fixed
print(inla_result$mlik)

par(mfrow = c(3, 3))
for (i in 1:nrow(inla_result$summary.hyperpar)) {
  plot(inla_result$marginals.hyperpar[[i]], 
       type = "l", 
       main = rownames(inla_result$summary.hyperpar)[i],
       xlab = "Value", ylab = "Density")
}

cpo <- inla_result$cpo$cpo
cat("Sum of log(CPO):", sum(log(cpo), na.rm = TRUE), "\n")
cat("Number of failures:", sum(inla_result$cpo$failure), "\n")


pred_probs <- matrix(inla_result$summary.fitted.values$mean, ncol = n_categories, byrow = TRUE)
colnames(pred_probs) <- levels(model_data$def_naturaleza)

# Compare with observed proportions
observed_props <- prop.table(table(model_data$def_naturaleza))
predicted_props <- colMeans(pred_probs)

comparison <- data.frame(
  Category = names(observed_props),
  Observed = as.numeric(observed_props),
  Predicted = as.numeric(predicted_props)
)

ggplot(comparison, aes(x = Category)) +
  geom_bar(aes(y = Observed, fill = "Observed"), stat = "identity", position = "dodge", alpha = 0.7) +
  geom_bar(aes(y = Predicted, fill = "Predicted"), stat = "identity", position = "dodge", alpha = 0.7) +
  labs(title = "Observed vs Predicted Proportions", y = "Proportion") +
  scale_fill_manual(values = c("Observed" = "blue", "Predicted" = "red")) +
  theme_minimal()

# 7. Check residuals (for each category)
residuals <- response_matrix - pred_probs
par(mfrow = c(2, 2))
for (i in 1:n_categories) {
  hist(residuals[,i], main = colnames(response_matrix)[i], xlab = "Residuals")
}

# 8. Posterior predictive checks (simulate from posterior and compare to observed)
# This requires sampling from the posterior
inla_samples <- inla.posterior.sample(100, inla_result)

# Compare simulated vs observed counts (simplified check)
sim_counts <- sapply(inla_samples, function(x) {
  probs <- matrix(exp(x$latent[grep("Predictor", rownames(x$latent))]), ncol = n_categories, byrow = TRUE)
  probs <- probs / rowSums(probs)
  sim_data <- t(apply(probs, 1, function(p) rmultinom(1, 1, p)))
  colSums(sim_data)
})

# Calculate credible intervals
cred_intervals <- apply(sim_counts, 1, quantile, probs = c(0.025, 0.5, 0.975))
observed_counts <- colSums(response_matrix)

pp_check <- data.frame(
  Category = colnames(response_matrix),
  Observed = observed_counts,
  Median = cred_intervals["50%",],
  Lower = cred_intervals["2.5%",],
  Upper = cred_intervals["97.5%",]
)

ggplot(pp_check, aes(x = Category, y = Observed)) +
  geom_point(size = 3, color = "blue") +
  geom_point(aes(y = Median), size = 3, color = "red") +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, color = "red") +
  labs(title = "Posterior Predictive Check", y = "Count") +
  theme_minimal()

###########################################

library(INLA)
library(dplyr)
library(tidyr)
library(pROC)
library(forcats) # Para manejar factores de forma útil (fct_relevel)

# Establecer un nivel de referencia para la variable de respuesta (importante para la interpretación)
# INLA usará el primer nivel alfabético por defecto; es mejor ser explícito.
train_data$def_naturaleza <- fct_relevel(train_data$def_naturaleza, "Violencia interpersonal")
train_data$id_caso <- seq(1:nrow(train_data))

# --- 2. Preparación de Datos (Apilamiento para INLA Multinomial) ---

# Obtener los nombres de las categorías de la variable de respuesta
response_levels <- levels(train_data$def_naturaleza)
n_response_levels <- length(response_levels)

# Crear el dataframe apilado
# Cada fila original se convierte en N_levels filas en el dataframe apilado.
# Cada una de estas N_levels filas representa una posible categoría de resultado.
# 'y' será 1 si esa fila representa la categoría observada, y 0 en caso contrario.
n_obs <- nrow(train_data)
df_stacked <- train_data %>%
  tidyr::expand_grid(outcome_level = response_levels) %>% # Crea todas las combinaciones
  mutate(
    y = as.integer(def_naturaleza == outcome_level),
    # Necesitamos un ID único para cada observación original que se repite
    # Esto es crucial para el efecto aleatorio que "une" las probabilidades
    original_obs_id = rep(1:n_obs, each = n_response_levels)
  ) %>%
  # Asegurarse que outcome_level sea un factor y con el mismo orden que la respuesta original
  mutate(outcome_level = factor(outcome_level, levels = response_levels))

# Verificar las primeras filas del dataframe apilado
head(df_stacked)
nrow(df_stacked) # Debe ser n_obs * n_response_levels

# --- 3. Definición y Entrenamiento del Modelo INLA ---

# Las covariables son las columnas que no son id_caso, def_naturaleza, y (las nuevas) y, outcome_level, original_obs_id
covariates_names <- setdiff(names(train_data), c("id_caso", "def_naturaleza"))

# Construir la fórmula del modelo
# y ~ 0 + outcome_level: Esto crea un efecto intercepto diferente para cada nivel de 'outcome_level'
# (uno de ellos será el de referencia, con coeficiente 0).
# outcome_level:covariable_i:beta_i: Es la interacción de la categoría con la covariable,
# representa el efecto de la covariable para esa categoría en comparación con la categoría base.

# La forma más sencilla si quieres efectos separados por cada outcome_level
# es incluir `outcome_level` en la fórmula, y dejar las covariables "normalmente".
# Sin embargo, para una multinomial clásica, usualmente quieres un efecto base
# para cada covariable + un efecto *diferencial* por cada outcome_level.
# Esto se logra típicamente con `outcome_level:covariable` o con `f(id, model="iid")` trucos.

# Para una multinomial logit donde 'Psicologica' es la base:
# El modelo estima coeficientes para "Fisica vs Psicologica", "Economica vs Psicologica", "Sexual vs Psicologica".
# Para cada covariable, su efecto también será diferencial.
# `f(original_obs_id, model = "iid", hyper = list(prec = list(initial = 1, fixed = TRUE)))`
# Este término es clave: fija la varianza del efecto aleatorio a cero (precisión infinita),
# lo que efectivamente "fuerza" las probabilidades de las 4 categorías a sumar 1 para cada
# `original_obs_id`. Es el "truco" para la regresión multinomial en INLA.

# NOTA: En la práctica, con 19 covariables categóricas, si cada una tiene ~3-4 niveles,
# ¡tendrás fácilmente más de 50-70 parámetros fijos, y esto se multiplica por (N_levels - 1)!
# Esto puede llevar a un gran número de parámetros.
# Asegúrate de que tu hardware pueda manejarlo.

# Formula alternativa si quieres coeficientes diferenciales para cada nivel
# `0 + outcome_level + outcome_level:(covariate_1 + covariate_2 + ...)`
# Esto es *mucho* más complejo porque tendrías (N_levels-1) * N_covs coeficientes para cada covariable.
# La interpretación se vuelve compleja.

# Una forma común y más interpretable es asumir que el intercepto cambia por outcome,
# y que los regresores tienen un efecto "global" y luego un efecto "diferencial" por outcome.
# Sin embargo, el "truco" de INLA para multinomial a menudo funciona mejor
# con una fórmula más sencilla, donde el `outcome_level` en la fórmula
# (ej. `y ~ 0 + outcome_level + vars + f(id)`) es clave.

# Vamos a construir la fórmula de la manera más directa para el truco multinomial,
# donde `outcome_level` se usa como un "intercepto" para cada grupo,
# y los efectos de las covariables son los mismos para todos los grupos (a menos que se interaccionen)
# o se definen en relación al baseline.

# Para obtener coeficientes específicos para cada comparación (ej. Fisca vs Psicologica),
# una forma robusta es incluir `outcome_level` y las covariables, y INLA gestionará las baselines.
# Aquí, `outcome_level` actuará como un factor de nivel en la parte fija, y las covariables
# afectarán la probabilidad de estar en cada nivel de `outcome_level` relativo al baseline.

# Para la formula con interceptos diferentes para cada nivel de respuesta
formula_string <- paste("y ~ 0 + outcome_level + ", paste(covariates_names, collapse = " + "))

# Agregamos el término crucial para el ID de observación original
formula_string <- paste(formula_string, "+ f(original_obs_id, model = 'iid', hyper = list(prec = list(initial = 1, fixed = TRUE)))")

model_formula <- as.formula(formula_string)
print(model_formula)

tic <- Sys.time()
model_inla <- inla(
  formula = model_formula,
  family = "Poisson", # Utilizamos una familia Poisson
  data = df_stacked,
  Ntrials = 1, # '1' porque cada fila representa un evento binario (ocurrió/no ocurrió)
  control.compute = list(
    dic = TRUE,
    waic = TRUE,
    cpo = TRUE,
    config = TRUE # Necesario para muestreo posterior y algunas métricas de ajuste si las quisiéramos
  ),
  control.predictor = list(compute = TRUE) # Necesario para obtener predicciones
)
saveRDS(model_inla, "models/model_inla_vs.rds")
toc <- Sys.time()
print(paste("Tiempo de entrenamiento:", round(toc - tic, 2), "minutos"))
summary(model_inla) # Esto puede ser muy largo

# --- 4. Validación de la Convergencia/Precisión de las Aproximaciones ---

# Revisar si INLA reportó algún problema
if (!is.null(model_inla$misc$warnings)) {
  cat("\nADVERTENCIAS DE INLA:\n")
  print(model_inla$misc$warnings)
} else {
  cat("\nNo se encontraron advertencias significativas de INLA.\n")
}

# Verificación de la convergencia global de la aproximación
cat("\nOK (TRUE si la aproximación se realizó exitosamente):\n")
print(model_inla$OK)

# Visualizar algunas distribuciones marginales de los parámetros fijos
# Escoge algunos coeficientes de interés para inspeccionar visualmente
# Por ejemplo, el intercepto de un nivel y un coeficiente de una covariable.

# Nombres de los parámetros fijos (coeficientes)
fixed_names <- rownames(model_inla$summary.fixed)
# Escoger algunos para graficar
params_to_plot <- c(
  grep("outcome_levelViolencia sexual", fixed_names, value = TRUE)[1],
  grep("ciclo_vital_Niñas y adolescentes", fixed_names, value = TRUE)[1],
  grep("rango_de_hora_del_hecho_x_3_horasNoche", fixed_names, value = TRUE)[1]
)

cat("\nGraficando algunas distribuciones marginales posteriores:\n")
par(mfrow = c(length(params_to_plot), 1), mar = c(4, 4, 2, 1) + 0.1)
for (param_name in params_to_plot) {
  if (param_name %in% fixed_names) {
    plot(model_inla$marginals.fixed[[param_name]], type = "l",
         main = paste("Posterior de", param_name), xlab = "Valor", ylab = "Densidad")
    # Añadir media y punto de 95% CI
    mean_val <- model_inla$summary.fixed[param_name, "mean"]
    Q025 <- model_inla$summary.fixed[param_name, "0.025quant"]
    Q975 <- model_inla$summary.fixed[param_name, "0.975quant"]
    abline(v = mean_val, col = "red", lty = 2)
    abline(v = c(Q025, Q975), col = "blue", lty = 3)
    legend("topright", legend = c("Media", "95% CI"), col = c("red", "blue"), lty = c(2, 3), cex = 0.8)
  }
}
par(mfrow = c(1, 1))

cat("\nLas distribuciones deben verse suaves y unimodales (a menos que haya razones para lo contrario).\n")


# --- 5. Cálculo del Área Bajo la Curva ROC (AUC) - Estrategia Uno-vs-Resto ---

# 1. Extraer los predictores lineales (eta)
# model_inla$summary.linear.predictor ya contiene los resúmenes para cada row en df_stacked
# Necesitamos la media de la distribución posterior del predictor lineal
preds_eta <- model_inla$summary.linear.predictor
model_inla$summary.linear.predictor
model_inla$subs
# Añadir los predictores lineales al df_stacked
df_stacked$eta <- preds_eta$mean

# 2. Reorganizar para aplicar Softmax
# Crear una tabla donde cada fila es una observacion original y las columnas son los etas para cada categoria
etas_wide <- df_stacked %>%
  select(original_obs_id, outcome_level, eta) %>%
  pivot_wider(names_from = outcome_level, values_from = eta, names_prefix = "eta_")

# Convertir etas a probabilidades usando softmax
# softmax(x_k) = exp(x_k) / sum(exp(x_j))
# Asumimos que la primera columna de intercepto ha sido fijada a 0 implícitamente por INLA
# No siempre es un efecto directo, pero en la formula y el truco de apilamiento, las betas son contrastes
# relativos al nivel base. Para el softmax, un 'eta' relativo a la base de 0 para la base es lo que se necesita.

# Para aplicar softmax, necesitamos todas las etas por observación.
# La baseline ("Psicologica") tiene sus coeficientes fijados a 0. Esto significa que su eta
# para el softmax es la suma de los efectos de las covariables para ese nivel base.
# Si la formula fue `y ~ 0 + outcome_level + covariates`, entonces `outcome_levelPsicologica`
# no aparecerá en `summary.fixed`, su "valor" implícito es 0 + los efectos base de las covariables.

# La manera correcta es tomar las predicciones `fitted.values` del modelo, que son las `lambda` de Poisson.
# En un modelo multinomial, las `lambda` son las probabilidades esperadas por categoría.
# Estas vienen en el orden del `df_stacked`.

probabilities <- model_inla$summary.fitted.values$mean

# Asignar estas probabilidades de vuelta al df_stacked
df_stacked$predicted_prob <- probabilities

# Reorganizar el df_stacked para que cada fila sea una observación original
# y las columnas sean las probabilidades previstas para cada categoría
probs_wide <- df_stacked %>%
  select(original_obs_id, outcome_level, predicted_prob) %>%
  pivot_wider(names_from = outcome_level, values_from = predicted_prob, names_prefix = "prob_") %>%
  # Unir con la verdadera categoría de respuesta
  left_join(train_data %>% select(id_caso, def_naturaleza), by = c("original_obs_id" = "id_caso"))

# Asegurarse de que el orden de las columnas de probabilidad coincida con los niveles de respuesta
probs_wide <- probs_wide %>%
  select(original_obs_id, def_naturaleza, all_of(paste0("prob_", response_levels)))

# Calcular AUC para cada clase (One-vs-Rest)
auc_results <- list()
for (level in response_levels) {
  # Crear la variable binaria para esta clase
  true_labels_binary <- as.numeric(probs_wide$def_naturaleza == level)
  
  # Usar las probabilidades predichas para esta clase
  predicted_probs_for_level <- probs_wide[[paste0("prob_", level)]]
  
  # Calcular ROC
  roc_obj <- roc(response = true_labels_binary, predictor = predicted_probs_for_level, quiet = TRUE)
  auc_results[[level]] <- auc(roc_obj)
  plot(roc_obj, main = paste("ROC Curve for", level, "vs All Others"), print.auc = TRUE, col = "blue")
}

cat("\nAUC para cada categoría (Uno-vs-Resto):\n")
print(unlist(auc_results))

# Calcular el AUC promedio (macro-average)
mean_auc <- mean(unlist(auc_results))
cat(paste("\nAUC promedio (Macro-average):", round(mean_auc, 3), "\n"))

# Para una métrica más completa, también podrías considerar la matriz de confusión
# (requiere un umbral de decisión, por ejemplo, eligiendo la clase con la probabilidad más alta)
# O métricas como la accuracy, precision, recall, F1-score.

# --- 6. Interpretación de los Coeficientes ---

cat("\n--- Interpretación de los Coeficientes (Razones de Odds) ---\n\n")

# Obtener los resúmenes de los parámetros fijos (coeficientes)
summary_fixed <- model_inla$summary.fixed

# Nivel de referencia de la variable de respuesta
baseline_response_level <- levels(train_data$def_naturaleza)[1]
cat(paste("Nivel de referencia (baseline) de def_naturaleza:", baseline_response_level, "\n\n"))

cat("Los Odds Ratios (ORs) se interpretan para cada categoría de respuesta, en comparación con la categoría de referencia (", baseline_response_level, ").\n")
cat("Para las covariables categóricas, los ORs son relativos al nivel de referencia de esa covariable (generalmente el primer nivel alfabético).\n\n")

# Extraer y mostrar los coeficientes y sus ORs
for (i in 1:nrow(summary_fixed)) {
  param_name <- rownames(summary_fixed)[i]
  mean_beta <- summary_fixed[i, "mean"]
  lower_ci <- summary_fixed[i, "0.025quant"]
  upper_ci <- summary_fixed[i, "0.975quant"]
  
  or <- exp(mean_beta)
  or_lower_ci <- exp(lower_ci)
  or_upper_ci <- exp(upper_ci)
  
  cat(paste0("Coeficiente: ", param_name, "\n"))
  cat(paste0("  Estimación (log-odds): ", round(mean_beta, 3), " (95% CI: [", round(lower_ci, 3), ", ", round(upper_ci, 3), "])\n"))
  cat(paste0("  Razón de Odds (OR):      ", round(or, 3), " (95% CI: [", round(or_lower_ci, 3), ", ", round(or_upper_ci, 3), "])\n"))
  
  # Interpretación específica
  if (grepl("outcome_level", param_name)) {
    # Coeficientes de los interceptos para cada categoría de respuesta
    level_name <- sub("outcome_level", "", param_name)
    cat(paste0("  Interpretación: Las odds de que la naturaleza de la violencia sea '", level_name, "' en lugar de '", baseline_response_level, "' son ", round(or, 2), " veces mayores cuando todas las demás covariables son cero (o están en su nivel de referencia).\n\n"))
  } else {
    # Coeficientes de las covariables
    # Necesitamos saber a qué contraste de respuesta se aplica (pero no está directamente en el nombre del coef)
    # Por la forma en que se construye el modelo (sin interacciones explícitas de outcome_level:covariable),
    # el efecto de cada covariable es común a todas las comparaciones vs la base.
    cat("  Interpretación:\n")
    if (or > 1) {
      cat(paste0("    Por cada unidad de aumento/nivel en '", param_name, "', las odds de que la naturaleza de la violencia sea CUALQUIERA DE LAS OTRAS CATEGORÍAS (Física, Económica, Sexual) en lugar de '", baseline_response_level, "' AUMENTAN en un factor de ", round(or, 2), ".\n\n"))
    } else if (or < 1) {
      cat(paste0("    Por cada unidad de aumento/nivel en '", param_name, "', las odds de que la naturaleza de la violencia sea CUALQUIERA DE LAS OTRAS CATEGORÍAS (Física, Económica, Sexual) en lugar de '", baseline_response_level, "' DISMINUYEN en un factor de ", round(or, 2), " (o aumentan en un factor de ", round(1/or, 2), " para la categoría de referencia).\n\n"))
    } else {
      cat(paste0("    No hay cambio significativo en las odds.\n\n"))
    }
    # Esto es una simplificación. Un modelo multinomial logit "completo"
    # tiene coeficientes de covariables que son específicos para cada comparación
    # (ej. beta_fisica_relacion, beta_economica_relacion).
    # Nuestra fórmula actual asume que el efecto de la covariable es el mismo para todos los contrastes (vs. baseline).
    # Si quieres efectos variables (que es más común), la formula debe ser:
    # `y ~ -1 + outcome_level + outcome_level:(covariable_1 + covariable_2 + ...) + f(original_obs_id, ...)`
    # Esa formula sería AÚN MÁS GRANDE en numero de coeficientes.
    # La interpretación anterior asume que `outcome_level` maneja solo los interceptos.
    # Si las covariables no tienen `outcome_level:` interacción, entonces su coeficiente
    # representa el efecto en el log-odds de cualquier alternativa vs la base.
  }
}

cat("\n--- Fin de la Interpretación ---\n")