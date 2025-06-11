##### Cargar librerías necesarias #####
library(dplyr)
library(ggplot2)
library(purrr)
library(here)
library(tidyr)
library(rstan)
library(bayesplot)
library(pROC)
library(explore)
library(readr)

##### Resumen y diagnóstico del modelo #####
# Leer resultado modelo
test_model <- readRDS(file.choose())
print(test_model, pars = c("alpha", "beta"), probs = c(0.025, 0.975))

##### Gráficos de diagnóstico #####
mcmc_acf(as.matrix(test_model, pars = c("alpha", "beta")))
mcmc_intervals(as.matrix(test_model, pars = "beta"))
mcmc_hist(as.matrix(test_model, pars = "beta"))
mcmc_trace(as.array(test_model), pars = c("alpha", "beta[1]", "beta[2]"))


# hacer gráficos de convergencia uno por uno
mcmc_trace(as.array(test_model), pars = c("alpha", "beta[1]", "beta[2]"))

##### Evaluación de ajuste y matriz de confusión #####

# Extraer coeficientes estimados
posterior_samples <- extract(test_model)

# Calcular la media de los coeficientes estimados
alpha_hat <- mean(posterior_samples$alpha)  
beta_hat <- colMeans(posterior_samples$beta)  

# Calcular probabilidades predichas
probs_predichas <- plogis(alpha_hat + X %*% beta_hat)

# Convertir a clases (umbral de 0.5)
predicciones_binarias <- ifelse(probs_predichas > 0.5, 1, 0)

# Matriz de confusión
confusion_matrix <- table(Real = data$ac_mental, Predicho = predicciones_binarias)



##### Métricas de desempeño y curva ROC #####
TN <- confusion_matrix[1,1]
FP <- confusion_matrix[1,2]
FN <- confusion_matrix[2,1]
TP <- confusion_matrix[2,2]

accuracy <- (TP + TN) / (TP + TN + FP + FN)
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
f1_score <- 2 * (precision * recall) / (precision + recall)

cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall (Sensibilidad):", recall, "\n")
cat("F1-Score:", f1_score, "\n")

roc_obj <- roc(data$ac_mental, as.numeric(probs_predichas))
auc(roc_obj)
plot(roc_obj, col = "#377eb8", main = "Curva ROC")


##### Interpretación: Odds Ratios #####
# Calcular Odds Ratios
odds_ratios <- exp(beta_hat)
intercepto_OR <- exp(alpha_hat)

# Crear tabla de resultados
tabla_resultados <- data.frame(
  Parametro = paste0("β", 1:length(odds_ratios)),
  Variable = colnames(X),
  Odds_Ratio = odds_ratios
)

# Mostrar tabla
print(tabla_resultados, row.names = FALSE)
