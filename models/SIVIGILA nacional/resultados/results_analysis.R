# En este archivo se hace evaluación del modelo y cálculo de métricas necesarias para el entendimiento del mismo

# Librerías necesarias
library(rstan)
library(bayesplot)
library(posterior)   
library(ggplot2)
library(dplyr)
library(tidyr)
library(pROC)
library(matrixStats) 
library(loo)
source(here("utils", "load_data.R"))

# Lectura y adecuación de la base de datos
base_datos_SIVIGILA <- read.csv(file.choose())

# Convertir a factor las variables para elegir nivel de referencia
base_datos_SIVIGILA <- base_datos_SIVIGILA %>%
  mutate(
    mujer_cabf = factor(mujer_cabf, levels = c("No", "Sí")),
    def_naturaleza = factor(def_naturaleza, levels = c("Negligencia y abandono", "Física", "Psicológica", "Sexual")),
    
    area = factor(area, levels = c("Cabecera municipal", "Centro poblado", "Rural disperso")),
    
    sexo_agre = factor(sexo_agre, levels = c("I","F", "M")),
    
    escenario = factor(escenario, levels = c("Espacio público y social","Vivienda")),
    conv_agre = factor(conv_agre, levels = c("No", "Sí")),
    pac_hos = factor(pac_hos, levels = c("No", "Sí")), 
  )

base_datos_SIVIGILA <- base_datos_SIVIGILA[!apply(is.na(base_datos_SIVIGILA), 1, any), ]


# Construir matriz de diseño
X <- model.matrix(ac_mental ~
                    edad_ + mujer_cabf + def_naturaleza +
                    area + sexo_agre + conv_agre  +
                    pac_hos + escenario,
                  data = base_datos_SIVIGILA)[, -1]

# Leer stanfit
SIVIGILA_model  <- readRDS(file.path(here(),"models","SIVIGILA nacional","resultados","SIVIGILA_model_1.rds"))
stan_array <- as.array(SIVIGILA_model)

# Extraer muestras
posterior_samples <- readRDS(file.path(here(),"models","SIVIGILA nacional","resultados","posterior_sample_SIVIGILA_1.rds"))

# Identificar betas
param_names <- dimnames(stan_array)[[3]]
colnames(X)

# Traceplots
sel_pars <- c("alpha", param_names[grepl("^beta\\[", param_names)][1:12])
sel_pars <- sel_pars[!is.na(sel_pars)]
mcmc_trace(stan_array, pars = sel_pars)

# Densidaddes
mcmc_dens_overlay(stan_array, pars = sel_pars)

# Autocorrelación
mcmc_acf(stan_array, pars = sel_pars, lags = 30)

# Imprimir resultados modelos
print(SIVIGILA_model)

# Calcular la media de los coeficientes estimados
alpha_hat <- mean(posterior_samples$alpha)
beta_hat <- colMeans(posterior_samples$beta)

# Calcular Odds Ratios
odds_ratios <- exp(beta_hat)
intercepto_OR <- exp(alpha_hat)

# Curva ROC
# Calcular probabilidades predichas
probs_predichas <- plogis(alpha_hat + X %*% beta_hat)

# Convertir a clases (umbral de 0.5)
predicciones_binarias <- ifelse(probs_predichas > 0.5, 1, 0)

# Matriz de confusión
confusion_matrix <- table(Real = base_datos_SIVIGILA$ac_mental, Predicho = predicciones_binarias)

# Cálculos necesarios para métricas de validación
TN <- confusion_matrix[1,1]
FP <- confusion_matrix[1,2]
FN <- confusion_matrix[2,1]
TP <- confusion_matrix[2,2]

accuracy <- (TP + TN) / (TP + TN + FP + FN)
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
f1_score <- 2 * (precision * recall) / (precision + recall)
roc_obj <- roc(base_datos_SIVIGILA$ac_mental, as.numeric(probs_predichas))

# Graficar curva ROC
plot(roc_obj, col = "#AE86D3")
title(sub = "Figura 3. Curva ROC del modelo de atención en salud mental SIVIGILA.")
print(auc(roc_obj))