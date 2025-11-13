# Este archivo permite ajustar un modelo logistico donde la variable de respuesta es salud mental.
# Se consideran todos los registros de la base de datos

# Librerías necesarias
library(dplyr)
library(tidyr)
library(rstan)

# Leer base de datos
data_model_SIVIGILA <- read.csv("data_model_SIVIGILA.csv")

data_model_SIVIGILA <- read.csv(file.choose())

# Convertir a factor las variables para elegir nivel de referencia
data_model_SIVIGILA <- data_model_SIVIGILA %>%
  mutate(
    mujer_cabf = factor(mujer_cabf, levels = c("No", "Sí")),
    def_naturaleza = factor(def_naturaleza, levels = c("Negligencia y abandono", "Física", "Psicológica", "Sexual")),
    
    area = factor(area, levels = c("Cabecera municipal", "Centro poblado", "Rural disperso")),
    
    sexo_agre = factor(sexo_agre, levels = c("I","F", "M")),
    
    escenario = factor(escenario, levels = c("Espacio público y social","Vivienda")),
    conv_agre = factor(conv_agre, levels = c("No", "Sí")),
    pac_hos = factor(pac_hos, levels = c("No", "Sí")), 
  )

# Muestreo estratificado
# Calcular proporciones por categoría de respuesta
prop_categ <- prop.table(table(data_model_SIVIGILA$ac_mental))
tam_categ <- round(10000 * prop_categ)

# Realizar muestreo estratificado según proporciones
set.seed(123)
muestra <- data_model_SIVIGILA %>%
  split(.$ac_mental) %>%
  map2_df(tam_categ, sample_n)

# Verificar que las proporciones en la muestra se mantengan
prop.table(table(muestra$ac_mental))

# Construir matriz de diseño
X <- model.matrix(ac_mental ~ 
                      edad_ + mujer_cabf + def_naturaleza +
                      area + sexo_agre + conv_agre  + 
                      pac_hos + escenario,
                    data = muestra)[, -1]

# Preparar datos para stan
data_stan <- list(
  N = nrow(muestra),
  K = ncol(X), ## Efectos fijos
  X = X,
  y = as.integer(muestra$ac_mental)
)

SIVIGILA_model <- stan(
  file = file.choose(),#"logistic_model.stan",
  data = muestra,
  iter = 50,
  chains = 4,
  warmup = 1,
  cores = 30
)

saveRDS(SIVIGILA_model, file = "SIVIGILA_model.rds")
posterior_sample <- extract(SIVIGILA_model)
saveRDS(posterior_sample_SIVIGILA, file = "posterior_sample_SIVIGIL.rds")
