# Este archivo tiene como objetivo hacer pruebas de multicolinealidad para la base de datos con que se ajsutaron los modelos

# Librerías y funciones necesarias
library(dplyr)
library(here)
library(car)
source(here("utils", "load_data.R"))

# Leer base de datos
data_model_SIVIGILA <- readRDS(file.choose())

# Selección de variables de interés
data_model_SIVIGILA <- data_model_SIVIGILA %>% select(c("ac_mental",
                                                        "edad_", "mujer_cabf","def_naturaleza",
                                                        "area", "sexo_agre", "conv_agre", "pac_hos" , "escenario"))

# Re-codificación de variables
data_model_SIVIGILA <- data_model_SIVIGILA %>%  mutate(
  ac_mental = case_when(
    ac_mental == "1" ~ 1,
    ac_mental == "2" ~ 0,
    TRUE ~ as.numeric(ac_mental)),
  
  edad_ = as.numeric(edad_),
  
  mujer_cabf = case_when(
    mujer_cabf == "1" ~ "Sí",
    mujer_cabf == "2" ~ "No",
    TRUE ~ mujer_cabf),
  
  escenario = case_when(
    escenario %in% c("7", "12", "8", "9","3", "11", "4","10", "1") ~ "Espacio público y social",
    escenario == "2" ~ "Vivienda",
    TRUE ~ escenario),
  
  def_naturaleza = case_when(
    def_naturaleza == "1" ~ "Física",
    def_naturaleza == "2" ~ "Psicológica",
    def_naturaleza == "3" ~ "Negligencia y abandono",
    def_naturaleza == "5" ~ "Sexual",
    def_naturaleza == "6" ~ "Sexual",
    def_naturaleza == "7" ~ "Sexual",
    def_naturaleza == "10" ~ "Sexual",
    def_naturaleza == "12" ~ "Sexual",
    def_naturaleza == "14" ~ "Sexual",
    def_naturaleza == "15" ~ "Sexual",
    TRUE ~ def_naturaleza),
  
  area = case_when(
    area == "1" ~ "Cabecera municipal",
    area == "2" ~ "Centro poblado",
    area == "3" ~ "Rural disperso",
    TRUE ~ area),
  
  conv_agre = case_when(
    conv_agre == "1" ~ "Sí",
    conv_agre == "2" ~ "No",
    TRUE ~ conv_agre),
  
  pac_hos = case_when(
    pac_hos == "1" ~ "Sí",
    pac_hos == "2" ~ "No",
    TRUE ~ pac_hos)
)

# Seleccionar rango de edad de interés
data_model_SIVIGILA <- data_model_SIVIGILA %>% filter(edad_ >= 14 & edad_ <= 50)

# Eliminar filas con valores faltantes
data_model_SIVIGILA <- data_model_SIVIGILA[!apply(is.na(data_model_SIVIGILA), 1, any), ]

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

# Adecuación de la base de datos
modelo <- glm(ac_mental ~ edad_ + mujer_cabf + def_naturaleza + area + sexo_agre + conv_agre + pac_hos + escenario,
              data = data_model_SIVIGILA, family = binomial)

# Extraer la matriz de diseño
X <- model.matrix(modelo)

# Calcular VIF generalizado
library(car)

# GVIF para detectar multicolinealidad
vif_resultados <- vif(modelo)
vif_resultados
#------------------------------------------------------------------------------
# Todas las variables tienen un valor VIF que se mantiene entre 1 y 1.3.
# Esto indica que, la colinealidad es mínima.
#------------------------------------------------------------------------------


# Rango de la matriz de diseño
rango_X <- qr(X)$rank

# Número total de columnas de la matriz X
columnas_X <- ncol(X)

rango_X
columnas_X

if (rango_X < columnas_X) {
  print("⚠️ Hay multicolinealidad perfecta o casi perfecta en la matriz del modelo.")
} else {
  print("✔️ No hay multicolinealidad perfecta en la matriz del modelo.")
}





# Prueba t para comparar edades entre los grupos de atención en salud mental

t.test(edad_ ~ ac_mental, data = data_model_SIVIGILA)
#------------------------------------------------------------------------------
# H0: mediaNO = mediaSI
# H1: mediaNO diferente mediaSI

# - media grupo 0: 27.86 años
# - media grupo 1: 26.10 años
# - diferencia ~ 1.76 años
# - vp < 2e-16: estadísticamente significativa

# La magnitud de la diferencia es muy pequeña: solo 1.7 años de diferencia entre 
# grupos, aunque resulte estadísticamente significativa (puede ser por la muestra grande)
#------------------------------------------------------------------------------


# Modelo logístico univariado para la edad
m_uni <- glm(ac_mental ~ edad_, data = data_model_SIVIGILA, family = binomial)
sum_m_uni <- summary(m_uni)

# Odds ratio del efecto de edad_
exp(sum_m_uni$coefficients[2])
#------------------------------------------------------------------------------
# El odds ratio para edad_ en el modelo univariado es de 0.9812 aprox. 
# Esto indica que, cada que la edad aumenta en un año, la probabilidad de 
# recibir atención en salud mental cambia menos del 1%.


# Aunque, el parámetro da estadísticamente significativo, esto indica que el efecto 
# de la edad sobre la variable respuesta tiene un efecto pero es muy pequeño (-0.018)
#------------------------------------------------------------------------------



# Modelos con y sin la variable edad_
m_con <- glm(ac_mental ~ edad_ + mujer_cabf + def_naturaleza + area +
               sexo_agre + conv_agre + pac_hos + escenario,
             data = data_model_SIVIGILA, family = binomial)

m_sin <- glm(ac_mental ~ mujer_cabf + def_naturaleza + area +
               sexo_agre + conv_agre + pac_hos + escenario,
             data = data_model_SIVIGILA, family = binomial)


library(pROC)

roc_con <- roc(data_model_SIVIGILA$ac_mental, fitted(m_con))
roc_sin <- roc(data_model_SIVIGILA$ac_mental, fitted(m_sin))

auc(roc_con)
auc(roc_sin)
#------------------------------------------------------------------------------
# Los valores de AUC para los modelos con y sin la variable edad_ son casi idénticos.
# Esto significa que incluir edad_ no mejora la capacidad predictiva del modelo.
#
# La diferencia en AUC es < 0.01, lo cual se considera irrelevante.
# Esto refuerza la conclusión de que edad_ no aporta valor predictivo al modelo.
#------------------------------------------------------------------------------




# Distribución de la edad según la variable respuesta
library(ggplot2)
ggplot(data_model_SIVIGILA, aes(x = factor(ac_mental),
                                y = edad_,
                                fill = factor(ac_mental))) +
  geom_violin(alpha = 0.3) +
  geom_boxplot(width = 0.2, outlier.alpha = 0) +
  labs(title = "Distribución de edad según atención",
       x = "Atención en salud mental (0 = No, 1 = Sí)",
       y = "Edad") +
  theme_minimal() +
  guides(fill = "none")



