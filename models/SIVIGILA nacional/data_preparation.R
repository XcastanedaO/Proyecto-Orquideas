# Este archivo tiene como  re-codificar las variables para el ajuste del modelo

# Librerías y funciones necesarias
library(here)
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

# Guardar base de datos
save_data(data_model_SIVIGILA, "data_model_SIVIGILA", type = "processed", format = "csv")
