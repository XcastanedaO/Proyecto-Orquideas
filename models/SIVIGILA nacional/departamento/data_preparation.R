# Este archivo tiene como objetivo preparar la data como se requiere para el modelo con departamento y región: 
# recodificación de variables, restricción de la edad y registros completos

# Librerías y funciones necesarias
library(dplyr)
library(here)
source(here("utils", "load_data.R"))

# Leer base de datos con NA y todas las columnas de interés: SIVIGILA_debugged_v2.rds
data_model_SIVIGILA <- readRDS(file.choose())

# Selección de variables de interés
data_model_SIVIGILA <- data_model_SIVIGILA %>% select(c("ac_mental",
                                "edad_", "mujer_cabf","def_naturaleza",
                                "area", "sexo_agre", "conv_agre", "pac_hos" , "escenario","cod_dpto_o","departamento_ocurrencia"))


# Corregir nivel duplicado en departamento
data_model_SIVIGILA <- data_model_SIVIGILA %>% 
  mutate(departamento_ocurrencia = case_when(
    departamento_ocurrencia == "Bogotá, D.c." ~ "Bogotá, D.C.",
    departamento_ocurrencia == "Archipiélago De San Andrés, Providencia y Santa Catalina"~ "San Andrés",
    TRUE ~ departamento_ocurrencia
  ))


# Crear columna para región
data_model_SIVIGILA <- data_model_SIVIGILA %>%
  mutate(
    region = case_when(
      departamento_ocurrencia %in% c(
        "Antioquia", "Bogotá, D.C.", "Boyacá", "Caldas", "Cundinamarca",
        "Huila", "Norte De Santander", "Quindío", "Risaralda",
        "Santander", "Tolima"
      ) ~ "Andina",
      
      departamento_ocurrencia %in% c(
        "Atlántico", "Bolívar", "Cesar", "Córdoba", "La Guajira",
        "Magdalena", "Sucre",
        "Archipiélago De San Andrés, Providencia Y Santa Catalina"
      ) ~ "Caribe",
      
      departamento_ocurrencia %in% c(
        "Chocó", "Cauca", "Nariño", "Valle Del Cauca"
      ) ~ "Pacífico",
      
      departamento_ocurrencia %in% c(
        "Arauca", "Casanare", "Meta", "Vichada"
      ) ~ "Orinoquía",
      
      departamento_ocurrencia %in% c(
        "Amazonas", "Caquetá", "Guaviare", "Guainía",
        "Putumayo", "Vaupés"
      ) ~ "Amazonía",
      
      TRUE ~ "Sin región"  
    )
  )


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

## Ciclo vida
data_model_SIVIGILA <- data_model_SIVIGILA %>% mutate(
  ciclo_vital = case_when(
    edad_ %in% c(0:19) ~ "Niñez y adolescencia",
    edad_ %in% c(20:34) ~ "Juventud",
    edad_ %in% c(35:59) ~ "Adultez",
    TRUE ~ "OTRO" # edades mayores a 60 años
  )
)

data_model_SIVIGILA <- data_model_SIVIGILA %>% filter(ciclo_vital != "OTRO")

# Eliminar filas con valores faltantes
data_model_SIVIGILA <- data_model_SIVIGILA[!apply(is.na(data_model_SIVIGILA), 1, any), ]

# Convertir a factor las variables para elegir nivel de referencia
data_model_SIVIGILA <- data_model_SIVIGILA %>%
  mutate(
    across(c(mujer_cabf, conv_agre, pac_hos), 
           ~ factor(.x, levels = c("No", "Sí"))),
    
    sexo_agre      = factor(sexo_agre, levels = c("M","F","I")),
    area           = factor(area, levels = c("Cabecera municipal", "Centro poblado", "Rural disperso")),
    escenario      = factor(escenario, levels = c("Espacio público y social", "Vivienda")),
    def_naturaleza = factor(def_naturaleza, 
                            levels = c("Física", "Psicológica", "Sexual","Negligencia y abandono")),
    
    across(c(cod_dpto_o, departamento_ocurrencia, region), as.factor)
  )
data_model_SIVIGILA$cod_dpto_o <- relevel(data_model_SIVIGILA$cod_dpto_o, ref = "05")
data_model_SIVIGILA$departamento_ocurrencia <- relevel(data_model_SIVIGILA$departamento_ocurrencia, ref = "Antioquia")
data_model_SIVIGILA$region <- relevel(data_model_SIVIGILA$region, ref = "Andina")

# Guardar base de datos
save_data(data_model_SIVIGILA, "data_model_SIVIGILA_geo", type = "processed", format = "csv")
saveRDS(data_model_SIVIGILA,paste0(here(),"/data/processed/data_model_SIVIGILA_geo.rds"))
