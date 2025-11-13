# Este archivo tiene como  re-codificar las variables para el ajuste del modelo

# Leer base de datos
data_model_SIVIGILA <- read.csv(file.choose())

data_model_SIVIGILA <- data_model_SIVIGILA %>% select(c("ac_mental",
                                "edad", "mujer_cabf","def_naturaleza",
                                "area_", "sexo_agre", "parentezco_agresor", 
                                "conv_agre", "pac_hos_" , "escenario"))

data <- data %>% mutate(escenario = case_when(
  escenario %in% c("Otro", "Área deportiva y recreativa", 
                   "Comercio y áreas de servicios", "Espacios abiertos", 
                   "Establecimiento educativo", "Institución de salud", "Lugar de trabajo",
                   "Lugares de esparcimiento con expendio de alcohol", "Vía Pública") ~ "Espacio público y social",
  TRUE ~ escenario
))

data <- data %>% mutate(parentezco_agresor = case_when(
  parentezco_agresor %in% c("Padre", "Madre") ~ "Familiar",
  parentezco_agresor == "Amigo(a)" ~ "Conocido(a)",
  parentezco_agresor == "Otro" ~ "Desconocido(a)",
  TRUE ~ parentezco_agresor
))

data <- data[!apply(is.na(data), 1, any), ]

data <- data %>%
  mutate(
    mujer_cabf = factor(mujer_cabf, levels = c(2, 1)),
    def_naturaleza = factor(def_naturaleza, levels = c("Negligencia y abandono", "Violencia física", "Violencia psicológica", "Violencia sexual")),
    
    area_ = factor(area_, levels = c("Cabecera municipal", "Centro poblado", "Rural disperso")),
    
    sexo_agre = factor(sexo_agre, levels = c("I","F", "M")),
    
    parentezco_agresor = factor(parentezco_agresor, levels = c("Desconocido(a)", "Pareja", "Familiar", "Ex_Pareja","Conocido(a)")),
    
    escenario = factor(escenario, levels = c("Espacio público y social","Vivienda")),
    conv_agre = factor(conv_agre, levels = c(2, 1)),
    pac_hos_ = factor(pac_hos_, levels = c(2, 1)), 
  )