library(dplyr)
library(tibble)
source("utils/load_data.R")
library(explore)
# model 1
nf_data <- load_data("INMLCF_no_fatales_debugged_2015_2024_v2.csv", type = "interim")

nf_data <- nf_data %>% mutate(
  agresor_ag = case_when(
    agresor_ag == "Amigo(a)" ~ "Conocido",
    TRUE ~ agresor_ag
  )
)
nf_data <- nf_data %>% filter(agresor_ag %in% c("Pareja","Familiar","Ex-pareja","Conocido")) 

nf_data <- nf_data %>% filter(def_naturaleza %in% c("Violencia sexual","Violencia interpersonal","Violencia intrafamiliar","Violencia sociopolítica")) 

nf_data <- nf_data %>% filter(circunstancia_del_hecho != "Bala perdida")
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

# nf_data <- nf_data %>% select(def_naturaleza, ciclo_vital, estado_civil, escolaridad, dia_del_hecho, rango_de_hora_del_hecho_x_3_horas, departamento_del_hecho_dane, codigo_dane_departamento,
                              # escenario_del_hecho, zona_del_hecho, agresor_ag)

nf_data <- nf_data %>% mutate(
  ciclo_vital = case_when(
    ciclo_vital == "Adulto Mayor" ~ "Adulto Mayor y Adultez",
    ciclo_vital == "Adultez" ~ "Adulto Mayor y Adultez",
    ciclo_vital == "Primera Infancia" ~ "Primera Infancia e Infancia",
    ciclo_vital == "Infancia" ~ "Primera Infancia e Infancia",
    TRUE ~ ciclo_vital
  )
)

nf_data <- nf_data %>% mutate(
  rango_de_hora_del_hecho_x_3_horas = case_when(
    rango_de_hora_del_hecho_x_3_horas == "Sin información" ~ "Sin información",
    rango_de_hora_del_hecho_x_3_horas %in% c("00:00 - 02:59", "03:00 - 05:59","21:00 - 23:59" ) ~ "Fuera horario laboral",
    TRUE ~ "Horario laboral"
  )
)

######
departamentos_colombia <- tribble(
  ~region, ~departamento, ~codigo_dane_departamento,
  
  # REGIÓN CARIBE
  "Caribe", "Atlántico", 8,
  "Caribe", "Bolívar", 13,
  "Caribe", "Cesar", 20,
  "Caribe", "Córdoba", 23,
  "Caribe", "La Guajira", 44,
  "Caribe", "Magdalena", 47,
  "Caribe", "San Andrés", 88,
  "Caribe", "Sucre", 70,
  
  # REGIÓN EJE CAFETERO
  "Eje Cafetero", "Antioquia", 5,
  "Eje Cafetero", "Caldas", 17,
  "Eje Cafetero", "Quindío", 63,
  "Eje Cafetero", "Risaralda", 66,
  
  # REGIÓN PACÍFICO
  "Pacífico", "Cauca", 19,
  "Pacífico", "Chocó", 27,
  "Pacífico", "Nariño", 52,
  "Pacífico", "Valle del Cauca", 76,
  
  # REGIÓN CENTRO ORIENTE
  "Centro Oriente", "Bogotá, D.C.", 11,
  "Centro Oriente", "Boyacá", 15,
  "Centro Oriente", "Cundinamarca", 25,
  "Centro Oriente", "Norte de Santander", 54,
  "Centro Oriente", "Santander", 68,
  
  # REGIÓN DE LOS LLANOS
  "Llanos", "Arauca", 81,
  "Llanos", "Casanare", 85,
  "Llanos", "Guainía", 94,
  "Llanos", "Guaviare", 95,
  "Llanos", "Meta", 50,
  "Llanos", "Vaupés", 97,
  "Llanos", "Vichada", 99,
  
  # REGIÓN CENTRO SUR AMAZONÍA
  "Centro Sur Amazonía", "Amazonas", 91,
  "Centro Sur Amazonía", "Caquetá", 18,
  "Centro Sur Amazonía", "Huila", 41,
  "Centro Sur Amazonía", "Putumayo", 86,
  "Centro Sur Amazonía", "Tolima", 73
)


######

nf_data <- nf_data %>%
  left_join(departamentos_colombia, by = "codigo_dane_departamento")
nf_data %>% filter(is.na(region.x))
table(nf_data$region)

nf_data <- nf_data %>% select(def_naturaleza, ciclo_vital, estado_civil, escolaridad, dia_del_hecho, rango_de_hora_del_hecho_x_3_horas,
                              escenario_del_hecho, zona_del_hecho, agresor_ag, region)
save_data(nf_data, "data_nf_modelo")
as.factor(nf_data$def_naturaleza)

nf_data %>% explore()
library(nnet)

modelo <- multinom(def_naturaleza ~ ciclo_vital+estado_civil+escolaridad+dia_del_hecho+rango_de_hora_del_hecho_x_3_horas+
         escenario_del_hecho+ zona_del_hecho+agresor_ag+region, data = nf_data)

summary(modelo)
