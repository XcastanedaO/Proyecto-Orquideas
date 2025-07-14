library(dplyr)
library(tibble)
source("utils/load_data.R")
library(explore)
# model 1
nf_data <- load_data("INMLCF_no_fatales_debugged_2015_2024_v2.csv", type = "interim")

nf_data <- nf_data %>% mutate(
  agresor_group = case_when(
  agresor_ag == "Delincuencia común" ~ "Actores del conflicto",
  agresor_ag == "Miembro de grupos alzados al margen de la ley" ~ "Actores del conflicto",
  agresor_ag == "Miembro de un grupo de la delincuencia organizada" ~  "Actores del conflicto",
    agresor_ag == "Miembros de las fuerzas armadas, de policía, policía judicial y servicios de inteligencia" ~ "Actores del conflicto",
  agresor_ag == "Encargado del cuidado" ~ "Conocido",
  agresor_ag == "Amigo(a)" ~ "Conocido",
  TRUE ~ agresor_ag
))

nf_data <- nf_data %>% filter(agresor_group %in% c("Pareja","Familiar","Ex-pareja","Conocido", "Actores del conflicto")) 

nf_data <- nf_data %>% filter(circunstancia_del_hecho != "Riña") #No corresponden a casos de VBG
nf_data <- nf_data %>% filter(circunstancia_del_hecho != "Bala perdida")

nf_data <- nf_data %>% filter(def_naturaleza %in% c("Violencia sexual","Violencia interpersonal","Violencia intrafamiliar","Violencia sociopolítica")) 

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


nf_data <- nf_data %>% mutate(
  ciclo_vital_ = case_when(
    grupo_de_edad_de_la_victima %in% c("00-04", "05-09","10-14","15-17","18-19") ~ "Niñas y adolescentes",
    grupo_de_edad_de_la_victima %in% c("20-24", "25-29","30-34") ~ "Jovenes",
    grupo_de_edad_de_la_victima %in% c("35-39", "40-44","45-49","50-54","55-59") ~ "Adultez",
    TRUE ~ grupo_de_edad_de_la_victima
  )
)

nf_data <- nf_data %>% filter(ciclo_vital_ %in% c("Niñas y adolescentes", "Jovenes", "Adultez"))

nf_data <- nf_data %>% mutate(
  rango_de_hora_del_hecho_x_3_horas = case_when(
    rango_de_hora_del_hecho_x_3_horas == "Sin información" ~ "Sin información",
    rango_de_hora_del_hecho_x_3_horas %in% c("00:00 - 02:59", "03:00 - 05:59") ~ "Madrugada",
    rango_de_hora_del_hecho_x_3_horas %in% c("06:00 - 08:59", "09:00 - 11:59", "12:00 - 14:59", "15:00 - 17:59") ~ "Dia",
    TRUE ~ "Noche"
  )
)

nf_data <- nf_data %>% mutate(
  dia_del_hecho = case_when(
    dia_del_hecho %in% c("Sábado", "Domingo") ~ "Fin de semana",
    TRUE ~ "semana"
  )
)

nf_data <- nf_data %>% select(def_naturaleza, ciclo_vital_, escolaridad, dia_del_hecho, rango_de_hora_del_hecho_x_3_horas, escenario_del_hecho, zona_del_hecho, agresor_group)
save_data(nf_data, "data_nf_modelo")

data <- model.matrix(~ ciclo_vital_ + escolaridad + rango_de_hora_del_hecho_x_3_horas + dia_del_hecho + escenario_del_hecho + zona_del_hecho + agresor_group, nf_data)

levels(as.factor(nf_data$def_naturaleza))
modelo <- multinom(def_naturaleza ~ ciclo_vital_+escolaridad+dia_del_hecho+rango_de_hora_del_hecho_x_3_horas+
         escenario_del_hecho+ zona_del_hecho+agresor_group, data = nf_data)

resumen <- summary(modelo) 

z <- resumen$coefficients / resumen$standard.errors
pvalores <- 2 * (1 - pnorm(abs(z)))

print(pvalores)
