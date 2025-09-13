library(dplyr)
library(readr)
library(lubridate) 

sivigila <- readRDS("SIVIGILA_debugged_v2.rds")

sivigila <- sivigila %>%
  mutate(
    fec_fecha = as.Date(fec_hecho),
    departamento_ocurrencia = case_when(
      departamento_ocurrencia %in% c("Bogotá, D.c.", "Bogotá, D.C.") ~ "Bogotá, D.C.",
      TRUE ~ departamento_ocurrencia
    )
  )

conteo_edad <- sivigila %>%
  count(grupo_edad, sort = TRUE)

conteo_orient <- sivigila %>%
  count(orient_sex, sort = TRUE)

conteo_departamento <- sivigila %>%
  count(departamento_ocurrencia, sort = TRUE)

grupos <- c("gp_discapa","gp_desplaz","gp_migrant","gp_carcela",
            "gp_gestan","gp_indigen","gp_pobicfb","gp_mad_com",
            "gp_desmovi","gp_psiquia","gp_vic_vio","gp_otros",
            "mujer_cabf","conv_agre")

conteos_grupos <- sivigila %>%
  summarise(across(all_of(grupos), ~sum(.x == 1, na.rm = TRUE)))

conteo_anual <- sivigila %>%
  mutate(anio = year(fec_hecho)) %>%
  count(anio, sort = TRUE)

conteos_grupos_departamento <- sivigila %>%
  group_by(departamento_ocurrencia) %>%
  summarise(across(all_of(grupos), ~sum(.x == 1, na.rm = TRUE))) %>%
  ungroup()

conteo_edad
conteo_orient
conteo_departamento
conteos_grupos
conteo_anual
conteos_grupos_departamento
