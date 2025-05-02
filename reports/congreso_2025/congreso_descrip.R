library(patchwork)
library(dplyr)
library(ggplot2)
library(here)
library(hms)
library(forcats)
library(lubridate)
library(stringr)
library(tidyr)
library(stringi)
library(knitr)
library(readr)
library(sf)
library(gmodels)   
library(DescTools)
library(kableExtra)

########### LECTURA DE LA BASE DE DATOS ######
base_datos <- load_data("data_model.csv", type = "processed")

base_datos <- base_datos %>% select(c("ac_mental",
                                "edad", "mujer_cabf","def_naturaleza",
                                "area_", "sexo_agre", "parentezco_agresor", 
                                "conv_agre", "pac_hos_" , "escenario", 
                                "tip_ss_"))

base_datos <- base_datos[!apply(is.na(base_datos), 1, any), ]

base_datos <- base_datos %>%
  mutate(across(
    .cols = !all_of(c("edad")),
    .fns = as.factor
  ))

########### TABLAS #########
###### tipo de seguridad social ####
base_datos <- base_datos %>% mutate(ac_mental = case_when(
  is.na(ac_mental) ~ "Sin información", 
  ac_mental == "1" ~ "Sí",
  ac_mental == "0" ~ "No",
  TRUE ~ ac_mental
) %>% factor())

base_datos <- base_datos %>% mutate(tip_ss_ = case_when(
  is.na(tip_ss_) ~ "Sin información", 
  tip_ss_ == "S" ~ "Subsidiado",
  tip_ss_ == "C" ~ "Contributivo",
  tip_ss_ == "I" ~ "Indeterminado",
  tip_ss_ == "N" ~ "No asegurado",
  tip_ss_ == "P" ~ "Excepción",
  tip_ss_ == "E" ~ "Especial",
  TRUE ~ tip_ss_
) %>% factor())

as.data.frame.matrix(round(prop.table(table(base_datos$tip_ss_, base_datos$ac_mental), margin =1)*100,2)) %>% arrange(No)

###### tipo de violencia ####
as.data.frame.matrix(round(prop.table(table(base_datos$def_naturaleza, base_datos$ac_mental), margin =1)*100,2)) %>% arrange(No)

###### sexo del agresor ####
base_datos <- base_datos %>% mutate(sexo_agre = case_when(
  is.na(sexo_agre) ~ "Sin información", 
  sexo_agre == "F" ~ "Femenino",
  sexo_agre == "I" ~ "Intersexual",
  sexo_agre == "M" ~ "Masculino",
  TRUE ~ sexo_agre
) %>% factor())
as.data.frame.matrix(round(prop.table(table(base_datos$sexo_agre, base_datos$ac_mental), margin =2)*100,2)) %>% arrange(No)

###### paciente hospitalizado ####
base_datos <- base_datos %>% 
  mutate(pac_hos_ = case_when(
    pac_hos_ == 1 ~ "Sí",
    pac_hos_ == 2 ~ "No"
  ) %>% factor())
as.data.frame.matrix(round(prop.table(table(base_datos$pac_hos_, base_datos$ac_mental), margin =1)*100,2)) %>% arrange(No)

###### parentesco con el agresor ####
base_datos <-base_datos %>% mutate(parentezco_agresor = case_when(
  parentezco_agresor == "Ex_Pareja" ~ "Ex-pareja",
  TRUE ~ parentezco_agresor
) %>% factor())
as.data.frame.matrix(round(prop.table(table(base_datos$parentezco_agresor, base_datos$ac_mental), margin =1)*100,2)) %>% arrange(No)

###### Convivencia con el agresor ####
base_datos <- base_datos %>% 
  mutate(conv_agre = case_when(
    conv_agre == 1 ~ "Sí",
    conv_agre == 2 ~ "No"
  ) %>% factor())
as.data.frame.matrix(round(prop.table(table(base_datos$conv_agre, base_datos$ac_mental), margin =1)*100,2)) %>% arrange(No)

###### escenario ####
base_datos <- base_datos %>% 
  mutate(escenario = case_when(
    escenario == "Lugares de esparcimiento con expendio de alcohol" ~ "Expendio alcohol",
    escenario == "Comercio y áreas de servicios" ~ "Comercio",
    escenario == "Área deportiva y recreativa" ~ "Deportivo/recreativo",
    escenario == "Lugar de trabajo" ~ "Lugar trabajo",
    escenario == "Institución de salud" ~ "Institución salud",
    escenario == "Establecimiento educativo" ~ "Educativo",
    TRUE ~ escenario))
as.data.frame.matrix(round(prop.table(table(base_datos$escenario, base_datos$ac_mental), margin =1)*100,2)) %>% arrange(No)

###### mujer cabeza de familia ####
base_datos <- base_datos %>% mutate(mujer_cabf = case_when(
  is.na(mujer_cabf) ~ "Sin información", 
  mujer_cabf == "1" ~ "Sí",
  mujer_cabf == "2" ~ "No",
  TRUE ~ mujer_cabf
) %>% factor())
as.data.frame.matrix(round(prop.table(table(base_datos$mujer_cabf, base_datos$ac_mental), margin =1)*100,2)) %>% arrange(No)

###### área ####
as.data.frame.matrix(round(prop.table(table(base_datos$area_, base_datos$ac_mental), margin =1)*100,2)) %>% arrange(No)

###### edad agrupada ########
base_datos <- base_datos %>% mutate(
  edad_g = case_when(
    edad >= 0 & edad <= 4 ~ "0-4",
    edad >= 5 & edad <= 9 ~ "5-9",
    edad >= 10 & edad <= 14 ~ "10-14",
    edad >= 15 & edad <= 17 ~ "15-17",
    edad >= 18 & edad <= 19 ~ "18-19",
    edad >= 20 & edad <= 24 ~ "20-24",
    edad >= 25 & edad <= 29 ~ "25-29",
    edad >= 30 & edad <= 34 ~ "30-34",
    edad >= 35 & edad <= 39 ~ "35-39",
    edad >= 40 & edad <= 44 ~ "40-44",
    edad >= 45 & edad <= 49 ~ "45-49",
    edad >= 50 & edad <= 54 ~ "50-54",
    edad >= 55 & edad <= 59 ~ "55-59",
    edad >= 60 & edad <= 64 ~ "60-64",
    edad >= 65 & edad <= 69 ~ "65-69",
    edad >= 70 & edad <= 74 ~ "70-74",
    edad >= 75 & edad <= 79 ~ "75-79",
    edad >= 80 ~ "80-más",
    TRUE ~ NA_character_
  ) %>% factor()
)  
base_datos$edad_g <- factor(base_datos$edad_g,
                                 levels = c("0-4", "5-9", "10-14", "15-17", "18-19", "20-24",
                                            "25-29", "30-34", "35-39", "40-44", "45-49",
                                            "50-54", "55-59", "60-64", "65-69", "70-74",
                                            "75-79", "80-más"))
as.data.frame.matrix(round(prop.table(table(base_datos$edad_g, base_datos$ac_mental), margin =1)*100,2))

########## GRÁFICOS CONJUNTOS ########
##### Tipo de seguridad ####
tip_ss <- base_datos %>%
  group_by(ac_mental,tip_ss_) %>%
  ggplot(aes(x = tip_ss_,fill = ac_mental)) +
  geom_bar(position = "fill", alpha = 0.85) +
  labs(
    x = "Tipo de seguridad social",
    y = "Proporción (%)",
    fill = "Atención en salud mental"
  ) +
  scale_fill_manual(values = c("#e07a5f","#c07dca")) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format()) +
  theme(legend.position = "top")

##### Edad ####
edad_mental <- base_datos %>%
  group_by(edad_g, ac_mental) %>%
  summarise(total = n()) %>%
  ungroup()

edad_mental <- edad_mental %>%
  group_by(edad_g) %>%
  mutate(porcentaje = total / sum(total) * 100) %>%
  ungroup()

edad_mental <- edad_mental %>%
  mutate(total_plot = ifelse(ac_mental == "Sí", -total, total))

edad_ <- ggplot(edad_mental, aes(x = edad_g, y = total_plot, fill = ac_mental)) +
  geom_bar(stat = "identity", width = 0.8) +
  geom_text(aes(
    label = paste0(round(porcentaje, 1), "%"),
    y = ifelse(total_plot < 0, total_plot - 320, total_plot + 320) # ajusta posición al final de la barra
  ),
  size = 2.3, color ="#4B0082",) +
  coord_flip() +
  scale_y_continuous(labels = abs) +
  labs(
    # title = "Atención en salud mental según grupo de edad",
    x = "Edad de la víctima",
    y = "Cantidad de casos",
    fill = "Atención en salud mental"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("No" = "#e07a5f", "Sí" = "#c07dca")) +
  theme(legend.position = "top")

#### sexo agresor ####
sexo <- base_datos %>%
  group_by(ac_mental, sexo_agre) %>%
  ggplot(aes(x = sexo_agre, fill = ac_mental)) +
  geom_bar(position = "fill", alpha = 0.85) +
  scale_fill_manual(values = c("#e07a5f","#c07dca")) +
  labs(
    x = "Sexo del agresor",
    y = "Proporción (%)",
    fill = "Atención en salud mental") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  theme(legend.position = "none")

#### tipo de violencia ####
tip_v <- base_datos %>%
  group_by(ac_mental, def_naturaleza) %>%
  ggplot(aes(x = def_naturaleza, fill = ac_mental)) +
  geom_bar(position = "fill", alpha = 0.85) +
  scale_fill_manual(values = c("#e07a5f","#c07dca")) +
  labs(
    x = "Tipo de violencia",
    y = "Proporción (%)",
    fill = "Atención en salud mental") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  theme(legend.position = "none")

#### paciente hospitalizado ####
hosp <- base_datos %>%
  group_by(ac_mental, pac_hos_) %>%
  ggplot(aes(x = pac_hos_, fill = ac_mental)) +
  geom_bar(position = "fill", alpha = 0.85) +
  scale_fill_manual(values = c("#e07a5f","#c07dca")) +
  labs(
    x = "Hospitalización",
    y = "Proporción (%)",
    fill = "Atención en salud mental") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  theme(legend.position = "top")

#### parentesco ####
parent <- base_datos %>%
  group_by(ac_mental, parentezco_agresor) %>%
  ggplot(aes(x = parentezco_agresor, fill = ac_mental)) +
  geom_bar(position = "fill", alpha = 0.85) +
  scale_fill_manual(values = c("#e07a5f","#c07dca")) +
  labs(
    x = "Parentesco con agresor",
    y = "Proporción (%)",
    fill = "Atención en salud mental") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  theme(legend.position = "none")

#### convivencia con agresor ####
conv_ag <- base_datos %>%
  group_by(ac_mental, conv_agre) %>%
  ggplot(aes(x = conv_agre, fill = ac_mental)) +
  geom_bar(position = "fill", alpha = 0.85) +
  scale_fill_manual(values = c("#e07a5f","#c07dca")) +
  labs(
    x = "Convivencia con agresor",
    y = "Proporción (%)",
    fill = "Atención en salud mental") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  theme(legend.position = "none")

#### escenario ####
escenario <- base_datos %>%
  group_by(ac_mental, escenario) %>%
  ggplot(aes(x = escenario, fill = ac_mental)) +
  geom_bar(position = "fill", alpha = 0.85) +
  scale_fill_manual(values = c("#e07a5f","#c07dca")) +
  labs(
    x = "Escenario del hecho",
    y = "Proporción (%)",
    fill = "Atención en salud mental") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  theme(legend.position = "top")

#### mujer cabeza de familia ####
cabf <- base_datos %>%
  group_by(ac_mental, mujer_cabf) %>%
  ggplot(aes(x = mujer_cabf, fill = ac_mental)) +
  geom_bar(position = "fill", alpha = 0.85) +
  scale_fill_manual(values = c("#e07a5f","#c07dca")) +
  labs(
    x = "Mujer cabeza de familia",
    y = "Proporción (%)",
    fill = "Atención en salud mental") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  theme(legend.position = "none")

#### Área de ocurrencia ####
area <- base_datos %>%
  group_by(ac_mental, area_) %>%
  ggplot(aes(x = area_, fill = ac_mental)) +
  geom_bar(position = "fill", alpha = 0.85) +
  scale_fill_manual(values = c("#e07a5f","#c07dca")) +
  labs(
    x = "Área de ocurrencia",
    y = "Proporción (%)",
    fill = "Atención en salud mental") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  theme(legend.position = "top")

##### unión ####
(edad_ | tip_ss)
((tip_v | sexo )  + plot_layout(widths = c(2, 1))) / ((parent | hosp) + plot_layout(widths = c(2, 1)))
escenario / (area | cabf | conv_ag) 


########## GRÁFICOS INDIVIDUALES ########
##### Tipo de seguridad ####
tip_ss_ <- base_datos %>%
  count(tip_ss_) %>%
  mutate(proporcion = n / sum(n) * 100) %>%
  ggplot(aes(x = tip_ss_, y = n)) +
  geom_col(fill = "#c07dca", alpha = 0.85) +  
  geom_text(aes(label = paste0(round(proporcion, 1), "%")), 
            vjust = -0.2, 
            color = "#4B0082", 
            fontface = "bold",
            size = 3.5) + 
  labs(x = "Tipo de seguridad", y = "Cantidad de casos") + 
  theme_minimal()

##### Edad ####
edad_i <-  base_datos %>%
  count(edad_g) %>%
  mutate(proporcion = n / sum(n) * 100) %>%
  ggplot(aes(x = edad_g, y = n)) +
  geom_col(fill = "#c07dca", alpha = 0.85) +  
  geom_text(aes(label = paste0(round(proporcion, 1), "%")), 
            vjust = -0.2, 
            color = "#4B0082", 
            fontface = "bold",
            size = 3.5) + 
  labs(x = "Edad de la víctima", y = "Cantidad de casos") + 
  theme_minimal()

#### sexo agresor ####
sexo_ <- base_datos %>%
  count(sexo_agre) %>%
  mutate(proporcion = n / sum(n) * 100) %>%
  ggplot(aes(x = sexo_agre, y = n)) +
  geom_col(fill = "#c07dca", alpha = 0.85) +  
  geom_text(aes(label = paste0(round(proporcion, 1), "%")), 
            vjust = -0.2, 
            color = "#4B0082", 
            fontface = "bold",
            size = 3.5) + 
  labs(x = "Sexo del agresor", y = "Cantidad de casos") + 
  theme_minimal()

#### tipo de violencia ####
tip_v_ <- base_datos %>%
  count(def_naturaleza) %>%
  mutate(proporcion = n / sum(n) * 100) %>%
  ggplot(aes(x = def_naturaleza, y = n)) +
  geom_col(fill = "#c07dca", alpha = 0.85) +  
  geom_text(aes(label = paste0(round(proporcion, 1), "%")), 
            vjust = -0.2, 
            color = "#4B0082", 
            fontface = "bold",
            size = 3.5) + 
  labs(x = "Tipo de violencia", y = "Cantidad de casos") + 
  theme_minimal()

#### paciente hospitalizado ####
hosp_ <- base_datos %>%
  count(pac_hos_) %>%
  mutate(proporcion = n / sum(n) * 100) %>%
  ggplot(aes(x = pac_hos_, y = n)) +
  geom_col(fill = "#c07dca", alpha = 0.85) +  
  geom_text(aes(label = paste0(round(proporcion, 1), "%")), 
            vjust = -0.2, 
            color = "#4B0082", 
            fontface = "bold",
            size = 3.5) + 
  labs(x = "Hospitalización", y = "Cantidad de casos") + 
  theme_minimal()

#### parentesco ####
parent_ <- base_datos %>%
  count(parentezco_agresor) %>%
  mutate(proporcion = n / sum(n) * 100) %>%
  ggplot(aes(x = parentezco_agresor, y = n)) +
  geom_col(fill = "#c07dca", alpha = 0.85) +  
  geom_text(aes(label = paste0(round(proporcion, 1), "%")), 
            vjust = -0.2, 
            color = "#4B0082", 
            fontface = "bold",
            size = 3.5) + 
  labs(x = NULL, y = "Cantidad de casos") + 
  theme_minimal()

#### convivencia con agresor ####
conv_ag_ <- base_datos %>%
  count(conv_agre) %>%
  mutate(proporcion = n / sum(n) * 100) %>%
  ggplot(aes(x = conv_agre, y = n)) +
  geom_col(fill = "#c07dca", alpha = 0.85) +  
  geom_text(aes(label = paste0(round(proporcion, 1), "%")), 
            vjust = -0.2, 
            color = "#4B0082", 
            fontface = "bold",
            size = 3.5) + 
  labs(x = "Convivencia con agresor", y = "Cantidad de casos") + 
  theme_minimal()

#### escenario ####
escenario_ <- base_datos %>%
  count(escenario) %>%
  mutate(proporcion = n / sum(n) * 100) %>%
  ggplot(aes(x = escenario, y = n)) +
  geom_col(fill = "#c07dca", alpha = 0.85) +  
  geom_text(aes(label = paste0(round(proporcion, 1), "%")), 
            vjust = -0.2, 
            color = "#4B0082", 
            fontface = "bold",
            size = 3.5) + 
  labs(x = NULL, y = "Cantidad de casos") + 
  theme_minimal()

#### mujer cabeza de familia ####
cabf_ <- base_datos %>%
  count(mujer_cabf) %>%
  mutate(proporcion = n / sum(n) * 100) %>%
  ggplot(aes(x = mujer_cabf, y = n)) +
  geom_col(fill = "#c07dca", alpha = 0.85) +  
  geom_text(aes(label = paste0(round(proporcion, 1), "%")), 
            vjust = -0.2, 
            color = "#4B0082", 
            fontface = "bold",
            size = 3.5) + 
  labs(x = "Mujer cabeza de familia", y = "Cantidad de casos") + 
  theme_minimal()

#### Área de ocurrencia ####
area_ <- base_datos %>%
  count(area_) %>%
  mutate(proporcion = n / sum(n) * 100) %>%
  ggplot(aes(x = area_, y = n)) +
  geom_col(fill = "#c07dca", alpha = 0.85) +  
  geom_text(aes(label = paste0(round(proporcion, 1), "%")), 
            vjust = -0.2, 
            color = "#4B0082", 
            fontface = "bold",
            size = 3.5) + 
  labs(x = "Área de ocurrencia", y = "Cantidad de casos") + 
  theme_minimal()
#### Atención en salud ####
ac_mental_ <- base_datos %>%
  count(ac_mental) %>%
  mutate(proporcion = n / sum(n) * 100) %>%
  ggplot(aes(x = ac_mental, y = n)) +
  geom_col(fill = "#c07dca", alpha = 0.85) +  
  geom_text(aes(label = paste0(round(proporcion, 1), "%")), 
            vjust = -0.2, 
            color = "#4B0082", 
            fontface = "bold",
            size = 3.5) + 
  labs(x = "Atención en salud mental", y = "Cantidad de casos") + 
  theme_minimal()

##### unión 1 ####
((edad_i | edad_ )  + plot_layout(widths = c(1.4, 1))) / ((tip_v_ | tip_v) + plot_layout(widths = c(1.4, 1)))
((tip_ss_ | tip_ss )  + plot_layout(widths = c(1.3, 1))) / ((sexo_ | sexo) + plot_layout(widths = c(1.3, 1)))
(hosp_ | hosp ) / (conv_ag_ | conv_ag)
(area_ | area ) / (cabf_ | cabf)
parent_ / parent
escenario_ / escenario

##### unión 2 ####
(edad_ | tip_ss)
((tip_v | sexo )  + plot_layout(widths = c(2, 1))) / ((parent | hosp) + plot_layout(widths = c(2, 1)))
escenario / (area | cabf | conv_ag) 
