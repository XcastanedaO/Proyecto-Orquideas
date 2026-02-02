# Leer base de datos
data_model_SIVIGILA <- readRDS(file.choose())

# Selección de variables de interés
data_model_SIVIGILA <- data_model_SIVIGILA %>% select(c("ac_mental",
                                                        "edad_", "mujer_cabf","def_naturaleza",
                                                        "area", "sexo_agre", "conv_agre", "pac_hos" , "escenario","tip_ss", "area"))

# Re-codificación de variables
data_model_SIVIGILA <- data_model_SIVIGILA %>%  mutate(
  ac_mental = case_when(
    ac_mental == "1" ~ "Sí",
    ac_mental == "2" ~ "No",
    TRUE ~ ac_mental),
  
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

data_model_SIVIGILA %>% 
  count(escenario) %>% 
  mutate(porc = round(100 * n / sum(n), 1)) %>% 
  arrange(desc(n)) %>%
  kable(format = "latex",
        col.names = c("Escenario", "Número casos", "Proporción"),
  ) %>%
  kable_styling(latex_options = "H",full_width = FALSE, position = "center")

ac <- data_model_SIVIGILA %>%
  count(ac_mental) %>%
  mutate(proporcion = n / sum(n) * 100) %>%
  ggplot(aes(x = ac_mental, y = n)) +
  geom_col(fill = "#AE86D3", alpha = 0.8) +  # Color de las barras
  geom_text(aes(label = paste0(round(proporcion, 1), "%")), 
            vjust = -0.2, 
            color = "#4B0082",  
            fontface = "bold",
            size = 4) + 
  labs(x = "Atención en salud mental", y = "Número de casos") + 
  theme_minimal()

edad <- ggplot(data_model_SIVIGILA, aes(x = edad_)) +
  geom_bar(aes(y = after_stat(count)), fill = "#4B0082", alpha = 0.6, color = "white") +
  geom_density(aes(y = after_stat(count)), color = "#c09dca", fill = "#c09dca", alpha = 0.3, linewidth = 1.1) +
  labs(x = "Edad de la víctima", y = "Cantidad de casos") + #title = "Distribución de edad de las víctimas",
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

tipo <- data_model_SIVIGILA %>%
  group_by(tip_ss, ac_mental) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(tip_ss) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = tip_ss, y = prop, fill = ac_mental)) +
  geom_bar(stat = "identity", position = "fill", alpha = 0.85) +
  geom_text(aes(label = scales::percent(prop, accuracy = 0.1)), 
            position = position_fill(vjust = 0.5), size = 3.5, color = "#4B0082",fontface = "bold") +
  labs(
    x = "Tipo de seguridad social",
    y = "Proporción (%)",
    fill = "Atención en salud mental"
  ) +
  scale_fill_manual(values = c("#90A9C3", "#AE86D3")) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format()) +
  theme(legend.position = "none")

data_model_SIVIGILA %>%
  group_by(ac_mental, pac_hos) %>%
  ggplot(aes(x = pac_hos, fill = ac_mental)) +
  geom_bar(position = "fill", alpha = 0.85) +
  scale_fill_manual(values = c("#9b9b9b","#c07dca")) +
  labs(
    x = "Hospitalización",
    y = "Proporción (%)",
    fill = "Atención en salud mental") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()
# data_model_SIVIGILA %>% filter(conv_agre !="") %>%
#   count(conv_agre) %>%
#   mutate(proporcion = n / sum(n) * 100) %>%  # Calculamos el porcentaje
#   ggplot(aes(x = reorder(conv_agre, n), y = n)) +
#   geom_col(fill = "#c07dca", alpha = 0.8) +
#   geom_text(aes(label = paste0(round(proporcion, 1), "%")), 
#             hjust = 0.9999, 
#             color = "#4B0082",  # Morado oscuro
#             fontface = "bold",
#             size = 2) +
#   # coord_flip() +
#   labs(title = "Convivencia con el agresor", 
#     x = "Convive con el gresor", 
#     y = "Casos") +
#   theme_minimal()

data_model_SIVIGILA %>%
  group_by(def_naturaleza) %>%
  count(ac_mental) %>%
  mutate(proporcion = round((n / sum(n))*100, 3)) %>%
ggplot(aes(x = def_naturaleza, y = ac_mental, fill = proporcion)) +
  geom_tile(color = "white", size = 0.5) +  # Agregar bordes a las celdas
  scale_fill_gradient(low = "white", high = "#7a1fa2", name = "Proporción (%)") +  # Mayor contraste en color
  labs(title = "Atenciones en salud según tipo de violencia",
       x = "Tipo de violencia",
       y = "Atención en salud") +
  theme_minimal(base_size = 14) +  # Mejor tamaño de fuente
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),  # Alinear etiquetas de ejes
    axis.text.y = element_text(size = 12),  # Aumentar tamaño del texto del eje Y
    legend.position = "right",  # Ubicar la leyenda a la derecha
    panel.grid = element_blank()  # Eliminar líneas de cuadrícula
  )
prop.table(table(data_model_SIVIGILA$ac_mental, data_model_SIVIGILA$escenario, margin=1))
