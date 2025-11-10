# El siguiente script contiene funciones para realizar acciones básicas y necesarias
# para realziar análisis descriptivo y recodificación de las variables

# Función para gráfico de barras con formato del proyecto
grafico_barras <- function(data, 
                           var_x, 
                           facet_var = NULL, 
                           titulo = NULL,
                           color_barras = "#AE86D3",
                           top10 = FALSE) {
  
  # Top 10 categorías según frecuencia
  if (top10) {
    data <- data %>%
      count(!!sym(var_x), sort = TRUE) %>%
      slice_max(order_by = n, n = 10) %>%
      mutate(!!sym(var_x) := fct_reorder(!!sym(var_x), n))
    
    p <- ggplot(data, aes(x = !!sym(var_x), y = n)) +
      geom_col(fill = color_barras, color = "white", alpha = 0.85)
    
  } else {
    p <- ggplot(data, aes(x = factor(!!sym(var_x)))) +
      geom_bar(fill = color_barras, color = "white", alpha = 0.85)
  }
  
  # Escala y etiquetas comunes
  p <- p +
    scale_y_continuous(labels = label_number(scale = 1/1000)) +
    labs(
      title = titulo,
      x = var_x,
      y = "Número de casos (miles)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.text = element_text(color = "black")
    )
  
  # Agregar facet_wrap si aplica
  if (!is.null(facet_var)) {
    p <- p + facet_wrap(as.formula(paste("~", facet_var)))
  }
  
  return(p)
}

# Función para recodificar variables
recodificar_variables <- function(data) {
  data %>%
    mutate(
      ac_mental = case_when(
        is.na(ac_mental) ~ "Sin información", 
        ac_mental == "1" ~ "Sí",
        ac_mental == "2" ~ "No",
        TRUE ~ as.character(ac_mental)
      ),
      tip_ss = case_when(
        is.na(tip_ss) ~ "Sin información", 
        tip_ss == "S" ~ "Subsidiado",
        tip_ss == "C" ~ "Contributivo",
        tip_ss == "I" ~ "Indeterminado",
        tip_ss == "N" ~ "No asegurado",
        tip_ss == "P" ~ "Excepción",
        tip_ss == "E" ~ "Especial",
        TRUE ~ as.character(tip_ss)
      ),
      sexo_agre = case_when(
        is.na(sexo_agre) ~ "Sin información", 
        sexo_agre == "F" ~ "Femenino",
        sexo_agre == "I" ~ "Intersexual",
        sexo_agre == "M" ~ "Masculino",
        TRUE ~ as.character(sexo_agre)
      ),
      pac_hos = case_when(
        pac_hos == 1 ~ "Sí",
        pac_hos == 2 ~ "No",
        TRUE ~ as.character(pac_hos)
      ),
      conv_agre = case_when(
        conv_agre == 1 ~ "Sí",
        conv_agre == 2 ~ "No",
        TRUE ~ as.character(conv_agre)
      ),
      escenario = case_when(
        escenario == "Lugares de esparcimiento con expendio de alcohol" ~ "Expendio alcohol",
        escenario == "Comercio y áreas de servicios" ~ "Comercio",
        escenario == "Área deportiva y recreativa" ~ "Deportivo/recreativo",
        escenario == "Lugar de trabajo" ~ "Lugar trabajo",
        escenario == "Institución de salud" ~ "Institución salud",
        escenario == "Establecimiento educativo" ~ "Educativo",
        TRUE ~ as.character(escenario)
      ),
      mujer_cabf = case_when(
        is.na(mujer_cabf) ~ "Sin información", 
        mujer_cabf == "1" ~ "Sí",
        mujer_cabf == "2" ~ "No",
        TRUE ~ as.character(mujer_cabf)
      ),
      departamento_ocurrencia = case_when(
              departamento_ocurrencia %in% c("Bogotá, D.c.", "Bogotá, D.C.") ~ "Bogotá, D.C.",
              TRUE ~ as.character(departamento_ocurrencia))
    ) %>%
    mutate(across(c(ac_mental, tip_ss, sexo_agre, pac_hos, conv_agre, mujer_cabf, escenario,mujer_cabf,departamento_ocurrencia), as.factor))
}
