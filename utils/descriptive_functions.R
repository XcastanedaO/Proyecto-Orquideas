# El siguiente script contiene funciones para realizar acciones básicas y necesarias
# para realziar análisis descriptivo y recodificación de las variables

# Función para gráfico de barras con formato del proyecto ####
grafico_barras <- function(data, 
                           var_x, 
                           facet_var = NULL, 
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

# Función para recodificar variables ####
recodificar_variables <- function(data) {
  
  vars_binarias <- c("gp_discapa", "gp_desplaz", "gp_migrant", "gp_carcela",  
                     "gp_gestan", "gp_indigen", "gp_pobicfb", "gp_mad_com", "mujer_cabf",
                     "gp_desmovi", "gp_psiquia", "gp_vic_vio", "gp_otros","ac_mental","pac_hos","conv_agre")
  
  data %>%
    mutate(
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
      escenario = case_when(
        is.na(escenario) ~ "Sin información",
        escenario == "1" ~ "Vía Pública",
        escenario == "2" ~ "Vivienda",
        escenario == "10" ~ "Expendio alcohol",
        escenario == "8" ~ "Comercio",
        escenario == "12" ~ "Deportivo/recreativo",
        escenario == "4" ~ "Lugar trabajo",
        escenario == "11" ~ "Institución salud",
        escenario == "3" ~ "Educativo",
        escenario == "7" ~ "Otro",
        escenario == "9" ~ "Espacios abiertos",
        TRUE ~ as.character(escenario)
      ),
      departamento_ocurrencia = case_when(
        departamento_ocurrencia %in% c("Bogotá, D.c.", "Bogotá, D.C.") ~ "Bogotá, D.C.",
        TRUE ~ as.character(departamento_ocurrencia)),
      def_naturaleza = case_when(
        is.na(def_naturaleza) ~ "Sin información",
        def_naturaleza == "1" ~ "Física",
        def_naturaleza == "2" ~ "Psicológica",
        def_naturaleza == "3" ~ "Negligencia y abandono",
        def_naturaleza == "5" ~ "Acoso sexual",
        def_naturaleza == "6" ~ "Acceso carnal",
        def_naturaleza == "7" ~ "Explotación sexual",
        def_naturaleza == "10" ~ "Trata de personas",
        def_naturaleza == "12" ~ "Actos sexuales",
        def_naturaleza == "14" ~ "Otras violencias sexuales",
        def_naturaleza == "15" ~ "Mutilación genital",
        TRUE ~ as.character(def_naturaleza)
      )
    ) %>%
    mutate(across(
      all_of(vars_binarias),
      ~ case_when(
        is.na(.) ~ "Sin información",
        . == 1 ~ "Sí",
        . == 2 ~ "No",
        TRUE ~ as.character(.)
      )
    )) %>%
    mutate(across(c(tip_ss, sexo_agre,escenario,departamento_ocurrencia,all_of(vars_binarias),def_naturaleza), as.factor))
}


# Función para gráfico de líneas con formato del proyecto ########
grafico_lineas <- function(data,
                           var_x,
                           var_y,
                           group = NULL,
                           facet_var = NULL,
                           top10 = FALSE,
                           facet_col = 1,
                           facet_row = 1,
                           color_linea = "#4B0082",
                           mostrar_label = TRUE) {
  
  df <- data
  
  # Si se desea trabajar con top 10 de var_y
  if (top10) {
    if (is.null(group)) {
      df <- df %>% 
        arrange(desc(!!sym(var_y))) %>% 
        slice_max(order_by = !!sym(var_y), n = 10)
    } else {
      df <- df %>%
        group_by(!!sym(group)) %>%
        arrange(desc(!!sym(var_y))) %>%
        slice_max(order_by = !!sym(var_y), n = 10) %>%
        ungroup()
    }
  }
  
  # Base del gráfico
  p <- ggplot(df, aes(x = !!sym(var_x), y = !!sym(var_y), group = !!sym(group))) +
    geom_line(color = color_linea, size = 1) +
    geom_point(color = color_linea, size = 2)
  
  # Agregar etiquetas solo si mostrar_label = TRUE
  if (mostrar_label) {
    p <- p + geom_label(aes(label = !!sym(var_y)), color = color_linea, size = 2.5)
  }
  
  # Personalización general
  p <- p +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      strip.text = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      axis.text = element_text(color = "black")
    ) +
    scale_y_continuous(labels = label_number(scale = 1/1000)) +
    scale_x_continuous(
      breaks = function(x) seq(floor(min(x, na.rm = TRUE)),
                               ceiling(max(x, na.rm = TRUE)), 1)
    )
  
  # Facetas (si aplica)
  if (!is.null(facet_var)) {
    p <- p + facet_wrap(as.formula(paste("~", facet_var)),
                        ncol = facet_col,
                        nrow = facet_row,
                        scales = "free_y")
  }
  
  return(p)
}

# Funciones para implementar funciones no paramétricas #####
test_cualit <- function(df, var_resp, var_pred) {
  alpha <- 0.05
  tab <- table(df[[var_resp]], df[[var_pred]])
  
  if (any(chisq.test(tab)$expected < 5)) {
    test <- tryCatch(
      fisher.test(tab, simulate.p.value = TRUE, B = 1e5),
      error = function(e) fisher.test(tab, simulate.p.value = TRUE, B = 5000)
    )
    cramerv <- DescTools::CramerV(tab, bias.correct = TRUE)
    tibble(variable = var_pred,
           prueba = "Fisher (simulación)",
           estadistico = NA,
           p.value = round(test$p.value,3),
           cramersV = round(cramerv,3),
           decision = ifelse(test$p.value < alpha,
                             "Posible asociación",
                             "No asociación"))
  } else {
    test <- chisq.test(tab, correct = FALSE)
    cramerv <- DescTools::CramerV(tab, bias.correct = TRUE)
    tibble(variable = var_pred,
           prueba = "Chi-cuadrado",
           estadistico = round(unname(test$statistic),3),
           p.value = round(test$p.value,3),
           cramersV = round(cramerv,3),
           decision = ifelse(test$p.value < alpha,
                             "Posible asociación",
                             "No asociación"))
  }
}

# Spearman
test_cuant <- function(df, var_resp, var_pred) {
  alpha <- 0.05
  y <- as.numeric(as.factor(df[[var_resp]]))
  x <- df[[var_pred]]
  
  test <- cor.test(x, y, method = "spearman", exact = FALSE)
  decision <- ifelse(test$p.value < alpha,
                     "Posible asociación",
                     "No hay evidencia estadística suficiente de asociación")
  
  tibble(variable = var_pred,
         prueba = "Spearman",
         estadistico = round(unname(test$statistic),3),
         p.value = round(test$p.value,3),
         decision = decision)
}