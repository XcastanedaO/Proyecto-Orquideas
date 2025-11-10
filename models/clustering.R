
# ============================================================================
# ANÁLISIS DE SEGMENTACIÓN PARA DATOS DE VIOLENCIA DE GÉNERO
# Métodos: K-modes, K-prototypes, y Análisis de Clases Latentes
# ============================================================================

# Cargar librerías necesarias
library(klaR)      # Para k-modes
library(clustMixType) # Para k-prototypes
library(dplyr)     # Manipulación de datos
library(ggplot2)   # Visualización
library(VIM)       # Para visualizar datos faltantes
library(poLCA)     # Para análisis de clases latentes
library(factoextra) # Para visualización de clusters
library(cluster)   # Para análisis de silueta

# ============================================================================
# 1. PREPARACIÓN DE DATOS DE EJEMPLO
# ============================================================================



# Convertir todas las variables a factor
datos_vbg <- nf_data %>% 
  mutate_all(as.factor)

# Explorar los datos
str(datos_vbg)
summary(datos_vbg)

# ============================================================================
# 2. ANÁLISIS DE CLUSTERS CON K-MODES
# ============================================================================

# Función para determinar el número óptimo de clusters
determinar_k_optimo <- function(datos, max_k = 10) {
  wss <- numeric(max_k)
  
  for (k in 1:max_k) {
    km_result <- kmodes(datos, k, iter.max = 100, weighted = FALSE)
    wss[k] <- km_result$withindiff
  }
  
  # Crear gráfico del método del codo
  plot(1:max_k, wss, type = "b", 
       xlab = "Número de clusters (k)", 
       ylab = "Suma de diferencias intra-cluster",
       main = "Método del Codo para K-modes")
  
  return(wss)
}

# Determinar número óptimo de clusters
wss_values <- determinar_k_optimo(datos_vbg, max_k = 8)



# Ejecutar k-modes con k=4 (ajustar según el análisis del codo)
k_optimo <- 4
km_result <- kmodes(datos_vbg, k_optimo, iter.max = 100, weighted = FALSE)
saveRDS(km_result, "kmodes_clust.txt")
# Agregar clusters a los datos
datos_vbg$cluster_kmodes <- as.factor(km_result$cluster)

# Mostrar centros de clusters (modas)
print("Centros de clusters (modas):")
print(km_result$modes)

# Tamaño de cada cluster
table(datos_vbg$cluster_kmodes)
save_data(datos_vbg, file_name = "data_nf_kmodes")
# ============================================================================
# 3. ANÁLISIS DE CLASES LATENTES
# ============================================================================

# Preparar datos para poLCA (necesita variables numéricas empezando en 1)
datos_lca <- datos_vbg %>% dplyr::select(-cluster_kmodes) %>%
  mutate_all(function(x) as.numeric(x))



# Función para comparar modelos LCA con diferente número de clases
comparar_lca <- function(datos, max_clases = 6) {
  resultados <- list()
  criterios <- data.frame(
    clases = 2:max_clases,
    AIC = NA,
    BIC = NA,
    loglik = NA
  )
  
  for (k in 2:max_clases) {
    formula_lca <- cbind(def_naturaleza, ciclo_vital_, escolaridad, dia_del_hecho,
                         rango_de_hora_del_hecho_x_3_horas, escenario_del_hecho, zona_del_hecho, agresor_group) ~ 1
    
    lca_model <- poLCA(formula_lca, datos, nclass = k, 
                       maxiter = 1000, verbose = FALSE, nrep = 10)
    
    resultados[[paste0("k", k)]] <- lca_model
    criterios[criterios$clases == k, "AIC"] <- lca_model$aic
    criterios[criterios$clases == k, "BIC"] <- lca_model$bic
    criterios[criterios$clases == k, "loglik"] <- lca_model$llik
  }
  
  print(criterios)
  
  # Visualizar criterios de selección
  par(mfrow = c(1, 2))
  plot(criterios$clases, criterios$AIC, type = "b", main = "AIC", xlab = "Número de clases")
  plot(criterios$clases, criterios$BIC, type = "b", main = "BIC", xlab = "Número de clases")
  par(mfrow = c(1, 1))
  
  return(resultados)
}

# Comparar modelos LCA
lca_models <- comparar_lca(datos_lca, max_clases = 4)

# Seleccionar mejor modelo (menor BIC típicamente)
# Aquí seleccionamos k=3 como ejemplo
mejor_lca <- lca_models$k3
saveRDS(mejor_lca, file = "models/mejor_lca.RData")
saveRDS(km_result, file = "models/kmodes.RData")
# Agregar clases latentes a los datos
datos_vbg$clase_latente <- as.factor(mejor_lca$predclass)

# ============================================================================
# 4. ANÁLISIS Y VISUALIZACIÓN DE RESULTADOS
# ============================================================================

# Función para crear perfiles de clusters
crear_perfil_cluster <- function(datos, variable_cluster) {
  perfiles <- datos %>%
    group_by(!!sym(variable_cluster)) %>%
    summarise_all(function(x) {
      tbl <- table(x)
      names(tbl)[which.max(tbl)]  # Moda de cada variable
    }) %>%
    dplyr::select(-!!sym(variable_cluster))
  
  return(perfiles)
}

# Perfiles para k-modes
perfiles_kmodes <- crear_perfil_cluster(datos_vbg, "cluster_kmodes")
print("Perfiles de clusters K-modes:")
print(perfiles_kmodes)

# Perfiles para clases latentes
perfiles_lca <- crear_perfil_cluster(datos_vbg, "clase_latente")
print("Perfiles de clases latentes:")
print(perfiles_lca)

# ============================================================================
# 5. VALIDACIÓN DE CLUSTERS
# ============================================================================

# Análisis de silueta para k-modes
# Crear matriz de distancia para variables categóricas
distancia_gower <- cluster::daisy(datos_vbg[, 1:8], metric = "gower")

# Calcular silueta
sil_kmodes <- silhouette(as.numeric(datos_vbg$cluster_kmodes), distancia_gower)
print(paste("Ancho promedio de silueta K-modes:", round(mean(sil_kmodes[, 3]), 3)))

# Visualizar silueta
plot(sil_kmodes, main = "Análisis de Silueta - K-modes")

# ============================================================================
# 6. ANÁLISIS COMPARATIVO ENTRE MÉTODOS
# ============================================================================

# Tabla cruzada entre métodos
tabla_comparacion <- table(datos_vbg$cluster_kmodes, datos_vbg$clase_latente)
print("Comparación entre K-modes y Clases Latentes:")
print(tabla_comparacion)

# Índice de Rand ajustado para comparar métodos
rand_ajustado <- adjustedRandIndex(datos_vbg$cluster_kmodes, 
                                            datos_vbg$clase_latente)
print(paste("Índice de Rand Ajustado:", round(rand_ajustado, 3)))

# ============================================================================
# 7. VISUALIZACIÓN DE RESULTADOS
# ============================================================================

# Función para crear gráficos de barras por cluster
graficar_clusters <- function(datos, variable_cluster, titulo) {
  # Seleccionar variables para graficar
  vars_interes <- c("def_naturaleza", "ciclo_vital_", "escolaridad", "dia_del_hecho",
                    "rango_de_hora_del_hecho_x_3_horas", "escenario_del_hecho", "zona_del_hecho", "agresor_group")
  
  plots <- list()
  
  for (var in vars_interes) {
    p <- datos %>%
      ggplot(aes(x = !!sym(var), fill = !!sym(variable_cluster))) +
      geom_bar(position = "fill") +
      coord_flip() +
      labs(title = paste(titulo, "-", var),
           y = "Proporción",
           fill = "Cluster") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    plots[[var]] <- p
  }
  
  return(plots)
}

# Crear gráficos para k-modes
plots_kmodes <- graficar_clusters(datos_vbg, "cluster_kmodes", "K-modes")

# Mostrar algunos gráficos
print(plots_kmodes$def_naturaleza)
print(plots_kmodes$ciclo_vital_)
print(plots_kmodes$agresor_group)
# ============================================================================
# 8. EXPORTAR RESULTADOS
# ============================================================================

# Crear resumen de resultados
resumen_segmentacion <- datos_vbg %>%
  group_by(cluster_kmodes, clase_latente) %>%
  summarise(
    n_casos = n(),
    prop_fisica = mean(tipo_violencia == "Física"),
    prop_psicologica = mean(tipo_violencia == "Psicológica"),
    prop_pareja = mean(relacion_agresor == "Pareja"),
    prop_hogar = mean(lugar_hecho == "Hogar"),
    .groups = "drop"
  )

print("Resumen de segmentación:")
print(resumen_segmentacion)

# Guardar resultados
# write.csv(datos_vbg, "datos_vbg_segmentados.csv", row.names = FALSE)
# write.csv(resumen_segmentacion, "resumen_segmentacion.csv", row.names = FALSE)

# ============================================================================
# 9. INTERPRETACIÓN DE CLUSTERS
# ============================================================================

interpretar_clusters <- function(datos, variable_cluster) {
  cat("\n=== INTERPRETACIÓN DE CLUSTERS ===\n")
  
  for (i in 1:length(unique(datos[[variable_cluster]]))) {
    cat(paste("\n--- CLUSTER", i, "---\n"))
    
    subset_data <- datos[datos[[variable_cluster]] == i, ]
    n_casos <- nrow(subset_data)
    cat(paste("Número de casos:", n_casos, "\n"))
    
    # Características predominantes
    for (var in names(datos)[1:7]) {  # Excluir variables de cluster
      moda <- names(sort(table(subset_data[[var]]), decreasing = TRUE))[1]
      prop <- max(table(subset_data[[var]])) / n_casos
      cat(paste("-", var, ":", moda, "(", round(prop*100, 1), "%)\n"))
    }
  }
}

# Interpretar clusters k-modes
interpretar_clusters(datos_vbg, "cluster_kmodes")

cat("\n=== ANÁLISIS DE SEGMENTACIÓN COMPLETADO ===\n")
cat("Los resultados muestran diferentes perfiles de violencia de género\n")
cat("que pueden ser útiles para políticas públicas focalizadas.\n")
```