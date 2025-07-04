# El siguiente script contiene funciones para realizar acciones básicas y necesarias
# para ejecutar el código, como leer y guardar datos

## Cargar librerías
library(readxl)
library(writexl)


## Función para cargar archivos en formatos .xlsx, .txt y .csv 

load_data <- function(file_name, type = "raw", ...) {
  # Validación del tipo de archivo
  if (!type %in% c("raw", "interim", "processed","auxiliary")) {
    stop("El tipo de archivo debe ser 'raw', 'interim' o 'processed'")
  }
  
  # Definir ruta relativa con base en el directorio raíz 
  #data_dir <- file.path("data", type)
  data_dir <- here("data", type)

  # path <- dirname(getwd())
  # if(sub(".*/", "", path) != "Proyecto-Orquideas"){
  #   path <- paste0(dirname(getwd()),"/Proyecto-Orquideas")
  # }
  
  # Crear la ruta completa al archivo
  # file_path <- file.path(path, data_dir, file_name)
  file_path <- file.path(data_dir, file_name)

  # Verificar si el archivo existe
  if (!file.exists(file_path)) {
    stop(paste("El archivo", file_path, "no existe."))
  }
  
  # Leer archivo según su extensión
  if (grepl("\\.csv$", file_name)) {
    data <- read_csv(file_path, ...)
  } else if (grepl("\\.(xlsx|xls)$", file_name)) {
    data <- read_excel(file_path, ...)
  } else if (grepl("\\.rds$", file_name)) {
    data <- readRDS(file_path)
  } else if (grepl("\\.txt$", file_name)) {
    data <- read.table(file_path, header = TRUE)
  } else {
    stop("Formato de archivo no soportado.")
  }
  
  return(data)
}

# Función para guardar conjunto de datos en formatos .csv, .txt, .rds y .xlsx

save_data <- function(data, file_name, type = "processed", format = "csv") {
  # Validación del tipo de directorio
  if (!type %in% c("raw", "interim", "processed","auxiliary")) {
    stop("El tipo de directorio debe ser 'raw', 'interim' o 'processed'")
  }
  
  # Validación del formato
  if (!format %in% c("csv", "xlsx","rds", "txt")) {
    stop("El formato debe ser 'csv', 'xlsx', 'rds' o 'txt'")
  }
  
  # path <- dirname(getwd())
  # if(sub(".*/", "", path) != "Proyecto-Orquideas"){
  #   path <- paste0(dirname(getwd()),"/Proyecto-Orquideas")
  # }
  
  # Definir ruta del directorio donde se guardará
  # data_dir <- file.path(path, "data", type)
  data_dir <- here("data", type)
  
  # Verificar si la ruta existe
  if (!dir.exists(data_dir)) {
    stop("La ruta donde se quiere guardar el archivo no existe.")
  }
  
  # Crear la ruta completa del archivo
  file_path <- file.path(data_dir, paste0(file_name, ".", format))
  
  # Guardar el archivo según el formato
  if (format == "csv") {
    write.csv(data, file_path, row.names = FALSE)
  } else if (format == "xlsx") {
    writexl::write_xlsx(data, file_path)
  } else if (format == "rds") {
    saveRDS(data, file_path)
  } else if (format == "txt") {
    write.table(data, file_path, row.names = FALSE, sep = "\t")
  }
  
  message(paste("Archivo guardado exitosamente en:\n", file_path))
}

