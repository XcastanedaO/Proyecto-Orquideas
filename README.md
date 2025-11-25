# Proyecto orquídeas

Este repositorio contiene recursos, código y documentación para el desarrollo del proyecto titulado "Implementación de herramientas analíticas para el estudio de la violencia de género y su impacto en la salud mental de mujeres y niñas en Colombia", seleccionado en la [convocatoria Orquídeas: Mujeres en la Ciencia 2024](https://minciencias.gov.co/convocatorias/convocatoria-orquideas-mujeres-en-la-ciencia-2024) del Ministerio de Ciencia, Tecnología e Innovación de Colombia (MinCiencias). 
El objetivo del proyecto es implementar metodologías estadísticas avanzadas y de aprendizaje automático para evaluar el efecto de los factores que influyen en la incidencia de la violencia de género contra las mujeres y niñas en Colombia e identificar el impacto de este tipo de violencia en su salud mental.

En esta rama específicamente se trabajará con la información proporcionada por el Instituto Nacional de Salud de Colombia sobre los casos sospechosos o probables de violencia de género que fueron reportados al SIVIGILA durante 2013 y 2024.

## Autores :pencil2:

- **Alejandra Estefania Patiño Hoyos** - *Doctora investigadora* - [GitHub](https://github.com/alejandraeph)
- **Verónica Seguro Varela** - *Joven investigadora* - [GitHub](https://github.com/vseguro)
- **Johnatan Cardona Jiménez** - *Experto* - [GitHub](https://github.com/JohnatanLAB)
- **Simón Ruiz Martínez** - *Experto* - [GitHub](https://github.com/simonruizm)
- **Mariana Hernádez Giraldo** - *Estudiante auxiliar* - [GitHub](https://github.com/MarianaHernandezG)
- **Ximena Castañeda Ochoa** - *Estudiante auxiliar* - [GitHub](https://github.com/XcastanedaO)
- **Tomás Rodríguez Taborda** - *Estudiante auxiliar* - [GitHub](https://github.com/torodriguezt)

## Manual :book:
[Este manual](analysis/code_guide.Rmd) explica acciones necesarias para la ejecución del código como: cargue y guardado de conjunto de datos, etc.


## Estructura del repositorio :file_folder:

```         
├── README.md          <- El archivo README para explicar estructura de la rama del proyecto
│
├── analysis  
│   ├── code_guide.Rmd   <- Archivo con explicación de funciones o acciones necesarias para el buen funcionamiento del código.
│   ├── debugging        
│       └── sivigila_debugging.rmd   <- Depuración de los datos provenientes del sistema de sivigila.
│   ├── descriptive_analysis   
│       └── SIVIGILA
|           ├── SIVIGILA_descriptive.rmd <- Análisis descriptivo de los datos depurados.
|           └── SIVIGILA_variable_selection.rmd <- Informe sobre análisis realizado para seleccion de variables candidatasa modelar
│   └── imputation   
│
├── data
│   ├── auxiliary
│       ├── barrios_veredas_Medellin <-  shape de barrios y veredas de Medellín 
│       ├── comunas_corregimientos_Medellin <- shape de comunas y corregimientos de Medellín 
│       ├── Proyecciones <-  Proyecciones de población femenino colombiana por sexo, edad, departamento, municipio y año
│       ├── CIE10 <- Lista oficial de diagnósticos CIE-10 
│       ├── CIUO88 <- Clasificación Internacional Uniforme de Ocupaciones
│       └── CUOC <- Clasificación Única Ocupaciones para Colombia  
│   ├── raw          
|       ├── SIVIGILA <- Base de datos originales enviadas por Instituto Nacional de Salud de Colombia junto con diccionario de datos
|       └── SIVIGILA_rds <- Base de datos originales enviadas por Instituto Nacional de Salud de Colombia junto con diccionario de datos en formato rds
│   ├── interim         
│       ├── SIVIGILA_debugged.rds <-  Base resultante de la depuración realizada en sivigila_debugging.Rmd
│       ├── SIVIGILA_debugged_spatial <- Variables de georreferenciación de la base resultante de la depuración realizada en sivigila_debugging.Rmd. Se encuentra en formato .rds y .csv
│       ├── SIVIGILA_debugged_spatial_depto <- Variables de interés con departamento de la base resultante de la depuración realizada en sivigila_debugging.Rmd. Se encuentra en formato .rds y .csv
│       ├── SIVIGILA_debugged_spatial_mun <- Variables de interés con municipio de la base resultante de la depuración realizada en sivigila_debugging.Rmd. Se encuentra en formato .rds y .csv
│       └── SIVIGILA_debugged_v2.rds <-  Versión de la base resultante de la depuración realizada en sivigila_debugging.Rmd con variables de interés
│   └── processed   
│       ├── data_model.csv <-  Base procesada para ajustar modelo 2 de MEData
│       ├── data_model_SIVIGILA.csv <- Base procesada para ajustar modelo SIVIGILA con variable de repsuesta salud menta y rango de edad víctima 14-50
│
├── models          
│   ├── model 2 MEData <- archivos con resultados del ajuste del modelo 2 MEData
│   └── SIVIGILA nacional
│       ├── resultados <-  resultados de ajustar modelo SIVIGILA con variable de repsuesta salud menta y rango de edad víctima 14-50 con código inicial
│       ├── resultados_IA <- resultados de ajustar modelo SIVIGILA con variable de repsuesta salud menta y rango de edad víctima 14-50 con código preprocesamiento IA
│       ├── data_preparation.R <- código para preprocesamiento de la base depurada versión 2 (SIVIGILA_debugged_v2.rds)
│       ├── logistic_model.stan <- Código stan para el ajuste de modelo de regresión logístico bayesiano
│       ├── model_statement.rmd <- informe sobre modelo ajustado
│       └── run_model_SIVIGILA.rds <-  código para ajustar modelo
├── reports  
│   └── sivigila_model_validation.rmd <-  informe sobre validación de los resultados del modelo ajustado
├── renv  <- ambiente virtual        
│
└── utils
    ├── descriptive_functions.R <- archivo con funciones para hacer gráficos con formatos predeterminados y re-codificación de variables categóricas
    └── load_data.R <- archivo con funciones para cargar y guardar base de datos   

```
