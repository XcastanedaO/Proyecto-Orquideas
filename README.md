# Proyecto orquídeas

Este repositorio contiene recursos, código y documentación para el desarrollo del proyecto titulado "Implementación de herramientas analíticas para el estudio de la violencia de género y su impacto en la salud mental de mujeres y niñas en Colombia", seleccionado en la [convocatoria Orquídeas: Mujeres en la Ciencia 2024](https://minciencias.gov.co/convocatorias/convocatoria-orquideas-mujeres-en-la-ciencia-2024) del Ministerio de Ciencia, Tecnología e Innovación de Colombia (MinCiencias). 
El objetivo del proyecto es implementar metodologías estadísticas avanzadas y de aprendizaje automático para evaluar el efecto de los factores que influyen en la incidencia de la violencia de género contra las mujeres y niñas en Colombia e identificar el impacto de este tipo de violencia en su salud mental.

## Autores :pencil2:

- **Alejandra Estefania Patiño Hoyos** - *Doctora investigadora* - [GitHub](https://github.com/alejandraeph)
- **Verónica Seguro Varela** - *Joven investigadora* - [GitHub](https://github.com/vseguro)
- **Johnatan Cardona Jiménez** - *Experto* - [GitHub](https://github.com/JohnatanLAB)
- **Simón Ruiz Martínez** - *Experto* - [GitHub](https://github.com/simonruizm)
- **Mariana Hernádez Giraldo** - *Estudiante auxiliar* - [GitHub](https://github.com/MarianaHernandezG)
- **Ximena Castañeda Ochoa** - *Estudiante auxiliar* - [GitHub](https://github.com/XcastanedaO)

## Instalación :wrench:
Las siguientes son configuraciones/librerías/lenguajes necesarios para el correcto funcionamiento del código:

## Manual :book:
[Este manual](analysis/code_guide.Rmd) explica acciones necesarias para la ejecución del código como: cargue y guardado de conjunto de datos, etc.


## Estructura del repositorio :file_folder:

```         
├── README.md          <- El archivo README para desarrolladores que utilizan este proyecto.
│
├── analysis  
│   ├── code_guide.Rmd   <- Archivo con explicación de funciones o acciones necesarias para el buen funcionamiento del código.
│   ├── debugging        <- Archivos .R para depuración de los conjuntos de datos.
│       ├── sivigila_debugging.R   <- Depuración de los datos provenientes del portal de sivigila.
│       ├── medata_debugging.R   <- Depuración de los datos provenientes de MEData.
│       ├── INMLCF_debugging.R   <- Depuración de ambas bases de datos provenientes del INMLCF.
│       └── datos_abiertos_debugging.R   <- Depuración de los datos provenientes del portal de Datos Abiertos.
│   └── descriptive_analysis   <- Archivos .R para el desarrollo del análisis descriptivo de los conjuntos de datos.
│       └── medata_descriptive.R   <- Análisis descriptivo de los datos provenientes de MEData.
│
├── data
│   ├── raw           <- Conjuntos de datos originales.
|       ├── SIVIGILA <- Carpeta con datos del 2012 al 2023 provenientes del portal oficial de SIVIGILA 
|       ├── medata_1900_2022.csv <- Datos orginales provenientes de MEData. 
│       ├── INMLCF.zip <- Datos sobre casos de lesiones fatales y no fatales desde 2015 al 2024 provenientes del INMLCF
|       ├── datos_abiertos_2015_2023.csv <- Datos orginales del 2015-2023 provenientes del portal de Datos Abiertos.
|       └── CUOC.csv <- Base de datos con Clasificació Única de Ocupaciones adaptada para Colombia. 
│   ├── interim         <- Conjuntos de datos intermedios, que han sido transformados.
│       └── medata_1900_2022_debugged.csv <- Datos depurados provenientes de MEData. 
│   └── processed       <- Conjuntos de datos finales para la modelación.
│
├── models             <- Archivos para la implementación de los modelos.
│
├── reports            <- Informes de los análisis realizados (HTML, PDF, LaTeX, etc.)
│
├── renv               <- Ambiente virtual donde se almacenarán las librerías implementadas.
│
└── utils
    └── load_data.R    <- Funciones para lectura y guardado de conjuntos de datos.

```
