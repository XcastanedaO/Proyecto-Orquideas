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
- **Tomás Rodriguez Taborda** - *Estudiante auxiliar* - [GitHub](https://github.com/torodriguezt)

## Instalación :wrench:
Las siguientes son configuraciones/librerías/lenguajes necesarios para el correcto funcionamiento del código:

## Manual :book:
[Este manual](analysis/code_guide.Rmd) explica acciones necesarias para la ejecución del código como: cargue y guardado de conjunto de datos, etc.


## Estructura del repositorio :file_folder:
La siguiente estructura es general y puede variar entre ramas:

```         
├── README.md          <- El archivo README para desarrolladores que utilizan este proyecto.
│
├── analysis  
│   ├── code_guide.Rmd   <- Archivo con explicación de funciones o acciones necesarias para el buen funcionamiento del código.
│   ├── debugging        <- Archivos .R para depuración de los conjuntos de datos.
|   ├── descriptive_analysis   <- Archivos .R para el desarrollo del análisis descriptivo de los conjuntos de datos.
│   └── imputation   <- Archivos .R para implementar método de imputación de datos.
│
├── data
│   ├── raw           <- Conjuntos de datos originales. 
│   ├── interim         <- Conjuntos de datos intermedios, que han sido transformados.
|   ├── processed       <- Conjuntos de datos finales para la modelación.
│   └── auxiliary       <- Conjuntos de datos con codificaciones o información de apoyo para realizar los análisis
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

## Ramas principales 🗂️
Este repositorio utiliza ramas para organizar distintas etapas o enfoques del proyecto. A continuación se describen las ramas principales:

- **main**: Versión final de proyecto.
- **Dev-models**: Rama en desarrollo. Aquí se realizan los análisis relacionados con los casos de lesiones fatales y no fatales
de causa externa entre 01/enero y 30/noviembre del 2024 atendidos por Instituto Nacional de Medicina Legal y Ciencias Forenses.
- **dev_medata**: Rama en desarrollo. Aquí se realizan los análisis relacionados con los casos sospechosos de violencia de género
que fueron reportados al SIVIGILA en Medellín. 
- **dev_sivigila**: Rama en desarrollo. Aquí se realizan los análisis relacionados con la información proporcionada por el Instituto Nacional de Salud de Colombia sobre los casos sospechosos o probables de violencia de género que fueron reportados al SIVIGILA durante 2013 y 2024.

