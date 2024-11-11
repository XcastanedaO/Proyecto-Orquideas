# Proyecto orquídeas

<div style="text-align: justify;">
Este repositorio contiene recursos, código y documentación para el desarrollo del proyecto titulado 
"Implementación de herramientas analíticas para el estudio de la violencia de género y su impacto en la salud 
mental de mujeres y niñas en Colombia", seleccionado en la 
[convocatoria Orquídeas: Mujeres en la Ciencia 2024](https://minciencias.gov.co/convocatorias/convocatoria-orquideas-mujeres-en-la-ciencia-2024) del Ministerio de Ciencia, Tecnología e Innovación de Colombia (MinCiencias). 
El objetivo del proyecto es implementar metodologías estadísticas avanzadas y de aprendizaje automático para evaluar el efecto de los factores que influyen en la incidencia de la violencia de género contra las mujeres y niñas en Colombia e identificar el impacto de este tipo de violencia en su salud mental.
</div>

## Autores :pencil2:

- **Alejandra Estefania Patiño Hoyos** - *Doctora investigadora* - [GitHub]()
- **Verónica Seguro Varela** - *Joven investigadora* - [GitHub](https://github.com/vseguro)
- **Johnatan Cardona Jiménez** - *Experto* - [GitHub]()
- **Simón Ruiz Martínez** - *Experto* - [GitHub]()
- **Mariana Hernádez Giraldo** - *Estudiante auxiliar* - [GitHub]()
- **Ximena Castañeda Ochoa** - *Estudiante auxiliar* - [GitHub](https://github.com/XcastanedaO)

## Instalación :wrench:
Las siguientes son configuraciones/librerías/lenguajes necesarios para el correcto funcionamiento del código:

## Manual de ayuda :book:
[Este manual](analysis/manual_ayuda.Rmd) explica acciones necesarias para la ejecución del código como: cargue y guardado de conjunto de datos, etc.


## Estructura del repositorio :file_folder:

```         
├── README.md          <- El archivo README para desarrolladores que utilizan este proyecto.
│
├── analysis  
│   └── descriptive_analysis   <- Archivos .R para el desarrollo del análisis descriptivo de los conjuntos de datos.
│
├── data
│   ├── raw             <- Conjuntos de datos originales.
│   ├── interim         <- Conjuntos de datos intermedios, que han sido transformados.
│   └── processed       <- Conjuntos de datos finales para la modelación.
│
├── models             <- Archivos para la implementación de los modelos.
│
├── reports            <- Informes de los análisis realizados (HTML, PDF, LaTeX, etc.)
│
├── renv               <- Ambiente virtual donde se almacenarán las librerías implementadas.
│
└── utils            <- Archivos auxiliares

```
