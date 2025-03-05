# Software de Diagnóstico Comunal

Este repositorio contiene una aplicación Shiny para el análisis y visualización de datos climáticos a nivel comunal en Chile.

## Características

- Visualización de series temporales de variables climáticas
- Selección de modelos climáticos
- Análisis de datos históricos y proyecciones futuras
- Exportación de gráficos

## Requisitos

- R 4.0.0 o superior
- Paquetes de R:
  - shiny
  - dplyr
  - ggplot2
  - shinyFiles
  - shinydashboard
  - shinydashboardPlus

## Instalación

```r
# Instalar los paquetes necesarios
install.packages(c("shiny", "dplyr", "ggplot2", "shinyFiles", "shinydashboard", "shinydashboardPlus"))
```

## Uso

Para ejecutar la aplicación:

```r
shiny::runApp("GUI.R")
```

## Estructura del Proyecto

- `GUI.R`: Interfaz gráfica de usuario y lógica de la aplicación
- `indicadores_futuros/procesar_arclim.R`: Funciones para procesar datos climáticos
- `funciones_auxiliares.R`: Funciones auxiliares para el procesamiento de datos
- `BBDD/`: Directorio con datos climáticos (no incluido en el repositorio) 