########################################################
# Taller: Visualización de datos con R y Shiny         #
# Docente: Alexis Adonai Morales Alberto               #
# SciData                                              #
# Sesión: 09                                           #
# Fecha: 30/06/2023                                    #
# Tema 4: Aplicación de Shiny (principios)             #
########################################################

# Instalar paquetería de Shiny ----

# install.packages("shiny", dependencies = T)

# librerías a utilizar ----

library(shiny)

#===================Estructura de una app de Shiny =====================#

ui <- fluidPage(
  numericInput("num", "Número uno", value=0, min=0, max=100),
  sliderInput("num2", "Número dos", value = 50, min=0, max=100),
  sliderInput("rng", "Rango", value=c(10,20), min = 0, max=100)
)

server <- function(input, output, session){
  
  
}


shinyApp(ui, server)