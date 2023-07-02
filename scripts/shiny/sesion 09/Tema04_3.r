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
  dateInput("dob", "¿Cuál es tu fecha de nacimiento"),
  dateRangeInput("holiday", "¿Cuando quieres irte de vacaciones la proxima vez?")
)

server <- function(input, output, session){
  
  
}


shinyApp(ui, server)