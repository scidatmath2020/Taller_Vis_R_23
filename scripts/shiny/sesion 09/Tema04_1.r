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
  textInput("name", "¿Cuál es tu nombre?"),
  passwordInput("password", "¿Cuál es tu contraseña?"),
  textAreaInput("story", "Cuentame sobre ti", row=3)
)

server <- function(input, output, session){
  
  
}


shinyApp(ui, server)
