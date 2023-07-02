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

# Vector de opciones -----

animales <- c("Perro", "Gato", "Ratón", "Pajaro", "Otro")

#===================Estructura de una app de Shiny =====================#

ui <- fluidPage(
  selectInput("state", "¿Cuál es tu estado favorito?", state.name,
              multiple = T),
  radioButtons("animal", "¿Cuál es tu animal favorito?", animales),
  checkboxInput("afirma", "Sí", value = T),
  checkboxInput("nega", "No"),
  fileInput("upload", NULL)
  )

server <- function(input, output, session){
 
  
}


shinyApp(ui, server)