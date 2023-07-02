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

#===================Estructura de app de Shiny =====================#

ui <- fluidPage(
  plotOutput("plot", width = "400px")
)

server <- function(input, output, session){
  
  output$plot <- renderPlot(plot(1:5), res=96)
  
}

shinyApp(ui, server)





