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
  tableOutput("static"),
  dataTableOutput("dynamic")
)

server <- function(input, output, session){
  output$static <- renderTable(head(mtcars))
  
  output$dynamic <- renderDataTable(mtcars, options = list(pageLength=5))

}

shinyApp(ui, server)





