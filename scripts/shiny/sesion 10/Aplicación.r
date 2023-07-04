########################################################
# Taller: Visualización de datos con R y Shiny         #
# Docente: Alexis Adonai Morales Alberto               #
# SciData                                              #
# Sesión: 10                                           #
# Fecha: 03/07/2023                                    #
# Tema 4: Aplicación de Shiny del STC metro            #
########################################################


# Cargar librerías -----

library(shiny)
library(shinydashboard)
library(tidyverse)
library(shinythemes)
library(shinyWidgets)


# Cargar datos ----

Ingresos_STC <- read.csv("Bases/STC/actu_ingreso.csv")

Ingresos_STC$fecha <- as.Date(Ingresos_STC$fecha)
Ingresos_STC$ingreso <- as.numeric(Ingresos_STC$ingreso)

## Crear totales ----

Ingresos_totales <- Ingresos_STC %>% 
  group_by(fecha, linea) %>% 
  summarise(tipo_ingreso = "Todas",
            ingreso = sum(ingreso, na.rm=T)) %>% 
  ungroup() %>% 
  select(fecha, tipo_ingreso, linea, ingreso)

## Crear base completa ----

Ingresos_STC2 <- rbind(Ingresos_STC, Ingresos_totales)

## Creación de variables de interés ----

Ingresos_STC2 <- Ingresos_STC2 %>% 
  mutate(Año = format(fecha, "%Y"))

# Vectores de opciones -----

## Lineas del metro -----

lineas <- unique(Ingresos_STC2$linea)

## Tipo de ingreso ----

Tipo_ingreso <- unique(Ingresos_STC2$tipo_ingreso)

## Cargando fuentes ----

extrafont::loadfonts(device="win")

# Estructura de ShinyApp ----

## Interfaz de usuario ----

ui <- fluidPage(
  theme = shinytheme('superhero'),
  # Título de la app incluyendo imagen 
  headerPanel(div(img(src="logo_metro.png",
                      height = "10%",
                      width="10%"), "Ingresos del STC Metro del 2012-2023"),
              windowTitle = "Mi tablero"),
  #Apariencia de pestañas en la aplicación 
  tabsetPanel(type="tabs",
              tabPanel('Gráficos: Ingresos por línea',
                       sidebarLayout(
                         sidebarPanel(
                           selectInput(
                             inputId = "Linea",
                             label = "Seleccione línea",
                             choices = lineas
                           )
                         ),
                         mainPanel(plotOutput('grafico1'))
                       )),
              tabPanel('Tablas: Ingresos por línea',
                       sidebarLayout(
                         sidebarPanel(
                           
                         ),
                         mainPanel(uiOutput("Tabla1"),
                                   downloadButton("download1", "Descarga DB"))
                       )),
              tabPanel('Gráficos: Ingresos por tipo',
                       sidebarLayout(
                         sidebarPanel(
                             selectInput(
                               inputId = "Tipo",
                               label = "Seleccione tipo de ingreso",
                               choices = Tipo_ingreso
                             )
                         ),
                         mainPanel(plotOutput('grafico2'))
                       )),
              tabPanel('Tablas: Ingresos por tipo',
                       sidebarLayout(
                         sidebarPanel(
                         ),
                         mainPanel(uiOutput("Tabla2"))
                       )))
  
  
  
)

## Servidor -----

server <- function(input, output, session){
  
  # Definir base o datos 
  MyData <- reactive({
    Data <- Ingresos_STC2 %>% 
      filter(linea == input$Linea) %>% 
      group_by(Año) %>% 
      summarise(Total = sum(ingreso, na.rm=T)) %>% 
      ungroup()
  })
  
  MyData2 <- reactive({
    Data <- Ingresos_STC2 %>% 
      filter(tipo_ingreso == input$Tipo) %>% 
      group_by(Año) %>% 
      summarise(Total = sum(ingreso, na.rm=T)) %>% 
      ungroup()
  })
  
  # Salidas de gráficos 
  
  output$grafico1 <- renderPlot({
    ggplot(data = MyData(),
           aes(x=Año, y=Total))+
      geom_col(col="black", fill="steelblue")+
      geom_text(aes(x=Año, 
                    y = Total+100000000, 
                    label=scales::comma(Total)),
                size=3.5)+
      labs(title = paste("Ingresos anuales por linea:", input$Linea),
           subtitle = "Del 2012-2023 en pesos",
           caption = "Elaboración propia con datos abiertos de la CDMX",
           x="",
           y="")+
      theme(legend.position = "bottom",
            panel.background = element_blank(),
            plot.background = element_rect(fill = "#E0EEE0"),
            legend.background = element_rect(fill = "#E0EEE0"),
            panel.grid.major = element_line(color="grey"),
            panel.grid.minor = element_blank(),
            plot.title = ggtext::element_markdown(hjust=0.5,
                                                  size=18,
                                                  colour="black"),
            plot.subtitle = ggtext::element_markdown(hjust=0.5,
                                                     size=16,
                                                     colour="black"),
            plot.caption = element_text(hjust=0, size=14,
                                        colour="black"),
            text = element_text(family = "Times New Roman"),
            axis.text = element_text(size=14, colour="black"),
            axis.ticks = element_blank(),
            axis.text.y = element_blank(),
            legend.text = element_text(size=14, colour="black"),
            legend.title = element_text(size=14, colour="black"))
  },
  height = 400, width = 800)
  
  output$grafico2 <- renderPlot({
    ggplot(data = MyData2(),
           aes(x=Año, y=Total))+
      geom_col(col="black", fill="steelblue")+
      geom_text(aes(x=Año, 
                    y = Total+100000000,
                    label=scales::comma(Total)),
                size=3.5)+
      labs(title = paste("Ingresos anuales por tipo:", input$Tipo),
           subtitle = "Del 2012-2023 en pesos",
           caption = "Elaboración propia con datos abiertos de la CDMX",
           x="",
           y="")+
      theme(legend.position = "bottom",
            panel.background = element_blank(),
            plot.background = element_rect(fill = "#E0EEE0"),
            legend.background = element_rect(fill = "#E0EEE0"),
            panel.grid.major = element_line(color="grey"),
            panel.grid.minor = element_blank(),
            plot.title = ggtext::element_markdown(hjust=0.5,
                                                  size=18,
                                                  colour="black"),
            plot.subtitle = ggtext::element_markdown(hjust=0.5,
                                                     size=16,
                                                     colour="black"),
            plot.caption = element_text(hjust=0, size=14,
                                        colour="black"),
            text = element_text(family = "Times New Roman"),
            axis.text = element_text(size=14, colour="black"),
            axis.ticks = element_blank(),
            axis.text.y = element_blank(),
            legend.text = element_text(size=14, colour="black"),
            legend.title = element_text(size=14, colour="black"))
  },
  height = 400, width = 800)
  
  
  output$Tabla1 <- renderUI({
    renderDataTable({MyData()})
  })
  
  output$download1 <- downloadHandler(
    filename = function(){
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file){
      write.csv(MyData(), file)
    }
  )
  
  output$Tabla2 <- renderUI({
    renderDataTable({MyData2()})
  })

  
}



## Salida ----

shinyApp(ui, server)



