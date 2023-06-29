########################################################
# Taller: Visualización de datos con R y Shiny         #
# Docente: Alexis Adonai Morales Alberto               #
# SciData                                              #
# Sesión: 07                                           #
# Fecha: 28/06/2023                                    #
# Tema 3: Gráfricos estadísticod con ggplot            #
########################################################

# Librerías a utilizar ----

library(tidyverse)
library(haven)
library(foreign)
library(readxl)
library(extrafont)
library(scales)
library(data.table)
library(devtools)
library(modeest)

# Importar datos -----

## Datos para gráfico de barras ----

ENIGH <- read.csv("Bases/ENIGH/ENIGH_SciData.csv")

# Histograma base ----


ENIGH %>% 
  ggplot(aes(x=edad_jefe))+
  geom_histogram()

## Histograma configurado con regla se Sturges y tema ----

ENIGH %>% 
  ggplot(aes(x=edad_jefe))+
  geom_histogram(breaks = pretty(range(ENIGH$edad_jefe),
                                 n=nclass.Sturges(ENIGH$edad_jefe),
                                 min.n = 1),
                 fill="steelblue", col="black")+
  scale_x_continuous(breaks = pretty(range(ENIGH$edad_jefe),
                                     n=nclass.Sturges(ENIGH$edad_jefe),
                                     min.n = 1))+
  scale_y_continuous(breaks = seq(0, 12000, 1000))+
  labs(title = "Histograma de la edad del jefe de familia",
       subtitle = "Intervalos construidos por la regla de Sturges",
       caption = "Elaboración propia con datos de ENIGH 2020",
       x="Intervalos de edad",
       y="Frecuencia")+
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
        legend.text = element_text(size=14, colour="black"),
        legend.title = element_text(size=14, colour="black"))


## Histograma configurado con regla se Sturges, tema y agregando media, median y moda ----

estadisticos_educajefe <- data.frame(
  "Estadísticos" = c("Media", "Mediana", "Moda"),
  "Resultados" = c(mean(ENIGH$edad_jefe),
                   median(ENIGH$edad_jefe),
                   mfv1(ENIGH$edad_jefe))
)


ENIGH %>% 
  ggplot(aes(x=edad_jefe))+
  geom_histogram(breaks = pretty(range(ENIGH$edad_jefe),
                                 n=nclass.Sturges(ENIGH$edad_jefe),
                                 min.n = 1),
                 fill="steelblue", col="black")+
  scale_x_continuous(breaks = pretty(range(ENIGH$edad_jefe),
                                     n=nclass.Sturges(ENIGH$edad_jefe),
                                     min.n = 1))+
  scale_y_continuous(breaks = seq(0, 12000, 1000))+
  geom_vline(data=estadisticos_educajefe,
             aes(xintercept = Resultados,
                 linetype=Estadísticos,
                 color = Estadísticos))+
  scale_color_manual(values=c("red", "green", "brown"))+
  labs(title = "Histograma de la edad del jefe de familia",
       subtitle = "Intervalos construidos por la regla de Sturges",
       caption = "Elaboración propia con datos de ENIGH 2020",
       x="Intervalos de edad",
       y="Frecuencia")+
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
        legend.text = element_text(size=14, colour="black"),
        legend.title = element_text(size=14, colour="black"),
        legend.key = element_rect(fill = "#E0EEE0"),
        axis.title = element_text(size=16))



ggsave(ENIGH %>% 
         ggplot(aes(x=edad_jefe))+
         geom_histogram(breaks = pretty(range(ENIGH$edad_jefe),
                                        n=nclass.Sturges(ENIGH$edad_jefe),
                                        min.n = 1),
                        fill="steelblue", col="black")+
         scale_x_continuous(breaks = pretty(range(ENIGH$edad_jefe),
                                            n=nclass.Sturges(ENIGH$edad_jefe),
                                            min.n = 1))+
         scale_y_continuous(breaks = seq(0, 12000, 1000))+
         geom_vline(data=estadisticos_educajefe,
                    aes(xintercept = Resultados,
                        linetype=Estadísticos,
                        color = Estadísticos))+
         scale_color_manual(values=c("red", "green", "brown"))+
         labs(title = "Histograma de la edad del jefe de familia",
              subtitle = "Intervalos construidos por la regla de Sturges",
              caption = "Elaboración propia con datos de ENIGH 2020",
              x="Intervalos de edad",
              y="Frecuencia")+
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
               legend.text = element_text(size=14, colour="black"),
               legend.title = element_text(size=14, colour="black"),
               legend.key = element_rect(fill = "#E0EEE0"),
               axis.title = element_text(size=16)),
       filename = paste0("Gráficos/", "Proyecto5.png"),
       height = 6,
       width = 15,
       units = "in",
       dpi = 700)


# Gráfico de densidad ------

## Densidad base ----


ENIGH %>% 
  ggplot(aes(x=edad_jefe))+
  geom_density()


## Densidad completa ----

ENIGH %>% 
  ggplot(aes(x=edad_jefe))+
  geom_density(color = "red",
               fill = "steelblue",
               alpha = 0.25)+
  scale_x_continuous(breaks = pretty(range(ENIGH$edad_jefe),
                                     n=nclass.Sturges(ENIGH$edad_jefe),
                                     min.n = 1))+
  scale_y_continuous(breaks = seq(0, 0.03, 0.001))+
  labs(title = "Densidad de la edad del jefe de familia",
       subtitle = "Intervalos construidos por la regla de Sturges",
       caption = "Elaboración propia con datos de ENIGH 2020",
       x="Intervalos de edad",
       y="Densidad / Probabilidad")+
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
        legend.text = element_text(size=14, colour="black"),
        legend.title = element_text(size=14, colour="black"),
        legend.key = element_rect(fill = "#E0EEE0"),
        axis.title = element_text(size=16))


ggsave(ENIGH %>% 
         ggplot(aes(x=edad_jefe))+
         geom_density(color = "red",
                      fill = "steelblue",
                      alpha = 0.25)+
         scale_x_continuous(breaks = pretty(range(ENIGH$edad_jefe),
                                            n=nclass.Sturges(ENIGH$edad_jefe),
                                            min.n = 1))+
         scale_y_continuous(breaks = seq(0, 0.03, 0.001))+
         labs(title = "Densidad de la edad del jefe de familia",
              subtitle = "Intervalos construidos por la regla de Sturges",
              caption = "Elaboración propia con datos de ENIGH 2020",
              x="Intervalos de edad",
              y="Densidad / Probabilidad")+
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
               legend.text = element_text(size=14, colour="black"),
               legend.title = element_text(size=14, colour="black"),
               legend.key = element_rect(fill = "#E0EEE0"),
               axis.title = element_text(size=16)),
       filename = paste0("Gráficos/", "Proyecto6.png"),
       height = 6,
       width = 15,
       units = "in",
       dpi = 700)























