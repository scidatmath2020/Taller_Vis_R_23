########################################################
# Taller: Visualización de datos con R y Shiny         #
# Docente: Alexis Adonai Morales Alberto               #
# SciData                                              #
# Sesión: 08                                           #
# Fecha: 29/06/2023                                    #
# Tema 3: Gráfricos estadísticos con ggplot2           #
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


# Histograma con gráfico de densidad -----

## Gráfico base ----

ENIGH %>% 
  ggplot(aes(x=edad_jefe))+
  geom_histogram(aes(y=after_stat(density)))+
  geom_density()


ENIGH %>% 
  ggplot(aes(x=edad_jefe))+
  geom_histogram(aes(y=..density..))+
  geom_density()


## Ajustes del gráfico ----

### Retomando el data.frame de "estaditicos_educajefe

estadisticos_educajefe <- estadisticos_educajefe %>% 
  mutate(Etiquetas = paste(Estadísticos, ":","", round(Resultados,2)))

ENIGH %>% 
  ggplot(aes(x=edad_jefe))+
  geom_histogram(aes(y=after_stat(density)),
                 breaks = pretty(range(ENIGH$edad_jefe),
                                 n = nclass.Sturges(ENIGH$edad_jefe),
                                 min.n = 1),
                 color="black", fill="steelblue",
                 alpha=0.3)+
  geom_density(color = "darkred",
               alpha = 0.25,
               linetype="solid",
               size=1)+
  geom_vline(data=estadisticos_educajefe,
             aes(xintercept = Resultados,
                 linetype= Etiquetas,
                 color = Etiquetas))+
  scale_x_continuous(breaks = pretty(range(ENIGH$edad_jefe),
                                     n = nclass.Sturges(ENIGH$edad_jefe),
                                     min.n = 1))+
  scale_color_manual(values=c("chocolate", "darkorchid4",
                              "gold4"))+
  labs(title = "Histograma y densidad de la edad del jefe de familia",
       subtitle = "Intervalos construidos por la regla de Sturges",
       caption = "Elaboración propia con datos de ENIGH 2020",
       x="Intervalos de edad",
       y="Densidad / Probabilidad",
       color="",
       linetype="")+
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
         geom_histogram(aes(y=after_stat(density)),
                        breaks = pretty(range(ENIGH$edad_jefe),
                                        n = nclass.Sturges(ENIGH$edad_jefe),
                                        min.n = 1),
                        color="black", fill="steelblue",
                        alpha=0.3)+
         geom_density(color = "darkred",
                      alpha = 0.25,
                      linetype="solid",
                      size=1)+
         geom_vline(data=estadisticos_educajefe,
                    aes(xintercept = Resultados,
                        linetype= Etiquetas,
                        color = Etiquetas))+
         scale_x_continuous(breaks = pretty(range(ENIGH$edad_jefe),
                                            n = nclass.Sturges(ENIGH$edad_jefe),
                                            min.n = 1))+
         scale_color_manual(values=c("chocolate", "darkorchid4",
                                     "gold4"))+
         labs(title = "Histograma y densidad de la edad del jefe de familia",
              subtitle = "Intervalos construidos por la regla de Sturges",
              caption = "Elaboración propia con datos de ENIGH 2020",
              x="Intervalos de edad",
              y="Densidad / Probabilidad",
              color="",
              linetype="")+
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
       filename = paste0("Gráficos/", "Proyecto7.png"),
       height = 6,
       width = 15,
       units = "in",
       dpi = 700)


# Gráfico de caja y bigotes ----


## Gráfico base ----

ENIGH %>% 
  ggplot(aes(x="", y=edad_jefe))+
  stat_boxplot()+
  geom_boxplot()


## Gráfico base modificando estructura ----


ENIGH %>% 
  ggplot(aes(x="", y=edad_jefe))+
  stat_boxplot(geom="errorbar",
               width=0.15,
               color="black")+
  geom_boxplot(fill="steelblue",
               alpha=0.5,
               color="black",
               outlier.colour = "red")


## Gráfico final -----

ENIGH %>% 
  ggplot(aes(x="", y=edad_jefe))+
  stat_boxplot(geom="errorbar",
               width=0.15,
               color="black")+
  geom_boxplot(fill="steelblue",
               alpha=0.5,
               color="black",
               outlier.colour = "red",
               width=0.5)+
  stat_summary(fun = mean, geom="point",
               shape=20, size=5, color="red", fill="red")+
  scale_y_continuous(breaks=seq(min(ENIGH$edad_jefe),
                                max(ENIGH$edad_jefe),
                                5))+
  labs(title = "Diagrama de cajas y bigotes de la edad del jefe de familia",
       subtitle = "",
       caption = "Elaboración propia con datos de ENIGH 2020",
       x="",
       y="Edad del jefe de familia",
       color="",
       linetype="")+
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


## Gráfico de cajas con factores -----

#1 Bajo
#2 Medio bajo
#3 Medio alto
#4 Alto

Valores <- c("Bajo", "Medio bajo", "Medio alto", "Alto")
est_socio <- c(1,2,3,4)

for (i in 1:4) {
  ENIGH$est_socio[ENIGH$est_socio==est_socio[i]] <- Valores[i]
}

unique(ENIGH$est_socio)

ENIGH$est_socio <- factor(ENIGH$est_socio,
                          levels =c("Bajo", "Medio bajo", "Medio alto", "Alto") )


ENIGH %>% 
  ggplot(aes(x=est_socio, y=edad_jefe))+
  stat_boxplot(geom="errorbar",
               width=0.15,
               color="black")+
  geom_boxplot(fill="steelblue",
               alpha=0.5,
               color="black",
               outlier.colour = "red",
               width=0.5)+
  stat_summary(fun = mean, geom="point",
               shape=20, size=5, color="red", fill="red")+
  scale_y_continuous(breaks=seq(min(ENIGH$edad_jefe),
                                max(ENIGH$edad_jefe),
                                5))+
  labs(title = "Diagrama de cajas y bigotes de la edad del jefe de familia",
       subtitle = "Dividido por factor de nivel socioeconómico",
       caption = "Elaboración propia con datos de ENIGH 2020",
       x="",
       y="Edad del jefe de familia",
       color="",
       linetype="")+
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
         ggplot(aes(x=est_socio, y=edad_jefe))+
         stat_boxplot(geom="errorbar",
                      width=0.15,
                      color="black")+
         geom_boxplot(fill="steelblue",
                      alpha=0.5,
                      color="black",
                      outlier.colour = "red",
                      width=0.5)+
         stat_summary(fun = mean, geom="point",
                      shape=20, size=5, color="red", fill="red")+
         scale_y_continuous(breaks=seq(min(ENIGH$edad_jefe),
                                       max(ENIGH$edad_jefe),
                                       5))+
         labs(title = "Diagrama de cajas y bigotes de la edad del jefe de familia",
              subtitle = "Dividido por factor de nivel socioeconómico",
              caption = "Elaboración propia con datos de ENIGH 2020",
              x="",
              y="Edad del jefe de familia",
              color="",
              linetype="")+
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
       filename = paste0("Gráficos/", "Proyecto8.png"),
       height = 6,
       width = 15,
       units = "in",
       dpi = 700)


# Gráfico de violin -----

## Gráfico base ----

ENIGH %>% 
  ggplot(aes(x="", y=edad_jefe))+
  geom_violin()

ENIGH %>% 
  ggplot(aes(x=edad_jefe, y=""))+
  geom_violin()


## Gráfico final -----

ENIGH %>% 
  ggplot(aes(x=est_socio, y=edad_jefe))+
  geom_violin(fill="steelblue",
               alpha=0.5,
               color="black",
               width=0.5)+
  stat_summary(fun = mean, geom="point",
               shape=20, size=5, color="red", fill="red")+
  scale_y_continuous(breaks=seq(min(ENIGH$edad_jefe),
                                max(ENIGH$edad_jefe),
                                5))+
  labs(title = "Diagrama de violin de la edad del jefe de familia",
       subtitle = "Dividido por factor de nivel socioeconómico",
       caption = "Elaboración propia con datos de ENIGH 2020",
       x="",
       y="Edad del jefe de familia",
       color="",
       linetype="")+
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
         ggplot(aes(x=est_socio, y=edad_jefe))+
         geom_violin(fill="steelblue",
                     alpha=0.5,
                     color="black",
                     width=0.5)+
         stat_summary(fun = mean, geom="point",
                      shape=20, size=5, color="red", fill="red")+
         scale_y_continuous(breaks=seq(min(ENIGH$edad_jefe),
                                       max(ENIGH$edad_jefe),
                                       5))+
         labs(title = "Diagrama de violin de la edad del jefe de familia",
              subtitle = "Dividido por factor de nivel socioeconómico",
              caption = "Elaboración propia con datos de ENIGH 2020",
              x="",
              y="Edad del jefe de familia",
              color="",
              linetype="")+
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
       filename = paste0("Gráficos/", "Proyecto9.png"),
       height = 6,
       width = 15,
       units = "in",
       dpi = 700)



