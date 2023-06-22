########################################################
# Taller: Visualización de datos con R y Shiny         #
# Docente: Alexis Adonai Morales Alberto               #
# SciData                                              #
# Sesión: 03                                           #
# Fecha: 21/06/2023                                    #
# Tema 2: Gráfricos básicos con ggplot                 #
########################################################

# Instalar paquetes para tema 02 -----

install.packages("extrafont", dependencies = T)

# Librerías a utilizar ----

library(tidyverse)
library(haven)
library(foreign)
library(readxl)
library(extrafont)

# Importar datos -----

ENIGH <- read.csv("Bases/ENIGH/ENIGH_SciData.csv")

# Gráfico báse con gggplot2 -----

## Gráfico vacio ----

ggplot(data = ENIGH)

ENIGH %>% 
  ggplot()

## Relación de variables ----

ggplot(data = ENIGH,
       aes(x = educa_jefe, y=ing_cor))

ggplot(data = ENIGH,
       aes(x = as.factor(educa_jefe), y=ing_cor))+
  labs(title="Gráfico base",
       subtitle = "Ejemplo",
       x="Educación",
       y="Ingreso corriente")


# Gráfico de barras -----

## Ejemplo 1: Ingreso promedio por entidad federativa ----

### Gráfico base ----

ENIGH %>% 
  group_by(NOM_ENT) %>% 
  summarise("Ingreso promedio" = sum(ing_cor*factor)/
              sum(factor)) %>% 
  ggplot(aes(x=NOM_ENT, y=`Ingreso promedio`))+
  geom_col()

### Añadir titulos -----

ENIGH %>% 
  group_by(NOM_ENT) %>% 
  summarise("Ingreso promedio" = sum(ing_cor*factor)/
              sum(factor)) %>% 
  ggplot(aes(x=NOM_ENT, y=`Ingreso promedio`))+
  geom_col()+
  labs(title = "Ingreso corriente promedio por entidad federativa",
       subtitle = "Cifras trimestrales en pesos",
       caption = "Elaboración propia con datos de ENIGH 2020",
       y="",
       x="Entidad")

### Cambiar colores de las barras ----

ENIGH %>% 
  group_by(NOM_ENT) %>% 
  summarise("Ingreso promedio" = sum(ing_cor*factor)/
              sum(factor)) %>% 
  ggplot(aes(x=NOM_ENT, y=`Ingreso promedio`))+
  geom_col(fill="steelblue",
           color="black")+
  labs(title = "Ingreso corriente promedio por entidad federativa",
       subtitle = "Cifras trimestrales en pesos",
       caption = "Elaboración propia con datos de ENIGH 2020",
       y="",
       x="Entidad")


### Añadir escala en eje de "Y" -----

ENIGH %>% 
  group_by(NOM_ENT) %>% 
  summarise("Ingreso promedio" = sum(ing_cor*factor)/
              sum(factor)) %>% 
  ggplot(aes(x=NOM_ENT, y=`Ingreso promedio`))+
  geom_col(fill="steelblue",
           color="black")+
  scale_y_continuous(breaks = seq(0, 80000, 5000))+
  labs(title = "Ingreso corriente promedio por entidad federativa",
       subtitle = "Cifras trimestrales en pesos",
       caption = "Elaboración propia con datos de ENIGH 2020",
       y="",
       x="Entidad")

### Añadir maginitud individual al gráfico ----

ENIGH %>% 
  group_by(NOM_ENT) %>% 
  summarise("Ingreso promedio" = sum(ing_cor*factor)/
              sum(factor)) %>% 
  ggplot(aes(x=NOM_ENT, y=`Ingreso promedio`))+
  geom_col(fill="steelblue",
           color="black")+
  geom_text(aes(x=NOM_ENT, y=`Ingreso promedio`+2000,
                label = round(`Ingreso promedio`)),
            size=3)+
  scale_y_continuous(breaks = seq(0, 80000, 5000))+
  labs(title = "Ingreso corriente promedio por entidad federativa",
       subtitle = "Cifras trimestrales en pesos",
       caption = "Elaboración propia con datos de ENIGH 2020",
       y="",
       x="Entidad")



### Método de exportación e importación con gráfico 

ENIGH %>% 
  group_by(NOM_ENT) %>% 
  summarise("Ingreso promedio" = sum(ing_cor*factor)/
              sum(factor)) %>% 
  ungroup() %>% 
  as.data.frame() %>% 
  write.csv(file = "Bases/ENIGH/Ingreso_promedio_ENT_SciData.csv",
            fileEncoding = "UTF-8",
            row.names = F)

Base <- read.csv("Bases/ENIGH/Ingreso_promedio_ENT_SciData.csv",
                 encoding = "UTF-8",
                 check.names = F)

ggplot(data=Base, aes(x=NOM_ENT, y=`Ingreso promedio`))+
  geom_col(fill="steelblue",
           color="black")+
  geom_text(aes(x=NOM_ENT, y=`Ingreso promedio`+2000,
                label = round(`Ingreso promedio`)),
            size=3)+
  scale_y_continuous(breaks = seq(0, 80000, 5000))+
  labs(title = "Ingreso corriente promedio por entidad federativa",
       subtitle = "Cifras trimestrales en pesos",
       caption = "Elaboración propia con datos de ENIGH 2020",
       y="",
       x="Entidad")



### Manipulación avanzada de tema ----

#### Importar fuentes del sistema ----

font_import() # Solo una vez se ejecuta en toda la vida de la versión de R
loadfonts(device="win", quiet=T) # Esto se hace en cada sesión de R


#### Aplicamos theme como la capa final de manipulación ----


ENIGH %>% 
  group_by(NOM_ENT) %>% 
  summarise("Ingreso promedio" = sum(ing_cor*factor)/
              sum(factor)) %>% 
  ggplot(aes(x=NOM_ENT, y=`Ingreso promedio`))+
  geom_col(fill="steelblue",
           color="black")+
  geom_text(aes(x=NOM_ENT, y=`Ingreso promedio`+2000,
                label = round(`Ingreso promedio`)),
            size=3.5,
            family="Times New Roman")+
  scale_y_continuous(breaks = seq(0, 80000, 5000))+
  labs(title = "Ingreso corriente promedio por *entidad federativa*",
       subtitle = "Cifras trimestrales en pesos",
       caption = "Elaboración propia con datos de ENIGH 2020",
       y="",
       x="Entidad")+
  theme(text = element_text(family="Times New Roman"),
        axis.text.x = element_text(angle = 90,
                                   hjust=1,
                                   size=14),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = ggtext::element_markdown(hjust = 0.5, size=18),
        plot.subtitle = element_text(hjust = 0.5, size=16),
        plot.caption = element_text(hjust=0, size=14),
        panel.grid.minor = element_blank())


### Cambiando orientación del gráfico -----


ENIGH %>% 
  group_by(NOM_ENT) %>% 
  summarise("Ingreso promedio" = sum(ing_cor*factor)/
              sum(factor)) %>% 
  ggplot(aes(x=NOM_ENT, y=`Ingreso promedio`))+
  geom_col(fill="steelblue",
           color="black")+
  coord_flip()+
  geom_text(aes(x=NOM_ENT, y=`Ingreso promedio`+2000,
                label = round(`Ingreso promedio`)),
            size=3.5,
            family="Times New Roman")+
  scale_y_continuous(breaks = seq(0, 80000, 5000))+
  labs(title = "Ingreso corriente promedio por *entidad federativa*",
       subtitle = "Cifras trimestrales en pesos",
       caption = "Elaboración propia con datos de ENIGH 2020",
       y="",
       x="")+
  theme(text = element_text(family="Times New Roman"),
        axis.text.y = element_text(size=14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = ggtext::element_markdown(hjust = 0.5, size=18),
        plot.subtitle = element_text(hjust = 0.5, size=16),
        plot.caption = element_text(hjust=0, size=14),
        panel.grid.minor = element_blank())


## Tarea 03: Con base en la variable "educa_jefe" y el calculo de ingreso promedio realice el gráfico de barras con todos los elementos visto en la clase 03
