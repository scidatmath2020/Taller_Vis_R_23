########################################################
# Taller: Visualización de datos con R y Shiny         #
# Docente: Alexis Adonai Morales Alberto               #
# SciData                                              #
# Sesión: 04                                           #
# Fecha: 21/06/2023                                    #
# Tema 2: Gráfricos básicos con ggplot                 #
########################################################

# Clase 3

# Instalar paquetes para tema 02 -----

install.packages("extrafont", dependencies = T)
install.packages("scales", dependencies = T)


# Librerías a utilizar ----

library(tidyverse)
library(haven)
library(foreign)
library(readxl)
library(extrafont)
library(scales)
library(openxlsx)
library(xlsx)

## Librerías para función de API INEGI ----

library(httr)
library(jsonlite)
library(tidyverse)
library(curl)

# Importar datos -----

## Datos para gráfico de barras ----

ENIGH <- read.csv("Bases/ENIGH/ENIGH_SciData.csv")

## Datos para gráfico de serie de tiempo (lineal) ----

### Proyecto función API INEGI -----

Series_INEGI_BIE <- function(id_serie, token, 
                             periodo = c("Mensual", "Trimestral")){
  url_data <- paste0(
    "https://www.inegi.org.mx/app/api/indicadores/desarrolladores/jsonxml/INDICATOR/",
    id_serie, "/es/0700/false/BIE/2.0/",
    token, "?type=json"
  )
  if(periodo == "Trimestral"){
    ## Llamando API 
    respuesta <- GET(url_data)
    datos <- content(respuesta, "text")
    flujodatos <- paste(datos, collapse = " ")
    
    ## Obtener lista de observaciones 
    flujodatos <- fromJSON(flujodatos)
    flujodatos <- flujodatos$Series
    datos2 <- 0;
    for(i in 1:length(flujodatos)){
      datos2[i] <- flujodatos[[i]]
    }
    datos2 <- as.data.frame(datos2[[10]])
    datos2 <- datos2 %>% 
      dplyr::select(TIME_PERIOD, OBS_VALUE) %>% 
      rename(Fecha = TIME_PERIOD,
             Serie = OBS_VALUE) %>% 
      mutate(Año = substr(Fecha, 1,4),
             Trim = substr(Fecha, 6,7),
             Trim = case_when(Trim=="01"~"01",
                              Trim=="02"~"04",
                              Trim=="03"~"07",
                              Trim=="04"~"10"),
             Fecha = paste0(Año,
                            "-",
                            Trim,
                            "-01"),
             Fecha = as.Date(Fecha, format="%Y-%m-%d"),
             Serie = as.numeric(Serie)) %>% 
      arrange(Fecha)
  }
  else{
    
  }
  if(periodo == "Mensual"){
    ## Llamando API 
    respuesta <- GET(url_data)
    datos <- content(respuesta, "text")
    flujodatos <- paste(datos, collapse = " ")
    
    ## Obtener lista de observaciones 
    flujodatos <- fromJSON(flujodatos)
    flujodatos <- flujodatos$Series
    datos2 <- 0;
    for(i in 1:length(flujodatos)){
      datos2[i] <- flujodatos[[i]]
    }
    datos2 <- as.data.frame(datos2[[10]])
    datos2 <- datos2 %>% 
      dplyr::select(TIME_PERIOD, OBS_VALUE) %>% 
      rename(Fecha = TIME_PERIOD,
             Serie = OBS_VALUE) %>% 
      mutate(Año = substr(Fecha, 1,4),
             Mes = substr(Fecha, 6,7),
             Fecha = paste0(Año,
                            "-",
                            Mes,
                            "-01"),
             Fecha = as.Date(Fecha, format="%Y-%m-%d"),
             Serie = as.numeric(Serie)) %>% 
      arrange(Fecha)
  }
  else{
    
  }
  datos2
}

#### Ejemplo de importación de datos -----

token_id <- "af847734-746b-8eb8-f0e6-4070cc851e47"

PIB <- Series_INEGI_BIE(id_serie = 493911,
                        token = token_id,
                        periodo = "Trimestral")

PIB_cre <- Series_INEGI_BIE(id_serie = 493913,
                            token = token_id,
                            periodo = "Trimestral") %>% 
  rename(PIB_C = Serie)


## Censo de población 2020 ----


Censo2020 <- read.csv(file = "Bases/CENSO2020/Censo_VS.csv")

Total_edades_sexo <- read.xlsx("Bases/CENSO2020/Datos_piramide_pob.xlsx",
                               sheetName = "ptc_edad")

Total_edades_sexo2 <- read.xlsx("Bases/CENSO2020/Datos_piramide_pob.xlsx",
                                sheetName = "ptc_total")

### Construcción de base para piramide poblacional ----

## División de edades 

Censo2020 <- Censo2020 %>% 
  mutate(Edad4=case_when(EDAD %in% c(0:4)~"De 0 a 4 años",
                         EDAD %in% c(5:9)~"De 5 a 9 años",
                         EDAD %in% c(10:14)~"De 10 a 14 años",
                         EDAD %in% c(15:19)~"De 15 a 19 años",
                         EDAD %in% c(20:24)~"De 20 a 24 años",
                         EDAD %in% c(25:29)~"De 25 a 29 años",
                         EDAD %in% c(30:34)~"De 30 a 34 años",
                         EDAD %in% c(35:39)~"De 35 a 39 años",
                         EDAD %in% c(40:44)~"De 40 a 44 años",
                         EDAD %in% c(45:49)~"De 45 a 49 años",
                         EDAD %in% c(50:54)~"De 50 a 54 años",
                         EDAD %in% c(55:59)~"De 55 a 59 años",
                         EDAD %in% c(60:64)~"De 60 a 64 años",
                         EDAD %in% c(65:69)~"De 65 a 69 años",
                         EDAD %in% c(70:74)~"De 70 a 74 años",
                         EDAD %in% c(75:79)~"De 75 a 79 años",
                         EDAD %in% c(80:84)~"De 80 a 84 años",
                         EDAD %in% c(85:130)~"Mas de 84 años",
                         EDAD ==999~"No especificado"))

Censo2020$Edad4<-factor(x=Censo2020$Edad4,
                          levels = c("De 0 a 4 años",
                                     "De 5 a 9 años",
                                     "De 10 a 14 años",
                                     "De 15 a 19 años",
                                     "De 20 a 24 años",
                                     "De 25 a 29 años",
                                     "De 30 a 34 años",
                                     "De 35 a 39 años",
                                     "De 40 a 44 años",
                                     "De 45 a 49 años",
                                     "De 50 a 54 años",
                                     "De 55 a 59 años",
                                     "De 60 a 64 años",
                                     "De 65 a 69 años",
                                     "De 70 a 74 años",
                                     "De 75 a 79 años",
                                     "De 80 a 84 años",
                                     "Mas de 84 años",
                                     "No especificado"))

Censo2020 <- Censo2020 %>% 
  mutate(Sexo=case_when(SEXO==1~"Hombre",
                        SEXO==3~"Mujer"))

## Total por edades y sexo 

Total_edades_sexo<-Censo2020 %>% 
  filter(EDAD %in% c(0:130)) %>% 
  group_by(Edad4, Sexo) %>% 
  summarise(Total=sum(FACTOR)) %>% 
  ungroup() %>% 
  group_by(Edad4) %>% 
  mutate(FR=Total/sum(Total),
         Porcentaje=paste(round(FR*100,2), 
                          "%", sep=""))

Total_edades_sexo2<-Censo2020 %>% 
  filter(EDAD %in% c(0:130)) %>% 
  group_by(Edad4, Sexo) %>% 
  summarise(Total=sum(FACTOR)) %>% 
  ungroup() %>% 
  mutate(FR=Total/sum(Total),
         Porcentaje=paste(round(FR*100,2), 
                          "%", sep=""))

### Exportar datos ----

list_of_datasets <- list("ptc_edad" = Total_edades_sexo, 
                         "ptc_total" = Total_edades_sexo2)


openxlsx::write.xlsx(list_of_datasets,
                     file = "Bases/CENSO2020/Datos_piramide_pob.xlsx")



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


#===============================================================================================================#


# Clase 4 

# Manipulación de colores -----

## Color de fondo del gráfico ----

ENIGH %>% 
  group_by(NOM_ENT) %>% 
  summarise("Ingreso promedio" = sum(ing_cor*factor)/
              sum(factor)) %>% 
  ggplot(aes(x=NOM_ENT, y=`Ingreso promedio`))+
  geom_col(fill="steelblue",
           color="black")+
  coord_flip()+
  geom_text(aes(x=NOM_ENT, y=`Ingreso promedio`-10000,
                label = comma(round(`Ingreso promedio`))),
            size=3.5,
            family="Times New Roman",
            colour="linen")+
  scale_y_continuous(breaks = seq(0, 80000, 5000))+
  labs(title = "Ingreso corriente promedio por *entidad federativa*",
       subtitle = "Cifras trimestrales en pesos",
       caption = "Elaboración propia con datos de ENIGH 2020",
       y="",
       x="")+
  theme(text = element_text(family="Times New Roman"),
        axis.text.y = element_text(size=14, colour="black"),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        plot.title = ggtext::element_markdown(hjust = 0.5, size=18,
                                              colour="black"),
        plot.subtitle = element_text(hjust = 0.5, size=16,
                                     colour="black"),
        plot.caption = element_text(hjust=0, size=14,
                                    colour="black"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.background = element_rect(fill="lightblue", colour=NA),
        panel.background = element_rect(fill="lightblue", colour=NA))


# Gráfico de barras con categorías -----

## Gráfico de barras dividido por dos categorías -----

ENIGH %>% 
  mutate(sexo_jefe = case_when(sexo_jefe==1~"Hombre",
                               sexo_jefe==2~"Mujer")) %>% 
  filter(NOM_ENT %in% c("Ciudad de México",
                        "México","Puebla",
                        "Querétaro", "Morelos",
                        "Hidalgo")) %>% 
  group_by(NOM_ENT, sexo_jefe) %>% 
  summarise("Ingreso Promedio" = sum(factor*ing_cor)/sum(factor)) %>% 
  ungroup() %>% 
  rename(Sexo = sexo_jefe,
         Entidad = NOM_ENT) %>% 
  ggplot(aes(Sexo, `Ingreso Promedio`))+
  geom_col(aes(fill=Sexo), col="black")+
  geom_text(aes(x=Sexo, y=`Ingreso Promedio`-10000,
                label = comma(round(`Ingreso Promedio`))),
            size=3.5,
            family="Times New Roman",
            colour="black")+
  scale_fill_manual(values = c("#8B814C", "#7CCD7C"))+
  facet_wrap(~Entidad, scales = "free")+
  labs(title = "Ingreso corriente promedio por *entidad federativa* y *sexo*",
       subtitle = "Cifras trimestrales en pesos",
       caption = "Elaboración propia con datos de ENIGH 2020",
       y="",
       x="")+
  theme(text = element_text(family="Times New Roman"),
        axis.text.x = element_text(size=14, colour="black"),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        plot.title = ggtext::element_markdown(hjust = 0.5, size=18,
                                              colour="black"),
        plot.subtitle = element_text(hjust = 0.5, size=16,
                                     colour="black"),
        plot.caption = element_text(hjust=0, size=14,
                                    colour="black"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.background = element_rect(fill="grey90", colour=NA),
        panel.background = element_rect(fill="gray90", colour=NA),
        legend.position = "none",
        strip.text = element_text(size=14, color="white"),
        strip.background = element_rect(fill="black"))


## Gráfico de barras apiladas -----


ENIGH %>% 
  mutate(sexo_jefe = case_when(sexo_jefe==1~"Hombre",
                               sexo_jefe==2~"Mujer")) %>% 
  filter(NOM_ENT %in% c("Ciudad de México",
                        "México","Puebla",
                        "Querétaro", "Morelos",
                        "Hidalgo")) %>% 
  mutate(NOM_ENT = factor(NOM_ENT, levels = c("Ciudad de México",
                                              "México",
                                              "Querétaro",
                                              "Puebla",
                                              "Hidalgo",
                                              "Morelos"))) %>% 
  group_by(NOM_ENT, sexo_jefe) %>% 
  summarise("Ingreso Promedio" = sum(factor*ing_cor)/sum(factor)) %>% 
  ungroup() %>% 
  rename(Sexo = sexo_jefe,
         Entidad = NOM_ENT) %>% 
  ggplot(aes(Entidad, `Ingreso Promedio`))+
  geom_col(aes(fill=Sexo), col="black")+
  geom_text(aes(x=Entidad, y=`Ingreso Promedio`,
                label = comma(round(`Ingreso Promedio`)),
                group = Sexo),
            size=3.5,
            family="Times New Roman",
            colour="black", vjust=1)+
  scale_fill_manual(values = c("#8B814C", "#7CCD7C"))+
  labs(title = "Ingreso corriente promedio por *entidad federativa* y *sexo*",
       subtitle = "Cifras trimestrales en pesos",
       caption = "Elaboración propia con datos de ENIGH 2020",
       y="",
       x="")+
  theme(text = element_text(family="Times New Roman"),
        axis.text.x = element_text(size=14, colour="black"),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        plot.title = ggtext::element_markdown(hjust = 0.5, size=18,
                                              colour="black"),
        plot.subtitle = element_text(hjust = 0.5, size=16,
                                     colour="black"),
        plot.caption = element_text(hjust=0, size=14,
                                    colour="black"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.background = element_rect(fill="grey90", colour=NA),
        panel.background = element_rect(fill="gray90", colour=NA),
        legend.position = "bottom",
        legend.background = element_rect(fill="gray90"),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14))


### Invertido ----

ENIGH %>% 
  mutate(sexo_jefe = case_when(sexo_jefe==1~"Hombre",
                               sexo_jefe==2~"Mujer")) %>% 
  filter(NOM_ENT %in% c("Ciudad de México",
                        "México","Puebla",
                        "Querétaro", "Morelos",
                        "Hidalgo")) %>% 
  mutate(NOM_ENT = factor(NOM_ENT, levels = c("Ciudad de México",
                                              "México",
                                              "Querétaro",
                                              "Puebla",
                                              "Hidalgo",
                                              "Morelos"))) %>% 
  group_by(NOM_ENT, sexo_jefe) %>% 
  summarise("Ingreso Promedio" = sum(factor*ing_cor)/sum(factor)) %>% 
  ungroup() %>% 
  rename(Sexo = sexo_jefe,
         Entidad = NOM_ENT) %>% 
  ggplot(aes(Entidad, `Ingreso Promedio`))+
  geom_col(aes(fill=Sexo), col="black")+
  geom_text(aes(x=Entidad, y=`Ingreso Promedio`,
                label = comma(round(`Ingreso Promedio`)),
                group = Sexo),
            size=3.5,
            family="Times New Roman",
            colour="black", hjust=1)+
  coord_flip()+
  scale_fill_manual(values = c("#8B814C", "#7CCD7C"))+
  labs(title = "Ingreso corriente promedio por *entidad federativa* y *sexo*",
       subtitle = "Cifras trimestrales en pesos",
       caption = "Elaboración propia con datos de ENIGH 2020",
       y="",
       x="")+
  theme(text = element_text(family="Times New Roman"),
        axis.text.y = element_text(size=14, colour="black"),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        plot.title = ggtext::element_markdown(hjust = 0.5, size=18,
                                              colour="black"),
        plot.subtitle = element_text(hjust = 0.5, size=16,
                                     colour="black"),
        plot.caption = element_text(hjust=0, size=14,
                                    colour="black"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.background = element_rect(fill="grey90", colour=NA),
        panel.background = element_rect(fill="gray90", colour=NA),
        legend.position = "bottom",
        legend.background = element_rect(fill="gray90"),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14))



## Gráfico de barras separadas -----

ENIGH %>% 
  mutate(sexo_jefe = case_when(sexo_jefe==1~"Hombre",
                               sexo_jefe==2~"Mujer")) %>% 
  filter(NOM_ENT %in% c("Ciudad de México",
                        "México","Puebla",
                        "Querétaro", "Morelos",
                        "Hidalgo")) %>% 
  mutate(NOM_ENT = factor(NOM_ENT, levels = c("Ciudad de México",
                                              "México",
                                              "Querétaro",
                                              "Puebla",
                                              "Hidalgo",
                                              "Morelos"))) %>% 
  group_by(NOM_ENT, sexo_jefe) %>% 
  summarise("Ingreso Promedio" = sum(factor*ing_cor)/sum(factor)) %>% 
  ungroup() %>% 
  rename(Sexo = sexo_jefe,
         Entidad = NOM_ENT) %>% 
  ggplot(aes(Entidad, `Ingreso Promedio`))+
  geom_col(aes(fill=Sexo), col="black",
           position = "dodge")+
  geom_text(aes(x=Entidad, y=`Ingreso Promedio`,
                label = comma(round(`Ingreso Promedio`)),
                group = Sexo),
            size=3.5,
            family="Times New Roman",
            colour="black", vjust=6,
            position = position_dodge(width = 1))+
  scale_fill_manual(values = c("#8B814C", "#7CCD7C"))+
  labs(title = "Ingreso corriente promedio por *entidad federativa* y *sexo*",
       subtitle = "Cifras trimestrales en pesos",
       caption = "Elaboración propia con datos de ENIGH 2020",
       y="",
       x="")+
  theme(text = element_text(family="Times New Roman"),
        axis.text.x = element_text(size=14, colour="black"),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        plot.title = ggtext::element_markdown(hjust = 0.5, size=18,
                                              colour="black"),
        plot.subtitle = element_text(hjust = 0.5, size=16,
                                     colour="black"),
        plot.caption = element_text(hjust=0, size=14,
                                    colour="black"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.background = element_rect(fill="grey90", colour=NA),
        panel.background = element_rect(fill="gray90", colour=NA),
        legend.position = "bottom",
        legend.background = element_rect(fill="gray90"),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14))
  

### Invertido -----

ENIGH %>% 
  mutate(sexo_jefe = case_when(sexo_jefe==1~"Hombre",
                               sexo_jefe==2~"Mujer")) %>% 
  filter(NOM_ENT %in% c("Ciudad de México",
                        "México","Puebla",
                        "Querétaro", "Morelos",
                        "Hidalgo")) %>% 
  mutate(NOM_ENT = factor(NOM_ENT, levels = c("Ciudad de México",
                                              "México",
                                              "Querétaro",
                                              "Puebla",
                                              "Hidalgo",
                                              "Morelos"))) %>% 
  group_by(NOM_ENT, sexo_jefe) %>% 
  summarise("Ingreso Promedio" = sum(factor*ing_cor)/sum(factor)) %>% 
  ungroup() %>% 
  rename(Sexo = sexo_jefe,
         Entidad = NOM_ENT) %>% 
  ggplot(aes(Entidad, `Ingreso Promedio`))+
  geom_col(aes(fill=Sexo), col="black",
           position = "dodge")+
  geom_text(aes(x=Entidad, y=`Ingreso Promedio`,
                label = comma(round(`Ingreso Promedio`)),
                group = Sexo),
            size=3.5,
            family="Times New Roman",
            colour="black", hjust=6,
            position = position_dodge(width = 1))+
  coord_flip()+
  scale_fill_manual(values = c("#8B814C", "#7CCD7C"))+
  labs(title = "Ingreso corriente promedio por *entidad federativa* y *sexo*",
       subtitle = "Cifras trimestrales en pesos",
       caption = "Elaboración propia con datos de ENIGH 2020",
       y="",
       x="")+
  theme(text = element_text(family="Times New Roman"),
        axis.text.y = element_text(size=14, colour="black"),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        plot.title = ggtext::element_markdown(hjust = 0.5, size=18,
                                              colour="black"),
        plot.subtitle = element_text(hjust = 0.5, size=16,
                                     colour="black"),
        plot.caption = element_text(hjust=0, size=14,
                                    colour="black"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.background = element_rect(fill="grey90", colour=NA),
        panel.background = element_rect(fill="gray90", colour=NA),
        legend.position = "bottom",
        legend.background = element_rect(fill="gray90"),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14))


# Gráfico lineal ----

PIB %>% 
  ggplot(aes(x=Fecha, y=Serie))+
  geom_line()

## PIB con presidentes ----

PIB <- PIB %>%
  mutate(Presidentes = case_when(Fecha %in% c(as.Date("1976-10-01"):as.Date("1982-10-01"))~"José López Portillo",
                                 Fecha %in% c(as.Date("1982-10-01"):as.Date("1988-10-01"))~"Miguel de la Madrid Hurtado",
                                 Fecha %in% c(as.Date("1988-10-01"):as.Date("1994-10-01"))~"Carlos Salinas de Gortari",
                                 Fecha %in% c(as.Date("1994-10-01"):as.Date("2000-10-01"))~"Ernesto Zedillo Ponce de León",
                                 Fecha %in% c(as.Date("2000-10-01"):as.Date("2006-10-01"))~"Vicente Fox Quezada",
                                 Fecha %in% c(as.Date("2006-10-01"):as.Date("2012-10-01"))~"Felipe Calderón Hinojonsa",
                                 Fecha %in% c(as.Date("2012-10-01"):as.Date("2018-10-01"))~"Enrique Peña Nieto",
                                 Fecha %in% c(as.Date("2018-10-01"):as.Date("2024-10-01"))~"Andrés Manuel López Obrador"),
         Partido = case_when(Fecha %in% c(as.Date("1976-10-01"):as.Date("1982-10-01"))~"PRI",
                             Fecha %in% c(as.Date("1982-10-01"):as.Date("1988-10-01"))~"PRI",
                             Fecha %in% c(as.Date("1988-10-01"):as.Date("1994-10-01"))~"PRI",
                             Fecha %in% c(as.Date("1994-10-01"):as.Date("2000-10-01"))~"PRI",
                             Fecha %in% c(as.Date("2000-10-01"):as.Date("2006-10-01"))~"PAN",
                             Fecha %in% c(as.Date("2006-10-01"):as.Date("2012-10-01"))~"PAN",
                             Fecha %in% c(as.Date("2012-10-01"):as.Date("2018-10-01"))~"PRI",
                             Fecha %in% c(as.Date("2018-10-01"):as.Date("2024-10-01"))~"MORENA"),
         )

PIB <- PIB %>% 
  inner_join(PIB_cre[,c(1:2)], by="Fecha")

PIB <- PIB %>% 
  mutate(Crisis = ifelse(PIB_C < 0, 1,0))

## Crear variable de presidentes ----

Presidentes_mex <- data.frame(
  "Nombre" = c("José López Portillo",
               "Miguel de la Madrid Hurtado",
               "Carlos Salinas de Gortari",
               "Ernesto Zedillo Ponce de León",
               "Vicente Fox Quezada",
               "Felipe Calderón Hinojonsa",
               "Enrique Peña Nieto",
               "Andrés Manuel López Obrador"),
  "Comienzo" = as.Date(c("1976-10-01",
                          "1982-10-01",
                          "1988-10-01",
                          "1994-10-01",
                          "2000-10-01",
                          "2006-10-01",
                          "2012-10-01",
                          "2018-10-01")),
  "Final" = as.Date(c("1982-10-01",
                       "1988-10-01",
                       "1994-10-01",
                       "2000-10-01",
                       "2006-10-01",
                       "2012-10-01",
                       "2018-10-01",
                       "2024-10-01")),
  "Partido" = c("PRI", "PRI", "PRI", "PRI", "PAN", "PAN", "PRI", "MORENA")
)


## Gráfico lineal utilizando rectas ----

yrng <- range(PIB$Serie)
xrng <- range(PIB$Fecha)

PIB %>% 
  ggplot(aes(x=Fecha, y=Serie))+
  geom_rect(aes(NULL, NULL, xmin = Comienzo, xmax = Final,
                fill = Partido), ymin = -Inf, ymax = Inf,
            data = Presidentes_mex)+
  geom_tile(aes(alpha = Crisis), 
            fill = "grey", height = Inf)+
  geom_line(linewidth=1)+
  scale_fill_manual(values = alpha(c("#641E16", "#3498DB", "#E74C3C"), 0.5))+
  scale_alpha_continuous(range = c(0, 1), breaks = c(0, 1),
                         labels=c("Sin crisis", "Periodo de crisis"))+
  geom_label(aes(x = Comienzo, y = yrng[1], label = Nombre),
            data = Presidentes_mex, size = 3, hjust = -0.1, vjust = 1,
            family="Times New Roman")+
  scale_y_continuous(breaks = seq(7500000, 25000000, 1000000),
                     labels = comma)+
  scale_x_date(date_breaks = "2 year",
               date_labels = "%Y")+
  theme(legend.position = "bottom")+
  labs(title = "Producto interno bruto en México de 1980-2023",
       subtitle = "En millones de pesos constantes del 2013: Periodos de crisis y partidos políticos gobernantes",
       caption = "Elaboración propia con datos del API BIE de INEGI",
       x="",
       y="")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#E0EEE0"),
        legend.background = element_rect(fill = "#E0EEE0"),
        plot.title = ggtext::element_markdown(hjust = 0.5, size=18,
                                              colour="black"),
        plot.subtitle = element_text(hjust = 0.5, size=16,
                                     colour="black"),
        plot.caption = element_text(hjust=0, size=14,
                                    colour="black"),
        text = element_text(family="Times New Roman"),
        axis.text.y = element_text(size=14, colour="black"),
        axis.text.x = element_text(size=14, colour="black"),
        axis.ticks = element_blank(),
        legend.text = element_text(size=14, colour="black"),
        legend.title = element_text(size=14, colour="black"))
  



# Gráfico de piramide ----

## Piramide base ----

ggplot(data=Total_edades_sexo2,
       aes(x=Edad4, y=Total, fill=Sexo))+
  geom_col(data=subset(Total_edades_sexo2,
                       Sexo=="Hombre") %>% 
             mutate(Total=-Total),
           width=0.5, fill="blue",
           col='black')+
  geom_col(data=subset(Total_edades_sexo2,
                       Sexo=="Mujer"),
           width=0.5, fill="pink",
           col='black')+
  coord_flip()


## Piramide final ----

ggplot(data=Total_edades_sexo2,
       aes(x=Edad4, y=Total, fill=Sexo))+
  geom_col(data=subset(Total_edades_sexo2,
                       Sexo=="Hombre") %>% 
             mutate(Total=-Total),
           width=0.5, col='black')+
  geom_col(data=subset(Total_edades_sexo2,
                       Sexo=="Mujer"),
           width=0.5, col='black')+
  geom_text(data=subset(Total_edades_sexo2,
                        Sexo=="Mujer"),
            aes(y=Total, label=Porcentaje),
            position = position_stack(vjust = 0.5))+
  geom_text(data=subset(Total_edades_sexo2,
                        Sexo=="Hombre"),
            aes(y=-Total, label=Porcentaje),
            position = position_stack(vjust = 0.5),
            col="white")+
  coord_flip()+
  scale_y_continuous(
    breaks = c(seq(-60000000,0,by=500000),
               seq(0,40000000, by=500000)),
    labels = c(seq(-60000000,0,by=500000)*-1,
               seq(0,40000000, by=500000)))+
  scale_fill_manual(values=c("#2980B9",
                             "#F5B7B1"))+
  labs(title = "Piramide poblacional de México",
       subtitle = "Año 2020",
       caption = "Elaboración propia con datos del censo 2020")+
  theme_classic()+
  theme(axis.text.x = element_text(angle=45,
                                   hjust=1),
        plot.title = element_text(face='bold',
                                  size=20,
                                  hjust=0.5),
        plot.subtitle = element_text(size=15,
                                     hjust=0.5),
        plot.caption = element_text(size=12,
                                    face='italic'),
        legend.position = "right")


# Gráfico de palabras

# https://r-charts.com/es/ranking/ggwordcloud/
# https://www.rpubs.com/Semilla_389/788115 

# Gráfico de radar 

# https://r-charts.com/es/ranking/ggradar/ 

# Gráfico de dispersión: Elaborarlo con mtcars 














