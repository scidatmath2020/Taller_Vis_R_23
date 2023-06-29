########################################################
# Taller: Visualización de datos con R y Shiny         #
# Docente: Alexis Adonai Morales Alberto               #
# SciData                                              #
# Sesión: 07                                           #
# Fecha: 28/06/2023                                    #
# Tema 2: Gráfricos básicos con ggplot                 #
########################################################

# Instalar paquetes para tema 02 -----

install.packages("extrafont", dependencies = T)
install.packages("scales", dependencies = T)
devtools::install_github("ricardo-bion/ggradar")


# Librerías a utilizar ----

library(tidyverse)
library(haven)
library(foreign)
library(readxl)
library(extrafont)
library(scales)
library(tseries)
library(forecast)
library(quantmod)
library(data.table)
library(devtools)
library(ggradar)

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

PIB_C <- Series_INEGI_BIE(id_serie = 493913,
                          token = token_id,
                          periodo = "Trimestral")

PIB <- PIB %>% 
  left_join(PIB_C[,c(1,2)], by="Fecha") %>% 
  rename(PIB = Serie.x,
         PIB_C = Serie.y) 

## Datos desde YahooFinance -----

getSymbols("^GSPC", scr="yahoo")
getSymbols("^MXX", scr="yahoo")

xts_to_dataframe <-function(xts_data){
  fecha = index(xts_data)
  valores = coredata(xts_data)
  salida = data.frame(fecha, valores)
  salida
}

GSPC <- xts_to_dataframe(GSPC$GSPC.Adjusted)
MXX <- xts_to_dataframe(MXX$MXX.Adjusted)
MMXX <- na.omit(MXX)
Datos <- MXX %>% 
  inner_join(GSPC, by = "fecha")

names(Datos) <- c("Fecha", "IPC", "SP500")


## Datos desde R: mtcars -----

data <- mtcars

## Datos de puntajes de jugadores -----

Jugadores <- read.csv("Bases/FIFA23/Puntuación_jugadores.csv",
                      encoding = "UTF-8", check.names = F)

## Tablas de población por edades de 4 años -----

Total_edades_sexo <- read_excel("C:/Users/usuario/Documents/Datos_piramide_pob.xlsx",
                                sheet="ptc_edad") 


Total_edades_sexo <- read_excel("Bases/CENSO2020/Datos_piramide_pob.xlsx",
                                sheet = "ptc_edad")


Total_edades_sexo2 <- read_excel("Bases/CENSO2020/Datos_piramide_pob.xlsx",
                                sheet = "ptc_total")


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
  ggplot(aes(x=Sexo, y=`Ingreso Promedio`))+
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


#===============================================================================================================#

# Clase 5

# Gráfico lineal ----

## Ejemplo básico ----

PIB %>% 
  ggplot(aes(x=Fecha, y=PIB))+
  geom_line()


## Proyecto 1: PIB comprando partidos políticos gobernantes y mostrando crisis ------

class(PIB$Fecha)
class(PIB$Serie)

PIB <- PIB %>% 
  mutate(Crisis = ifelse(PIB_C < 0, 1,0))

Presidentes_mex <- data.frame(
  "Nombre" = c("José López Portillo",
               "Miguel de la Madrid Hurtado",
               "Carlos Salinas de Gortari",
               "Erensto Zedillo Ponce de León",
               "Vicente Foz Quezada",
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
  "Partido" = c("PRI", "PRI",
                "PRI", "PRI",
                "PAN", "PAN",
                "PRI", "MORENA")
)



PIB$Crisis[is.na(PIB$Crisis)] <- 0


PIB %>% 
  ggplot(aes(x=Fecha, y=PIB))+
  geom_rect(aes(NULL, NULL, xmin=Comienzo, xmax=Final,
                fill = Partido), ymin = -Inf, ymax=Inf,
            data=Presidentes_mex)+
  geom_tile(aes(alpha=Crisis),
            fill = "grey", height = Inf)+
  geom_line(linewidth=1,
            linetype = "solid")+
  scale_fill_manual(values = alpha(c("#641E16", "#3498DB", "#E74C3C"),0.3))+
  scale_alpha_continuous(range=c(0,1), breaks = c(0,1),
                         labels = c("Sin crisis", "Crisis"))+
  geom_label(aes(x=Comienzo, y=range(PIB$PIB)[1], label=Nombre),
             data=Presidentes_mex, size=3, hjust=-0.1, vjust=1.2,
             family = "Times New Roman")+
  scale_y_continuous(breaks = seq(7500000, 25000000, 1000000),
                     labels = scales::comma)+
  scale_x_date(date_breaks = "2 year",
               date_labels = "%Y")+
  labs(title = "Producto Interno Bruto en México de *1980-2023*",
       subtitle = "En millones de pesos constantes del 2013: *Periodos de crisis* y *partidos políticos gobernantes*",
       caption = "Elaboración propia con datos del API BIE de INEGI",
       fill = "",
       alpha="",
       x = "",
       y = "")+
  theme(legend.position = "bottom",
        panel.background = element_blank(),
        plot.background = element_rect(fill = "#E0EEE0"),
        legend.background = element_rect(fill = "#E0EEE0"),
        panel.grid.major = element_line(colour = "grey"),
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

## ¿Cómo exportamos un gráfico? ----

PY1 <- PIB %>% 
  ggplot(aes(x=Fecha, y=PIB))+
  geom_rect(aes(NULL, NULL, xmin=Comienzo, xmax=Final,
                fill = Partido), ymin = -Inf, ymax=Inf,
            data=Presidentes_mex)+
  geom_tile(aes(alpha=Crisis),
            fill = "grey", height = Inf)+
  geom_line(linewidth=1)+
  scale_fill_manual(values = alpha(c("#641E16", "#3498DB", "#E74C3C"),0.3))+
  scale_alpha_continuous(range=c(0,1), breaks = c(0,1),
                         labels = c("Sin crisis", "Crisis"))+
  geom_label(aes(x=Comienzo, y=range(PIB$PIB)[1], label=Nombre),
             data=Presidentes_mex, size=3, hjust=-0.1, vjust=1.2,
             family = "Times New Roman")+
  scale_y_continuous(breaks = seq(7500000, 25000000, 1000000),
                     labels = scales::comma)+
  scale_x_date(date_breaks = "2 year",
               date_labels = "%Y")+
  labs(title = "Producto Interno Bruto en México de *1980-2023*",
       subtitle = "En millones de pesos constantes del 2013: *Periodos de crisis* y *partidos políticos gobernantes*",
       caption = "Elaboración propia con datos del API BIE de INEGI",
       fill = "",
       alpha="",
       x = "",
       y = "")+
  theme(legend.position = "bottom",
        panel.background = element_blank(),
        plot.background = element_rect(fill = "#E0EEE0"),
        legend.background = element_rect(fill = "#E0EEE0"),
        panel.grid.major = element_line(colour = "grey"),
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


### Crear directorio -----

dir.create("Gráficos")

ggsave(PY1, 
       filename = paste0("Gráficos/", "Proyecto1.png"),
       height = 6,
       width = 15,
       units = "in",
       dpi = 700)

# Gráfico lineal con dos ejes ----

## Función para trabajar con dos ejes ----

eje_sec <- function(primario, secundario, na.rm=T){
  desde <- range(secundario, na.rm=na.rm)
  hasta <- range(primario, na.rm=na.rm)
  escalado <- function(x){
    rescale(x, from= desde, to = hasta)
  }
  reversa <- function(x){
    rescale(x, from = hasta, to=desde)
  }
  list(fwd = escalado, rev = reversa)
}


### Creación del segundo eje ----

sec <- with(Datos,
            eje_sec(IPC,
                    SP500))

class(Datos$Fecha)

## Elaboración del gráfico -----

ggplot(data = Datos, aes(x=Fecha))+
  geom_line(aes(y=IPC, colour = "IPC"))+
  geom_line(aes(y=sec$fwd(SP500), colour="S&P500"))+
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y")+
  scale_y_continuous(name = "IPC",
                     labels = scales::comma,
                     breaks = seq(10000, 60000, 5000),
                     sec.axis = sec_axis(~sec$rev(.),
                                         name = "S&P500",
                                         breaks = seq(0,5000,500),
                                         labels = scales::comma))+
  scale_colour_manual(values = c("red", "yellow"))+
  labs(title = "Niveles de las bolsas de valores: México *(IPC)* y EUA *(SP500)*",
       subtitle = "Del *2007* al *2023*: Series diarias en puntos",
       caption = "Elaboración propia con datos de Yahoo Finance",
       x="",
       colour = "Bolsas:")+
  theme(legend.position = "bottom",
        panel.background = element_rect(fill="grey40"),
        plot.background = element_rect(fill = "black"),
        legend.background = element_rect(fill = "black"),
        legend.key = element_rect(fill="black"),
        panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_blank(),
        plot.title = ggtext::element_markdown(hjust=0.5,
                                              size=18,
                                              colour="white"),
        plot.subtitle = ggtext::element_markdown(hjust=0.5,
                                                 size=16,
                                                 colour="white"),
        plot.caption = element_text(hjust=0, size=14,
                                    colour="white"),
        text = element_text(family = "Times New Roman"),
        axis.text = element_text(size=14, colour="white"),
        axis.ticks = element_line(color="white"),
        axis.title = element_text(size=14, colour="white"),
        legend.text = element_text(size=14, colour="white"),
        legend.title = element_text(size=14, colour="white"))


ggsave(ggplot(data = Datos, aes(x=Fecha))+
         geom_line(aes(y=IPC, colour = "IPC"))+
         geom_line(aes(y=sec$fwd(SP500), colour="S&P500"))+
         scale_x_date(date_breaks = "1 year",
                      date_labels = "%Y")+
         scale_y_continuous(name = "IPC",
                            labels = scales::comma,
                            breaks = seq(10000, 60000, 5000),
                            sec.axis = sec_axis(~sec$rev(.),
                                                name = "S&P500",
                                                breaks = seq(0,5000,500),
                                                labels = scales::comma))+
         scale_colour_manual(values = c("red", "yellow"))+
         labs(title = "Niveles de las bolsas de valores: México *(IPC)* y EUA *(SP500)*",
              subtitle = "Del *2007* al *2023*: Series diarias en puntos",
              caption = "Elaboración propia con datos de Yahoo Finance",
              x="",
              colour = "Bolsas:")+
         theme(legend.position = "bottom",
               panel.background = element_rect(fill="grey40"),
               plot.background = element_rect(fill = "black"),
               legend.background = element_rect(fill = "black"),
               legend.key = element_rect(fill="black"),
               panel.grid.major = element_line(colour = "grey"),
               panel.grid.minor = element_blank(),
               plot.title = ggtext::element_markdown(hjust=0.5,
                                                     size=18,
                                                     colour="white"),
               plot.subtitle = ggtext::element_markdown(hjust=0.5,
                                                        size=16,
                                                        colour="white"),
               plot.caption = element_text(hjust=0, size=14,
                                           colour="white"),
               text = element_text(family = "Times New Roman"),
               axis.text = element_text(size=14, colour="white"),
               axis.ticks = element_blank(),
               axis.title = element_text(size=14, colour="white"),
               legend.text = element_text(size=14, colour="white"),
               legend.title = element_text(size=14, colour="white")), 
       filename = paste0("Gráficos/", "Proyecto2.png"),
       height = 6,
       width = 15,
       units = "in",
       dpi = 700)


# Gráfico de dispersión -----

data <- data %>% 
  mutate(cilindros = as.character(cyl))

data %>% 
  ggplot(aes(x=hp, y=mpg, color = cilindros))+
  geom_point()+
  scale_x_continuous(breaks = seq(25, 350, 25))+
  scale_y_continuous(breaks = seq(10, 35, 1))+
  scale_color_manual(values=c("#5DADE2", "#BB8FCE", "#DC7633"))+
  labs(title = "Niveles de cosumo de *combustible* por cantidad de *caballos de fuerza*",
       subtitle = "Para 32 vehículos de los años 1972-1973",
       caption = "Elaboración propia con la base mtcars",
       x="Caballos de fuerza (HP)",
       y="Millas por galón (MPG)",
       color = "Cilindros:")+
  theme(legend.position = "bottom",
        panel.background = element_rect(fill="white"),
        plot.background = element_rect(fill = "#ABEBC6"),
        legend.background = element_rect(fill = "#ABEBC6"),
        legend.key = element_rect(fill="#ABEBC6"),
        panel.grid.major = element_line(colour = alpha("#7F8C8D", alpha=0.5)),
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
        axis.ticks = element_line(color="black"),
        axis.title = element_text(size=14, colour="black"),
        legend.text = element_text(size=14, colour="black"),
        legend.title = element_text(size=14, colour="black"))


# NOTA: Colores en rgb ----

Colores <- c(rgb(93, 173, 226, maxColorValue = 255),
             rgb(187, 143, 206, maxColorValue = 255),
             rgb(220, 118, 51, maxColorValue = 255))


# Gráfico de radar ----

## Realizando gráfico ----

ggradar(Jugadores[c(5,8),], grid.min = 0, grid.max = 100,
        grid.mid = 50,
        values.radar = c(0, 50, 100),
        group.colours = c("#C0392B", "#5DADE2"),
        background.circle.colour = "white",
        gridline.min.linetype = 1,
        gridline.mid.linetype = 1,
        gridline.max.linetype = 1,
        gridline.mid.colour = "grey",
        legend.position = "bottom",
        legend.title = "Jugador:")+
  theme(plot.background = element_rect(fill = "#FAD7A0"),
        panel.background = element_rect(fill = "#FAD7A0"),
        legend.background = element_rect(fill = "#FAD7A0"),
        legend.key = element_rect(fill = "#FAD7A0"),
        plot.title = element_text(hjust = 0.5, size=16))+
  labs(title = "Gráfico de radar: Puntaje de características de los jugadores")
  

# Gráfico de piramide poblacional -----

Total_edades_sexo$Edad4 <- factor(x=Total_edades_sexo$Edad4,
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


Total_edades_sexo2$Edad4 <- factor(x=Total_edades_sexo2$Edad4,
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

## Con totales -----


Total_edades_sexo %>% 
  ggplot(aes(x=Edad4, y=Total, fill=Sexo))+
  geom_col(data = subset(Total_edades_sexo,
                         Sexo=="Hombre") %>% 
             mutate(Total = -Total),
           width=0.9,
           col="black")+
  geom_col(data = subset(Total_edades_sexo,
                         Sexo=="Mujer") %>% 
             mutate(Total = Total),
           width=0.9,
           col="black")+
  geom_text(data = subset(Total_edades_sexo,
                          Sexo=="Mujer"),
            aes(y=Total, label=comma(Total)),
            position = position_stack(vjust=0.5),
            size=2.4)+
  geom_text(data = subset(Total_edades_sexo,
                          Sexo=="Hombre"),
            aes(y=-Total, label=comma(Total)),
            position = position_stack(vjust=0.5),
            size=2.4)+
  coord_flip()+
  scale_y_continuous(breaks = c(seq(-60000000,0,by=500000),
                                seq(0, 40000000, by=500000)),
                     labels = comma(c(seq(-60000000,0,by=500000)*-1,
                                      seq(0, 40000000, by=500000))))+
  scale_fill_manual(values=c("#2980B9",
                             "#F5B7B1"))+
  labs(title = "Piramide poblacional en México para el año 2020",
       subtitle = "División por sexo según la edad por cada 4 años",
       caption = "Elaboración propia con datos del CENSO 2020",
       x="",
       y="")+
  theme(legend.position = "bottom",
        panel.background = element_blank(),
        plot.background = element_rect(fill = "#E0EEE0"),
        legend.background = element_rect(fill = "#E0EEE0"),
        panel.grid.major = element_line(colour = "grey"),
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
        axis.text.x=element_text(angle=90, hjust=1))

  
## Con porcentajes: Por categoría de edad -----


Total_edades_sexo %>% 
  ggplot(aes(x=Edad4, y=Total, fill=Sexo))+
  geom_col(data = subset(Total_edades_sexo,
                         Sexo=="Hombre") %>% 
             mutate(Total = -Total),
           width=0.9,
           col="black")+
  geom_col(data = subset(Total_edades_sexo,
                         Sexo=="Mujer") %>% 
             mutate(Total = Total),
           width=0.9,
           col="black")+
  geom_text(data = subset(Total_edades_sexo,
                          Sexo=="Mujer"),
            aes(y=Total, label= Porcentaje),
            position = position_stack(vjust=0.5),
            size=2.4)+
  geom_text(data = subset(Total_edades_sexo,
                          Sexo=="Hombre"),
            aes(y=-Total, label=Porcentaje),
            position = position_stack(vjust=0.5),
            size=2.4)+
  coord_flip()+
  scale_y_continuous(breaks = c(seq(-60000000,0,by=500000),
                                seq(0, 40000000, by=500000)),
                     labels = comma(c(seq(-60000000,0,by=500000)*-1,
                                      seq(0, 40000000, by=500000))))+
  scale_fill_manual(values=c("#2980B9",
                             "#F5B7B1"))+
  labs(title = "Piramide poblacional en México para el año 2020",
       subtitle = "División por sexo según la edad por cada 4 años",
       caption = "Elaboración propia con datos del CENSO 2020",
       x="",
       y="")+
  theme(legend.position = "bottom",
        panel.background = element_blank(),
        plot.background = element_rect(fill = "#E0EEE0"),
        legend.background = element_rect(fill = "#E0EEE0"),
        panel.grid.major = element_line(colour = "grey"),
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
        axis.text.x=element_text(angle=90, hjust=1))


## Piramide porcentaje del total ----

Total_edades_sexo2 %>% 
  ggplot(aes(x=Edad4, y=Total, fill=Sexo))+
  geom_col(data = subset(Total_edades_sexo2,
                         Sexo=="Hombre") %>% 
             mutate(Total = -Total),
           width=0.9,
           col="black")+
  geom_col(data = subset(Total_edades_sexo2,
                         Sexo=="Mujer") %>% 
             mutate(Total = Total),
           width=0.9,
           col="black")+
  geom_text(data = subset(Total_edades_sexo2,
                          Sexo=="Mujer"),
            aes(y=Total, label= Porcentaje),
            position = position_stack(vjust=0.5),
            size=2.4)+
  geom_text(data = subset(Total_edades_sexo2,
                          Sexo=="Hombre"),
            aes(y=-Total, label=Porcentaje),
            position = position_stack(vjust=0.5),
            size=2.4)+
  coord_flip()+
  scale_y_continuous(breaks = c(seq(-60000000,0,by=500000),
                                seq(0, 40000000, by=500000)),
                     labels = comma(c(seq(-60000000,0,by=500000)*-1,
                                      seq(0, 40000000, by=500000))))+
  scale_fill_manual(values=c("#2980B9",
                             "#F5B7B1"))+
  labs(title = "Piramide poblacional en México para el año 2020",
       subtitle = "División por sexo según la edad por cada 4 años",
       caption = "Elaboración propia con datos del CENSO 2020",
       x="",
       y="")+
  theme(legend.position = "bottom",
        panel.background = element_blank(),
        plot.background = element_rect(fill = "#E0EEE0"),
        legend.background = element_rect(fill = "#E0EEE0"),
        panel.grid.major = element_line(colour = "grey"),
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
        axis.text.x=element_text(angle=90, hjust=1))

ggsave(Total_edades_sexo %>% 
         ggplot(aes(x=Edad4, y=Total, fill=Sexo))+
         geom_col(data = subset(Total_edades_sexo,
                                Sexo=="Hombre") %>% 
                    mutate(Total = -Total),
                  width=0.9,
                  col="black")+
         geom_col(data = subset(Total_edades_sexo,
                                Sexo=="Mujer") %>% 
                    mutate(Total = Total),
                  width=0.9,
                  col="black")+
         geom_text(data = subset(Total_edades_sexo,
                                 Sexo=="Mujer"),
                   aes(y=Total, label=comma(Total)),
                   position = position_stack(vjust=0.5),
                   size=2.4)+
         geom_text(data = subset(Total_edades_sexo,
                                 Sexo=="Hombre"),
                   aes(y=-Total, label=comma(Total)),
                   position = position_stack(vjust=0.5),
                   size=2.4)+
         coord_flip()+
         scale_y_continuous(breaks = c(seq(-60000000,0,by=500000),
                                       seq(0, 40000000, by=500000)),
                            labels = comma(c(seq(-60000000,0,by=500000)*-1,
                                             seq(0, 40000000, by=500000))))+
         scale_fill_manual(values=c("#2980B9",
                                    "#F5B7B1"))+
         labs(title = "Piramide poblacional en México para el año 2020",
              subtitle = "División por sexo según la edad por cada 4 años",
              caption = "Elaboración propia con datos del CENSO 2020",
              x="",
              y="")+
         theme(legend.position = "bottom",
               panel.background = element_blank(),
               plot.background = element_rect(fill = "#E0EEE0"),
               legend.background = element_rect(fill = "#E0EEE0"),
               panel.grid.major = element_line(colour = "grey"),
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
               axis.text.x=element_text(angle=90, hjust=1)), 
       filename = paste0("Gráficos/", "Proyecto3.png"),
       height = 6,
       width = 15,
       units = "in",
       dpi = 700)


# Gráfico de tarta/circular/pastel -----


Poblacion_sexo <- Total_edades_sexo %>% 
  group_by(Sexo) %>% 
  summarise(Total = sum(Total)) %>% 
  ungroup() %>% 
  mutate(Porcentaje = Total/sum(Total),
         Etiquetas = paste0(round(Porcentaje*100, 2), "%")) 


## Gráfico base ----


Poblacion_sexo %>% 
  ggplot(aes(x="", y=Porcentaje, fill=Sexo))+
  geom_col()+
  coord_polar(theta="y")


## Etiquetas dentro del circulo ------


### Como texto ----

Poblacion_sexo %>% 
  ggplot(aes(x="", y=Porcentaje, fill=Sexo))+
  geom_col()+
  geom_text(aes(label=Etiquetas),
             position = position_stack(vjust=0.5))+
  coord_polar(theta="y")


### Como etiqueta ----

Poblacion_sexo %>% 
  ggplot(aes(x="", y=Porcentaje, fill=Sexo))+
  geom_col()+
  geom_label(aes(label=Etiquetas),
            position = position_stack(vjust=0.5))+
  coord_polar(theta="y")


## Presentación final ----

P4 <- Poblacion_sexo %>% 
  ggplot(aes(x="", y=Porcentaje, fill=Sexo))+
  geom_col(color = "black")+
  geom_text(aes(label=Etiquetas),
             position = position_stack(vjust=0.5),
             family="Times New Roman")+
  coord_polar(theta="y")+
  scale_fill_manual(values=c("#2980B9",
                             "#F5B7B1"))+
  labs(title = "Porcentaje poblacional en México segun el sexo para el año 2020",
       subtitle = "División porcentual",
       caption = "Elaboración propia con datos del CENSO 2020",
       x="",
       y="")+
  theme(legend.position = "bottom",
        panel.background = element_blank(),
        plot.background = element_rect(fill = "#E0EEE0"),
        legend.background = element_rect(fill = "#E0EEE0"),
        panel.grid.major = element_blank(),
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
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size=14, colour="black"),
        legend.title = element_text(size=14, colour="black"))



P4 <- Poblacion_sexo %>% 
  ggplot(aes(x="", y=Porcentaje, fill=Sexo))+
  geom_col(color = "black")+
  geom_text(aes(label=Etiquetas),
            position = position_stack(vjust=0.5),
            family="Times New Roman")+
  coord_polar(theta="y")+
  scale_fill_manual(values=c("#2980B9",
                             "#F5B7B1"))+
  labs(title = "Porcentaje poblacional en México segun el sexo para el año 2020",
       subtitle = "División porcentual",
       caption = "Elaboración propia con datos del CENSO 2020",
       x="",
       y="")+
  theme(legend.position = "bottom",
        panel.background = element_blank(),
        plot.background = element_rect(fill = "#E0EEE0"),
        legend.background = element_rect(fill = "#E0EEE0"),
        panel.grid.major = element_blank(),
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
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size=14, colour="black"),
        legend.title = element_text(size=14, colour="black"),
        aspect.ratio = 0.5)


ggsave(P4,
       filename = paste0("Gráficos/", "Proyecto4.png"),
       height = 6,
       width = 15,
       units = "in",
       dpi = 700)


