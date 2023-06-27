########################################################
# Taller: Visualización de datos con R y Shiny         #
# Docente: Alexis Adonai Morales Alberto               #
# SciData                                              #
# Sesión: 05                                           #
# Fecha: 26/06/2023                                    #
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
library(tseries)
library(forecast)
library(quantmod)
library(data.table)

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
        axis.ticks = element_blank(),
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

