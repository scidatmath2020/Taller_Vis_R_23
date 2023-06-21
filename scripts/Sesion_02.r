########################################################
# Taller: Visualización de datos con R y Shiny         #
# Docente: Alexis Adonai Morales Alberto               #
# SciData                                              #
# Sesión: 02                                           #
# Fecha: 20/06/2023                                    #
# Tema 1: Manipulación de bases de datos               #
########################################################

# Inicio sesión 1

# Instalación de paqueterías ----

install.packages("readxl", dependencies = T)
install.packages("haven", dependencies = T)
install.packages("tidyverse", dependencies = T)


# Librerías a utilizar ----

library(tidyverse)
library(haven)
library(foreign)
library(readxl)

# Importando base de datos -----

## Conociendo ruta de trabajo ----

getwd()

## Importando CSV ----

ENIGH <- read.csv("Bases/ENIGH/ENIGH_SciData.csv",
                  encoding = "UTF-8")

## Importando dta ----

ENIGH <- read.dta("Bases/ENIGH/concentradohogar.dta")

## Importando sav -----

ENIGH <- read_sav("Bases/ENIGH/concentradohogar.sav")

## Importando DBF -----

ENIGH <- read.dbf("Bases/ENIGH/concentradohogar.dbf")

## Función especial para descargar ENIGH e importarla directo ------

# Funciones para descargar encuestas ----

## Función "ENIGH nueva serie encuestas del 2016 al 2020" ----

#https://inegi.org.mx/contenidos/programas/enigh/nc/2020/microdatos/enigh2020_ns_viviendas_dbf.zip

enigh_inegi_NS <- function(año = NA, section = c("hogares", "poblacion",
                                                 "gastohogar", "erogaciones",
                                                 "gastotarjetas", "ingresos",
                                                 "gastopersona", "trabajos",
                                                 "agro", "noagro",
                                                 "concentradohogar",
                                                 "agroproductos",
                                                 "agroconsumo",
                                                 "agrogasto",
                                                 "noagroimportes",
                                                 "ingresos_JCF")){
  url_enigh = "https://www.inegi.org.mx/contenidos/programas/enigh/nc/"
  url_base  = paste0(url_enigh,
                     año, "/microdatos/enigh",
                     año, "_ns_",section,"_dbf.zip")
  if (file.exists(paste0(getwd(), "/datos"))){
    zipdir = paste0(getwd(), "/datos")
  } else {
    dir.create(paste0(getwd(), "/datos"))
    zipdir = paste0(getwd(), "/datos")
    
  }
  if (file.exists(paste0(getwd(), "/zip"))){
    temp_enigh = paste0(getwd(), "/zip")
  } else {
    dir.create(paste0(getwd(), "/zip"))
    temp_enigh = paste0(getwd(), "/zip")
    
  }
  file_name <- basename(temp_enigh)
  utils::download.file(url_base, paste(temp_enigh, file_name, sep="/"))
  utils::unzip(paste(temp_enigh, file_name, sep="/"), exdir=zipdir)
  if(section == "viviendas"){
    viviendas <-foreign::read.dbf(paste0(zipdir,
                                         "\\", section, ".dbf"))
    viviendas 
  }
  
  else if (section == "hogares"){
    hogares <-foreign::read.dbf(paste0(zipdir,
                                       "\\", section, ".dbf"))
    hogares
  }
  
  else if (section =="poblacion"){
    poblacion<-foreign::read.dbf(paste0(zipdir,
                                        "\\", section, ".dbf"))
    poblacion                             
  }
  
  else if (section =="gastoshogar"){
    gastoshogar<-foreign::read.dbf(paste0(zipdir,
                                          "\\", section, ".dbf"))
    gastoshogar                             
  }
  
  else if (section =="erogaciones"){
    erogaciones<-foreign::read.dbf(paste0(zipdir,
                                          "\\", section, ".dbf"))
    erogaciones                             
  }
  
  else if (section =="gastotarjetas"){
    gastotarjetas<-foreign::read.dbf(paste0(zipdir,
                                            "\\", section, ".dbf"))
    gastotarjetas                             
  }
  
  else if (section =="ingresos"){
    ingresos<-foreign::read.dbf(paste0(zipdir,
                                       "\\", section, ".dbf"))
    ingresos                            
  }
  
  else if (section =="gastospersona"){
    gastospersona<-foreign::read.dbf(paste0(zipdir,
                                            "\\", section, ".dbf"))
    gastospersona                             
  }
  
  else if (section =="trabajos"){
    trabajos<-foreign::read.dbf(paste0(zipdir,
                                       "\\", section, ".dbf"))
    trabajos                             
  }
  
  else if (section =="agro"){
    agro<-foreign::read.dbf(paste0(zipdir,
                                   "\\", section, ".dbf"))
    agro                             
  }
  
  else if (section =="noagro"){
    noagro<-foreign::read.dbf(paste0(zipdir,
                                     "\\", section, ".dbf"))
    noagro                             
  }
  
  else if (section =="concentradohogar"){
    concentradohogar<-foreign::read.dbf(paste0(zipdir,
                                               "\\", section, ".dbf"))
    concentradohogar                             
  }
  
  else if (section =="agroproductos"){
    agroproductos<-foreign::read.dbf(paste0(zipdir,
                                            "\\", section, ".dbf"))
    agroproductos                             
  }
  
  else if (section =="agroconsumo"){
    agroconsumo<-foreign::read.dbf(paste0(zipdir,
                                          "\\", section, ".dbf"))
    agroconsumo                             
  }
  
  else if (section =="agrogasto"){
    agrogasto<-foreign::read.dbf(paste0(zipdir,
                                        "\\", section, ".dbf"))
    agrogasto                             
  }
  
  else if (section =="noagroimportes"){
    noagroimportes<-foreign::read.dbf(paste0(zipdir,
                                             "\\", section, ".dbf"))
    noagroimportes                             
  }
  
  else if (section =="ingresos_JCF"){
    ingresos_JCF<-foreign::read.dbf(paste0(zipdir,
                                           "\\", section, ".dbf"))
    ingresos_JCF                             
  }
}

concentradohogar <- enigh_inegi_NS(año = "2020",
                                   section = "concentradohogar")

# Crear columnas o variables ------

x <- 10

names(ENIGH)

## Crear variable: CVE_ENT ----

ENIGH <- ENIGH %>% 
  rename(folioviv = `ï..folioviv`)

### Opción 1 usando "$" ----

ENIGH$CVE_ENT <- substr(ENIGH$folioviv, 1,2)

head(ENIGH$CVE_ENT)

ENIGH$folioviv

class(ENIGH$folioviv)

str(ENIGH)

### Opción 2 usando "mutate" y "pipe" de dplyr -----

ENIGH <- ENIGH %>% #ctrl+shift+m
  mutate(CVE_ENT2 = substr(folioviv, 1, 2))


## Recodificación -----

ENIGH$CVE_ENT2[ENIGH$CVE_ENT2=="01"] <- "Aguascalientes"

ENIGH <- ENIGH %>% 
  mutate(CVE_ENT2 = case_when(CVE_ENT2=="01"~"Aguascalientes",
                              CVE_ENT2=="02"~"Baja California",
                              CVE_ENT2=="03"~"Baja California Sur",
                              CVE_ENT2=="04"~"Campeche",
                              CVE_ENT2=="05"~"Coahuila de Zaragoza",
                              CVE_ENT2=="06"~"Colima",
                              CVE_ENT2=="07"~"Chiapas",
                              CVE_ENT2=="08"~"Chihuahua",
                              CVE_ENT2=="09"~"Ciudad de México",
                              CVE_ENT2=="10"~"Durango",
                              CVE_ENT2=="11"~"Guanajuato",
                              CVE_ENT2=="12"~"Guerrero",
                              CVE_ENT2=="13"~"Hidalgo",
                              CVE_ENT2=="14"~"Jalisco",
                              CVE_ENT2=="15"~"México",
                              CVE_ENT2=="16"~"Michiacán de Ocampo",
                              CVE_ENT2=="17"~"Morelos",
                              CVE_ENT2=="18"~"Nayarit",
                              CVE_ENT2=="19"~"Nuevo León",
                              CVE_ENT2=="20"~"Oaxaca",
                              CVE_ENT2=="21"~"Puebla",
                              CVE_ENT2=="22"~"Querétaro",
                              CVE_ENT2=="23"~"Quinata Roo",
                              CVE_ENT2=="24"~"San Luis Potosí",
                              CVE_ENT2=="25"~"Sinaloa",
                              CVE_ENT2=="26"~"Sonora",
                              CVE_ENT2=="27"~"Tabasco",
                              CVE_ENT2=="28"~"Tamaulipas",
                              CVE_ENT2=="29"~"Tlaxcala",
                              CVE_ENT2=="30"~"Veracruz de Ignacio de la Llave",
                              CVE_ENT2=="31"~"Yucatán",
                              CVE_ENT2=="32"~"Zacatecas"))

ENIGH$NOM_ENT <- ENIGH$CVE_ENT

CVE_ENT <- unique(ENIGH$CVE_ENT)
Ent <-unique(ENIGH$CVE_ENT2)

for (i in 1:32) {
  ENIGH$NOM_ENT[ENIGH$NOM_ENT==CVE_ENT[i]] <- Ent[i]
}

# Exportación de avances de la clase 1 (Solo base de datos)-----

write.csv(ENIGH, file = "Bases/ENIGH/ENIGH_SciData.csv",
          row.names = FALSE, fileEncoding = "UTF-8")


# Tarea 1 -----

# a) Sustraer de ubica_geo la clave de entidad (primeros dos digitos)
# b) Recodificar la variable "Educa_jefe" con base en el descriptor de archivos


#====================================================================================================#

# Inicio de sesión 2 

# Repaso y solución de la sesión 1 -----

ENIGH <- read.csv("Bases/ENIGH/ENIGH_SciData.csv")

ENIGH$CVE_ENT <- substr(ENIGH$folioviv, 1,2)

nchar("100013605")

nchar("1703015424")

## Convertir cadena númerica en texto ----

ENIGH$folioviv <- as.character(ENIGH$folioviv)
ENIGH$folioviv <- as.numeric(ENIGH$folioviv)

### Ifelse para aplicar una condición y combinar con funciones -----

#### Con cadena de texto -----

ENIGH$folioviv <- ifelse(nchar(ENIGH$folioviv)==9,
                         paste0("0", ENIGH$folioviv),
                         paste(ENIGH$folioviv))

ENIGH$CVE_ENT <- substr(ENIGH$folioviv, 1,2)


### Sin cadena de texto ----

ENIGH$CVE_ENT <- ifelse(nchar(ENIGH$folioviv)==9,
                        substr(ENIGH$folioviv, 1,1),
                        substr(ENIGH$folioviv, 1,2))

head(ENIGH$CVE_ENT)
tail(ENIGH$CVE_ENT)

# Filtros ------

## Método 1: Usando sintaxis de R -----

### Mayor o igual a un número -----

ENIGH_15ymas <- ENIGH[ENIGH$edad_jefe >= 15,]

### Menor o igual a un número -----

ENIGH_menoresa18 <- ENIGH[ENIGH$edad_jefe <= 18,]

### Igual a una característica -----

ENIGH_Aguascalientes <- ENIGH[ENIGH$NOM_ENT=="Aguascalientes",]

### Entre varios valores ----

ENIGH_15_65 <- ENIGH[ENIGH$edad_jefe >= 15 & ENIGH$edad_jefe <= 65,]
table(ENIGH_15_65$edad_jefe)

### Entre varios elementos ----

ENIGH_AMY <- ENIGH[ENIGH$NOM_ENT %in% c("Aguascalientes", "México", "Yucatán"),]

### Con varias columas -----

#### Con & ----

ENIGH_Aguascalientes_25 <- ENIGH[ENIGH$edad_jefe >= 25 & ENIGH$NOM_ENT=="Aguascalientes",]

#### Con | -----

ENIGH_Aguascalientes_25 <- ENIGH[ENIGH$NOM_ENT=="Aguascalientes" | ENIGH$edad_jefe >= 25,]

##### Ordenar después del filtro -----

ENIGH_Aguascalientes_25 <- ENIGH_Aguascalientes_25[order(ENIGH_Aguascalientes_25$edad_jefe, decreasing = F),]
head(ENIGH_Aguascalientes_25)

## Filtros con dplyr y la función filter -----

### Mayor o igual a un número -----

ENIGH_15ymas <- ENIGH %>% 
  filter(edad_jefe >= 15)

### Menor o igual a un número -----

ENIGH_menoresa18 <- ENIGH %>% 
  filter(edad_jefe <= 18)

### Igual a una característica -----

ENIGH_Aguascalientes <- ENIGH %>% 
  filter(NOM_ENT == "Aguascalientes")

### Entre varios valores ----

ENIGH_15_65 <- ENIGH %>% 
  filter(edad_jefe >= 15 & edad_jefe <= 65)

### Entre varios elementos ----

ENIGH_AMY <- ENIGH %>% 
  filter(NOM_ENT %in% c("Aguascalientes", "México", "Yucatán"))

### Con varias columas -----

#### Con & ----

ENIGH_Aguascalientes_25 <- ENIGH %>% 
  filter(edad_jefe >= 25 & NOM_ENT == "Aguascalientes")

#### Con | -----

ENIGH_Aguascalientes_25 <-  ENIGH %>% 
  filter(edad_jefe >= 25 | NOM_ENT == "Aguascalientes")


# Agrupamientos con tables y resumen -----

## Conteo de frecuencias ----

Viv_freq <- ENIGH %>% 
  group_by(NOM_ENT) %>% 
  count() %>% 
  ungroup() %>% 
  as.data.frame()

## Frecuencias usando un factor de expansión ----

Viv_n_pob <- ENIGH %>% 
  group_by(NOM_ENT) %>% 
  summarise("Total" = sum(factor))%>% 
  as.data.frame()

### Añadiendo el total como una fila más ----

#### Método 1 ----

Viv_n_pob <- rbind(Viv_n_pob,
                   data.frame(
                     "NOM_ENT" = "Total",
                     "Total" = sum(Viv_n_pob$Total)
                   ))

#### Método 2 ----

Viv_n_pob <- Viv_n_pob %>% 
  bind_rows(summarise(.,across(where(is.numeric), sum),
                      across(where(is.character), ~"Total")))

## Agrupamiento por dos elementos -----

Sexo <- c("Mujer", "Hombre")
Sexo_binario <- unique(ENIGH$sexo_jefe)

for (i in 1:2) {
  ENIGH$sexo_jefe[ENIGH$sexo_jefe==Sexo_binario[i]] <- Sexo[i]
}


### Jefes de familia en las viviendas por sexo ----

Viv_sex <- ENIGH %>% 
  group_by(NOM_ENT, sexo_jefe) %>% 
  summarise(Total = sum(factor))


# Tarea 2 -----


# A) Realice filtros utilizando la decodificación del nivel de estuidos y la edad 
# B) Realizar tablas agrupandolas con la variables decodificada del nivel de estudios 


# Ejemplo de lo que veremos en tema 2 ----

Viv_n_pob[-33,] %>% 
  ggplot(aes(x=NOM_ENT, y=Total))+
  geom_col()


ENIGH %>% 
  group_by(NOM_ENT) %>% 
  count() %>% 
  ungroup() %>% 
  as.data.frame() %>% 
  ggplot(aes(x=NOM_ENT, y=n))+
  geom_col()




