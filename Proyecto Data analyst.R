#Carga de librerias de R para hacer la graficas de mapas
library(ggplot2)
library(dplyr)
library(tidyverse)
library(geojsonio)
library(broom)
library(sf)
#install.packages("installr", dependencies = TRUE)

#Carga de archivos de mapas y de bases de captación y cartera por municipio 
#Asegurar de cambiar la ruta en donde se tengan los archivos

setwd("C:/Users/Saulo/Documents/BEDU/Impacto Banca Mexicana")

banca_mun <- read.table("Banca_mun.csv",header=TRUE,sep = ",")
head(banca_mun)

banca_est <- read.table("Banca_est.csv",header=TRUE,sep = ",")
head(banca_est)

map_mun <- st_read("areas_geoestadisticas_municipales.shp")
head(map_mun)

map_est <- st_read("areas_geoestadisticas_estatales.shp")
head(map_est)

#Data frame por municipios
map_mun$CVE_ENT <- as.numeric(map_mun$CVE_ENT)
map_mun$CVE_MUN <- as.numeric(map_mun$CVE_MUN)
map_mun <- map_mun %>% mutate(clave = map_mun$CVE_ENT*1000+map_mun$CVE_MUN)
map_mun_covid <- map_mun %>%left_join(banca_mun,by = c("clave" = "cve_municipio"))
map_mun_covid[is.na(map_mun_covid)]<-0
head(map_mun_covid)
summary(map_mun_covid)


#Data frame de mapas por estado
map_est$CVE_ENT <- as.numeric(map_est$CVE_ENT)
map_est_covid <- map_est %>%left_join(banca_est,by = c("CVE_ENT" = "cve_estado"))
map_est_covid[is.na(map_mun_covid)]<-0
head(map_est_covid)
summary(map_est_covid)




#Crecimeinto de cartera
brks <- c(-2000,-3.7,0,0.001,max(map_mun_covid$CrecCartera2020, na.rm = TRUE))
map_mun_covid%>%
  mutate(cartera_cut = cut(CrecCartera2020, breaks = brks, right = FALSE)) %>%
  ggplot(aes(fill = cartera_cut)) +
  geom_sf(colour = "grey75", size = 0.01)+
  labs(title = "Crecimiento de cartera 2020",
       subtitle = "En miles de millones")+
  scale_fill_manual("Crec Cartera",
                    values = c("red3", "orange", "yellow", "green4"))


#Zoom de crecimiento de cartera  
map_mun_covid_z <- map_mun_covid %>% 
    filter(CVE_ENT == 9 | CVE_ENT == 15 | CVE_ENT == 21 | CVE_ENT == 17 | CVE_ENT == 29)
map_mun_covid_z%>%
  mutate(cartera_cut = cut(CrecCartera2020, breaks = brks, right = FALSE)) %>%
  ggplot(aes(fill =  cartera_cut )) +
  geom_sf(colour = "grey75", size = 0.01)+
  labs(title = "Crecimiento de cartera 2020",
       subtitle = "En miles de millones")+
  scale_fill_manual("Crec Cartera",
                    values = c("red3", "orange", "yellow", "green4"))


  #Crecimeinto de captación
  brks <- c(-2000,0,0.06,max(map_mun_covid$CrecCaptacion2020, na.rm = TRUE))
  map_mun_covid%>%
    mutate(captacion_cut = cut(CrecCaptacion2020, breaks = brks, right = FALSE)) %>%
    ggplot(aes(fill = captacion_cut)) +
    geom_sf(colour = "grey75", size = 0.01)+
    scale_fill_manual("Crec captación",
                      values = c("red3", "olivedrab3", "green4"))+
    labs(title = "Crecimiento de captación 2020",
         subtitle = "Miles de millones")

  
  #Zoom de crecimeinto de captacion  
  map_mun_covid_z <- map_mun_covid %>% 
    filter(CVE_ENT == 9 | CVE_ENT == 15 | CVE_ENT == 21 | CVE_ENT == 17 | CVE_ENT == 29)
  map_mun_covid_z%>%
    mutate(captacion_cut = cut(CrecCaptacion2020, breaks = brks, right = FALSE)) %>%
    ggplot(aes(fill =  captacion_cut )) +
    geom_sf(colour = "grey75", size = 0.01)+
    labs(title = "Crecimiento de captación 2020",
         subtitle = "En millones")+
    scale_fill_manual("Crec Captación",
                      values = c("red3", "olivedrab3", "green4"))
  
  #casos covid por cada 100 mil habitantes
  brks <- c(0,1,9,24,max(map_mun_covid$Activos_100_mil_hab, na.rm = TRUE))
  map_mun_covid%>%
    mutate(activos_cut = cut(Activos_100_mil_hab, breaks = brks, right = FALSE)) %>%
    ggplot(aes(fill = activos_cut)) +
    geom_sf(colour = "grey75", size = 0.01)+
    scale_fill_manual("Activos_100_mil_hab",
                      values = c("green4", "yellow","orange","red3"))+
    labs(title = "Casos activos de covid ago 2020",
         subtitle = "Por cada 100 mil habitantes")

  #Zoom casos activos covid por cada 100 mil habitantes
  map_mun_covid_z <- map_mun_covid %>% 
    filter(CVE_ENT == 9 | CVE_ENT == 15 | CVE_ENT == 21 | CVE_ENT == 17 | CVE_ENT == 29)
  map_mun_covid_z%>%
    mutate(activos_cut = cut(Activos_100_mil_hab, breaks = brks, right = FALSE)) %>%
    ggplot(aes(fill = activos_cut)) +
    geom_sf(colour = "grey75", size = 0.01)+
    scale_fill_manual("Activos_100_mil_hab",
                      values = c("green4", "yellow","orange","red3"))+
    labs(title = "Casos Activos Covid",
         subtitle = "Por cada 100 mil hab")
  
  
  #Agrupaciones de informaicón de covid
  covid_casos %>% summarise(confirmados=sum(confirmados,na.rm = TRUE),
                            negativos=sum(negativos,na.rm = TRUE),
                            sospechoso=sum(sospechoso,na.rm = TRUE),
                            defuncion=sum(defuncion,na.rm = TRUE),
                            activos=sum(activos,na.rm = TRUE))
  
  
  
  
  #Crecimeinto de cartera
  brks <- c(min(map_est_covid$CrecCartera2020),-804,-40, na.rm = FALSE)
  map_est_covid%>%
    mutate(cartera_cut = cut(CrecCartera2020, breaks = brks, right = FALSE)) %>%
    ggplot(aes(fill = cartera_cut)) +
    geom_sf(colour = "grey30", size = 0.01)+
    labs(title = "Crecimiento de cartera 2020",
         subtitle = "En miles de millones")+
    scale_fill_manual("Crec Cartera",
                      values = c("red3", "orange", "green4"))
  
  #Crecimeinto de captacion por estado
  brks <- c(min(map_est_covid$CrecCaptacion2020),0,20000,max(map_est_covid$CrecCaptacion2020)+1, na.rm = TRUE)
  map_est_covid%>%
    mutate(captacion_cut = cut(CrecCaptacion2020, breaks = brks, right = FALSE)) %>%
    ggplot(aes(fill = captacion_cut)) +
    geom_sf(colour = "grey30", size = 0.01)+
    labs(title = "Crecimiento de captación 2020",
         subtitle = "Miles de millones")+
    scale_fill_manual("Crec Captación",
                      values = c("red3", "olivedrab3", "green4"))
  
  
  
  #Activos de covid en Agosto del 2020 por estado
  brks <- c(0,30,50,max(map_est_covid$Activos_100_mil_hab)+1, na.rm = TRUE)
  map_est_covid%>%
    mutate(Activos_cut = cut(Activos_100_mil_hab, breaks = brks, right = FALSE)) %>%
    ggplot(aes(fill = Activos_cut)) +
    geom_sf(colour = "grey30", size = 0.01)+
    labs(title = "Casos activos de covid ago 2020",
         subtitle = "Por cada 100 mil habitantes")+
    scale_fill_manual("Casos activos de covid",
                      values = c("yellow","orange","red3"))
  

  
  
  
  
  
  
  








