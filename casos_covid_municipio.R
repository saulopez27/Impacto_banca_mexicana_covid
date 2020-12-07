#Librerias para trabajar la informiacón de covid
library(tidyverse)
library(dplyr)

#Asegurar que la ruta este deacuerdo a donde se tienen sus archivos
setwd("C:/Users/Saulo/Documents/BEDU/Impacto Banca Mexicana")
#Fuente https://www.gob.mx/salud/documentos/datos-abiertos-152127


#lecturar de archivos por covid
covid_casos <- read.table("200903COVID19MEXICO.csv",header=TRUE,sep = ",")
covid_casos$FECHA_SINTOMAS <- as.Date(covid_casos$FECHA_SINTOMAS, format='%Y-%m-%d')
head(covid_casos)



#cambiar por fecha de consulta en redtable
covid_casos$FECHA_14D <- as.Date("2020-09-04", format='%Y-%m-%d')
covid_casos <- covid_casos %>% mutate(confirmados=ifelse(RESULTADO==1,1,0),
                                      negativos=ifelse(RESULTADO==2,1,0),
                                      sospechoso=ifelse(RESULTADO==3,1,0),
                                      defuncion=ifelse(FECHA_DEF!="9999-99-99" & RESULTADO==1,1,0),
                                      activos  =ifelse(RESULTADO==1 & ((FECHA_14D-FECHA_SINTOMAS)<14),1,0),
                                      cve_munedo=as.numeric(paste(ENTIDAD_RES,str_sub(paste("000",MUNICIPIO_RES,sep=""),-3,-1),sep="")))

#Resumen para validar con info publicada en https://coronavirus.gob.mx/datos/  
covid_casos %>% summarise(confirmados=sum(confirmados,na.rm = TRUE),
                          negativos=sum(negativos,na.rm = TRUE),
                          sospechoso=sum(sospechoso,na.rm = TRUE),
                          defuncion=sum(defuncion,na.rm = TRUE),
                          activos=sum(activos,na.rm = TRUE))

#cambiar por fecha de consulta en redtable
covid_casos <- covid_casos %>% mutate(Periodo=ifelse(FECHA_INGRESO<=as.Date("2019-12-31", format='%Y-%m-%d'),201912,
                                              ifelse(FECHA_INGRESO<=as.Date("2020-01-31", format='%Y-%m-%d'),202001,
                                              ifelse(FECHA_INGRESO<=as.Date("2020-02-29", format='%Y-%m-%d'),202002,
                                              ifelse(FECHA_INGRESO<=as.Date("2020-03-31", format='%Y-%m-%d'),202003,
                                              ifelse(FECHA_INGRESO<=as.Date("2020-04-30", format='%Y-%m-%d'),202004,
                                              ifelse(FECHA_INGRESO<=as.Date("2020-05-31", format='%Y-%m-%d'),202005,
                                              ifelse(FECHA_INGRESO<=as.Date("2020-06-30", format='%Y-%m-%d'),202006,
                                              ifelse(FECHA_INGRESO<=as.Date("2020-07-31", format='%Y-%m-%d'),202007,
                                              ifelse(FECHA_INGRESO<=as.Date("2020-08-31", format='%Y-%m-%d'),202008,
                                              ifelse(FECHA_INGRESO<=as.Date("2020-09-30", format='%Y-%m-%d'),202009
                                              )))))))))),
                                      )

#cambiar por fecha para saber los casos activos en el momento del reporte
covid_casos <- covid_casos %>% mutate(
Activos_periodo = ifelse(RESULTADO==1 & Periodo==201912 & ((as.Date("2019-12-31", format='%Y-%m-%d')-FECHA_SINTOMAS)<14),1,
                  ifelse(RESULTADO==1 & Periodo==202001 & ((as.Date("2020-01-31", format='%Y-%m-%d')-FECHA_SINTOMAS)<14),1,
                  ifelse(RESULTADO==1 & Periodo==202002 & ((as.Date("2020-02-29", format='%Y-%m-%d')-FECHA_SINTOMAS)<14),1,
                  ifelse(RESULTADO==1 & Periodo==202003 &((as.Date("2020-03-31", format='%Y-%m-%d')-FECHA_SINTOMAS)<14),1,
                  ifelse(RESULTADO==1 & Periodo==202004 &((as.Date("2020-04-30", format='%Y-%m-%d')-FECHA_SINTOMAS)<14),1,
                  ifelse(RESULTADO==1 & Periodo==202005 &((as.Date("2020-05-31", format='%Y-%m-%d')-FECHA_SINTOMAS)<14),1,
                  ifelse(RESULTADO==1 & Periodo==202006 &((as.Date("2020-06-30", format='%Y-%m-%d')-FECHA_SINTOMAS)<14),1,
                  ifelse(RESULTADO==1 & Periodo==202007 &((as.Date("2020-07-31", format='%Y-%m-%d')-FECHA_SINTOMAS)<14),1,
                  ifelse(RESULTADO==1 & Periodo==202008 &((as.Date("2020-08-31", format='%Y-%m-%d')-FECHA_SINTOMAS)<14),1,
                  ifelse(RESULTADO==1 & Periodo==202009 &((as.Date("2020-09-04", format='%Y-%m-%d')-FECHA_SINTOMAS)<14),1,0
                  )))))))))),
                 )

#Data frame final para generar la información
head(covid_casos_mun)
covid_casos_mun <- covid_casos %>% group_by(cve_munedo,Periodo) %>%  summarise(confirmados=sum(confirmados,na.rm = TRUE),
                                                                       negativos=sum(negativos,na.rm = TRUE),
                                                                       sospechoso=sum(sospechoso,na.rm = TRUE),
                                                                       defuncion=sum(defuncion,na.rm = TRUE),
                                                                       activos=sum(activos,na.rm = TRUE),
                                                                       Activos_periodo=sum(Activos_periodo,na.rm = TRUE)
                                                                       )
#Generación de tablas de casos de Covid
write.csv(covid_casos_mun,"20200904_COVID_MUNICIPAL.csv",row.names = FALSE)















