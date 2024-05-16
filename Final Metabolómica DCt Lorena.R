#Creador: Lorena Cuevas
# Exámen Final Metabólomica 

#Codigo inical para todos los documetos :)
install.packages("pacman") #Instala los paquetes de datos y formula #spara que lo lea como texto 


library(pacman) #ejecuta los paquetes instalados previamente 

p_load("readr", #pload carga la instruccion de reader que llama a la base de datos 
       "dplyr")  #dplyr facilita el manejo de datos

#flecha nombra las variables
#Nombrar con flecha la base de datos que voy a utilizar cuidar ortografia y usar puras minusculas
#read con terminacion csv es para leer un archivo de excel, file con URL entre comillas
datos<- read.csv(file = "https://raw.githubusercontent.com/ManuelLaraMVZ/Transcript-mica/main/examen2")

#head para ver datos en la consola
head(datos) 

#extracción de genes contoles (referencia)

Controles <- datos%>% #ordenar tablas en filas y columnas
  filter(Condicion=="Control") #filtrar en base a una condicion 

head(Controles)

#sacar promedios 

promedios_controles  <-Controles%>%
  summarise(Mean_C1=mean(Cx1),
            Mean_C2=mean(Cx2),
            Mean_C3=mean(Cx3),
            Mean_T1=mean(T1),
            Mean_T2=mean(T2),
            Mean_T3=mean(T3)) %>%
  mutate(Gen="Promedio_controles") %>% #para generar la columna 
  select(7,1,2,3,4,5,6) #para pasarlo al inicio

promedios_controles

#####################################################
#extraer los genes de la tabla "datos"

genes <- datos%>%
  filter (Condicion=="Target") %>% 
  select(-2)
head(genes)
####################################################
#Sacar el 2^`-DCT

DCT <- genes %>% 
  mutate(DCT_C1=2^-(Cx1-promedios_controles$Mean_C1),
         DCT_C2=2^-(Cx2-promedios_controles$Mean_C2),
         DCT_C3=2^-(Cx3-promedios_controles$Mean_C3),
         DCT_T1=2^-(T1-promedios_controles$Mean_T1),
         DCT_T2=2^-(T2-promedios_controles$Mean_T2),
         DCT_T3=2^-(T3-promedios_controles$Mean_T3),) %>% 
  select(-2,-3,-4,-5,-6,-7)
DCT
