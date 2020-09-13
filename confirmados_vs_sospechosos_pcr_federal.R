#PCR Federal
library("dplyr")
library("tidyr")
library(ggplot2)
library(scales) 
library(stringr)
#función para quitar la comas en los datos númericos
comas<-function(x)
{
numero<-matrix(0,length(x),1)
for(i in 1:length(x))
{
	if(x[i]=='0')
	{
		numero[i]<-0
	}
	else
	{
		texto<-str_split(x[i],",")
		if(length(texto[[1]])==1)
		{
			numero[i]<-texto[[1]]			

		}
		else
		{
			numero[i]<-str_c(texto[[1]][1],texto[[1]][2])
		}
	}
}

x<-numero
}

#importo los datos como un dataset
datos<-as.data.frame(read.csv('DA_Radar_Pruebas.csv',header=T, sep=','))
datos<-datos[1:432,]
datos2<-rename(datos,fecha=ï..Fecha,UG=U.de.G, serologica =SerolÃ³gicas)

glimpse(datos2)

levels(datos2$serologica) <- c(levels(datos2$serologica), '0')
levels(datos2$Federal) <- c(levels(datos2$Federal), '0')
levels(datos2$UG) <- c(levels(datos2$UG), '0')
levels(datos2$Privado) <- c(levels(datos2$Privado), '0')

datos3<-datos2
datos2$Federal[datos2$Federal=="Na"]<-0
datos2$serologica[datos2$serologica=="Na"]<-0
datos2$UG[datos2$UG=="Na"]<-0
datos2$Privado[datos2$Privado=="Na"]<-0


########################################################################################################################
####En esta parte aún me falta crear la función para generalizar la automatización de la lectura de los datos con coma##
##quitar las comas en los campos de caracter                                                                          ##
########################################################################################################################


datos2$serologica<-comas(datos2$serologica)

datos2$Federal<-comas(datos2$Federal)

datos2$UG<-comas(datos2$UG)

datos2$Privado<-comas(datos2$Privado)




#########################

glimpse(datos2)

#CASOS CONFIRMADOS

pcr<- datos2 %>% filter(Resultado=='Confirmados') 

##en esta parte voy a sumar toda la parte de las muestras pcr
#Tengo que hacer dos cosas sumas todos los positivos por día y 
#posteriormente hacer la suma de confirmados, descartados y sospechosos, para el final hacer 
#la tasa de confirmación y/o positividad

#

#suma de cada fecha para positivos PCR
total_positivos<- mutate(pcr, suma_positivos=as.numeric(pcr$Privado)+as.numeric(pcr$Federal)+as.numeric(pcr$UG)) 

#actualizo la fecha para ese dataframe
u<-as.character(total_positivos$fecha)

dias<-as.Date(u,format="%d/%m/%Y")

#se grafican por fecha
ggplot(data = total_positivos) +
  geom_point(aes(x =dias , y =as.numeric(suma_positivos),colour='Confirmados',shape='Confirmados'),color = 'blue')+geom_line(aes(x =dias , y =as.numeric(suma_positivos),colour='Confirmados'),color = 'black', size = 1)+xlab('Fecha reportada') + 
   scale_colour_manual("",breaks = c('Confirmados'),values = c('black'))+
  ylab('Número de casos') +  labs(title = "Pruebas PCR confirmadas (Federal, UG y Privado) al 03/09/2020")+scale_x_date(labels = date_format("%d/%m/%Y"))+
theme_bw()
