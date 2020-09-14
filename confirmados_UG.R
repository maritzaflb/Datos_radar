#Confirmación de PCR para federal,UG y privado contra sospechosos de los mismos laboratorios
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

datos2$serologica<-comas(datos2$serologica)

datos2$Federal<-comas(datos2$Federal)

datos2$UG<-comas(datos2$UG)

datos2$Privado<-comas(datos2$Privado)


glimpse(datos2)

#CASOS CONFIRMADOS y sospechosos

pcr<- datos2 %>% filter(Resultado=='Confirmados') 
pcr_s<- datos2 %>% filter(Resultado=='Sospechosos') 

pcr_cs<-pcr %>% mutate(sospechosos=pcr_s$Federal,confirmados=pcr$Federal)

pcr_cs<-pcr_cs[1:(dim(pcr_cs)[1]-14),]
u<-as.character(pcr_cs$fecha)

dias<-as.Date(u,format="%d/%m/%Y")

#se grafican por fecha
ggplot(data =pcr_cs) +
  geom_point(aes(x =dias , y =as.numeric(Federal),colour='Confirmados',shape='Confirmados'),color = 'black')+geom_line(aes(x =dias , y =as.numeric(Federal),colour='Confirmados'),color = 'blue', size = 1)+xlab('Fecha reportada') + 
 geom_point(aes(x =dias , y =as.numeric(sospechosos),colour='Confirmados',shape='Sospechosos'),color = 'black')+geom_line(aes(x =dias , y =as.numeric(sospechosos),colour='Sospechosos'),color = 'red', size = 1)+xlab('Fecha reportada') + 
   scale_colour_manual("",breaks = c('Confirmados', 'Sospechosos'),values = c('blue','red'))+
  ylab('Número de casos') +  labs(title = "Pruebas PCR Federal confirmadas contra sospechosas al 07/09/2020")+scale_x_date(labels = date_format("%d/%m/%Y"))+
theme_bw()
