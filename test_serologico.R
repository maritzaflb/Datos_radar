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

#importo los datos como un dataframe
datos<-as.data.frame(read.csv('DA_Radar_Pruebas.csv',header=T, sep=','))
datos<-datos[1:(dim(datos)[1]-1),]
datos2<-rename(datos, fecha=ï..Fecha,UG=U.de.G, serologica =SerolÃ³gicas)

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

library(stringr)


##quitar las comas en los campos de caracter


datos2$serologica<-comas(datos2$serologica)
datos2$Federal<-comas(datos2$Federal)
datos2$UG<-comas(datos2$UG)
datos2$Privado<-comas(datos2$Privado)


#########################

glimpse(datos2)

#CASOS CONFIRMADOS

anticuerpos<- datos2 %>% filter(Resultado=='Confirmados') 

#También filtro descartados
anticuerpos_negativos<- datos2 %>% filter(Resultado=='Descartados') 

#filtro sospechosos
anticuerpos_sospechosos<- datos2 %>% filter(Resultado=='Sospechosos') 

#Cambio el formato de la fecha
u<-as.character(anticuerpos$fecha)

dias<-as.Date(u,format="%d/%m/%Y")

serologicas<-select(anticuerpos,fecha,Resultado,serologica) #confirmados

serologicas2<-select(anticuerpos_negativos,fecha,Resultado,serologica) #negativos

union <- mutate(serologicas, sero_neg =anticuerpos_negativos$serologica) #sospechosos


win.graph()

#se grafican por fecha
ggplot(data = union) +
  geom_point(aes(x =dias , y =as.numeric(serologica),colour='Confirmados',shape='Confirmados'),color = 'blue')+geom_line(aes(x =dias , y =as.numeric(serologica),colour='Confirmados'),color = 'black', size = 1)+ 
 geom_point(aes(x =dias , y =as.numeric(sero_neg),colour='Negativos', shape='Negativos'),color = 'black')+geom_line(aes(x =dias , y =as.numeric(sero_neg),colour='Negativos'),color = 'blue', size = 1)+
  scale_colour_manual("",breaks = c('Confirmados', 'Descartados'),values = c('black','blue'))+
  ylab('Número de casos') +  labs(title = "Pruebas serológicas confirmadas y descartadas al 03/09/2020")+scale_x_date(labels = date_format("%d/%m/%Y"))+
theme_bw()

#win.graph()
#se grafican por fecha
#ggplot(data = serologicas2) +
# geom_point(color = 'slateblue', size = 2, alpha = 0.6)+ geom_smooth(color = 'red')+xlab('Fecha') + 
#  ylab('Resultado por fecha') +  labs(title = "Pruebas serológicas por día")+scale_x_date(labels = date_format("%d/%m/%Y"))+
#theme_bw()




###Tasa serologica

##############################################################################################################################
t_serologica<-select(datos2,fecha,Resultado,serologica)

#suma de casos por fecha
sumas<-xtabs(as.numeric(t_serologica$serologica) ~ as.factor(fecha),t_serologica)
suma<-deno<-matrix(0,(length(sumas)-1),1)
for(i in 1:(length(sumas)-1))
{
suma[i]<-sumas[i]
}

#acumulado

por_dia<-t_serologica %>% filter(Resultado=='Confirmados') %>% arrange(fecha)
for(i in 1:(length(sumas)-1))
{
	
	if(sumas[i]!=0)
	{
		deno[i]<-as.numeric(por_dia$serologica[i])/as.numeric(sumas[i])
	}
	else
	{
		deno[i]<-0
	}
}


#acumulado

#solo los confirmados de las serologicas

por_dia<-t_serologica %>% filter(Resultado=='Confirmados') 	%>% arrange(fecha)
tasa_s<- mutate(por_dia, acumulado = deno) %>% arrange(fecha)

dia<-as.character(por_dia$fecha)
dias_t<-as.Date(dia,format="%d/%m/%Y")

win.graph()
ggplot(data = tasa_s,aes(x =dias_t , y =acumulado )) +
 geom_point(color = 'blue')+ geom_line(color = 'blue')  + 
  ylab('Tasa por fecha') +  labs(title = "Tasa de pruebas serológicas por fecha de reporte al 07/09/2020")+scale_x_date(labels = date_format("%d/%m/%Y"))+
theme_bw()


