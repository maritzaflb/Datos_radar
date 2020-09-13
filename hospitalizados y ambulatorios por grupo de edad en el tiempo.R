library("dplyr")
library("tidyr")
library(ggplot2)
library(scales) 
library(stringr)


#Datos importados como dataframe
casos<-as.data.frame(read.csv('DA_Radar_Casos.csv',header=T, sep=','))

#TIPOPRUEBA

casos<-rename(casos, fecha=ï..FEC_INI_SIN, edad=EDAD, sexo=SEXO, paciente=TIPO_PACIENTE, resultado=RESULTADO_FINAL,municipio=CVE_MUN_RES,laboratorio=LABORATORIO, prueba=TIPOPRUEBA)
caso<-casos[1:(dim(casos)[1])-1,]
#ORDENO POR FECHA
#total de casos confirmados entre ambulatorios y hospitalizados 
confirmado<-caso %>% filter(resultado=='CONFIRMADO')

#contar cuántos de los pcr fueron ambulatorios y cuántos de serologicos

#ambulatorios
pcr_a<-confirmado %>% filter(paciente=='AMBULATORIO') 

pcr_aa<-pcr_a%>%select(fecha,edad)


pcra<-pcr_aa %>% arrange(fecha)



fecha_a<-pcra%>%  
dplyr::group_by(fecha)%>%
dplyr::summarise(fec=n()) 


array_fecha<-fecha_a$fecha #aqui tengo las fechas

g_g<-NULL
g_j<-NULL
g_a<-NULL
#aqui voy a meter un for para correr el arreglo
for(j in 1:length(array_fecha))
{
	conjunto<-pcra%>% filter(fecha==array_fecha[j])
	#aqui voy a contar cuántos hay de cada edad en esas fechas
	fechac<-conjunto%>%  
	dplyr::group_by(edad) %>%  
	dplyr::summarise(edad_n=n())

	joven<-0
	grande<-0
	adol<-0

	for(i in 1:length(fechac$edad))
	{
		if(as.numeric(fechac$edad[i])>=60)
		{
			grande<-grande+as.numeric(fechac$edad_n[i])
		}
		if(as.numeric(fechac$edad[i])<60 & as.numeric(fechac$edad[i])>=30)
		{
			joven<-joven+as.numeric(fechac$edad_n[i])
		}
		if(as.numeric(fechac$edad[i])<30 & as.numeric(fechac$edad[i])>=0)
		{
			adol<-adol+as.numeric(fechac$edad_n[i])
		}

	}

g_g<-rbind(g_g,grande)
g_j<-rbind(g_j,joven)
g_a<-rbind(g_a,adol)

}

g<-as.vector(g_g)
j<-as.vector(g_j)
a<-as.vector(g_a)



dato_fecha<-fecha_a%>% mutate(grupo_g=g,grupo_j=j,grupo_a=a)

dato_fechaa<-dato_fecha[1:(dim(dato_fecha)[1]-14),]

dia<-as.character(dato_fechaa$fecha)
dias_t<-as.Date(dia,format="%d/%m/%Y")

##############################################################################
win.graph()
ggplot(data= dato_fechaa) +
 geom_point(aes(x =dias_t , y =as.numeric(grupo_g),colour='Mayores de 60',shape='Mayores de 60'),color = 'blue')+ geom_line(aes(x =dias_t , y =as.numeric(grupo_g)),color = 'blue')  + xlab('Fecha de registro')+
 geom_point(aes(x =dias_t , y =as.numeric(grupo_j),colour='De 59 a 30',shape='De 59 a 30'),color = 'red')+ geom_line(aes(x =dias_t , y =as.numeric(grupo_j)),color = 'red')  + xlab('Fecha de registro')+
 geom_point(aes(x =dias_t , y =as.numeric(grupo_a),colour='De 29 a 0',shape='De 29 a 0'),color = 'black')+ geom_line(aes(x =dias_t , y =as.numeric(grupo_a)),color = 'black')  + xlab('Fecha de registro')+
  ylab('Número de casos') +  labs(title = "Ambulatorios confirmados de covid-19 por grupos de edad en Jalisco (07/09/2020)")+
theme_bw()


############################################################################
#hospitalizados

#total de casos confirmados entre ambulatorios y hospitalizados 
confirmado_h<-caso %>% filter(resultado=='CONFIRMADO')

#contar cuántos de los pcr fueron ambulatorios y cuántos de serologicos

#ambulatorios
pcr_h<-confirmado_h %>% filter(paciente=='HOSPITALIZADO') 

pcr_hh<-pcr_h%>%select(fecha,edad)


pcrh<-pcr_hh %>% arrange(fecha)



fecha_h<-pcrh%>%  
dplyr::group_by(fecha)%>%
dplyr::summarise(fech=n()) 


array_fecha_h<-fecha_h$fecha #aqui tengo las fechas

g_g_h<-NULL
g_j_h<-NULL
g_a_h<-NULL
#aqui voy a meter un for para correr el arreglo
for(j in 1:length(array_fecha_h))
{
	conjunto_h<-pcrh%>% filter(fecha==array_fecha_h[j])
	#aqui voy a contar cuántos hay de cada edad en esas fechas
	fec<-conjunto_h%>%  
	dplyr::group_by(edad) %>%  
	dplyr::summarise(edad_n_h=n())

	joven_h<-0
	grande_h<-0
	adol_h<-0
	for(i in 1:length(fec$edad))
	{
		if(as.numeric(fec$edad[i])>=60)
		{
			grande_h<-grande_h+as.numeric(fec$edad_n_h[i])
		}
		if(as.numeric(fec$edad[i])<60 & as.numeric(fec$edad[i])>=30)

		{
			joven_h<-joven_h+as.numeric(fec$edad_n_h[i])
		}
		if(as.numeric(fec$edad[i])<30 & as.numeric(fec$edad[i])>=0)
		{
			adol_h<-adol_h+as.numeric(fec$edad_n_h[i])
		}
	}

g_g_h<-rbind(g_g_h,grande_h)
g_j_h<-rbind(g_j_h,joven_h)
g_a_h<-rbind(g_a_h,adol_h)

}

g_h<-as.vector(g_g_h)
j_h<-as.vector(g_j_h)
a_h<-as.vector(g_a_h)


dato_fecha_h<-fecha_h%>% mutate(grupo_g_h=g_h,grupo_j_h=j_h,grupo_a_h=a_h)

dato_fecha_h<-dato_fecha_h[1:(dim(dato_fecha_h)[1]-14),]
dia_h<-as.character(dato_fecha_h$fecha)
dias_t_h<-as.Date(dia_h,format="%d/%m/%Y")


##############################################################################
win.graph()
ggplot(data= dato_fecha_h) +
 geom_point(aes(x =dias_t_h , y =as.numeric(grupo_g_h),colour='Mayores de 60',shape='Mayores de 60'),color = 'blue')+ geom_line(aes(x =dias_t_h , y =as.numeric(grupo_g_h)),color = 'blue')  + xlab('Fecha de registro')+
 geom_point(aes(x =dias_t_h , y =as.numeric(grupo_j_h),colour='De 59 a 30',shape='De 59 a 30'),color = 'red')+ geom_line(aes(x =dias_t_h , y =as.numeric(grupo_j_h)),color = 'red')  + xlab('Fecha de registro')+
 geom_point(aes(x =dias_t_h , y =as.numeric(grupo_a_h),colour='De 29 a 0',shape='De 29 a 0'),color = 'black')+ geom_line(aes(x =dias_t_h , y =as.numeric(grupo_a_h)),color = 'black')  + xlab('Fecha de registro')+
  ylab('Número de casos') +  labs(title = "Hospitalizados confirmados de covid-19 por grupos de edad en Jalisco (07/09/2020)")+
theme_bw()


