library("dplyr")
library("tidyr")
library(ggplot2)
library(scales) 
library(stringr)




#Datos importados como dataframe
casos<-as.data.frame(read.csv('DA_Radar_Casos.csv',header=T, sep=','))
#datos en el dataframe
#FEC_INI_SIN
#EDAD
#SEXO
#TIPO_PACIENTE
#RESULTADO_FINAL
#CVE_MUN_RES
#LABORATORIO
#TIPOPRUEBA

casos<-rename(casos, fecha=ï..FEC_INI_SIN, edad=EDAD, sexo=SEXO, paciente=TIPO_PACIENTE, resultado=RESULTADO_FINAL,municipio=CVE_MUN_RES,laboratorio=LABORATORIO, prueba=TIPOPRUEBA)
caso<-casos[1:(dim(casos)[1])-1,]
#ORDENO POR FECHA
por_fecha<-caso %>% arrange(fecha)
#EN ESTA PARTE VOY A TOMAR SOLO A LOS CONFIRMADOS

#total de casos confirmados entre ambulatorios y hospitalizados 
confirmado<-por_fecha %>% filter(resultado=='CONFIRMADO')

#contar cuántos de los pcr fueron ambulatorios y cuántos de serologicos

pcr_hospitalizado<-confirmado %>% filter(paciente=='HOSPITALIZADO' & prueba=='PCR') 


deteccion_h<-pcr_hospitalizado%>%  
dplyr::group_by(fecha) %>%  
dplyr::summarise(casos=n()) 

s<-s[1:(dim(s_h)[1]-14),]
s_h<-as.data.frame(deteccion_h)

dia<-as.character(s_h$fecha)
dias_t<-as.Date(dia,format="%d/%m/%Y")



#AQUI SE GRAFICAN LOS HOSPITALIZADOS POR PCR SOLAMENTE PORQUE NO HAY DE SEROLÓGICOS
win.graph()
ggplot(data= s_h,aes(x =dias_t , y =as.numeric(casos))) +
 geom_point(color = 'blue')+ geom_line(color = 'blue')  + xlab('Fecha de inicio de síntomas')+
  ylab('Número de casos') +  labs(title = "Hospitalizados confirmados al 07/09/2020")+scale_x_date(labels = date_format("%d/%m/%Y"))+
theme_bw()


#en estos datos los serologicos solo estan registrados como ambulatorios y no hay hospitalizados
serologico_hospitalizado<-confirmado %>% filter(paciente=='HOSPITALIZADO' & prueba=='SEROLOGICA')



deteccion_s<-serologico_hospitalizado%>%  
dplyr::group_by(fecha) %>%  
dplyr::summarise(casos_s=n()) 
s_s<-as.data.frame(deteccion_s)

dia<-as.character(s_s$fecha)
dias_t<-as.Date(dia,format="%d/%m/%Y")

win.graph()
ggplot(data= s_s,aes(x =dias_t , y =as.numeric(casos_s))) +
 geom_point(color = 'blue')+ geom_line(color = 'blue')  + xlab('Fecha de inicio de síntomas')+
  ylab('Número de casos') +  labs(title = "Ambulatorios por detección de prueba serológica al 03/09/2020")+scale_x_date(labels = date_format("%d/%m/%Y"))+
theme_bw()

#tabla<-s %>% mutate(s_s,sero=s_s$casos_s)
#GRAFICA DE HOSPITALIZADOS CON PCR VS HOSPITALIZADOS CON SEROLOGIA

#win.graph()
#ggplot(data= s_s,aes(x =dias_t , y =as.numeric(casos_s))) +
# geom_point(color = 'blue')+ geom_line(color = 'blue')  + xlab('Fecha de inicio de síntomas')+
#  ylab('Número de casos') +  labs(title = "Ambulatorios por detección de prueba serológica al 03/09/2020")+scale_x_date(labels = date_format("%d/%m/%Y"))+
#theme_bw()


#confirmados que se hospitalizaron de pcr y serologicos

confirmado<-por_fecha %>% filter(resultado=='CONFIRMADO' & paciente=='AMBULATORIO')


deteccion<-confirmado%>%  
dplyr::group_by(fecha) %>%  
dplyr::summarise(casos=n()) 
s<-as.data.frame(deteccion)

s<-s[1:(dim(s)[1]-14),]
dia<-as.character(s$fecha)
dias_t<-as.Date(dia,format="%d/%m/%Y")

win.graph()
ggplot(data= s,aes(x =dias_t , y =as.numeric(casos))) +
 geom_point(color = 'blue')+ geom_line(color = 'blue')  + xlab('Fecha de inicio de síntomas')+
  ylab('Número de casos') +  labs(title = "AMBULATORIO con prueba positiva por PCR y serológica al 07/09/2020")+scale_x_date(labels = date_format("%d/%m/%Y"))+
theme_bw()



