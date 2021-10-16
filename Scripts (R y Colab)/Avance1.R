#Cargando el paquete Survival
library(survival)

#cargando el conjunto de datos
library(readr)
urlfile<-"https://raw.githubusercontent.com/ElmerZelaya/MM-700-Seminario-de-Investigacion/main/DATA.csv"
data<-read.csv(urlfile)

#mostrando la data
head(data)

#recodificando las variables categoricas con la funcion factor():

data$actividad<-factor(data$actividad,levels=c("1","2","3","4","5"),
                       labels=c("actividades prof","alojamiento","comercio","comida","transporte"))
data$financiamiento<-factor(data$financiamiento,levels=c("1","2","3","4"),
                            labels=c("Banco","Cooperativa","Inst. no bancaria","familiar"))
data$tamano<-factor(data$tamano,levels=c("1","2"),
                    labels=c("micro","pequena"))
head(data)


# ~ ~ ~ ~ Analisis descriptivo de los datos ~ ~ ~ ~ #

#Comenzaremos mostrando el numero de empresas por grupo en sus factores.

######   Actividades Economicas    #####

# frecuencias absoluta
table(data$actividad) 
# Frecuencia relativas
prop.table(table(data$actividad))
#grafico de barra
barplot(prop.table(table(data$actividad)),col="deepskyblue4",ylim = c(0, 0.40),space = 0.1,
        xlab="Actividades economicas", ylab="Frecuencia relativa", 
        main="Actividades Economicas") 

######   Tipo de financiamiento   #####

# frecuencias absoluta
table(data$financiamiento)
# Frecuencia relativas
prop.table(table(data$financiamiento))
#Gráfica de frecuencia relativa
barplot(prop.table(table(data$financiamiento)), ylim = c(0, 0.35),col="deepskyblue4",space =
          0.1,xlab="Tipo de Financiamiento", ylab="Frecuencia relativa", 
        main="Tipo de Financiamiento")

######   tamano de la empresa   ######

# frecuencias absoluta
table(data$tamano)
# Frecuencia relativas
prop.table(table(data$tamano))
#Gráfica de frecuencia relativa
barplot(prop.table(table(data$tamano)),col="deepskyblue4",space = 0.1,ylim = c(0, 0.80),xlab
        ="Tamaño de la Empresa", ylab="Frecuencia relativa", 
        main="Tamano de la empresa")

# creando una base de datos temporal 
#(se utilizara para fines de una mejor comprensión de las gráficas y tablas)  

data_temp<-data                                                                 
data_temp$estado<-factor(data_temp$estado,levels=c("0","1"),                    
                         labels=c("censura","observación"))                     

# analisis descriptivo de las censuras y observaciones (0 indica censura, 1 indica observacion).

# En toda la muestra:

#frecuencia absoluta
table(data_temp$estado)
#Frecuencia relativa
prop.table(table(data_temp$estado)) 
#grafica de frecuencia absoluta
barplot(table(data_temp$estado),space = 0.1,col=c("darkslategray","cyan3"),ylim = c(0,270)
        ,ylab = "Frecuencia absoluta",main="Censuras y Eventos Observados")

# por factores:

######  Actividades economicas ######
# Frecuenica absoluta
table(data$actividad,data_temp$estado) 
#frecuencia relativa dentro del factor
prop.table(table(data$financiamiento,data_temp$estado)) 
#Frecuencia relativa dentro del grup
table(data$actividad,data_temp$estado)/as.numeric(table(data$actividad))
#grafica de frecuencias absoluta
barplot(table(data$actividad,data_temp$estado),beside = TRUE,col=c("darkslategray","deepskyblue4","cyan3","skyblue","lightcyan")
        ,ylim = c(0,120),ylab = "Frecuencia absoluta")
legend(1500, 0.9,legend=c("Act. Profesionales","Alojamiento","Comercio","Comida","Transporte"),
       fill =c("darkslategray","deepskyblue4","cyan3","skyblue","lightcyan"),x = "top", box.col = 4,box.lwd = 2)


######   Tipo de financiamiento   #######

#Frecuencia absoluta
table(data$financiamiento,data_temp$estado)
#Frecuencia relativa dentro del factor
prop.table(table(data$financiamiento,data_temp$estado))
#frecuencia relativa en el grupo
table(data$financiamiento,data_temp$estado)/as.numeric(table(data$financiamiento)) 
#grafica frecuencia absoluta
barplot(table(data$financiamiento,data_temp$estado),beside = TRUE,col=c("deepskyblue4","cyan3","skyblue","lightcyan")
        ,ylim = c(0,120),ylab = "Frecuencia absoluta")
legend(1500, 0.9,legend=c("Banco","Cooperativa","Inst. no bancaria","Familiar"),
       fill =c("deepskyblue4","cyan3","skyblue","lightcyan"),x = "top", box.col = 4,box.lwd = 2)

#######   tamano de la empresa   #######

#frecuencia absoltuta
with(data,table(data$tamano,data_temp$estado))
#frecuancia relativa dentro del factor
prop.table(table(data$tamano,data_temp$estado))
#frecuencia relativa dentro del grup
table(data$tamano,data_temp$estado)/as.numeric(table(data$tamano)) 
#grafica de frecuencia absoluta
barplot(table(data$tamano,data_temp$estado),beside = TRUE,col=c("deepskyblue4","cyan3")
        ,ylim = c(0,210),ylab = "Frecuencia absoluta")
legend(1500, 0.9,legend=c("Micro","Pequña"),
       fill =c("deepskyblue4","cyan3"),x = "top", box.col = 4,box.lwd = 2)

# Eliminando la data temporal  
rm("data_temp")                                      

#Tiempos de supervivencia.

#Mostrando todos los tiempos(incluyendo cuando ocurre el evento y cuando no)
#un signo "+" acompañando el numero indica cuando hubo censura
Surv ( data$tiempo , data$estado )  

#tiempos donde se presenta el evento
data$tiempo [ data$estado == 1 ]
 
#tiempos donde se presenta el evento (ordenados)
sort(data$tiempo [ data$estado == 1 ])

#frecuencia de estos tiempos 
table( data$tiempo [ data$estado == 1 ] )
# Frecuencia relativas
prop.table(table(data$tiempo[data$estado==1])) 
#grafica de frecuencia relativa
barplot(prop.table(table(data$tiempo[data$estado==1])),space =0,ylim = c(0, 0.25),xlab = ,50,col
        ="deepskyblue4" , ylab="Frecuencia relativa")


#distribucion de los tiempos de supervivencia.

#######   actividades economicas   ######
table( data$actividad[data$estado==1], data$tiempo [ data$estado == 1 ])

######    tipo de financiamiento   ######
table( data$financiamiento[data$estado==1], data$tiempo [ data$estado == 1 ])

######    tamano de la empresa   #######
table( data$tamano[data$estado==1], data$tiempo [ data$estado == 1 ])
