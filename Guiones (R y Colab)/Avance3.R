#Cargando los paquetes 
install.packages("survminer")
library(survival)
library(ggplot2)
library(survminer)

# cargando el conjunto de datos
library(readr)
urlfile<-"https://raw.githubusercontent.com/ElmerZelaya/MM-700-Seminario-de-Investigacion/main/DATA.csv"
datos<-read.csv(urlfile)

# recodificando las variables categoricas con la funcion factor():

datos$actividad<-factor(datos$actividad,levels=c("1","2","3","4","5"),
                       labels=c("actividades prof","alojamiento","comercio","comida","transporte"))
datos$financiamiento<-factor(datos$financiamiento,levels=c("1","2","3","4"),
                            labels=c("Banco","Cooperativa","Inst. no bancaria","familiar"))
datos$tamano<-factor(datos$tamano,levels=c("1","2"),
                    labels=c("micro","pequena"))

# Estimación de la Función de Supervivecia S(t) con el estimador Kaplan-Meier (Global).
# y calculando el intervalo de confianza del 95% 

datos.km <- survfit(Surv(datos$tiempo, datos$estado==1)  ~ 1, datos, type = "kaplan-meier",conf.type="plain", error="greenwood",conf.int=0.95)
summary(datos.km)

#Curva de supervivencia de la curva de supervivencia (Global)

ggsurvplot(fit = datos.km,data=datos,xlim=c(0,45), 
           xlab = "Tiempo (semanas)", ylab = "Probabilidad de supervivencia", legend.title = "Estimación", 
           legend.labs = "K-M",risk.table = T)$plot +
  geom_segment(data=data.frame(c(.074,.316,.492)) , aes(x = c(2.5,20,42) , y = 1-c(.074,.316,.492), xend = c(2.5,20,42), yend = 0), lty = 2) +
  geom_segment(data=data.frame(c(.074,.316,.492)) , aes(x = 0, y = 1-c(.074,.316,.492), xend = c(2.5,20,42), yend = 1-c(.074,.316,.492)), lty =2)


# Estimación de la función de Supervivencia S(t) por grupos con sus respectivos I.C. del 95% 
####   Actividades económicas   #### 

# Actividades Profesionales 
 
act_profesionales.km <- survfit(Surv(tiempo, estado) ~ 1, data =  subset.data.frame(datos,actividad=="actividades prof"),  type = "kaplan-meier",
                                conf.type="plain", error="greenwood",conf.int=0.95)
summary(act_profesionales.km)

# Curva de supervivencia
 
ggsurvplot(fit = act_profesionales.km, data = datos,  xlim=c(0,45), 
           xlab = "Tiempo", ylab = "Probabilidad de supervivencia", legend.title = "Estimación", 
           legend.labs ="Actividades profesionales" ,risk.table = TRUE)$plot +
    geom_segment(data=data.frame(c(.15,.30)) , aes(x = c(7,20) , y = 1-c(.15,.30), xend = c(7,20), yend = 0), lty = 2) +
    geom_segment(data=data.frame(c(.15,.30)) , aes(x = 0, y = 1-c(.15,.30), xend = c(7,20), yend = 1-c(.15,.30)), lty =2)
 

# Alojamiento
  
alojamiento.km <- survfit(Surv(tiempo, estado) ~ 1, data = subset.data.frame(datos,actividad=="alojamiento"),  type = "kaplan-meier",
                          conf.type="plain", error="greenwood",conf.int=0.95)
summary(alojamiento.km)

#Curva de supervivencia
ggsurvplot(fit = alojamiento.km, data = datos, title = "Curva de Supervivencia",xlim=c(0,45), 
           xlab = "Tiempo", ylab = "Probabilidad de supervivencia", legend.title = "Estimación", 
           legend.labs ="alojamiento" ) 

# Comercio 
comercio.km <- survfit(Surv(tiempo, estado) ~ 1, data =  subset.data.frame(datos,actividad=="comercio"),  type = "kaplan-meier",
                       conf.type="plain", error="greenwood",conf.int=0.95)
summary(comercio.km)

#Curva de supervivencia
ggsurvplot(fit = comercio.km, data = datos, title = "Curva de Supervivencia",xlim=c(0,45), 
           xlab = "Tiempo", ylab = "Probabilidad de supervivencia", legend.title = "Estimación", 
           legend.labs ="Comercio" ) 

# Comida 
comida.km <- survfit(Surv(tiempo, estado) ~ 1, data =subset.data.frame(datos,actividad=="comida"),  type = "kaplan-meier",
                     conf.type="plain", error="greenwood",conf.int=0.95)
summary(comida.km)

#Curva de supervivencia
 
ggsurvplot(fit = comida.km, data = datos, title = "Curva de Supervivencia",xlim=c(0,45), 
           xlab = "Tiempo", ylab = "Probabilidad de supervivencia", legend.title = "Estimación", 
           legend.labs ="comida" )$plot+ 
  geom_segment(data=data.frame(c(.362,.557)) , aes(x = c(20,42) , y = 1-c(.362,.557), xend = c(20,42), yend = 0), lty = 2) +
  geom_segment(data=data.frame(c(.362,.557)) , aes(x = 0, y = 1-c(.362,.557), xend = c(20,42), yend = 1-c(.362,.557)), lty =2)


# Transporte
transporte.km <- survfit(Surv(tiempo, estado) ~ 1, data = subset.data.frame(datos,actividad=="transporte"),  type = "kaplan-meier",
                         conf.type="plain", error="greenwood",conf.int=0.95)
summary(transporte.km)

#Curva de supervivencia
ggsurvplot(fit = transporte.km, data = datos, title = "Curva de Supervivencia",xlim=c(0,45), 
           xlab = "Tiempo", ylab = "Probabilidad de supervivencia", legend.title = "Estimación", 
           legend.labs ="transporte" ) 
 
####   Tipo de Financiamiento   ####

# Bancos

Banco.km <- survfit(Surv(tiempo, estado) ~ 1, data = subset.data.frame(datos,financiamiento=="Banco"),  type = "kaplan-meier",
                    conf.type="plain", error="greenwood",conf.int=0.95)
summary(Banco.km)

#Curva de supervivencia
 
ggsurvplot(fit = Banco.km, data = datos,xlim=c(0,45), 
           xlab = "Tiempo (semanas)", ylab = "Probabilidad de supervivencia", legend.title = "Estimación", 
           legend.labs ="Banco" )$plot +
  geom_segment(data=data.frame(c(.065,.271,.439)) , aes(x = c(2.5,22,42) , y = 1-c(.065,.271,.439), xend = c(2.5,22,42), yend = 0), lty = 2) +
  geom_segment(data=data.frame(c(.065,.271,.439)) , aes(x = 0, y = 1-c(.065,.271,.439), xend = c(2.5,22,42), yend = 1-c(.065,.271,.439)), lty =2)

 

# Cooperativas
 
Cooperativa.km <- survfit(Surv(tiempo, estado) ~ 1, data = subset.data.frame(datos,financiamiento=="Cooperativa"),  type = "kaplan-meier",
                          conf.type="plain", error="greenwood",conf.int=0.95)
summary(Cooperativa.km)

#Curva de supervivencia
ggsurvplot(fit = Cooperativa.km, data = datos, title = "Curva de Supervivencia",xlim=c(0,45), 
           xlab = "Tiempo", ylab = "Probabilidad de supervivencia", legend.title = "Estimación", 
           legend.labs ="Cooperativa" ) 

 

#Instituciones no bancarias

Nobancaria.km <- survfit(Surv(tiempo, estado) ~ 1, data = subset.data.frame(datos,financiamiento=="Inst. no bancaria"),  type = "kaplan-meier",
                                conf.type="plain", error="greenwood",conf.int=0.95)
summary(Nobancaria.km)

#Curva de supervivencia
ggsurvplot(fit = Nobancaria.km, data = datos, title = "Curva de Supervivencia",xlim=c(0,45), 
           xlab = "Tiempo", ylab = "Probabilidad de supervivencia", legend.title = "Estimación", 
           legend.labs ="Inst. no bancaria" ) 
 
# Familiar

familiar.km <- survfit(Surv(tiempo, estado) ~ 1, data =  subset.data.frame(datos,financiamiento=="familiar"),  type = "kaplan-meier",
                       conf.type="plain", error="greenwood",conf.int=0.95)
summary(familiar.km)

#Curva de supervivencia
 
ggsurvplot(fit = familiar.km, data = datos,xlim=c(0,45), 
           xlab = "Tiempo (semanas)", ylab = "Probabilidad de supervivencia", legend.title = "Estimación", 
           legend.labs ="familiar" )$plot +
  geom_segment(data=data.frame(c(.108,.515,.585)) , aes(x = c(2.5,26,42) , y = 1-c(.108,.515,.585), xend = c(2.5,26,42), yend = 0), lty = 2) +
  geom_segment(data=data.frame(c(.108,.515,.585)) , aes(x = 0, y = 1-c(.108,.515,.585), xend = c(2.5,26,42), yend = 1-c(.108,.515,.585)), lty =2)

 
####   Tamaño de la empresa   ####

# Micro

micro.km <- survfit(Surv(tiempo, estado) ~ 1, data = subset.data.frame(datos,tamano=="micro"),  type = "kaplan-meier",
                    conf.type="plain", error="greenwood",conf.int=0.95)
summary(micro.km)

#Curva de supervivencia
ggsurvplot(fit = micro.km, data = datos,xlim=c(0,45), 
           xlab = "Tiempo (semanas)", ylab = "Probabilidad de supervivencia", legend.title = "Estimación", 
           legend.labs ="micro" )$plot +
  geom_segment(data=data.frame(c(.151,.325,.501)) , aes(x = c(7,20,42) , y = 1-c(.151,.325,.501), xend = c(7,20,42), yend = 0), lty = 2) +
  geom_segment(data=data.frame(c(.151,.325,.501)) , aes(x = 0, y = 1-c(.151,.325,.501), xend = c(7,20,42), yend = 1-c(.151,.325,.501)), lty =2) 

 
# Pequeña

pequena.km <- survfit(Surv(tiempo, estado) ~ 1, data =  subset.data.frame(datos,tamano=="pequena"),  type = "kaplan-meier",
                      conf.type="plain", error="greenwood",conf.int=0.95)
summary(pequena.km)

#Curva de supervivencia
 
ggsurvplot(fit = pequena.km, data = datos,xlim=c(0,45), 
           xlab = "Tiempo (semanas)", ylab = "Probabilidad de supervivencia", legend.title = "Estimación", 
           legend.labs ="pequena" )$plot +
  geom_segment(data=data.frame(c(.162,.291,.462)) , aes(x = c(7,20,42) , y = 1-c(.162,.291,.462), xend = c(7,20,42), yend = 0), lty = 2) +
  geom_segment(data=data.frame(c(.162,.291,.462)) , aes(x = 0, y = 1-c(.162,.291,.462), xend = c(7,20,42), yend = 1-c(.162,.291,.462)), lty =2) 
 

