#Cargando los paquetes 
library(survival)
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

## calculando hr e IC del 95% de los grupos que presentaron diferencia significativa en la experiencia de supervivencia.

# ACTIVIDADES ECONOMICAS

# alojamiento - transporte
logrank<-survdiff(Surv(tiempo, estado) ~ actividad, data = subset.data.frame(datos,datos$actividad=="transporte"|
                                                                             datos$actividad=="alojamiento"), rho=0) 

cat("Numero de eventos observados\n","Alojamiento:",logrank$obs[1],"\n Transporte :",logrank$obs[2])
cat("Numero de eventos esperados\n","Alojamiento:",logrank$exp[1],"\n Transporte :",logrank$exp[2])

hr<-(logrank$obs[1]/logrank$exp[1])/(logrank$obs[2]/logrank$exp[2])
cat("Cociente de riesgo:", hr)

# IC del 95%
Li<-exp(log(hr)-qnorm(0.975)*sqrt(1/(logrank$exp[1]+1/logrank$exp[2])))
Ls<-exp(log(hr)+qnorm(0.975)*sqrt(1/(logrank$exp[1]+1/logrank$exp[2])))
cat("IC del 95%\n","(",Li,",",Ls,")")

# comida - transporte
logrank<-survdiff(Surv(tiempo, estado) ~ actividad, data = subset.data.frame(datos,datos$actividad=="transporte"|
                                                                             datos$actividad=="comida"), rho=0) 

cat("Numero de eventos observados\n","Comida    :",logrank$obs[1],"\n Transporte:",logrank$obs[2])
cat("Numero de eventos esperados\n","Comida    :",logrank$exp[1],"\n Transporte:",logrank$exp[2])

hr<-(logrank$obs[1]/logrank$exp[1])/(logrank$obs[2]/logrank$exp[2])
cat("Cociente de riesgo:", hr)

# IC del 95%
Li<-exp(log(hr)-qnorm(0.975)*sqrt(1/(logrank$exp[1]+1/logrank$exp[2])))
Ls<-exp(log(hr)+qnorm(0.975)*sqrt(1/(logrank$exp[1]+1/logrank$exp[2])))
cat("IC del 95%\n","(",Li,",",Ls,")")

# comercio - comida
logrank<-survdiff(Surv(tiempo, estado) ~ actividad, data = subset.data.frame(datos,datos$actividad=="comercio"|
                                                                              datos$actividad=="comida"), rho=0) 

cat("Numero de eventos observados\n","Comercio:",logrank$obs[1],"\n Comida  :",logrank$obs[2])
cat("Numero de eventos esperados\n","Comercio:",logrank$exp[1],"\n Comida  :",logrank$exp[2])

hr<-(logrank$obs[1]/logrank$exp[1])/(logrank$obs[2]/logrank$exp[2])
cat("Cociente de riesgo:", hr)

# IC del 95%
Li<-exp(log(hr)-qnorm(0.975)*sqrt(1/(logrank$exp[1]+1/logrank$exp[2])))
Ls<-exp(log(hr)+qnorm(0.975)*sqrt(1/(logrank$exp[1]+1/logrank$exp[2])))
cat("IC del 95%\n","(",Li,",",Ls,")")


# TIPO DE FINANCIAMIENTO
# Banco - Familiar
logrank<-survdiff(Surv(tiempo, estado) ~ financiamiento, data = subset.data.frame(datos,datos$financiamiento=="Banco"|
                                                                               datos$financiamiento=="familiar"), rho=0) 

cat("Numero de eventos observados\n","Banco   :",logrank$obs[1],"\n Familiar:",logrank$obs[2])
cat("Numero de eventos esperados\n","Banco   :",logrank$exp[1],"\n Familiar:",logrank$exp[2])


hr<-(logrank$obs[1]/logrank$exp[1])/(logrank$obs[2]/logrank$exp[2])
cat("Cociente de riesgo:", hr)

# IC del 95%
Li<-exp(log(hr)-qnorm(0.975)*sqrt(1/(logrank$exp[1]+1/logrank$exp[2])))
Ls<-exp(log(hr)+qnorm(0.975)*sqrt(1/(logrank$exp[1]+1/logrank$exp[2])))
cat("IC del 95%\n","(",Li,",",Ls,")")
