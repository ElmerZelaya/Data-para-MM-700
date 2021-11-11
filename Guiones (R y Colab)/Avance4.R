#Cargando los paquetes 
library(survival)
library(survminer)
library(dplyr)

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

# Comparando los grupos de las actividades económicas

survfit(Surv(tiempo, estado==1) ~ actividad, data=datos) %>%
  ggsurvplot( conf.int = F,pval=T,xlim=c(0,45))

logrank<-survdiff(Surv(tiempo, estado) ~ actividad, data = datos, rho = 0)
# Valor p
1 - pchisq(logrank$chisq, length(logrank$n) - 1) 

# Comparación en parejas (Actividades económicas)

pairwise_survdiff(Surv(tiempo, estado) ~ actividad, data = datos, p.adjust.method = "none", 
                  rho = 0)


# Comparando los grupos de lo tipos de Financiamientos

survfit(Surv(tiempo, estado==1) ~ financiamiento, data=datos) %>%
  ggsurvplot( conf.int = F,pval=T,xlim=c(0,45))
 
logrank<-survdiff(Surv(tiempo, estado) ~ financiamiento, data = datos, rho = 0)
# Valor p
1 - pchisq(logrank$chisq, length(logrank$n) - 1) 

# Comparación en parejas(Tipos de Financiamientos)

pairwise_survdiff(Surv(tiempo, estado) ~ financiamiento, data = datos, p.adjust.method = "none", 
                  rho = 0)


# Comparando los grupos de tamano de las empresas

survfit(Surv(tiempo, estado==1) ~ tamano, data=datos) %>%
  ggsurvplot(conf.int = F,pval=T,xlim=c(0,45))

logrank<-survdiff(Surv(tiempo, estado) ~ tamano, data = datos, rho = 0)
# Valor p
1 - pchisq(logrank$chisq, length(logrank$n) - 1) 



