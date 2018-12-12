#AGENDA DE MOVILIZACION

library(haven)
library(margins)
library(rms)
library(MASS)
library(lmtest)

setwd("C:/Users/Santiago/Desktop/Master/Protesta y nuevas formas de participacion/Ejercicio1")

cis3126 <- read_dta("DA3126_v12.dta")

#Usar las preguntas P20 (uso de Internet) y P20a (uso de redes sociales) del cuestionario 3126 del CIS, sobre uso de
#redes sociales, y las preguntas P16 y P17 del mismo estudio para observar la
#participación en asociaciones.

#Creamos una nueva variable: si el usuario usa redes sociales o no
cis3126$redes<-cis3126$P20

#Limpiamos datos
cis3126$redes[cis3126$redes == 9]<-NA
cis3126$redes[cis3126$redes == 2]<-0
cis3126$redes[cis3126$redes == 1]<-0 #Convertimos en cero todos los usuarios antes de reclasificarlos

cis3126$redes[cis3126$P20A01==1|cis3126$P20A02 == 1]<-1 #Esta linea clasifica 
                                  #como usuario a todos aquellos que usan por lo
                                  #menos una red social (Twitter o Facebook)

#Renombramos variables y convertimos a factor
cis3126$redes <-factor(cis3126$redes,
                       levels = c(0,1),
                       labels = c("No usuario", "Usuario"))

#Limpieza P1601 (Asistencia a manifestaciones)
cis3126$manifesta <-cis3126$P1601
cis3126$manifesta[cis3126$manifesta == 9]<-NA
cis3126$manifesta[cis3126$manifesta ==3]<- 0 
cis3126$manifesta[cis3126$manifesta ==2]<-1 #Esto nos permite dicotomizar la variable

cis3126$manifesta <-factor(cis3126$manifesta,
                           levels = c(0,1),
                           labels = c("No asiste a manifestaciones", "Sí asiste a manifestaciones"))

#Limpieza p17 (Pertenencia a alguna asociación)
cis3126$asocia <-cis3126$P17

cis3126$asocia[cis3126$asocia == 9]<-NA
cis3126$asocia[cis3126$asocia ==2]<-0

cis3126$asocia <- factor(cis3126$asocia,
                         levels=c(0,1),
                         labels=c("No pertenece", "Pertenece a alguna asociacion"))

#Variables sociodemográficas
#Edad cis3126$EDAD
cis3126$edad.r <-cis3126$EDAD
cis3126$edad.r[cis3126$edad.r==99] <-NA #Recodificamos 99 años como NA

#Genero cis3126$SEXO
cis3126$hombre<-cis3126$SEXO
cis3126$hombre[cis3126$hombre==2]<-0

cis3126$hombre <-factor(cis3126$hombre,
                        levels= c(0,1),
                        labels= c("Mujer","Hombre"))


#Escolaridad (cis3126$ESTUDIOS)
# 1-5=1; Estudios básicos
# 6=2; Estudios universitarios

cis3126$escolaridad <-cis3126$ESTUDIOS

cis3126$escolaridad[cis3126$escolaridad >6]<-NA
cis3126$escolaridad[cis3126$escolaridad < 6]<-1
cis3126$escolaridad[cis3126$escolaridad == 6]<-2


cis3126$escolaridad <-factor(cis3126$escolaridad,
                             levels= c(1,2),
                             labels= c("Estudios básicos","Estudios universitarios"))

#Tests de correlación
cor.test(as.numeric(cis3126$redes),as.numeric(cis3126$asocia))
#Se rechaza la hipótesis nula; sí hay correlación entre las variables (0.1410517)

cor.test(as.numeric(cis3126$redes),as.numeric(cis3126$manifesta))
#Se rechaza la hipótesis nula; sí hay correlación entre las variables(0.1931584)


#Tabla de contingencias
#Usuarios de redes sociales y pertenencia a alguna asociación
prop.table(table(cis3126$redes,cis3126$asocia),1)

#Usuarios de redes sociales y asistencia a manifestaciones
prop.table(table(cis3126$redes,cis3126$manifesta),1)

plot(cis3126$redes,cis3126$asocia)
plot(cis3126$redes,cis3126$manifesta)

library(gmodels)

#Tabla de contingencias
#Usuarios de redes sociales y pertenencia a alguna asociación
tabla1<-CrossTable(cis3126$redes, cis3126$asocia, digits=1, expected=T, 
                   asresid=TRUE, prop.chisq=F, chisq=TRUE, format=c("SPSS"))


#Usuarios de redes sociales y asistencia a manifestaciones
tabla2<-CrossTable(cis3126$redes, cis3126$manifesta, digits=1, expected=T, 
                   asresid=TRUE, prop.chisq=F, chisq=TRUE, format=c("SPSS"))


myvars <- c("redes","manifesta","asocia","edad.r","hombre","escolaridad")  
data<-cis3126[myvars]
data<- na.omit(data)
dim(data)

##########################################################################
#Modelo de manifestacion:                                                #
#¿La pertenencia a redes ayuda a explicar si alguien se manifiesta o no? #
##########################################################################

mod.manifest <-glm(manifesta ~ redes+edad.r+hombre+escolaridad, data=data,family="binomial")

summary(mod.manifest)

#Obtenemos los coeficientes
exp(coef(mod.manifest))

#Multicolinealidad: no parece haber problemas de multicolinealidad
logit.vif<- vif(mod.manifest)
logit.vif
#Heterocedasticidad: se rechaza la Ho: se cumple el supuesto de heterocedasticidad
logit.het<-bptest(mod.manifest)
logit.het
#Calculamos efectos marginales
margins_manifest <- margins(mod.manifest)
summary(margins_manifest)
write.csv(summary(margins_manifest),file="marginales_manifestacion.csv", row.names=F)

plot(margins_manifest, main="Gráfica 1- Efectos marginales sobre manifestacion",
     pch=15, las=2, labels=c("Edad","Estudios Superiores","Género","Uso de redes"))

##################################################################################
#Modelo de asociación:                                                           #
#¿La pertenencia a redes ayuda a explicar si alguien pertenece a una asociación? #
##################################################################################
mod.asoc <-glm(asocia ~ redes+edad.r+hombre+escolaridad, data=data,family="binomial")

summary(mod.asoc)

#Obtenemos los coeficientes
exp(coef(mod.asoc))

#Multicolinealidad: no parece haber problemas de multicolinealidad
logit.vif<- vif(mod.asoc)
logit.vif
#Heterocedasticidad: se rechaza la Ho: se cumple el supuesto de heterocedasticidad
logit.het<-bptest(mod.asoc)
logit.het
#Calculamos efectos marginales
margins_asoc <- margins(mod.asoc)
summary(margins_asoc)
write.csv(summary(margins_asoc),file="marginales_asociacion.csv", row.names=F) #Exportamos a csv

plot(margins_asoc, main="Gráfica 2- Efectos marginales sobre asociación",
     pch=15, las=2, labels=c("Edad","Estudios Superiores","Género","Uso de redes"))

