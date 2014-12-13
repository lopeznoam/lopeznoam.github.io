

#                       Instituto de Opinión Pública-PUCP
#
#       Inseguridad y percepción de inseguridad en Lima Metropolitana 
#
#                            Noam López Villanes 
#                            
#                  Pontificia Universidad Católica del Perú
#                            
#    Citar así: López, Noam (2013) Inseguridad y percepción de inseguridad 
#    en Lima Metropolitana. Serie Cuadernos de Investigación IOP. 
#    Lima: IOP - PUCP.
#
#    E-mail: lopez.noam@pucp.pe
#
#*****************************************************************************#

#*****************************************************ORIGEN DEL PROGRAMA

                            
#IMPORTANDO BASES DE DATOS

#Si la data esta en SPSS (.sav)
library(foreign)  #necesario para que funcione read.spss
ejemplo07<- read.spss("IOP_0807_01_B.sav",use.value.labels=TRUE, 
                    max.value.labels=Inf, to.data.frame=TRUE)
ejemplo08<- read.spss("IOP_0708_01_B.sav",use.value.labels=TRUE, 
                      max.value.labels=Inf, to.data.frame=TRUE)
ejemplo09<- read.spss("IOP_0809_01_B.sav",use.value.labels=TRUE, 
                      max.value.labels=Inf, to.data.frame=TRUE)
ejemplo10<- read.spss("IOP_0710_01_B.sav",use.value.labels=TRUE, 
                      max.value.labels=Inf, to.data.frame=TRUE)

#ACTIVIDADES INTRODUCTORIAS (mínería de datos)

#para ver el nombre de las variables
names(ejemplo07)
names(ejemplo08)
names(ejemplo09)
names(ejemplo10)


#instalando paquetes
install.packages("Hmisc") 
install.packages("GPArotation")
install.packages("psych")
install.packages("multilevel")
install.packages("ggplot2")
install.packages("reshape")
install.packages("GGally")
install.packages("vioplot")

#abriendo paquetes
library(psych)
library(Hmisc) 
library(GPArotation)
library(multilevel)
library(ggplot2)
library(vioplot)
library(reshape)
library(GGally)

#estadistica descriptiva
describe(ejemplo07) #uso del comando "describe"

#creacion de etiquetas

label(ejemplo07$P35A)<-"¿CON QUÉ FRECUENCIA EN SU BARRIO SE VEN CASOS DE VENTA DE DROGAS A PEQUEÑA ESCALA?"
label(ejemplo07$P35B)<-"¿CON QUÉ FRECUENCIA EN SU BARRIO SE VEN CASOS DE PROSTITUCIÓN?"          
label(ejemplo07$P35C)<-"¿CON QUÉ FRECUENCIA EN SU BARRIO SE VEN CASOS DE ACTOS DE VANDALISMO?"       
label(ejemplo07$P35D)<-"¿CON QUÉ FRECUENCIA EN SU BARRIO SE VEN CASOS DE ALCOHOLISMO EN LAS CALLES?"          
label(ejemplo07$P35E)<-"¿CON QUÉ FRECUENCIA EN SU BARRIO SE VEN CASOS DE SUCIEDAD, FALTA DE SALUBRIDAD PÚBLICA?"         
label(ejemplo07$P35F)<-"¿CON QUÉ FRECUENCIA EN SU BARRIO SE VEN CASOS DE RUIDOS O PROBLEMAS PRODUCIDOS POR BARES, DISCOTECAS, ETC.?"
label(ejemplo07$P35G)<-"¿CON QUÉ FRECUENCIA EN SU BARRIO SE VEN CASOS DE MENDICIDAD DE NIÑOS?"    
label(ejemplo07$P35H)<-"¿CON QUÉ FRECUENCIA EN SU BARRIO SE VEN CASOS DE ESCÁNDALOS Y RIÑAS CALLEJERAS?"      

label(ejemplo08$P51A)<-"¿CON QUÉ FRECUENCIA EN SU BARRIO SE VEN CASOS DE VENTA DE DROGAS A PEQUEÑA ESCALA?"
label(ejemplo08$P51B)<-"¿CON QUÉ FRECUENCIA EN SU BARRIO SE VEN CASOS DE ATRACOS / ROBOS AL PASO EN LA CALLE?"          
label(ejemplo08$P51C)<-"¿CON QUÉ FRECUENCIA EN SU BARRIO SE VEN CASOS DE ROBOS EN LAS VIVIENDAS?"       
label(ejemplo08$P51D)<-"¿CON QUÉ FRECUENCIA EN SU BARRIO SE VEN CASOS DE PROSTITUCIÓN?"          
label(ejemplo08$P51E)<-"¿CON QUÉ FRECUENCIA EN SU BARRIO SE VEN CASOS DE ACTOS DE VANDALISMO / DESTRUCCIÓN DE LA PROPIEDAD PÚBLICA O PRIVADA?"         
label(ejemplo08$P51F)<-"¿CON QUÉ FRECUENCIA EN SU BARRIO SE VEN CASOS DE ALCOHOLISMO EN LAS CALLES?"
label(ejemplo08$P51G)<-"¿CON QUÉ FRECUENCIA EN SU BARRIO SE VEN CASOS DE SUCIEDAD, FALTA DE SALUBRIDAD PÚBLICA?"    
label(ejemplo08$P51H)<-"¿CON QUÉ FRECUENCIA EN SU BARRIO SE VEN CASOS DE RUIDOS O PROBLEMAS PRODUCIDOS POR BARES, DISCOTECAS, ETC.?"      
label(ejemplo08$P51I)<-"¿CON QUÉ FRECUENCIA EN SU BARRIO SE VEN CASOS DE MENDICIDAD DE NIÑOS?"    
label(ejemplo08$P51J)<-"¿CON QUÉ FRECUENCIA EN SU BARRIO SE VEN CASOS DE ESCÁNDALOS Y RIÑAS CALLEJERAS?"      

label(ejemplo09$P42A)<-"donde 0 es Nada segura y 10 Muy segura, ¿cuán seguro se siente usted en el siguiente lugar.A bordo de un vehículo de transporte público?"
label(ejemplo09$P42B)<-"¿cuán seguro se siente usted en el siguiente lugar.En los paraderos?"          
label(ejemplo09$P42C)<-"¿cuán seguro se siente usted en el siguiente lugar.En los terminales de bus?"       
label(ejemplo09$P42D)<-"¿cuán seguro se siente usted en el siguiente lugar.En un aeropuerto?"          
label(ejemplo09$P42E)<-"¿cuán seguro se siente usted en el siguiente lugar.En su vecindario?"         
label(ejemplo09$P42F)<-"¿cuán seguro se siente usted en el siguiente lugar.En su trabajo?"
label(ejemplo09$P42G)<-"¿cuán seguro se siente usted en el siguiente lugar.En la calle?"    
label(ejemplo09$P42H)<-"¿cuán seguro se siente usted en el siguiente lugar.En un centro comercial?"    

label(ejemplo10$P47A)<-"¿con qué frecuencia en su barrio se ven casos de alcoholismo en las calles ?"
label(ejemplo10$P47B)<-"¿con qué frecuencia en su barrio se ven casos de robos/Atracos al paso en la calle?"          
label(ejemplo10$P47C)<-"¿con qué frecuencia en su barrio se ven casos de suciedad/ falta de salubridad pública?"       
label(ejemplo10$P47D)<-"¿con qué frecuencia en su barrio se ven casos de venta y consumo de drogas?"          
label(ejemplo10$P47E)<-"¿con qué frecuencia en su barrio se ven casos de escándalos y riñas callejeras?"         
label(ejemplo10$P47F)<-"¿con qué frecuencia en su barrio se ven casos de actos de vandalismo/ destrucción de la propiedad pública o privada?"
label(ejemplo10$P47G)<-"¿con qué frecuencia en su barrio se ven casos de robos en las viviendas?"    
label(ejemplo10$P47H)<-"¿con qué frecuencia en su barrio se ven casos de ruidos o problemas producidos por los bares, discotecas, etc.?"      
label(ejemplo10$P47I)<-"¿con qué frecuencia en su barrio se ven casos de mendicidad de niños?"
label(ejemplo10$P47J)<-"¿con qué frecuencia en su barrio se ven casos de prostitución?"



#gráficos
plot(ejemplo07$P35A) #para replicar solo cambiar nombre de variable


#correlaciones
cor.test(IDH,Rur) #para replicar solo cambiar nombres de variables


#CREACIO–N DE INDICADORES


#**********DATA 2007 ********

#confiabilidad o consistencia interna (alfa de cronbach)

#para trabajar a nivel de variables
attach(ejemplo07)

P35A<-as.numeric(P35A)
P35B<-as.numeric(P35B)
P35C<-as.numeric(P35C)
P35D<-as.numeric(P35D)
P35E<-as.numeric(P35E)
P35F<-as.numeric(P35F)
P35G<-as.numeric(P35G)
P35H<-as.numeric(P35H)

          
a<-data.frame(P35A,P35B,P35C,P35D,P35E,P35F,P35G,P35H)
cronbach(a)

describe(a)

#COMPONENTES PRINCIPALES

#Usaremos el paquete "psych"

pca<- principal(a, nfactors=1, scores=TRUE, missing=TRUE, rotate="varimax");pca
print(pca$loadings,cutoff=.33, sort=TRUE)
print(pca$communality)
print(pca$weights)
print(pca$scores)
p7<-data.frame(pca$scores) #le damos formato de data
names(p7)<-c("inseguridad07");
p7 #nombrando variables

attach(p7)

#creacion de la funcion reescaladora (de 0 a 1)
range01 <- function(x){(x-min(x))/(max(x)-min(x))}

#Puntaje de 0 a 100
inseguridad071 <- range01(inseguridad07)
inseguridad07100 <- (inseguridad071)*100
p7$inseguridad07100<-inseguridad07100 #incorporando a la data el indicador de 0 a 100

#creando ranking
p7$rank_inseguridad07100<-rank(inseguridad07100) #incorporando a la data el ranking

#kmeans (grupos más parecidos)
adaptk7 <- kmeans(p7[,2],4)
kmedias_adapt7 <- data.frame(adaptk7$cluster)
p7<-cbind(p7,kmedias_adapt7)
p7<-cbind(UBIGEO07,p7)
p7
#Estadísticas y graficas de las variables 2007

pie(adaptk7$cluster)

#2007
names(data07)
data07$inseguridad07100

#indicador
par(mfrow=c(2,1),mar=c(3,5,3,5))
hist(data07$inseguridad07100,breaks=40, freq=T, main="Indicador de percepción de inseguridad 2007",
     xlim=c(0,100), ylim=c(0,100),col="blue",border=F
)
boxplot(data07$inseguridad07100,ylim=c(0,100),col="blue",horizontal=T)

#variables
par(mfrow=c(1,1))
vioplot(a$P35A,a$P35B,a$P35C,a$P35D,a$P35E,a$P35F,a$P35G,a$P35H,
        col="gray87", rectCol="gray",colMed="black")



#Cambiando la forma de la data
aa <- melt(a)
#Boxplots

box07<- ggplot(aa,aes(x=variable,y=value)) 
box07 + geom_boxplot(fill = "white", colour = "black")
box07 + geom_violin(fill = "white", colour = "black")
box07 + geom_boxplot(outlier.colour = "green", outlier.size = 2)


#**********DATA 2008 ********

#confiabilidad o consistencia interna (alfa de cronbach)

#para trabajar a nivel de variables
attach(ejemplo08)

P51A<-as.numeric(P51A)
P51B<-as.numeric(P51B)
P51C<-as.numeric(P51C)
P51D<-as.numeric(P51D)
P51E<-as.numeric(P51E)
P51F<-as.numeric(P51F)
P51G<-as.numeric(P51G)
P51H<-as.numeric(P51H)
P51I<-as.numeric(P51I)
P51J<-as.numeric(P51J)

#alfa de cronbach
b<-data.frame(P51A,P51B,P51C,P51D,P51E,P51F,P51G,P51H,P51I,P51J)
cronbach(b)

#COMPONENTES PRINCIPALES

#Usaremos el paquete "psych"

pca<- principal(b, nfactors=1, scores=TRUE, missing=TRUE, rotate="varimax");pca
print(pca$loadings,cutoff=.33, sort=TRUE)
print(pca$communality)
print(pca$weights)
print(pca$scores)
p8<-data.frame(pca$scores) #le damos formato de data
names(p8)<-c("inseguridad08");p8 #nombrando variables

attach(p8)

#creacion de la funcion reescaladora (de 0 a 1)
range01 <- function(x){(x-min(x))/(max(x)-min(x))}

#Puntaje de 0 a 100
inseguridad081 <- range01(inseguridad08)
inseguridad08100 <- (inseguridad081)*100
p8$inseguridad08100<-inseguridad08100 #incorporando a la data el indicador de 0 a 100

#creando ranking
p8$rank_inseguridad08100<-rank(inseguridad08100) #incorporando a la data el ranking

#kmeans (grupos más parecidos)
adaptk8 <- kmeans(p8[,2],4)
kmedias_adapt8 <- data.frame(adaptk8$cluster)
p8<-cbind(p8,kmedias_adapt8)
p8<-cbind(UBIGEO08,p8)



#Cambiando la forma de la data
bb <- melt(b)
#Boxplots

box08<- ggplot(bb,aes(x=variable,y=value)) 
box08 + geom_boxplot(fill = "white", colour = "black")
box08 + geom_violin(fill = "white", colour = "black")
box08 + geom_boxplot(outlier.colour = "green", outlier.size = 2)



#**********DATA 2009 ********

#confiabilidad o consistencia interna (alfa de cronbach)

#para trabajar a nivel de variables
attach(ejemplo09)

P42A<-as.numeric(P42A)
P42B<-as.numeric(P42B)
P42C<-as.numeric(P42C)
P42D<-as.numeric(P42D)
P42E<-as.numeric(P42E)
P42F<-as.numeric(P42F)
P42G<-as.numeric(P42G)
P42H<-as.numeric(P42H)

c<-data.frame(P42A,P42B,P42C,P42D,P42E,P42F,P42G,P42H)
cronbach(c)

#COMPONENTES PRINCIPALES

#Usaremos el paquete "psych"

pca<- principal(c, nfactors=1, scores=TRUE, missing=TRUE, rotate="varimax");pca
print(pca$loadings,cutoff=.33, sort=TRUE)
print(pca$communality)
print(pca$weights)
print(pca$scores)
p9<-data.frame(pca$scores) #le damos formato de data
names(p9)<-c("inseguridad09");p9 #nombrando variables

attach(p9)

#creacion de la funcion reescaladora (de 0 a 1)
range01 <- function(x){(x-min(x))/(max(x)-min(x))}

#Puntaje de 0 a 100
inseguridad091 <- range01(inseguridad09)
inseguridad09100 <- (inseguridad091)*100
p9$inseguridad09100<-inseguridad09100 #incorporando a la data el indicador de 0 a 100

#creando ranking
p9$rank_inseguridad09100<-rank(inseguridad09100) #incorporando a la data el ranking

#kmeans (grupos más parecidos)
adaptk <- kmeans(p9[,2],4)
kmedias_adapt <- data.frame(adaptk$cluster)
p9<-cbind(p9,kmedias_adapt)
p9<-cbind(UBIGEO09,p9)



#Cambiando la forma de la data
cc <- melt(c)
#Boxplots

box09<- ggplot(cc,aes(x=variable,y=value)) 
box09 + geom_boxplot(fill = "white", colour = "black")
box09 + geom_violin(fill = "white", colour = "black")
box09 + geom_boxplot(outlier.colour = "green", outlier.size = 2)


#**********DATA 2010 ********

#confiabilidad o consistencia interna (alfa de cronbach)

#para trabajar a nivel de variables
attach(ejemplo10)

P47A<-as.numeric(P47A)
P47B<-as.numeric(P47B)
P47C<-as.numeric(P47C)
P47D<-as.numeric(P47D)
P47E<-as.numeric(P47E)
P47F<-as.numeric(P47F)
P47G<-as.numeric(P47G)
P47H<-as.numeric(P47H)
P47I<-as.numeric(P47I)
P47J<-as.numeric(P47J)

d<-data.frame(P47A,P47B,P47C,P47D,P47E,P47F,P47G,P47H,P47I,P47J)
cronbach(d)

#COMPONENTES PRINCIPALES

#Usaremos el paquete "psych"

pca<- principal(d, nfactors=1, scores=TRUE, missing=TRUE, rotate="varimax");pca
print(pca$loadings,cutoff=.33, sort=TRUE)
print(pca$communality)
print(pca$weights)
print(pca$scores)
p10<-data.frame(pca$scores) #le damos formato de data
names(p10)<-c("inseguridad10");p10 #nombrando variables

attach(p10)

#creacion de la funcion reescaladora (de 0 a 1)
range01 <- function(x){(x-min(x))/(max(x)-min(x))}

#Puntaje de 0 a 100
inseguridad101 <- range01(inseguridad10)
inseguridad10100 <- (inseguridad101)*100
p10$inseguridad10100<-inseguridad10100 #incorporando a la data el indicador de 0 a 100

#creando ranking
p10$rank_inseguridad10100<-rank(inseguridad10100) #incorporando a la data el ranking

#kmeans (grupos más parecidos)
adaptk <- kmeans(p10[,2],4)
kmedias_adapt <- data.frame(adaptk$cluster)
p10<-cbind(p10,kmedias_adapt)
p10<-cbind(UBIGEO10,p10)


#Cambiando la forma de la data
dd <- melt(d)
#Boxplots

box10<- ggplot(dd,aes(x=variable,y=value)) 
box10 + geom_boxplot(fill = "white", colour = "black")
box10 + geom_violin(fill = "white", colour = "black")
box10 + geom_boxplot(outlier.colour = "green", outlier.size = 2)

################################################################3


#EXPORTANDO LA DATA
data07<-cbind(ejemplo07,p7)
data08<-cbind(ejemplo08,p8)
data09<-cbind(ejemplo09,p9)
data10<-cbind(ejemplo10,p10)

write.csv(data07,file="data07.csv") #para excel y otros
write.csv(data08,file="data08.csv") #para excel y otros
write.csv(data09,file="data09.csv") #para excel y otros
write.csv(data10,file="data10.csv") #para excel y otros



#Estadísticas y graficas de las variables y de los indicadores creados

#2007
names(data07)
data07$inseguridad07100

par(mfrow=c(2,1),mar=c(3,5,3,5))
hist(data07$inseguridad07100,breaks=40, freq=T, main="Indicador compuesto de inseguridad 2007",
     xlim=c(0,100), ylim=c(0,100),col="blue",border=F)
boxplot(data07$inseguridad07100,ylim=c(0,100),col="blue",horizontal=T)


names(data08)
data08$inseguridad08100

par(mfrow=c(2,1),mar=c(3,5,3,5))
hist(data08$inseguridad08100,breaks=40, freq=T, main="Indicador compuesto de inseguridad 2008",
     xlim=c(0,100), ylim=c(0,40),col="gray48",border=F
)
boxplot(data08$inseguridad08100,ylim=c(0,100),col="gray48",horizontal=T)

names(data09)
data09$inseguridad09100

par(mfrow=c(2,1),mar=c(3,5,3,5))
hist(data09$inseguridad09100,breaks=40, freq=T, main="Indicador compuesto de inseguridad 2009",
     xlim=c(0,100), ylim=c(0,100),col="blue",border=F
)
boxplot(data09$inseguridad09100,ylim=c(0,100),col="blue",horizontal=T)


names(data10)
data10$inseguridad10100

par(mfrow=c(2,1),mar=c(3,5,3,5))
hist(data10$inseguridad10100,breaks=40, freq=T, main="Indicador compuesto de inseguridad 2010",
     xlim=c(0,100), ylim=c(0,100),col="blue",border=F
)
boxplot(data10$inseguridad10100,ylim=c(0,100),col="blue",horizontal=T)

par(mfrow=c(1,1))


#Densidad de kernel y boxplots de los indicadores compuestos creados
indicators<-cbind(data07$inseguridad07100, data08$inseguridad08100,
                       data09$inseguridad09100,data10$inseguridad10100)

indicators<-data.frame(indicators)
names(indicators)
names(indicators) <- c("IPI 2007","IPI 2008","IPI 2009", "IPI 2010")

indicatorx<-melt(indicators)

#Boxplots

boxtodos<- ggplot(indicatorx,aes(x=variable,y=value)) 
boxtodos + geom_boxplot(outlier.colour = "green", outlier.size = 2)
boxtodos + geom_point()
boxplot(data07$inseguridad07100, data08$inseguridad08100, data09$inseguridad09100,
        data10$inseguridad10100, col="white", rectCol="gray",colMed="black")


#scatterplot de los indicadores
distritos<-read.csv("Indicadores compuestos de percepción de inseguridad.csv")
names(distritos)
plotmatrix(distritos[,7:10], colour="gray20") +
  geom_smooth(method="lm")

#correlaciones anivel distrital
attach(distritos)
cor.test(IPI2007,IPI2008, method="spearm")
cor.test(IPI2007,IPI2009, method="spearm")
cor.test(IPI2007,IPI2010, method="spearm")
cor.test(IPI2008,IPI2009, method="spearm")
cor.test(IPI2009,IPI2008, method="spearm")
cor.test(IPI2009,IPI2010, method="spearm")
cor.test(IPI2010,IPI2008, method="spearm")

#Citas
citation(package = "ggplot2")
citation(package = "Hmisc")
citation(package = "GPArotation")
citation(package = "psych")
citation(package = "multilevel")
citation(package = "reshape")

#*****************************************************FIN DEL PROGRAMA

