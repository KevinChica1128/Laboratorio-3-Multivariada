# Kevin García - 1533173
# Alejandro Vargas - 1525953
# Alejandro Soto - 1532457
# Laboratorio 2 - Multivariada

#base de datos
provincias=read.table("clipboard",header=T)
View(provincias)
#*****Lab 2
#funciones
#Porcentaje de incercia
library("ade4")
library("FactoMineR")
x11()
PCA.results=PCA(provincias[,-1],ncp=9)
PCA.results$eig[2,3]#porcentaje de varianza explicada por los dos primeros ejes
X11()
barplot(PCA.results$eig[,2],ylim = c(0,70),xlab='Componentes',
        main='Gráfico de barras del porcentaje de inercia explicada por cada componente',ylab = '% de varianza explicado')#grafico del porcentaje explicado

#resultados pca
lp<-PCA.results$eig[,1] #Valores propios
CI<-PCA.results$ind$coord #Componentes individuos
CV<-PCA.results$var$coord #Componentes variables
VI<-sqrt(PCA.results$ind$contrib)/10 #Vectores propios individuos
VV<-sqrt(PCA.results$var$contrib)/10 #Vectores propios variables


#1)nube de individuos
X11()
plot(PCA.results)#grafico de los individuos dos primeras dimenciones
#2)Circulo de correlaciones
x11()
plot(PCA.results,choix="var")
#3) representacion simultanea
library(factoextra)
x11()
fviz_pca_biplot(PCA.results)
#4)contribuciones y cosenos cuadrados
PCA.results$var$cos2 #cosenos cuadrados
PCA.results$var$contrib #contribuciones
#f) Descriptivas
summary(provincias[,-1])
X11()
boxplot(provincias[,-1],xlab='Variable',ylab='Presupuesto Familiar',main='Gráfico de cajas del presupuesto familiar por variable')
#Matriz de correlaciones
cor(provincias[,-1])

#Indice
I=PCA.results$var$coord[,1]%*%t(provincias[,-1]) 


sqrt(PCA.results$var$contrib[,1])/10 ##Sacar los vectores propios a partir de la funcion pca

#Relaciones de transición
N<-diag(1/51,nrow = 51,ncol = 51)
N1.2<-sqrt(N)

#Construción de Z
sd2 <- function (x) {
  
  sqrt(sum((x - mean(x))^2) / (length(x)))
  
} 
x1<-provincias$X1
x2<-provincias$X2
x3<-provincias$X3
x4<-provincias$X4
x5<-provincias$X5
x6<-provincias$X6
x7<-provincias$X7
x8<-provincias$X8
x9<-provincias$X9

#Estandarización

z1 <- (x1 - mean(x1))/sd2(x1) 
z2 <- (x2 - mean(x2))/sd2(x2)
z3 <- (x3 - mean(x3))/sd2(x3)
z4 <- (x4 - mean(x4))/sd2(x4)
z5 <- (x5 - mean(x5))/sd2(x5)
z6 <- (x6 - mean(x6))/sd2(x6)
z7 <- (x7 - mean(x7))/sd2(x7)
z8 <- (x8 - mean(x8))/sd2(x8)
z9 <- (x9 - mean(x9))/sd2(x9)

Z <- matrix(c(z1,z2,z3,z4,z5,z6,z7,z8,z9),51,9)

svd(N1.2%*%Z) #Vectores propios
u<-svd(N1.2%*%Z)$v#Indivduos
v<-svd(N1.2%*%Z)$u #Variables
l<-svd(N1.2%*%Z)$d #Valores propios

eigen(cor(provincias[,-1])) #Otra forma de calcular vectores propios (cambia algunos signos)

#Relaciones de transición
#Variables
C1V<-sqrt(l[1])*u[,1]  #Componente 1 variables Relación de transición
C2V<-sqrt(l[2])*u[,2]  #Componente 2 variables Relación de transición
PCA.results$var$coord[,1] #Componente 1 variables Función PCA
PCA.results$var$coord[,2] #Componente 2 variables Función PCA

#Individuos
C1I<-sqrt(l[1])*solve(N1.2)%*%v[,1]  #Componente 1 variables Relación de transición
C2I<-sqrt(l[2])*solve(N1.2)%*%v[,2]  #Componente 2 variables Relación de transición
PCA.results$ind$coord[,1] #Componente 1 variables Función PCA
PCA.results$ind$coord[,2] #Componente 2 variables Función PCA

for (i in 1:9) {
  suma<-sum(sd2(PCA.results$var$coord[,i])^2)  
}

sd2(PCA.results$ind$coord[,1])^2 #La varianza en las componentes de los individuos si da el valor propio
c(sd2(PCA.results$var$coord[,1])^2,
sd2(PCA.results$var$coord[,2])^2,
sd2(PCA.results$var$coord[,3])^2,
sd2(PCA.results$var$coord[,4])^2,
sd2(PCA.results$var$coord[,5])^2,
sd2(PCA.results$var$coord[,6])^2,
sd2(PCA.results$var$coord[,7])^2,
sd2(PCA.results$var$coord[,8])^2,
sd2(PCA.results$var$coord[,9])^2)
