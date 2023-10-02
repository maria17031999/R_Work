##MARÍA LÓPEZ MORENO
##ESTADÍSTICA COMPUTACIONAL I

##Con ayuda de los datos swiss disponibles en el libro base de R

data(swiss)
head(swiss)

attach(swiss)


##I. Gráficos de dispersión
##a) Hacer un gráfico de puntos de las variables Fertility frente Agriculture.
plot(Fertility, Agriculture)

##b) Cambiar las etiquetas de los ejes por unas apropiadas al análisis.
plot(Fertility, Agriculture, xlab="Fertilidad", 
	ylab="Porcentaje agricultura")


##plot.new()

##c) Cambiar los puntos por defecto por el carácter "*" y ponerlos de color azul.
plot(Fertility, Agriculture, xlab="Fertilidad", 
	ylab="Porcentaje de agricultura", pch=42, col="blue")

##d) Poner los límites de los dos ejes de 0 a 100.
plot(Fertility, Agriculture, xlab="Fertilidad", 
	ylab="Porcentaje de agricultura", pch=42, col="blue", xlim=c(0,100), 
	ylim=c(0,100))

##e) Añadir título y subtítulo a la gráfica.
plot(Fertility, Agriculture, xlab="Fertilidad", 
	ylab="Porcentaje de agricultura", main="Relación la fertilidad y 
	el % de agricultores", cex.main=1.5, sub="Estudio en diferentes regiones
	de Suiza en 1888",cex.sub=0.4, pch=42, col="blue", xlim=c(0,100), 
	ylim=c(0,100))

##Conclusiones

r_plot <- lm(Agriculture~Fertility) ##la llamo plot pq es del ejercicio de plot

summary(lm(Agriculture~Fertility))

abline(r_plot)

##otros gráficos
library(tidyverse)
library(ggplot2)

ggplot(swiss, aes(Fertility, Agriculture)) +
	geom_point(shape = 42) +
	geom_smooth(se = FALSE) +
	geom_smooth(method = 'lm', se=FALSE) +
	labs(title = "Relación fertilidad y % agricultores",
	     subtitle = "Estudio en diferentes regiones de Suiza en 1888",
	     caption = "De los datos swiss",
	     x = "Fertilidad",
	     y = "Porcentaje de agricultura") +
	scale_x_continuous(limits = c(0,100))

loess(Agriculture~Fertility)	
summary(loess(Agriculture~Fertility))	 


## II. Boxplot
##a) Construir un boxplot conjunto de todas las variables del data frame.
par(mar=c(5, 4, 4, 9), xpd=TRUE) ## Cambiamos mar para hacer hueco a la leyenda
						##y xpd=T para  

boxplot(Agriculture, Catholic, Education, Examination, Fertility, Infant.Mortality,
	names=c("A","B","C","D","E","F"),main="Boxplot de las variables de swiss")

##letters[1:7]

legend("topright", lwd=0.5, inset=c(-0.58,0),
	legend=c("A: Agricultura", "B: Catolicismo", "C: Educación",
	"D: Examinación", "E: Fertilidad", "F: Mort. inf"))

#plot.new()

##par(mfrow=c(3,2)) ##La ventana gráfica tendrá 3 filas y 2 columnas
##boxplot(Agriculture, main="Agricultura")
##boxplot(Catholic, main="Catolicismo")
##boxplot(Education, main="Educación")
##boxplot(Examination, main="Examinación")
##boxplot(Fertility, main="Fertility")
##boxplot(Infant.Mortality, main="Mortalidad infantil")

##b) Cambiar los colores de las cajas (rojo) y de las líneas que rodean las
## cajas (verde)
boxplot(Agriculture, Catholic, Education, Examination, Fertility, Infant.Mortality,
	names=c("A","B","C","D","E","F"), main="Boxplot de las variables de swiss",
	 border="green", col="red")


legend("topright", lwd=0.5, inset=c(-0.52,0),
	legend=c("A: Agricultura", "B: Catolicismo", "C: Educación",
	"D: Examinación", "E: Fertilidad", "F: Mort. inf"))



##c) Modificar la longitud de los bigotes para que se ajusten a las 
## observaciones más extremas.
boxplot(Agriculture, Catholic, Education, Examination, Fertility, Infant.Mortality,
	names=c("A","B","C","D","E","F"),main="Boxplot de las variables de swiss", 
	border="green", col="red",range=0)
	
legend("topright", lwd=0.5, inset=c(-0.48,0),
	legend=c("A: Agricultura", "B: Catolicismo", "C: Educación",
	"D: Examinación", "E: Fertilidad", "F: Mort. inf"))

##d) Hacer que el boxplot sea horizontal.
boxplot(Agriculture, Catholic, Education, Examination, Fertility, Infant.Mortality,
	names=c("A","B","C","D","E","F"),main="Boxplot de las variables de swiss", 
	border="green", col="red",range=0, horizontal=TRUE)

legend("topright", lwd=0.5, inset=c(-0.48,0),
	legend=c("A: Agricultura", "B: Catolicismo", "C: Educación",
	"D: Examinación", "E: Fertilidad", "F: Mort. inf"))

##e) Hacer que las cajas tengan distintos anchos.
boxplot(Agriculture, Catholic, Education, Examination, Fertility, Infant.Mortality,
	names=c("A","B","C","D","E","F"),main="Boxplot de las variables de swiss", 
	border="green", col="red",range=0, horizontal=TRUE, width = c(1,3,5,7,9,11))

legend("topright", lwd=0.5, inset=c(-0.48,0),
	legend=c("A: Agricultura", "B: Catolicismo", "C: Educación",
	"D: Examinación", "E: Fertilidad", "F: Mort. inf"))

##f) Construir el boxplot tipo notch
boxplot(Agriculture, Catholic, Education, Examination, Fertility, Infant.Mortality,
	names=c("A","B","C","D","E","F"),main="Boxplot de las variables de swiss", 
	border="green", col="red",range=0, horizontal=TRUE, width = c(1,3,5,7,9,11),
	notch=TRUE)

legend("topright", lwd=0.5, inset=c(-0.48,0),
	legend=c("A: Agricultura", "B: Catolicismo", "C: Educación",
	"D: Examinación", "E: Fertilidad", "F: Mort. inf"))



## III. Histograma

##a) Construir el histograma de la variable Education.
hist(Education, xlab="Nivel de educación", ylab="Frecuencia absoluta",
	main="Histograma de la variable Educación", sub="De los datos Swiss",
	cex.sub=0.5)

##b) Cambiar los colores de las barras.
colores <- c(rgb(1,0,0.5,0.3),rgb(1,0,0.8,0.5),rgb(0.5,0,0.8,0.7),
	rgb(0.2,0,1,0.9),rgb(0.2,0.5,1), rgb(0.7,0,0.7,0.1))
hist(Education, xlab="Nivel de educación", ylab="Frecuencia absoluta",
	main="Histograma de la variable Educación", sub="De los datos Swiss",
	cex.sub=0.5, col=1:6)

##c) Poner los puntos de corte en los puntos (0, 10, 20, ..., 100).
hist(Education, xlab="Nivel de educación", ylab="Frecuencia absoluta",
	main="Histograma de la variable Educación", sub="De los datos Swiss",
	cex.sub=0.5, col=colores, breaks=c(0,20,40,60,80,100))

##d) Pintar la línea de densidad de los datos encima del histograma 
##(para realizar esta acción es necesario pasa el gráfico a modo "probabilidad").

hist(Education, xlab="Nivel de educación", ylab="Frecuencia relativa",
	main="Histograma de la variable Educación", sub="De los datos Swiss",
	cex.sub=0.5, col=colores, ylim=c(0,0.08),breaks=c(0,20,40,60,80,100), 
	freq=FALSE)

densidad_educacion <- density(Education)

lines(densidad_educacion,col=rgb(0.8,0,0),lwd=2)

ggplot(swiss, aes(Education)) +
	geom_histogram(col='black', fill='pink') +
	labs(title = "Histograma variable Educación",
	     subtitle = "Estudio en diferentes regiones de Suiza en 1888",
	     caption = "De los datos swiss",
	     x = "Nivel educación",
	     y = "Frecuencia relativa")
	

	


## IV. Otros gráficos

##a) Mediante la instrucción cut, discretizar las variables Catholic y 
##Agriculture en cuatro grupos de tamaño similar. Hacer un coplot de Fertility 
##frente Education teniendo en cuenta esas dos covariables.

##nclass.Sturges(Catholic)
##nclass.Sturges(Agriculture)

##range(Catholic, na.rm=T)    //Ver el rango de la variable Catholic (max,min)
##range(Agriculture, na.rm=T)

int <- max(Catholic) - min(Catholic)
division <- int/4
division

int <- max(Agriculture) - min(Agriculture)
division2 <- int/4
division2

m1 <- min(Catholic)
m2 <- min(Agriculture)

catholic2 <-cut(Catholic, breaks=c(m1, m1+division, m1+2*division,
	m1+3*division, m1+4*division))

agriculture2 <- cut(Agriculture, breaks=c(m2, m2+division2, m2+2*division2,
	 m2+3*division2, m2 + 4*division2))

#coplot(Fertility ~ Education| Catholic * Agriculture)

coplot(Fertility ~ Education| catholic2 * agriculture2)

#catholic3 <-cut(Catholic, breaks=seq(0,100,by=25))
#agriculture3 <- cut(Agriculture, breaks=seq(0,100,by=25))

#coplot(Fertility ~ Education| catholic3 * agriculture3)


#table(Catholic, useNA="ifany")
#table(Agriculture, useNA="ifany")

##b) Practicar otros gráficos y opciones con estos y otros datos. Poner leyendas,
##añadir nuevos puntos/líneas a los gráficos, cambiarlos tipos de líneas, los 
##ejes, colores, etc...

install.packages("plotrix")
library(plotrix)

str(swiss)


## histStack
factor_c <- cut(Catholic, breaks=c(m1, m1+division, m1+2*division,
	m1+3*division, m1+4*division))

str(factor_c)

max(Catholic)
levels(Catholic)
levels(factor_c)
histStack(Agriculture ~ factor_c,swiss, col=colores, xlab="eje x", ylab="eje y",
	main="Relación entre agricultura y catolicismo")
	
legend("topleft", fill = colores, title="Leyenda",
	legend=c("2.15%-26.6%","26.6%-51.1%","51.1%-75.5%","75.5%-100%"))


##podemos hacer algo parecido con la variable educación

range(Education)

eje_ed <- c(1,15,30,45,60)

c <- cut(Education, breaks=eje_ed)


histStack(Infant.Mortality ~ c, swiss,
	col=colores, xlab="eje x", ylab="eje y",
	main="Relación entre educación y mortalidad infantil")

legend("topleft", fill = colores, title="Nivel de ed.",
	legend=c("Muy bajo","Bajo","Medio","Alto"))

## pie3D

etiquetas <- c("Muy bajo","Bajo","Medio","Alto")
c2 <- table(c)

pie3D(c2,labels=etiquetas,main="Niveles de educacion",col=colores,
	explode=0.1,labelcol="black",border="white")



##vamos a leer los datos proporcionados en clase

datos <- read.csv("2Sesion_ECI_datos.txt")

head(datos)
str(datos)
attach(datos)
c2 <- table(continent)

etiquetas2 <- c("Africa", "Americas", "Asia", "Europe", "Oceania")

colores <- c(rgb(1,0,0.5,0.3),rgb(1,0,0.8,0.5),rgb(0.5,0,0.8,0.7),
	rgb(0.2,0,1,0.9),rgb(0.2,0.5,1), rgb(0.7,0,0.7,0.1))

pie3D(c2,labels=etiquetas2,main="Registro según continente",col=colores,
	labelcol="grey",border="white", cex.main=1.2, mar=c(4,4,4,8))


detach(datos)



## barp()

Infant.Mortality

range(Infant.Mortality)
eje_mort <- c(10.8,14.75,18.7,22.65,26.6)
nombres <- c("Bajo","Medio","Alto","Muy alto","Extremo")
nombres2 <- c("R1","R2","R3","R4","R5","R6","R7")

n <- length(Infant.Mortality)
s <- sample(1:n, 7, replace=FALSE)
regiones1 <- Infant.Mortality[s]

barp(regiones1, names.arg=nombres2, col=colores, xlab="Regiones", 
	 main="Mortalidad", height.lab=nombres, height.at=eje_mort,
	ylab="% mortalidad infantil",shadow=TRUE,cylindrical=TRUE)

s2 <- sample(1:n, 7, replace=FALSE)
regiones2 <- Infant.Mortality[s2]

regiones3 <- rbind(regiones1, regiones2)

barp(regiones3, names.arg=nombres2, xlab="Regiones", 
	 main="Mortalidad infantil", col=c("pink","purple"), 
	ylab="% mortalidad", height.at=eje_mort)

detach(swiss)

## f(x)=(96-8pi^2-96sin(x))/(3pi^2)
x <- seq(0, 2*pi+0.1, 0.05)
y <- (96-8*(pi^2)-96*sin(x))/(3*(pi^2))

abline(v=0, col="red")
abline(v=2*pi, col="red")
abline(v=pi, col="red")

abline(h=0, col="green")
abline(h=pi/2, col="green")

plot(x,y, xlim=c(0,2*pi), ylim=c(-2*pi,2*pi), type="l", lwd=2,
	main="Estudio poblacional")

polygon(x=c(0,pi,pi,0), y=c(0,0,pi/2,pi/2), col=rgb(1,0,0.5,0.3))

minimox <- min(which(x >=0))
maximox <- max(which(x <=0.2))
polygon(x = c(x[c(minimox, minimox:maximox, maximox)]), y = c(0, 
    y[minimox:maximox], 0), col = rgb(0, 1, 0, 0.5))

minimox2 <- min(which(x>=pi-0.2))
maximox2 <- max(which(x<=pi+0.05))
polygon(x = c(x[c(minimox2, minimox2:maximox2, maximox2)]), y = c(0, 
    y[minimox2:maximox2], 0), col = rgb(0, 1, 0, 0.5))

colores <- c(rgb(0, 1, 0, 0.5),rgb(1,0,0.5,0.3), "black")

legend("bottomleft", col=colores, legend=c("Extinción", "Crecimiento ilimtado",
	"Se estabiliza"), lwd=3)

lines(x,sin(x),col="blue",lwd=3)

locator(2)

##########

##ggplot

library(tidyverse)
library(ggplot2)


data(package = .packages(all.available = TRUE))

library(dslabs)
data(death_prob)
head(death_prob)
attach(death_prob)
levels(sex) ##ver qué sexos se tienen en cuenta
ggplot(data=death_prob, aes(x=age, y=prob, color=sex)) +
	geom_line() +
	geom_point()


##########
##Relación entre bases robadas y carreras anotadas

library(Lahman)
data(Teams)
attach(Teams)
?Teams

min(yearID)
max(yearID)

equipos <- Teams[Teams$yearID>=1980 && Teams$yearID<=2001]
SBxjuego <- SB / G
Rxjuego <- R / G


ggplot(equipos, aes(SBxjuego, Rxjuego, color=WSWin)) + 
  geom_point()

##########

RLU <- c(0.11, 0.22, 0.40, 0.79, 1.59, 2.84, 9.98)
conc <- c(0.16, 0.32, 0.63, 1.25, 2.5, 5, 10)
datos <- data.frame(RLU, conc)

library(ggplot2)
ggplot(datos, aes(x=RLU, y=conc)) + 
  geom_point() + theme_light()

mod1 <- lm(conc ~ RLU)
mod1

mod2 <- lm(conc ~ RLU + I(RLU^4))
mod2

mod3 <- poly(conc ~ RLU + I(RLU^2) + I(RLU^3) +I(RLU^4))
mod3


ggplot(datos, aes(x=RLU, y=conc)) + 
  geom_point() +
  geom_smooth(method='lm', formula=y~x, se=F, col=rgb(0.7,0,0.3)) +
  geom_smooth(method='lm', formula=y~x+I(x^4), se=F, col=rgb(1,0,0.5)) +
  geom_smooth(method='lm', formula=y~x+I(x^2)+I(x^3)+I(x^4), se=F, 
	col=rgb(0.2,0,0.8))





##EJERCICIO 2

##A partir del conjunto de datos VADeaths disponible en el libro base de R:
data(VADeaths)
head(VADeaths)


str(VADeaths)

?VADeaths

##a) Describir el conjunto de datos


##b) Construir una tabla con la información recogida en dicho conjunto
## de datos.

data_frame <- as.data.frame.table(VADeaths)  
head(data_frame)
str(data_frame)
attach(data_frame)


##c) Determinar las frecuencias marginales para cada variable


margin.table(VADeaths)

v1 <- margin.table(VADeaths,1)    ##marginal grupos de edad
v1
    
v2 <- margin.table(VADeaths,2)  ##marginal grupos de zonas
v2


margin.table(VADeaths,1)/margin.table(VADeaths) #frec. marginal relativa
margin.table(VADeaths,2)/margin.table(VADeaths)

margin.table(VADeaths,1)/1000

prop.table(VADeaths,1)  #condicionada edad
prop.table(VADeaths,2)  #condicionada zona
#prop.table(VADeaths)   #frecuencia relativa sin condicionar de cada suceso

v3 <- prop.table(VADeaths,1)
v4 <- prop.table(VADeaths,2)

#v4[,2]  #Ejemplo, condicionada a mujer urbana
#v3[3,]  #Ejemplo, condicionada a tener entre 60 y 64 años



##d) Construir el diagrama de barras para cada una de las variables

#coplot(Var1 ~ Freq | Var2, data = data_frame,
       #  main = "Gráfico de las 3 variables",
       #  ylab = "Var1")


barplot(VADeaths, main="Muertes (cada 1000 habs) - 1940",
         xlab="índice mortalidad (por 1000)", col=colores,
	legend.text=levels(Var1),args.legend=list(x="topright"))


barplot(v1, col=colores, ylab="Frecuencia absoluta",
	main="Índice mortalidad según años")

barplot(v2, col=colores, ylab="Frecuencia absoluta",
	main="Índice mortalidad según zonas")

par(mar=c(5,4,4,9),xpd=TRUE)

barplot(t(v3), col=colores, ylab="Probabilidad",
	main="Probabilidad condicionada de mortalidad según edad")

legend("topright", inset=c(-0.5,0),legend=levels(Var2), fill=colores) 


barplot(v4, col=colores, ylab="Probabilidad",
	main="Probabilidad condicionada de mortalidad según zona")

legend("topright", inset=c(-0.3,0), fill=colores, legend=levels(Var1))



pdf(file="GraficoTarea2.pdf")
par(mfrow=c(2,1))

barplot(v1, col=colores, ylab="Frecuencia absoluta",
	main="Índice mortalidad según años")

barplot(v2, col=colores, ylab="Frecuencia absoluta",
	main="Índice mortalidad según zonas")

dev.off()


detach(data_frame)





##EJERCICIO 3

##A partir del conjunto de datos thuesen de la librería ISwR:
library("ISwR")
data(thuesen)
head(thuesen)
str(thuesen)

##a) Describir el conjunto de datos
?thuesen

##b) Analizar la relación lineal entre las variables blood.glucose
## y short.velocity
attach(thuesen)

r <- lm(short.velocity ~ blood.glucose) ##Recta de regresión (rel. lineal)
r

##c) Determinar la bondad del ajuste

summary(r)

##es el porcentaje de la variación en la variable de respuesta que es 
## explicado por un modelo lineal i.e, miramos R2


plot(blood.glucose, short.velocity, xlab="Glucosa en sangre",
	ylab="Velocidad corta", main="Relación entre las variables",
	sub="de los datos thuesen", cex.sub=0.5)
abline(r)

ggplot(thuesen, aes(blood.glucose, short.velocity)) +
	geom_point() +
	geom_smooth(method = 'lm', se=FALSE) +
	labs(title = "Gráfico de dispersión",
	     subtitle = "Variables: glucosa en sangre y velocidad corta",
	     caption = "De los datos thuesen",
	     x = "Glucosa en sangre",
	     y = "Velocidad corta")



##residuos <- rstandard(r)
##hist(residuos)
##plot(fitted.values(r), rstandard(r), xlab="Valores ajustados",
##	ylab="Valores estandarizados")
##abline(h=0)

detach(thuesen)


