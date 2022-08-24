library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(Rcmdr)
library(faraway)
library(stats)
library(agricolae)
library(kableExtra)
#1
setwd("D:/uoc/estadistica avanzada/PEC4/" );

gpa <- 
  read.table("gpa.csv", 
             header=TRUE, stringsAsFactors=TRUE, sep=",", na.strings="NA", dec=".", 
             strip.white=TRUE)
head(gpa)
str(gpa)


#1.1

class(gpa$tothrs)
as.character(gpa$tothrs)

gpa$tothrs<-extract_numeric(gpa$tothrs)


#1.2

summary(gpa)


gpa$colgpa
dim(gpa)[1]


sum(is.na(gpa))/dim(gpa)[1]
NA.reg <- which(is.na(gpa$colgpa))
gpaclean <- gpa[-NA.reg,]
dim(gpaclean)



#1.3


gpaclean$gpaletter <- NULL
gpaclean$gpaletter[ gpaclean$colgpa >= 3.5 & gpaclean$colgpa <= 4 ] <- "A"
gpaclean$gpaletter[ gpaclean$colgpa >= 2.5 & gpaclean$colgpa < 3.5 ] <- "B"
gpaclean$gpaletter[ gpaclean$colgpa >= 1.5 & gpaclean$colgpa < 2.5] <- "C"
gpaclean$gpaletter[ gpaclean$colgpa >= 0 & gpaclean$colgpa < 1.5] <- "D"



#2.1

summary(gpaclean)

estudio<- select(gpaclean, sat,tothrs,hsize,hsrank,hsperc,colgpa)
media_aritmetica<-apply(estudio, 2, mean, na.rm = TRUE)
mediana<-apply(estudio, 2, median, na.rm = TRUE)
desviacion_estandar<-apply(estudio, 2, sd, na.rm = TRUE)
desviacion_abs_med<-apply(estudio, 2, mad, na.rm = TRUE)
rango_interc<-apply(estudio, 2, IQR, na.rm = TRUE)
resumen<-rbind(media_aritmetica,mediana,desviacion_estandar,desviacion_abs_med,rango_interc)
resumen

#2.2.1


ggplot(gpaclean, aes(x="", y=sat)) + 
  geom_boxplot()

ggplot(gpaclean, aes(x=sat, y=female)) + 
  geom_boxplot()
ggplot(gpaclean, aes(x=sat, y=athlete)) + 
  geom_boxplot()
ggplot(gpaclean, aes(x=sat, y=gpaletter)) + 
  geom_boxplot()

#2.2.2



gpaclean$excelente<-1*(gpaclean$gpaletter=="A")

Barplot(as.factor(gpaclean$excelente), style="divided", 
        legend.pos="above", xlab="Calificaciones excelentes", ylab="Porcentaje", scale="percent", 
        label.bars=TRUE, col=rainbow(2))
#2.2.3


#3.1 a


IC <- function( x, NC=0.9 ){
  alfa<-1-NC
  n<-length(x)
  desv<-sd(x)
  media<-mean(x)
  
  #Z contiene el valor para el que se cumple que P(X>z)=alfa/2
  z<-qnorm (alfa/2,lower.tail=FALSE)
  
  #Se devuelven los dos valores del intervalo
  
  return(c(media-(z*desv/sqrt(n)),media+(z*desv/sqrt(n))))
  
}

IC(gpaclean$sat,0.95)
t.test(gpaclean$sat)
#El intervalo de confianza para un valor de NC indica que la media de la población se encuentra en dicho intervalo con una confianza del NC*100%

#3.1 b 

sat_hombres<-filter(gpaclean,female==FALSE)%>%select(sat)

sat_mujeres<-filter(gpaclean,female==TRUE)%>%select(sat)

IC(sat_hombres$sat,0.95)
IC(sat_mujeres$sat,0.95)
# el rango es de uns 12 unidades y existe una diferncia de unosos 43 puntos ente ambos talcomo se ve en la media calculada y en los diagramas de caja anteriores







#3.2

# 3 Salario
# 3.1 Pregunta
# 
# ¿La nota media semestral para las estudientes mujeres es distinta que la de los estudiantes hombres?
#   3.2 Hipótesis
# 
# Planteamos la hipótesis nula y la hipótesis alternativa:
# 
#   H0 mu1=mu2
# 
# H1 mu1!=mu2
# 
# 
# 3.3 Test a aplicar
# 
# Aquí podemos aplicar un contraste de hipótesis de dos muestras independientes (estudiantes mujeres y estudiantes hombres) sobre la media.
# 
# Como no tenemos información la varianza es desconocida.
# 
# Para ver si las varianzas son iguales o no Hacemos un F-Test usamos la función var.test,
# 
# que hace el siguiente contraste de hipótesis bilateral con las varianzas de cada muestra:
# 
#   H0 var1=var2
# 
# H1 var1!=var2




colgpa_hombres<-filter(gpaclean,female==FALSE)%>%select(colgpa)

colgpa_mujeres<-filter(gpaclean,female==TRUE)%>%select(colgpa)


var.test(colgpa_mujeres$colgpa, colgpa_hombres$colgpa)



#el pvalor es menor a 0.05 luego descartamos la hipótesis nula y asumimos que las varianzas muestrales son distintas para un nivel de confianza del 0.95





medias <- function( x1, x2, NC=0.95 ){
  
  alfa<-1-NC
  n1<-length(x1)
  n2<-length(x2)
  s1<-sd(x1)
  s2<-sd(x2)
  m1<-mean(x1)
  m2<-mean(x2)

  denominador<-((s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1))
  numerador <- (s1^2/n1 + s2^2/n2)^2
  grados <- numerador /denominador
  t_obs<-(m1-m2)/sqrt( s1^2/n1 + s2^2/n2 )
  t_crit<-qt(alfa/2,df=grados,lower.tail = FALSE)*2
  p_valor<-pt(abs(t_obs),df=grados,lower.tail = FALSE)
  
  
  return (data.frame(NC=NC,t_obs=t_obs,t_crit=t_crit,p_valor=p_valor))
}


 medias<-medias(colgpa_mujeres$colgpa, colgpa_hombres$colgpa)
medias %> % kable( caption="¿El salario?")
 t.test(colgpa_mujeres$colgpa, colgpa_hombres$colgpa)
 
 
 
 #4
 

 
model <- lm(colgpa ~ sat + female + tothrs +athlete +hsperc, data = gpaclean)

#4.1
summary(model)
vif(model)
res <- resid(model)

plot(fitted(model), res)
abline(0,0)

qqnorm(res)


qqline(res)

plot(density(res))
     
#4.2

# previo, aplicad el modelo de regresión para predecir la
# nota media de un estudiante hombre, atleta, con una nota de entrada de 800, un total de horas en el semestre
# de 60 y una posición relativa en el ranking del 60 %.

newdf <- data.frame(female=c(FALSE),athlete=c(TRUE),sat=c(800),tothrs=c(60),hsperc=c(60))
predict(model,newdf)



#5 
# Estimad un modelo logístico para predecir la probabilidad de ser un estudiante excelente al final del primer
# semestre en la universidad en función de las variables: female, athlete, sat, tothrs, black, white y hsperc.

#5.1
gpaclean$excelente<-as.factor(gpaclean$excelente)
model_2=glm(formula=excelente~female+athlete+sat+tothrs+black+white+hsperc,family=binomial(link=logit),data=gpaclean)



summary(model_2)

#5.2
model_2=glm(formula=excelente~female+sat+tothrs+black+white+hsperc,family=binomial(link=logit),data=gpaclean)
model_2=glm(formula=excelente~female+sat+tothrs+black+hsperc,family=binomial(link=logit),data=gpaclean)

summary(model_2)

#5.3

levels(model_2$model$excelente)
exp(coefficients(model_2))
exp(confint(model_2))


#5.4

# Con que probabilidad una estudiante mujer, no atleta, con un sat de 1200 puntos, 50 horas cursadas, de
# raza negra y con un ranking relativo (hsperc) del 10% será excelente?

pred<-predict(model_2, data.frame(female=TRUE,athlete=FALSE,sat=1200, tothrs=50, black=TRUE,hsperc=10),type ="response")
pred
  
  
  

#6

gpaclean$race<-NULL
gpaclean[which(gpaclean$black==TRUE),c("race")]<-"black"
gpaclean[which(gpaclean$white==TRUE),c("race")]<-"white"
gpaclean[which((gpaclean$white==FALSE)&(gpaclean$black==FALSE)),c("race")]<-"other"



gpaclean$race<-as.factor(gpaclean$race)


#6.1
boxplot(colgpa~race,data=gpaclean,xlab="Raza", ylab="Nota media")


#6.2

# Nos planteamos  si el factor raza es significativo para distribución de colgpa
# 
# La hipótesi nula no lo es y en la impotésis alternativa existen al menos dos niveles cuyos efectos son significativamente distintos
# 
#   H0: alfa0=alfa1=alfa2=0
#   H1: alfai != alfaj para i != j
# 
# 
#   alfa0 es el efecto del nivel black
#   alfa1 es el efecto del nivel white
#   alfa2 es el efecto del nivel other
# 


#6.3


modelo_cg_race<-lm(colgpa~race,data=gpaclean)
taov<-anova(modelo_cg_race)
taov
# Treatments 	SST 	k???1 	SST/(k???1) 	MST/MSE
# Error 	SSE 	N???k 	SSE/(N???k)

#we reject the null hypothesis of equal population means and conclude that there is a (statistically) significant difference among the population means
#What we do not know at this point is whether the three means are all different or which of the three means is different from the other two, and by how much. 
#6.4 y ¿6.5?


pairwise.t.test(gpaclean$colgpa,gpaclean$race, p.adj=c("none"))
#Realicemos las comparaciones múltiples con la corrección de Bonferroni.
LSD.test(modelo_cg_race,"race",group=F,p.adj="bonferroni",console=T)
LSD.test(modelo_cg_race,"race",group=T,p.adj="bonferroni",console=T)


#6.6
res <- resid(modelo_cg_race)
shapiro.test(res)
qqnorm(residuals(results))
qqline(residuals(results))




# plot(fitted(model), res)
# abline(0,0)

qqnorm(res)


qqline(res)

plot(density(res))
#la hipótesis nula es aquella que afirma que la distribución es normal):

#6.7


#Fit an analysis of variance model by a call to lm for each stratum.
#modelo_aov<- aov(colgpa ~ race, data = gpaclean)



sum(gpaclean$race=="white")
sum(gpaclean$race=="black")
sum(gpaclean$race=="other")


plot(modelo_cg_race, 1)
# plot(fitted(modelo_cg_race),res)
# abline(0,0)

# Observamos tres tiras verticales de puntos que están situadas en las medias de cada grupo.
# Como hemos dicho, estas corresponden a los valores ajustados de las observaciones. La
# disposición de los residuos muestra una dispersión parecida en cada tira. Por

bartlett.test(colgpa~race,data=gpaclean)

modelo_cg_race$fitted.values


newdf <- data.frame(race="white")
predict(modelo_cg_race,newdf)
newdf <- data.frame(race="black")
predict(modelo_cg_race,newdf)
newdf <- data.frame(race="other")
predict(modelo_cg_race,newdf)




#7.1

interaction.plot(gpaclean$female,gpaclean$race,gpaclean$colgpa)


#7.2

#Fit an analysis of variance model by a call to lm for each stratum.
modelo_aov<- aov(colgpa ~ race+female, data = gpaclean)


#7.3

modelo_aov
plot(modelo_aov, which = 1)
plot(modelo_aov, which = 2)
shapiro.test(residuals(modelo_aov))
anova(modelo_aov)
