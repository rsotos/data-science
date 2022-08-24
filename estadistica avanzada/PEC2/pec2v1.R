#Se carga el archivo csv en un data frame
#Para las variables no numéricas se crean factores, el separador
setwd("D:/uoc/estadistica avanzada/PEC2/" );
Salarios <-
  read.table("CensusIncome_clean.csv",
             header=TRUE, stringsAsFactors=TRUE, sep=",", na.strings="NA", dec=".",
             strip.white=TRUE)
#Se comprueba que se ha leido bien la tabla

str(Salarios)

#Haciendo un histograma de frecuencias puede verse  cierta similitud con una distribución normal pero con una  asimetría positiva

hist(Salarios$age, breaks="Sturges", col="ivory",ylab="Frecuencia",xlab="age",title="Histograma para variable age")

#Considerando el teorema del límite central, dado que el tamañano de la muestra de edades que disponemos,
# podemos asumir que ste teorema establece que el contraste de hipótesis sobre la media de una muestra 
#se aproxima a una distribución normal 

#Otra alternativa es susar el test de shapiro
#para ello seleccionamos una subconjunto con 5000 muestras
age_sample<-sample(Salarios$age,5000)
shapiro.test(age_sample)
#El marcador es proximo a 1 y el p valor muy bajo
#por tanto podemos asumir la normalidad de los datos de la variable age

#definimos la funcion  Intervalo de confianza de la media

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

#El intervalo de confianza para un valor de NC indiquca que 
# la media de la población se encuentra en dicho intervalo
#con una confianza del NC*100%
#Por eso cuando aumentamos el valor del NC aumenta el rango 
#del intervalo

IC(Salarios$age,0.9)
IC(Salarios$age,0.95)


# 2 Salario

#¿El salario medio de para los trabajadores autónomos es inferior que el del resto de trabajadores?

#Planteamos la hipótesis nula y la hipótiesis alternativa:

#H0 mu1=mu2
#H1 mu1<mu2

#siendo mu1 la media del salario de las personas self-employed y mu2 la media del resto de la población

#Aquí podemos aplicar un contraste de hipótesis de dos muestras independienttes (self-employed y el resto de categorías profesionales)
#sobre la media. Como no tenmos información  la varianza es desconocida.

#PAra ver si las varianzas son iguales o no Hacemos un F-Test



#usamos la funcion var.test, que hace el siguiente contraste de hipótesis bilateral con las varianzas de cada muestra
# H0 var1=var2
# H1 var1!=var2


muestra1<-Salarios$income[Salarios$workclass=="Self-Employed"]
muestra2<-Salarios$income[Salarios$workclass!="Self-Employed"]

var.test(muestra1, muestra2)

#El p-valor es inferior al nivel de significancia 0.05
# Por lo tanto no podemos rechazar la hipótesis nula y asumimos que las varianzas de las muestras son  homogéneas


varianzas <- function( x1, x2, NC=0.95 ){
  
  grados<-n1+n2-2
  x1<-muestra1
  x2<-muestra2
  alfa<-1-NC
  n1<-length(x1)
  n2<-length(x2)
  s1<-sd(x1)
  s2<-sd(x2)
  m1<-mean(x1)
  m2<-mean(x2)
  S<-sqrt(((n1-1)*s1^2+ (n2-1)*s2^2)/(n1+n2-2))
  z_obs<-(m1-m2)/(S*sqrt((1/n1)+(1/n2)))
  z_crit<-qt(1-alfa,df=grados,lower.tail = TRUE)
  p_valor<-pt(z_obs,df=grados,lower.tail = TRUE)
  
 # return (c(z_obs,z_crit,p_value))
  return (data.frame(z_obs=z_obs,z_crit=z_crit,p_valor=p_valor))
  
  
}
varianzas(muestra1,muestra2,0.9)
t.test(muestra1,muestra2,alternative="less", var.equal=TRUE,conf.level=0.9)
varianzas(muestra1,muestra2,0.95)
t.test(muestra1,muestra2,alternative="less", var.equal=TRUE)






