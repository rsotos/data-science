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

tabla<- rbind(IC(Salarios$age,0.9),
IC(Salarios$age,0.95))
as.character(tabla)

tabla %>%
  kbl() %>%
  kable_styling()


# 2 Salario

#¿La media poblacional del salario de  los trabajadores autónomos es inferior a la  del resto de trabajadores?

#Planteamos la hipótesis nula y la hipótiesis alternativa:

#H0 mu1=mu2
#H1 mu1<mu2

#siendo mu1 la media del salario de las personas self-employed y mu2 la media del resto de la población

#Aquí podemos aplicar un contraste de hipótesis de dos muestras independienttes (self-employed y el resto de categorías profesionales)
#sobre la media. Como no tenemos información  la varianza es desconocida.
#Se trata de  un contraste unilateral por la izquierda

#Para ver si las varianzas son iguales o no Hacemos un F-Test
#Usamos la funcion var.test, que hace el siguiente contraste de hipótesis bilateral con las varianzas de cada muestra
# H0 var1=var2
# H1 var1!=var2


muestra1<-Salarios$income[Salarios$workclass=="Self-Employed"]
muestra2<-Salarios$income[Salarios$workclass!="Self-Employed"]

var.test(muestra1, muestra2)

#El p-valor es inferior al nivel de significancia 0.05
# Por lo tanto no podemos rechazar la hipótesis nula y asumimos que las varianzas de las muestras son  iguales




#A continuación definimos la función que nos devolverá el p valor el valor observado y el valor crítico

varianzas <- function( x1, x2, NC=0.95 ){
  
  
 
  alfa<-1-NC
  n1<-length(x1)
  n2<-length(x2)
  grados<-n1+n2-2 #estos son los grados de la funcion t de student que usaremos para calcualr el p valor y el valor crítico
  s1<-sd(x1)
  s2<-sd(x2)
  m1<-mean(x1)
  m2<-mean(x2)
  S<-sqrt(((n1-1)*s1^2+ (n2-1)*s2^2)/(n1+n2-2)) #esta es la desviación típica común 
  z_obs<-(m1-m2)/(S*sqrt((1/n1)+(1/n2)))
  z_crit<-qt(alfa,df=grados,lower.tail = TRUE)
  p_valor<-pt(z_obs,df=grados,lower.tail = TRUE)
  
 
  return (data.frame(z_obs=z_obs,z_crit=z_crit,p_valor=p_valor))
  
  
}

#ahora invocamos la función dos veces con un NC de 0.9 y de 0.95 respectivamente
#comparamos con la funciónt.test para comprobar que coinciden el p valor y el valor observado
varianzas(muestra1,muestra2,0.90)
t.test(muestra1,muestra2,alternative="less", var.equal=TRUE,conf.level=0.9)
varianzas(muestra1,muestra2,0.95)
t.test(muestra1,muestra2,alternative="less", var.equal=TRUE)

#en ambos casos el p valor es no es menor que el nivel de significacncia
# ademas el valor observado es mayor que el valor crítico.
#por tanto no podemos rechazar la hipótesis nula  de quelas medias de ambas muestras son iguales

n1<-length(muestra1)
n2<-length(muestra2)
grados<-n1+n2-2
xpos <- seq(- 10, 10, by = 1)
plot(dt(xpos, df = grados),type="l")


#3 Proporción de Self-Employed

#¿La porporción de personas self-employed en la población es superior al 10%?
#El contraste de hipotesis seria el siguiente:
#H0: p=0.1
#H1: p>0.1
#donde p es la proporcion muestral de las personas self-employed

#En la grafica  de abajo observamos que en esta muestra se cumple la hipotesis alternativa
#Las personas Self-emloyed superan el 10%
library(lessR)
PieChart(workclass, data=Salarios, xlab="", ylab="", main="workclass", 
         col=rainbow(4), scale="percent")
#Planteamos un contraste de hipótesis sobre la proporción para muestras suficientemente grandes
#Se trata por tanto de un contraste unilateral por la derecha

#el estadistico de contraste que usaremos lo definiremos en la función "proporcion" y tendra como parámetros
# la proporción de la muestra, el paramatro poblacional (con valor 0,1) y el tamaño de la muestra
#este estadístico  seguira una distribución normal bajo la hipótesis nula


proporcion <- function( p_muestra,n, p0, NC=0.95 ){
  
  

  alfa<-1-NC
  #calculo del valor observado con el estadístico
  z_obs<-(p_muestra-p0)/sqrt(p0*(1-p0)/n)
  #como el estadisstico sigue una distribucion normal 
  #para calcular el valor critico  y el p valor se usan pnorm y qnorm
  z_crit<-qnorm(alfa,lower.tail = FALSE) 
  p_valor<-pnorm(z_obs,lower.tail = FALSE)
  
  
  return (data.frame(z_obs=z_obs,z_crit=z_crit,p_valor=p_valor))
  
  
}

#usamos muestra1, que contiene los self-employed
n<-nrow(Salarios)
p_muestra<-length(muestra1)/n
proporcion(p_muestra,n,0.1)

#el pvalor es menor que el nivel de significacncia (0.05) Y e valor observado es mayor que el valor critico
#esto nos lleva a rechazar la hipoteis nula y a afirmar que la proporción de self employed en la población 
#es superior al 10% con un niverl de confianza del 95%





#5 Proporcion de Self-Employed en mujeres y hombres

#¿La proporción de Self-Employed entre las muejres de la población es menor a la proporción de Self-Employed entre los hombres de la población?


if (!require('ggplot2')) install.packages('ggplot2'); library('ggplot2')


#HAcemos una gráfica de barras en la que se puede apreciar para la muestra dada que la porporción de Self-Employed 
#dentro del subconjunto de mujeres es claramente menor que dentro del subjonjunto de hombres 

ggplot(data=Salarios,aes(x=gender,fill=workclass))+geom_bar()+ggtitle("Porporción de workclass por gender")+labs(x="Gender")


#Pasamos a enunciar las hipótesis:
#H0: p1=p2
#H1: p1<p2


#siendo p1 proporcion de Self-Employed entre las mujeres y p2 la correspondiente entre los hombres de la población
#Haremos por tanto un contrste de proporción sobre dos muestras
# Considerararemos que las muestras son  lo suficiente grandes
#Se trata de  un contraste unilateral por la izquierda
# El estadístico de contraste lo definiremos en la funcion "proporciones"
#recibira como parametros las proporciones y tamaños de cada muestras asi como el nivel de confianza


proporciones <- function( p1,p2,n1, n2, NC=0.97 ){
  
  
  alfa<-1-NC
  
  
  p<-(n1*p1 + n2*p2) / (n1+n2)
  z_obs<-(p1-p2)/( sqrt(p*(1-p)*(1/n1+1/n2)) )
  z_crit<-qnorm(alfa, lower.tail=TRUE)
  p_valor<-pnorm(z_obs,lower.tail = TRUE)
  
  
  return (data.frame(z_obs=z_obs,z_crit=z_crit,p_valor=p_valor))
  
  
}

#Aquí preparamos  los parámetros

xf<-Salarios[(Salarios$gender=="f"),]
xm<-Salarios[(Salarios$gender=="m"),]

self_f<-xf[(xf$workclass=="Self-Employed"),]
self_m<-xm[(xm$workclass=="Self-Employed"),]
n1<-nrow(xf)
n2<-nrow(xm)
p1<-nrow(self_f)/n1
p2<-nrow(self_m)/n2

#llamamos a la función

tabla<-proporciones(p1,p2,n1,n2)

tabla$p_valor<-as.character(tabla$p_valor)




#el pvalor es inferior al nivel de significacncia
#También el valor observado es menor que el valor critico estando por tanto fuera del rango de aceptación
#De la hipótesis nula
# Portanto podemos afirmar con un nicel de confianza del 97% que la proporcion de self-employed en las mujeres es menor 
# a la porporcion de self-employed en los hombres

success<-c( p1*n1, p2*n2)
 nn<-c(n1,n2)
prop.test(success, nn, alternative="less", correct=FALSE)



#6 Dependencia Self-Employed- Gender

#¿ Las variables Self-Employed y Gender están relacionadas o son indenpendientes?

# Formulamos las siguientes hipótesis:

# H0: Self-Employed y Gender son independientes
# H1: Self-Employed y Gender están relacionadas

#Aplicaremos el del test chi cuadrado.
#Primero construiremos la tabal de contigencia:

Salarios$isSE <- ifelse( Salarios$workclass=="Self-Employed", "Self-Employed","Other")
tc<-table( Salarios$isSE, Salarios$gender )
tc







#Con estos datos podemos y con los valores esperados de la tabla de contingencia si las variables 
# no estuvieran relacionadas podemos calcular la formula que nos dará el valor observado
#Esta sigue una distribución Chi cuadrado con 1 grado de libertad.


testchicuadrado <- function( tc, NC=0.97 ){
  n<-sum(tc)
  e<-matrix(nrow = 2, ncol = 2)
  e[1,1]<-sum(tc[1,])*sum(tc[,1])/n
  e[1,2]<-sum(tc[1,])*sum(tc[,2])/n
  e[2,1]<-sum(tc[2,])*sum(tc[,1])/n
  e[2,2]<-sum(tc[2,])*sum(tc[,2])/n
  
  alfa<-1-NC
  
  z_obs<-sum((tc-e)^2/e)
  
 
  p_valor<-pchisq(z_obs,df=1)
  
  
  return (data.frame(z_obs=z_obs,p_valor=p_valor))
  
  
}

#vemos comparando con chisq.test que que se obtiene un valor observado aproximado. 
#el p valor es muy pequeño y no podemos rechazar la hipotesis nula
# Portanto con un nivel de confianza del 97%  podemos afirma que no se observa relacion entre 
testchicuadrado(tc)
chisq.test(tc, correct=TRUE)





library(kableExtra)
preguntas<- c("Intervalo de confianza de la media de edad al 95%",
              
              "¿La media poblacional del salario de  los trabajadores autónomos es inferior a la  del resto de trabajadores?",
              
              "¿La proporción de personas self-employed en la población es superior al 10%?",
              
              "¿La proporción de Self-Employed entre las mujeres de la población es menor a la proporción de Self-Employed entre los hombres de la población?",
              
              "¿ Las variables Self-Employed y Gender están relacionadas o son indenpendientes?"
              
              )

valores<- c("38.40277 38.69698",
            "z_obs: 1 5.77555, z_crit: -1.281578, pvalor: 1"
          
            )
nivelconf<-c("0.95","0.95","0.95","0.97","0.97")

Conclusiones<-c("El intervalo de confianza para un valor de NC indica que  la media de la población se encuentra en dicho intervalo con una confianza del NC*100%",
                "No podemos rechazar la hipótesis nula de que ambas medias poblacionales son iguales",
                "Rechazamos la hipoteis nula y a afirmar que la proporción de self employed en la población es superior al 10% con un nivel de confianza del 95%",
                "Podemos afirmar con un nivel de confianza del 97% que la proporción de self-employed en las mujeres es menor a la proporción de self-employed en los hombres",
                "Con un nivel de confianza del 97% podemos afirmar que las variables no son independientes"
                )

resumen<-data.frame(apartado=c(2,3,4,5,6),
           preguntas=preguntas,
           conclusiones=conclusiones
           )

resumen


resumen %>%
  kbl() %>%
  kable_styling()
