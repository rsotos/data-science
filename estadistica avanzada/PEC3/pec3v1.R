# 1 Carga de datos
## Volcado en DAtaframe y presentacion de datos

library(dplyr)
library('ggplot2')
library('corrplot')
library('faraway')
library(car)
library(questionr)


setwd("D:/uoc/estadistica avanzada/pec3/" );

airs <- 
  read.table("dat_Air_Stations.csv", 
             header=TRUE, stringsAsFactors=TRUE, sep=",", na.strings="NA", dec=".", 
             strip.white=TRUE)
head(airs)
str(airs)
summary(airs)




#1.1 a


airs$Fecha<-as.Date(airs$Fecha, "%m/%d/%y")

OMS_media_PM10<-45
OMS_media_O3<-60
OMS_media_SO2<-25
OMS_media_NO2<-40
OMS_max_diaria_O3<-100
OMS_max_diaria_NO2<-120

max_med<-airs %>%
  group_by(Fecha,Nombre) %>%
  summarize(mediaPM10 = mean(PM10, na.rm=TRUE), maxPM10=max(PM10),
            mediaNO2 = mean(NO2, na.rm=TRUE), maxNO2=max(NO2),
            mediaO3 = mean(O3, na.rm=TRUE), maxO3=max(O3),
            mediaSO2 = mean(SO2, na.rm=TRUE), maxNSO2=max(SO2))
 






kkfuti<-select((max_med%>%filter(mediaPM10>OMS_media_PM10)),Nombre)%>%
  group_by(Nombre) %>%summarize(supera_OMS_PM10 = n())

kkfuti2<-select((max_med%>%filter(mediaO3>OMS_media_O3)),Nombre)%>%
  group_by(Nombre) %>%summarize(supera_OMS_03= n())


kkfuti[,c("supera_OMS_03")]<-kkfuti2$supera_OMS_03


select((max_med%>%filter(mediaSO2>OMS_media_SO2)),Estacion)%>%
  group_by(Estacion) %>%summarize(n = n())

select((max_med%>%filter(mediaNO2>OMS_media_NO2)),Estacion)%>%
  group_by(Estacion) %>%summarize(n = n())



select((max_med%>%filter(maxO3>OMS_max_diaria_O3)),Estacion)%>%
  group_by(Estacion) %>%summarize(n = n())

select((max_med%>%filter(maxNSO2>OMS_max_diaria_NO2)),Estacion)%>%
  group_by(Estacion) %>%summarize(n = n())



aux = airs %>% distinct(Estacion)
distinct_Estacion = aux$Estacion



ggplot(data = max_med[max_med$Estacion==1,], aes(x =Fecha , y = maxPM10)) +geom_line(color = "#00AFBB", size = 2)+ geom_smooth(color = "firebrick") + theme_bw() +ggtitle("Estacion 1")

ggplot(data = max_med[max_med$Estacion==2,], aes(x =Fecha , y = maxPM10)) +geom_line(color = "#00AFBB", size = 2)+ geom_smooth(color = "firebrick") + theme_bw() +ggtitle("Estacion 2")

ggplot(data = max_med[max_med$Estacion==3,], aes(x =Fecha , y = maxPM10)) +geom_line(color = "#00AFBB", size = 2)+ geom_smooth(color = "firebrick") + theme_bw() +ggtitle("Estacion 3")

ggplot(data = max_med[max_med$Estacion==4,], aes(x =Fecha , y = maxPM10)) +geom_line(color = "#00AFBB", size = 2)+ geom_smooth(color = "firebrick") + theme_bw() +ggtitle("Estacion 4")

ggplot(data = max_med[max_med$Estacion==10,], aes(x =Fecha , y = maxPM10)) +geom_line(color = "#00AFBB", size = 2)+ geom_smooth(color = "firebrick") + theme_bw() +ggtitle("Estacion 10")



aux<- vector('list', 2)
aux[1]<-"sads"
aux[2]<-3

levels(airs$Nombre)
##1.1 c

# est_Constitucion<-airs %>%filter(Estacion==1, (SO2 %in% c(60,58)))
# 
# est_Const<-airs %>%filter(Estacion==1) %>%group_by(Fecha)%>%select(Estacion,Fecha,SO2,PM10,NO2,O3)
# prueba<-est_Const%>%filter(SO2==max(SO2))

est_Constitucion_SO2<-airs %>%filter(Estacion==1) %>%group_by(Fecha)%>%filter(SO2==max(SO2))%>%select(Fecha,SO2,TMP,HR,RS,vv,LL,PRB)
elim.pos <- which(names(est_Constitucion_SO2)=="Fecha") 
est_Constitucion_SO2<-est_Constitucion_SO2[,-elim.pos]

 cor1<- est_Constitucion_SO2 


cor1<-est_Constitucion_SO2  %>% select(SO2,vv)



mat_Cor_aux<-cor(na.omit(cor1))


corrplot(mat_Cor_aux, method = 'number')





matriz<-select(airs,SO2,PM10,NO2,O3,TMP,HR,RS,vv,LL,PRB)
matriz_cor<-cor(na.omit(matriz))


# matriz_cor<-matriz_cor[-(5:10),]
# matriz_cor<-matriz_cor[,-(1:4)]

mat_Cor_aux<-cor(na.omit(matriz_cor))

corrplot(mat_Cor_aux, method = 'number')
par( mfrow=c(2,2),y="kakita")
corrplot(mat_Cor_aux, method = 'number',title= "adsa")
corrplot(mat_Cor_aux, method = 'number')
corrplot(mat_Cor_aux, method = 'number')
corrplot(mat_Cor_aux, method = 'number')
par( mfrow=c(1,1))


#1.2



model <- lm(O3 ~ NO2 , data = airs)
summary(model)



model <- lm(O3 ~ NO2+Nombre , data = airs)
summary(model)

contrasts(airs$Nombre)


#R y coeficiciente beta y su significancia


#1.3
res <- resid(model)
plot(fitted(model), res)
abline(0,0)


data_Montevil<- airs %>% filter(Nombre=="Estacion de Montevil") %>% select(O3,NO2,vv,RS,HR,LL,TMP)
data_Constitucion<- airs %>% filter(Nombre=="Estacion Avenida Constitucion") %>% select(O3,NO2,vv,RS,HR,LL,TMP)
modelo_Montevil <- lm(O3 ~ NO2 + vv + RS +HR +LL, data = data_Montevil)
summary(modelo_Montevil)

modelo_Constitucion <- lm(O3 ~ NO2 + vv + RS +HR +LL, data = data_Constitucion)
summary(modelo_Constitucion)


modelo_Montevil <- lm(O3 ~ NO2 + vv + RS +HR +LL+TMP, data = data_Montevil)
summary(modelo_Montevil)
vif(modelo_Montevil)


modelo_Constitucion <- lm(O3 ~ NO2 + vv + RS +HR +LL+TMP, data = data_Constitucion)
summary(modelo_Constitucion)

vif(modelo_Constitucion)

mat_Cor_aux<-select(data_Montevil,vv,TMP)

mat_Cor_aux<-cor(na.omit(mat_Cor_aux))

corrplot(mat_Cor_aux, method = 'number')



month=relevel(month, ref = '9')




#1.5
avPlots(modelo_Montevil, main="as")


res <- resid(modelo_Montevil)

plot(fitted(modelo_Montevil), res)
abline(0,0)
qqqqqqq<-fitted(modelo_Montevil)
zzz<-data.frame(fitted(modelo_Montevil))

(qqqqqqq<0)==True


qqnorm(res)

#add a straight diagonal line to the plot
qqline(res) 
plot(density(res))


#1.6 




newdf <- data.frame(NO2=c(40),vv=c(2),RS=c(100),HR=c(80),LL=(0.10),TMP=c(25))
predict(modelo_Montevil,newdf)


#ods ratio cercano a 1--<hay independencia 
#confusion:  metes una vqrianble al modelo de reg logistica y altera el co9eficiente de otra variable
#interaccion:

#2.3 predict

#2.4 hipoteis nula: el modelo se ajusta a la realidad

#2.1
airs$icPM10<-NULL

airs$icPM10[(airs["PM10"]>0)&(airs["PM10"]<=45)]="aceptable"
airs$icPM10[(airs["PM10"]>45)&(airs["PM10"]<=180)]="mejorable"

airs$icPM10<-as.factor(airs$icPM10)

airs$icO3<-NULL

airs$icO3[(airs["O3"]>0)&(airs["O3"]<=60)]="aceptable"
airs$icO3[(airs["O3"]>60)&(airs["O3"]<=170)]="mejorable"

airs$icO3<-as.factor(airs$icO3)

airs$RSre<-NULL

airs$RSre[(airs["RS"]>0)&(airs["RS"]<=100)]="normal baja"
airs$RSre[(airs["RS"]>100)&(airs["RS"]<=700)]="normal alta"

airs$RSre<-as.factor(airs$RSre)

airs$month<-months(airs$Fecha)


data_Montevil<- airs %>% filter(Nombre=="Estacion de Montevil") %>% select(O3,NO2,vv,RS,PRB,HR,LL,TMP,month,icPM10,icO3,RSre)
#data_Montevil<- airs %>% filter(Estacion==10) %>% select(O3,NO2,vv,RS,PRB,HR,LL,TMP,month,icPM10,icO3,RSre)



aux_Montevil<-data_Montevil %>% filter(month %in% c("enero","agosto")) %>% select(month,icO3,icPM10)

tc<-table(data_Montevil$icPM10,data_Montevil$RSre)
tc<-table(data_Montevil$icO3,data_Montevil$RSre)


tc<-table(aux_Montevil$icPM10,aux_Montevil$month)
tc<-table(aux_Montevil$icO3,aux_Montevil$month)
tc
## oDds para PM10 mejorable con radiacion alta


numerador<-(tc[2,1]/sum(tc[,1]))
numerador<-numerador/(1-numerador)
denominador<-(tc[2,2]/sum(tc[,2]))
denominador<-denominador/(1-denominador)
OR<-numerador/denominador
OR

tc<-table(data_Montevil$icPM10,data_Montevil$RSre)
tc<-table(data_Montevil$icO3,data_Montevil$RSre)
tc
1/odds.ratio(tc)

#2.2

Se define devianza del modelo (estadístico D) una medida del grado de
diferencia entre las frecuencias observadas y las predichas por el modelo
Para que el modelo sea bueno, la devianza residual debe ser menor
que la devianza nula.

Existe confusión cuando la asociación entre la variable respuesta o dependiente
y la variable independiente elegida (el factor de interés) difiere significativamente
según si se considera, o no, otra variable explicativa. A esta última
variable se la denomina variable de confusión para la asociación.

modelo_a=glm(formula=icPM10~RSre+PRB+vv,family=binomial(link=logit),data=data_Montevil)

summary(modelo_a)

data_Montevil$month<-as.factor(data_Montevil$month)
data_Montevil$month<-relevel(data_Montevil$month, ref = 'enero')

modelo_b=glm(formula=icPM10~RSre+PRB+vv+month,family=binomial(link=logit),data=data_Montevil)

summary(modelo_b)



modelo_c=glm(formula=icPM10~RSre+PRB+vv+month+TMP,family=binomial(link=logit),data=data_Montevil)

summary(modelo_c)



modelo_k=glm(formula=icPM10~RSre+PRB+vv+TMP,family=binomial(link=logit),data=data_Montevil)

summary(modelo_k)



levels(modelo$model$icPM10)[0]
pred<-predict(modelo_b, data.frame(vv=0.6,RSre="normal alta", PRB=1013, month="agosto"),type ="response")
pred





library(ResourceSelection)
hoslem.test(modelo_c$y,fitted(modelo_c))
hoslem.test(modelo_a$y,fitted(modelo_a))
hoslem.test(modelo_b$y,fitted(modelo_b))
hoslem.test(modelo_k$y,fitted(modelo_k))


library(pROC)



aux_Montevil<-data_Montevil  %>% select(month,icO3,icPM10,PRB,vv)

prob=predict(modelo_b, aux_Montevil, type="response")



r=roc(modelo_a$y,fitted(modelo_a), data=modelo_a$model)
r=roc(modelo_c$y,fitted(modelo_c), data=modelo_c$model)
r=roc(modelo_b$y,fitted(modelo_b), data=modelo_b$model)


plot(r)
auc(r)
modelo_b$model
