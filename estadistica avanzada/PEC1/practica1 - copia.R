

# 1 Carga de datos
## Volcado en DAtaframe y presentacion de datos





Salarios <- 
  read.table("D:/uoc/estadistica avanzada/sesion 1/CensusIncomedataset.csv", 
  header=TRUE, stringsAsFactors=TRUE, sep=";", na.strings="NA", dec=".", 
  strip.white=TRUE)
  head(Salarios)
  str(Salarios)
  summary(Salarios)


# 2 Verificacion y normalizacion de las variables

#borramos variables
Salarios <- within(Salarios, {
   capital_gain <- NULL
   capital_loss <- NULL
   fnlwgt <- NULL 
  })

# quitamos filas con mas de cinco NA
nrow(Salarios)
Indices<-which(rowSums(is.na(Salarios))>=5)
Salarios[Indices,]
Indices
Salarios<-Salarios[-Indices,]
nrow(Salarios)

#categorizamos los años de educacion

attach(Salarios)
education_cat<-education_num
education_cat[education_num<7]<-"Primaria"
education_cat[education_num>=7 & education_num<9]<-"Secundaria"
education_cat[education_num>=9 & education_num<13]<-"Universitaria"
education_cat[education_num>=13]<-"Postuniversitaria"
Salarios$education_cat<-education_cat
table(education_cat)

#renombramos variable
names(Salarios)[c(9)] <- c("gender")





## 3 Duplicados


##En primer lugar se deja de tratar los valores de CS_ID como factores para poder nuevos en caso de que sea necesario
Salarios$CS_ID<-as.character(Salarios$CS_ID)

##Se obtienen los  indices de los registros con valores de CS_ID  ya usados en registros anteriores
Indices<-which(duplicated(Salarios$CS_ID))


##estos son los valores repetidos
Salarios[Indices,c("CS_ID")]

##se parte del CS_ID del ukltimo registro y se le suma 1
##se hace una busqueda hacia adelante para encontrar el siguiente valor no asignado
##Se repite el proceso para cada registro que tenga un valor de CS_ID repetido

iterador<-1
for (i in Indices){
  
  codigo_aux<- paste("CS",as.character(nrow(Salarios)+iterador),sep = "")
  while(!identical(which(Salarios$CS_ID==codigo_aux),integer(0))){
    iterador<-iterador+1
    codigo_aux<- paste("CS",as.character(nrow(Salarios)+iterador),sep = "")
  }
  
  cat("Para el registro ",i,"  se asigna el codigo: ",codigo_aux,"\n")
  Salarios[i,c("CS_ID")]<-codigo_aux
  iterador<-iterador+1
  
}

##Verificamos que ya no hay  duplicados 
Indices<-which(duplicated(Salarios$CS_ID))
Salarios[Indices,c("CS_ID")]






## 4 Normalizacion de las variables cualitativas


#recortar espacios en blanco en valores de las variables cualitativas
variables_cualitativas<-names(Filter(is.factor,Salarios))

for(name  in variables_cualitativas){
  Salarios[[name]]<-factor(trimws(Salarios[[name]],"l"))
}

#observamos el orden en el que se encuentran las categorías y ese es el que aplicamos para recortarlas a un caracter
levels(Salarios$marital_status)
levels(Salarios$marital_status)<-c("D","M","S","X","W")
levels(Salarios$marital_status)
head(Salarios$marital_status)
table(Salarios$marital_status)

#distribucion de marital_Status
barplot(table(Salarios$marital_status), main = "Frequencia absoluta",
        col = rainbow(5))
        
#refactorizacion de la variable gender
library(car)
#niveles actuales
levels(Salarios$gender) 
Salarios$gender <- Recode(Salarios$gender, 
                   '"Fem"="f"; "Female"="f";"F"="f"; "female"="f"; "M"="m"; "male"="m"; "Male"="m"',
                   as.factor=TRUE)
#niveles despues de refactorizar                   
levels(Salarios$gender) 
#distribucion de gender
barplot(table(Salarios$gender), main = "Frequencia absoluta",
        col = rainbow(2))



#5 normalizacion variables cuantitativas

#Para age  y educartion_num se hace un casting a integer

Salarios$age<-as.integer(Salarios$age)
Salarios$education_num<-as.integer(Salarios$education_num)
head(Salarios[, c('age', 'education_num')])
tail(Salarios[, c('age', 'education_num')])



## Se hace una copia de de income y  numeric se hace una copia para comparar el antes y el despues de la nomalizacion

income_before<-Salarios$income
hours_per_week_before<-Salarios$hours_per_week


##Se sustituyen las comas por puntos

Salarios$hours_per_week<-gsub(",", ".", Salarios$hours_per_week)
Salarios$income<-gsub(",", ".", Salarios$income)

## Se obtienen los indices de los valores expresados en miles de euros en income
K_EUR_Indice<-regexpr("Milers",Salarios$income)

##Se obtienen los indices de los registros que contienen valores numericos válidos para income 
Indice_aux_hours_per_week<-regexpr("[[:digit:]]+\\.*[[:digit:]]*",Salarios$hours_per_week)
Indice_aux_income<-regexpr("[[:digit:]]+\\.*[[:digit:]]*",Salarios$income)

##Aux contiene los valores numericos aislados del resto de caracteres
Aux_hours_per_week<-regmatches(Salarios$hours_per_week,regexpr("[[:digit:]]+\\.*[[:digit:]]*",Salarios$hours_per_week))
Aux_income<-regmatches(Salarios$income,regexpr("[[:digit:]]+\\.*[[:digit:]]*",Salarios$income))

##En caso de que hubiera, aquí se mostrarian los indices y valores que no tenian un valor valido
which(Indice_aux_hours_per_week==-1)
Salarios[which(Indice_aux_hours_per_week==-1), c('hours_per_week')]
which(Indice_aux_income==-1)
Salarios[which(Indice_aux_income==-1), c('income')]

##Para el resto de registros, con valores de income validos , se hace la modificación
Salarios[which(Indice_aux_hours_per_week!=-1), c('hours_per_week')]<-Aux_hours_per_week
Salarios[which(Indice_aux_income!=-1), c('income')]<-Aux_income

## paso a numeric
Salarios$income<-as.numeric(Salarios$income)
Salarios$hours_per_week<-as.numeric(Salarios$hours_per_week)

##Ayudandonos del vector de indices que contiene los registros qen los que income esta expresado en miles de euros pasamos a unidades de euros
Salarios[which(K_EUR_Indice!=-1), c('income')]<-Salarios[which(K_EUR_Indice!=-1), c('income')]*1000



##se muestran  los nuevos ¡valores de las dos columnas comparados con los que traia la tabla originalmente

# compara_income= cbind(Salarios$income,income_before)
# 
# head(compara_income)
# tail(compara_income)
# compara_hours_per_week= cbind(Salarios$hours_per_week,hours_per_week_before)
# 
# head(compara_hours_per_week)
# tail(compara_hours_per_week)
compara_income<-data.frame(Salarios$income,income_before)
head(compara_income)
tail(compara_income)

compara_hours_per_week<-data.frame(Salarios$hours_per_week,hours_per_week_before)
head(compara_hours_per_week)
tail(compara_hours_per_week)


 # head(Salarios[, c('hours_per_week', 'income')])
 # head(hours_per_week_before)
 # head(income_before)
 # tail(Salarios[, c('hours_per_week', 'income')])
 # tail(hours_per_week_before)
 # tail(income_before)



##6 Valores atipicos


## para age borramos primero los que a aprecen por encima de 200 
Boxplot( ~ age, data=Salarios, id=list(method="y"))
Salarios[which(Salarios$age>200),c("age")]<-NA




## borramos los dos valores anormalmente altos
Boxplot( ~ income, data=Salarios, id=list(method="y"))

Salarios[c(1,7),c("income")]<-NA


Boxplot( ~ hours_per_week, data=Salarios, id=list(method="y"))

Salarios[which((Salarios$hours_per_week>80)&(Salarios$hours_per_week<7)),c("hours_per_week")]<-NA



Boxplot( ~ education_num, data=Salarios, id=list(method="y"))

Salarios[which((Salarios$education_num<5)),c("education_num")]<-NA


##7 imputacion de valores

Salarios[which(is.na(Salarios$age)),c("age")]<-as.integer(mean(Salarios$age,na.rm=TRUE))

#length(which(rowSums(is.na(Salarios))>0))
length(which(is.na(Salarios$age)))

media<-mean(Salarios$income,na.rm=TRUE)
media_m<-mean(Salarios[which(Salarios$gender=="m"),c("income")],na.rm=TRUE)
media_f<-mean(Salarios[which(Salarios$gender=="f"),c("income")],na.rm=TRUE)


Salarios[which((is.na(Salarios$income))&(Salarios$gender=="m")),c("income")]<-media_m
Salarios[which((is.na(Salarios$income))&(Salarios$gender=="f")),c("income")]<-media_f
length(which((is.na(Salarios$income))))

# mean_by_gender<-numSummary(Salarios[,"income", drop=FALSE], groups=Salarios$gender, statistics=c("mean"))

#for(index_gender in levels(Salarios$gender)){
# 
#   Salarios[which((is.na(Salarios$income))&&(Salarios$gender==index_gender)),c("income") ]<-mean_by_gender[index_gender]
# 
# }
# 
# 
# Salarios[which((is.na(Salarios$income))&&(Salarios$gender=="m")),c("income") ]<-mean_by_gender["m"]
#  Salarios[which((is.na(Salarios$income))&&(Salarios$gender=="f")),c("income") ]<-mean_by_gender["f"]


library(VIM)
 
 Salarios_m<-Salarios[which(Salarios$gender=="m"),]
 Salarios_f<-Salarios[which(Salarios$gender=="f"),]
Salarios_m<-kNN(Salarios_m,variable=c("education_num"),k=11,dist_var=c("age","income","hours_per_week"))
Salarios_f<-kNN(Salarios_f,variable=c("education_num"),k=11,dist_var=c("age","income","hours_per_week"))

Salarios_m<-kNN(Salarios_m,variable=c("hours_per_week"),k=11,dist_var=c("age","income","education_num"))
Salarios_f<-kNN(Salarios_f,variable=c("hours_per_week"),k=11,dist_var=c("age","income","education_num"))



Salarios<-rbind(Salarios_m,Salarios_f)

Salarios <- within(Salarios, {
  hours_per_week_imp <- NULL
  education_num_imp <- NULL
 
})

length(which((is.na(Salarios$education_num))))
length(which((is.na(Salarios$hours_per_week))))




##8 Estudio descriptivo

####media windsor

# pruebax=c(1,2,40,67,34,90,43,38,22,49,900, -300)
# length(pruebax)
# perc=0.1
# umbrales<-quantile(pruebax, probs=c(perc,1.0-perc))
# 
# 
# pruebax<-pruebax[(pruebax>umbrales[1])]
# pruebax<-pruebax[(pruebax<umbrales[2])]
# 
# 
# length(pruebax)
# pruebax



media.windsor <- function( x, perc=0.05){
  
  ##se calculan los valores umbrales que determinan los dos percentelies: perc y 1-perc. 
  #Todo lo que quede respectivamente por debajo y por arriba sera sustituido por el umbral correpondiente
  umbrales<-quantile(x, probs=c(perc,1.0-perc))
  
  
  x[x<umbrales[1]]<-umbrales[1]
  x[x>umbrales[2]]<-umbrales[2]
  
  return(mean(x,na.rm = TRUE))
  
}

media.recortada <- function( x, perc=0.05){
  
  ##se calculan los valores umbrales que determinan los dos percentelies: perc y 1-perc. 
  #Todo lo que quede respectivamente por debajo y por arriba n sera considerado par calcular la media
  umbrales<-quantile(x, probs=c(perc,1.0-perc))
  
  
  x<-x[(x>umbrales[1])]
  x<-x[(x<umbrales[2])]
  
  return(mean(x,na.rm = TRUE))
  
}

percents<-as.numeric(c(0.05,0.1,0.15,0.2))
for (i in percents){
  media.windsor(income,perc=i)
  media.recortada(income,perc=i)
  
}
media.windsor(Salarios$income,perc=0.05)

media.recortada(Salarios$age,perc=0.05)

attach(Salarios)
media_windsor<-c(media.windsor(age),media.windsor(education_num),media.windsor(income),media.windsor(hours_per_week))
media_recortada<-c(media.recortada(age),media.recortada(education_num),media.recortada(income),media.recortada(hours_per_week))


##estudio descriptivo



estudio<- data.frame(age,education_num,income,hours_per_week)
media_aritmetica<-apply(estudio, 2, mean, na.rm = TRUE)
mediana<-apply(estudio, 2, median, na.rm = TRUE)
desviacion_estandar<-apply(estudio, 2, sd, na.rm = TRUE)
desviacion_abs_med<-apply(estudio, 2, mad, na.rm = TRUE)
rango_interc<-apply(estudio, 2, IQR, na.rm = TRUE)
resumen<-rbind(media_aritmetica,mediana,media_windsor,media_recortada,desviacion_estandar,desviacion_abs_med,rango_interc)
resumen




hist(Salarios$age, scale="percent", breaks="Sturges", col="darkgray",    ylab="Porcentaje")

##9 escribir salida


fix(Salarios)


write.csv(Salarios, file="D:/uoc/estadistica avanzada/sesion 1/CensusIncome_clean.csv",row.names=F)


class(colnames(Salarios))

