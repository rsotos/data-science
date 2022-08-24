
Salarios <- 
  read.table("D:/uoc/estadistica avanzada/sesion 1/CensusIncomedataset.csv", 
             header=TRUE, stringsAsFactors=TRUE, sep=";", na.strings="NA", dec=".", 
             strip.white=TRUE)



Salarios$income2<-Salarios$income

##Para income igualmente se sustituyen las comas por puntos


Salarios$income<-gsub(",", ".", Salarios$income)

## Se obtienen los indices de los valores expresados en miles de euros
K_EUR_Indice<-regexpr("Milers",Salarios$income)

##Se obtienen lis indices de los registros que contienen valores numericos válidos para income 
Indice_aux<-regexpr("[[:digit:]]+\\.*[[:digit:]]*",Salarios$income)
##Aux contiene los valores numericos aislados del resto de caracteres
Aux<-regmatches(Salarios$income,regexpr("[[:digit:]]+\\.*[[:digit:]]*",Salarios$income))
##estos son los registros que tienen valores NA
which(Indice_aux==-1)
Salarios[which(Indice_aux==-1), c('income')]
##Para el resto de registros, con valores de income validos , se hace la modificación
Salarios[which(Indice_aux!=-1), c('income')]<-Aux

## paso a numeric
Salarios$income<-as.numeric(Salarios$income)

##Ayudandonos de vector de indices que contien los registros qen los que income esta expresado en miles de euros pasamos a unidades de euros
Salarios[which(K_EUR_Indice!=-1), c('income')]<-Salarios[which(K_EUR_Indice!=-1), c('income')]*1000
fix(Salarios)
nrow(Salarios)
length(income_before)

which(duplicated(levels(Salarios$CS_ID)))

within(Salarios,Salarios$CS_ID<- ave(as.character(Salarios$CS_ID),FUN=make.unique))

Indices<-which(duplicated(Salarios$CS_ID))
Indices

##estos son los valores repetidos
Salarios[Indices,c("CS_ID")]

##se busca el ultimo valor de CSID asignado y a partir de ahi se asignan a los registros con codigos duplicados

factores_CS_ID<-levels(Salarios$CS_ID)
aux<-which(levels(Salarios$CS_ID)==Salarios[nrow(Salarios),c("CS_ID")])

aux
factores_CS_ID[aux]
aux<-aux+1
kk=which(Salarios$CS_ID=="CS32560")
!identical(which(Salarios$CS_ID=="CS32561"),integer(0))
which(Salarios$CS_ID=="CS32561")
kkita<-"CS32561"
which(factores_CS_ID=="CS32561")
substr("CS32561",2,)
while(!identical(which(factores_CS_ID[aux]!="CS32561"),integer(0))){
  aux<-aux+1
}


dropfactor(Salario$CS_ID)
library(Rcmdr)
factores_CS_ID<-levels(Salarios$CS_ID)
factores_CS_ID<-append(factores_CS_ID,"CS32563")
Salarios[1,c("CS_ID")]<-factores_CS_ID["32556"]
which(factores_CS_ID=="CS32563")
Salarios[1,]

factores_CS_ID[aux]!="5"
aux<-1
while(!identical(which(Salarios$CS_ID==factores_CS_ID[aux]),integer(0))){
  aux<-aux+1
}
aux<-1
while(factores_CS_ID[aux]!="CS32561"){
  aux<-aux+1
}
aux
aux<- paste("CS",as.character(nrow(Salarios)),sep = "")
aux<-1
codigo_aux<- paste("CS",as.character(nrow(Salarios)+aux),sep = "")
codigo_aux
iterador<-1
for (i in Indices){
  
  codigo_aux<- paste("CS",as.character(nrow(Salarios)+iterador),sep = "")
  while(!identical(which(factores_CS_ID[aux]!=codigo_aux),integer(0))){
    iterador<-iterador+1
    codigo_aux<- paste("CS",as.character(nrow(Salarios)+iterador),sep = "")
  }
  
  print(codigo_aux)
  Salarios[i,c("CS_ID")]<-codigo_aux
  iterador<-iterador+1
  
}
iterador<-1
codigo_aux<- paste("CS",as.character(nrow(Salarios)+iterador),sep = "")
identical(which(Salarios$CS_ID==codigo_aux),integer(0))

while(!identical(which(Salarios$CS_ID!=codigo_aux),integer(0))){
  iterador<-iterador+1
  codigo_aux<- paste("CS",as.character(nrow(Salarios)+iterador),sep = "")
}
codigo_aux
#####bucle manual
iterador<-1
codigo_aux<- paste("CS",as.character(nrow(Salarios)+iterador),sep = "")

which(Salarios$CS_ID==codigo_aux)


iterador<-iterador+1
codigo_aux<- paste("CS",as.character(nrow(Salarios)+iterador),sep = "")

codigo_aux
####



Salarios[1,c("CS_ID")]<-"CS32570"
for (i in Indices)
{
  aux<-aux+1

  
  while(!identical(which(Salarios$CS_ID==factores_CS_ID[aux]),integer(0))){
    aux<-aux+1
  }
     Salarios[i,c("CS_ID")]<-factores_CS_ID[aux]
  
}

##comprobamos que si no hay  duplicados y si el ultimo factor de CS_ID asignado es ahora otro mayor
Indices<-which(duplicated(Salarios$CS_ID))
Salarios[Indices,c("CS_ID")]
Indices

fix(factores_CS_ID)
