str(Salarios)
variables_cualitativas<-names(Filter(is.factor,Salarios))

for(name  in variables_cualitativas){
  Salarios[[name]]<-factor(trimws(Salarios[[name]],"l"))
}
fix(Salarios)
#observamos el orden en el que se encuentran las categorías y ese es el que aplicamos para recortarlas a un caracter
levels(Salarios$marital_status)
levels(Salarios$marital_status)<-c("D","M","S","X","W")
c
head(Salarios$marital_status)
class(table(Salarios$marital_status))


#Histograma de frecuencias
hist(table(Salarios$marital_status),
     col = rainbow(5),
     xlab = levels(Salarios$marital_status),
     ylab = "Frecuencias",
     main = "Histograma de estado civil",
     breaks = 6)

Hist(table(Salarios$marital_status),  scale="percent", 
                    col="darkgray")

levels(Salarios$marital_status)<-(c)
barplot(table(Salarios$marital_status), main = "Frequencia absoluta",
        col = rainbow(5))


# fixtrimws(Salarios$workclass,"l")
# trimws(Salarios)
# kakita<-c(" A", " B ", "  C ", "D ")
# kakita<-Salarios$workclass
# trimws(kakita,"l")
#Salarios$workclass<-trimws(Salarios$workclass,"l")

#Salarios["workclass"]<-trimws(Salarios["workclass"],"l")


# head(Salarios[ncol(Salarios)])
# 
# 
# class(Salarios[["workclass"]])
# variables_cualitativas<-names(Filter(is.factor,Salarios))
# variables_cualitativas
# salarios$vari
levels(Salarios$gender)

Salarios$gender <- Recode(Salarios$gender, 
                   '"Fem"="F"; "Female"="F"; "female"="F"; "m"="M"; "male"="M"; "Male"="M"; ;',
                   as.factor=TRUE)
Salarios$age<-as.integer(Salarios$age)
Salarios$education_num<-as.integer(Salarios$education_num)
head(Salarios$age)
head(Salarios$education_num)




#install.packages("stringr")

Salarios$hours_per_week<-gsub(",", ".", Salarios$hours_per_week)

Salarios$hours_per_week<-regmatches(Salarios$hours_per_week,regexpr("[[:digit:]]+\\.*[[:digit:]]*",Salarios$hours_per_week))
Salarios$hours_per_week<-as.numeric(Salarios$hours_per_week)
head(Salarios[["hours_per_week":"income"]])
tail(Salarios$hours_per_week)
Salarios[, c('hours_per_week', 'income')]


Salarios$income<-gsub(",", ".", Salarios$income)
K_EUR_Indice<-regexpr("Milers",Salarios$income)

Indice_aux<-regexpr("[[:digit:]]+\\.*[[:digit:]]*",Salarios$income)
Aux<-regmatches(Salarios$income,regexpr("[[:digit:]]+\\.*[[:digit:]]*",Salarios$income))


K_EUR_Indice!=-1
which(Indice_aux==-1)
Salarios[which(Indice_aux==-1), c('income')]
fix(Salarios)  
Salarios[which(Indice_aux!=-1), c('income')]<-aux
Salarios$income<-as.numeric(Salarios$income)
Salarios[which(K_EUR_Indice!=-1), c('income')]<-Salarios[which(K_EUR_Indice!=-1), c('income')]*100
fix(Salarios)
