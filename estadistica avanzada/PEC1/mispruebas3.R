Boxplot( ~ age, data=Salarios, id=list(method="y"))
which(Salarios$age>10000)
fixSalarios<-Salarios[-which(Salarios$age>200),]
Boxplot( ~ income, data=Salarios, id=list(method="y"))
Boxplot( ~ hours_per_week, data=Salarios, id=list(method="y"))
Boxplot( ~ education_num, data=Salarios, id=list(method="y"))

Salarios[which(Salarios$age>200),c("age")]<-"NA"
Salarios[which(Salarios$age>200),c("age")]

length(which(Salarios$hours_per_week>85))
length(which(Salarios$education_num<2))

