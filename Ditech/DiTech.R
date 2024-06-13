mensagem<- "Hello world"
print(mensagem)

#primeiro codigo
?ggplot2

#instalação de pacote
install.packages("ggplot2")
install.packages("readxl")

#Carregando pacote
library(readxl)

pnorm(1.96, mean=0, sd=1, lower.tail = FALSE)

#Didatica Tech Primeiros passos com  variaveis
a<-"Heitor"
b<-"João"

c<-c(a,b)

summary(a)
d<- c(10,20,30,40,45,23,37)
sqrt(var(d))
?summary

Nome<- "João"
Sobrenome<- "Silva"

nome_completo<- str_c(Nome," ", Sobrenome)
nome_completo

#Fatores 

CargaHoraria <- as.factor(c(220,220,150,100,100))
summary(CargaHoraria)

#Lista 
Lis<- list(c(1,2,3,4,5),c("Arthur","Julia","Usopp","Luffy"))
is.list(Lis)
mode(list)

#Matriz

m<-matrix(1:9,nrow=3)
?matrix

#Data frame

#definindo pasta de trabalho 
setwd("C:/Users/Arthu/Downloads")

#Importando arquivo
df<- read.csv("BRL=X.csv")
df<- read_excel("Customer_demographics_and_sales_Lab5.xlsx")

print(df, n=5)
mode(df)
class(df)
str(df)
names(df)
df$nome_entregador
summary(df)
df[1,4] 

#Filtros 
vogais <- c("a","e","i","o","u")
vogais[3]
vogais[-3]
vogais[3:5] 
vogais[3:length(vogais)] 
vogais[vogais=="e"]

#Filtros com Data frames
df[1,1:4]
view(df$STATE)
df[-1]
view(df)
head(df)
print(df)

#Condicionais IF; For; While 

if(5>3) { cc<-"Quero um emprego"
print(cc)
} else {print(a)
}

i<-1
for(i in 1:9){
  print("Quero um emprego")
}

while(i<15){
  print(i)
  i<-i+1
}

#Criação de Funções
