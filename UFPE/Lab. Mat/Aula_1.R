#Definindo pasta de trabalho
setwd("C:/Users/Arthu/OneDrive/Documentos/MeusProjetos/R_Study_cases/UFPE/Lab. Mat")
getwd()

area<-c(303,379,961,295,332,47,122,11,53,2749)
riqueza<-c(3,10,20,7,8,4,8,3,5,23)

area
riqueza

modelo1<-lm(riqueza~area)

summary(modelo1)

#Juros composto
taxa.de.juros.anual<-0.05
capital.inicial<-1000
prazo<-10
capital.final<-capital.inicial*(1+taxa.de.juros.anual)^prazo
capital.final

#Banco
tx.juros<-c(4.5,5.1,4,3.6,3,5,4.6,4.8,3.6,5)/100
saldos<-c(10,150,45,20,100,75,15,67,9,2)*1000
juros<-saldos*tx.juros
juros

capital.final2<-saldos*(c(1,1,1,1,1,1,1,1,1,1)+tx.juros)^5
capital.final2

#ex3
anos<-c(1,2,5,10)
tx.juro.anual<-0.04
capital.final3<-1000*(1+tx.juro.anual)^anos
capital.final3

#ex4
cash.flow<-c(-1000,250,350,450)
taxa.de.desconto<-0.045
desconto<-(1+taxa.de.desconto)^-(0:3)
fc<-cash.flow*desconto
fc
VPL<-cumsum(fc)
VPL

#ex5
serie<-rnorm(1000,mean=100,sd=50)

ordenados<-sort(serie)
ordenados
min(ordenados)

Serie2<-round(ordenados,2)
Serie2

diferença<-mean(Serie2[1:500])-mean(Serie2[501:1000])
diferença

media.menores<-mean(ordenados[1:100])
print(round(media.menores,2))
media.maiores<-mean(ordenados[901:1000])
print(round(media.maiores,2))

#ex6
cc<-function(cap.ini,t.juro,prazos){
  resultado<-cap.ini*(1+t.juro)^prazos
  resultado
}
cc(1000,0.05,30)
cc(1000,c(0.04,0.05,0.06),30)

#Teste da função
cap1<-c(1000,2000,3000)
cc(cap1,0.05,5)

cc1<-function(cap.ini,t.juro,prazos){
  if(cap.ini>50000) {
    t.juro<-t.juro+0.005
  }else {
    t.juro<-t.juro}
  resultado<-cap.ini*(1+t.juro)^prazos
  resultado
}
cc1(55000,0.005,5)
t.juro

#ex7
x<-1:100
y1<-(1000-0.20*x)
y1
plot(y1)
x<-101:200
y2<-(1050-0.21*x)
y2
plot(y2)
x<-201:300
y3<-(1090-0.22*x)
y3
plot(y3)

y<-c(y1,y2,y3)
plot(y)

#Aula 13/06/2024
#Aplicação da parte acima 

##Aula 17/06/2024

#ex8
b1<-rnorm(100,mean=0.20,sd=0.2) #Coef. angular da média com média 0.20 e Desv. p. 0.2
print(b1)

b2<-rnorm(100,mean=0.21,sd=0.2) #Coef. angular da média com média 0.21 e Desv. p. 0.2
print(b2)

b3<-rnorm(100,mean=0.22,sd=0.2) #Coef. angular da média com média 0.22 e Desv. p. 0.2
print(b3)

x<-1:100
y1<-(1000-b1*x)
y11<-sort(y1,decreasing=TRUE)
plot(y11)

x<-101:200
y2<-(1050-b2*x)
y22<-sort(y2,decreasing=TRUE)
plot(y22)

x<-201:300
y3<-(1090-b3*x)
y33<-sort(y3,decreasing=TRUE)
plot(y33)

y<-c(y1,y2,y3) #Desordenado
plot(y)

y<-c(y11,y22,y33) #Ordenado
plot(y)

hist(b1,col=1)#black
hist(b2,col=2)#pink
hist(b3,col=3)#green

#Ex 09
y1<-0;y2<-0;y3<-0;y4<-0

for(i in 1:500){
  #x=1:500
  y1[i]<-1000
}

for(i in 1:500){
  #x=501:1000
  y2[i]<-500 
}

for(i in 1:500){
  #x=1001:1500
  y3[i]<-200
}

for(i in 1:500){
  #x=1501:2000
  y4[i]<-100
}

y<-c(y1,y2,y3,y4)
plot(y)
