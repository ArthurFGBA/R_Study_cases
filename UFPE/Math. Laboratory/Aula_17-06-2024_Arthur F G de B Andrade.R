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
