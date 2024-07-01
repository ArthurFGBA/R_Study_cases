#Aula 3 - 27/06/2024
#sejam 
#set x5 e x6
x5<- c(1,4,6,8,12)
x6<- c(-2,-3,4,10,14)

#encontrar elementos de x5 em que x6 são positivos
x7<- x5[x6>0]
print(x7)

#27/06
#Joaquim Schork

set.seet(1357531) #Define sementes aleatórias para reprodutibilidade

N<- 1000
y<- rnorm(N,2,3)

x1<- rnorm(N) #criar váriaeveis aletórias
x2<- runif(N)+0.25*x1
x3<- rpois(N,3) - 0.3*x1+0.5*x2
y<- rnorm(N,2,3) + 5*x1+2*x2+0.5*x3

#Exercício 
# 1) plotar os vetores x1, x2, x3, y
# 2) elaborar matriz de correlação entre entre as variaveis
# 3) calcular y = f(x1,x2,x3) utilizando lm
# 4) calcular y (1); y(10); y(100)
# 5) calcular y1 = log(y)

#1)

plot(x1)
plot(x2)
plot(x3)
plot(y)

plot(x1,x2)
plot(x1,x3)
plot(x2,x3)
plot(y,x1)
plot(y,x2)
plot(y,x3)

install.packages("rgl")
library(rgl)
plot3d(y, x1, x2, col = "blue", size = 3)

#2)

cor(x1,x1)
cor(x1,x2)
cor(x1,x3)
cor(x2,x1)
cor(x2,x2)
cor(x2,x3)
cor(x3,x1)
cor(x3,x2)
cor(x3,x3)
cor(y,x1)
cor(y,x2)
cor(y,x3)

matriz<- data.frame(y,x1,x2,x3)
matriz_cor<- cor(matriz, use = "complete.obs")
matriz_cor

install.packages("ggcorrplot")
library(ggcorrplot)
ggcorrplot(matriz_cor, lab = TRUE)

#3)
model<-lm(y ~ x1+x2+x3, matriz)
summary(model)

#4)
predict(model, newdata = data.frame(x1 = 1, x2 = 1, x3 = 1))
predict(model, newdata = data.frame(x1 = 10, x2 = 10, x3 = 10))
predict(model, newdata = data.frame(x1 = 100, x2 = 100, x3 = 100))

#5)
y1<-ifelse(y>0,log(y),0)
y1

#6)
summary(y)
summary(y1)
summary(x1)
summary(x2)
summary(x3)

#1) ordenada
x1<- sort(x1)
x2<- sort(x2)
x3<- sort(x3)
y<- sort(y)

plot(x1)
plot(x2)
plot(x3)
plot(y)

plot(x1,x2)
plot(x1,x3)
plot(x2,x3)
plot(y,x1)
plot(y,x2)
plot(y,x3)

install.packages("rgl")
library(rgl)
plot3d(y, x1, x2, col = "blue", size = 3)

#2) ordenada

cor(x1,x1)
cor(x1,x2)
cor(x1,x3)
cor(x2,x1)
cor(x2,x2)
cor(x2,x3)
cor(x3,x1)
cor(x3,x2)
cor(x3,x3)
cor(y,x1)
cor(y,x2)
cor(y,x3)

matriz<- data.frame(y,x1,x2,x3)
matriz_cor<- cor(matriz, use = "complete.obs")
matriz_cor

install.packages("ggcorrplot")
library(ggcorrplot)
ggcorrplot(matriz_cor, lab = TRUE)

#3) ordenada
model<-lm(y ~ x1+x2+x3, matriz)
summary(model)

#4) ordenada
predict(model, newdata = data.frame(x1 = 1, x2 = 1, x3 = 1))
predict(model, newdata = data.frame(x1 = 10, x2 = 10, x3 = 10))
predict(model, newdata = data.frame(x1 = 100, x2 = 100, x3 = 100))

#5) ordenada
y1<-ifelse(y>0,log(y),0)
y1

#6) ordenada
summary(y)
summary(y1)
summary(x1)
summary(x2)
summary(x3)
