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

#Aula 2 - 20/06/2024
##Matrizes
matrix(1:4,2,2) #Dados, nrows, ncol

matrix(rnorm(9,20,2),3,3) #Por meio de valores aleatórios

matrix(1:6,2,3)

matrix(c("a","b","c","j"),2,2,byrow = TRUE)

matriz<-matrix(c(1,1,1,2,2,2),2,3)
colnames(matriz)<-c("Coluna 1", "Coluna 2", "Coluna 3")
rownames(matriz)<-c("linha 1", "linha 2")
matriz

matriz1<-matrix(c(0.1,0.1,0.3,0.4,0.5,0.6),2,3)
colnames(matriz1)<-c("Coluna 1", "Coluna 2", "Coluna 3")
rownames(matriz1)<-c("linha 1", "linha 2")
matriz1

#IPTU
iptu<-0.20
iptu

#Matriz IPTU
matriziptu<- matriz*iptu
matriziptu

#Matriz Agregada multiplique elemento por elemento de cada matriz
matriz2<-matriz*matriz1*iptu
matriz2

#Matriz Demanda
matrizdemanda<-matrix(c(10,20,25,30,33,40),2,3)
colnames(matrizdemanda)<-c("Coluna 1", "Coluna 2", "Coluna 3")
rownames(matrizdemanda)<-c("linha 1", "linha 2")
matrizdemanda

#Matriz Preço
matrizpreco<-matrix(c(10,20,30,40,50,60),2,3)
colnames(matrizpreco)<-c("Coluna 1", "Coluna 2", "Coluna 3")
rownames(matrizpreco)<-c("linha 1", "linha 2")
matrizpreco

#Operações com Matriz
# A*B  Multiplicação elemento a elemento
# A%*%B  Multiplicação matricial
# t(A)   Transposta 
# diag(n)  Matriz identidade de dimensão n 
# solve(A)  Inversa
# solve(A,b) Devolve o veto x dna equação b=Ax
# 
# rowMeans(A)  Devolve o vetor das médias por linha
# rowSums(A)   Devolve o vetor das somas por linha
# colMeans(A)  Devolve o vector das médias por coluna
# colSums(A)   Devolve o vector das somas por colunamatriz e vetor 

#Selecionar valores de um vetor maiores que 3 
x1<-c(1,3,5,4,7,9,2,4.5,10)
#obter valores maiores que 3
x2<-x1[x1>3]
print(x2)


#Selecionar valores onde >3 e <9
x3<-x1[(x1>3)&(x1<9)]
print(x3)

#Selecionar Valores <3 e >9
x4<-x1[(x1<3)|(x1>9)]
print(x4)

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

#Aula 01/07/2024
'sistemas lineares

Resolver sistema de equações lineares .
resolver sistemas de imaginemos o seguinte sistema de equações

-4x + 0.3y = 12.3
54.3x - 4y = 45

Podemos resolvê-lo da seguinte forma
'
coefs<- matrix(c(-4,0.3,54.3,-4),2,2, byrow = TRUE)
coefs

ys<- c(12.3, 45)
ys

solve(coefs, ys) #resolução do sistema linear ou retornar a matriz inversa 
ys[1]
ys[2]

coeficientes<-solve(coefs, ys)
coeficientes[1]
coeficientes[2]

#plot

#exemplo 1
a<-1:20
b<-a^2
plot(a,b,col = "red")

#exemplo 2
a<-1:20
b<-a^2
plot(a,b,type = "l", col = "red")

#exemplo 3
a<-1:20
b<-a^2
plot(a,b, col = "red")
lines(rev(a), b, col = "blue") #adição de linhas
points(a, 400-b, col = "green") #adição de pontos

#Exemplo 4:
a<-1:20
b<-a^2
plot(a,b, pch=2)
points(a, 400-b, pch=5)
points(a, 200-b, pch=10)

plot(0:20, 0:20, pch = 0:20)

#Exemplo 5: 
a<-1:20
b<-a^2
plot(a, b, type = "l",  col = "green")
lines(a, 2*b, lwd = 4, col = "red")
lines(a, 0.5*b, lty = 2, col = "blue")
lines(a, 3*b, lty = 3, col = "brown")
lines(a, 4*b, lty = 2, lwd = 4, col = "yellow")

#Exemplo 6:
x <- 0:20
y <- x^3
plot(c(0,20), c(-8000,8000), type = "n", xlab = NA, ylab = NA)
lines(x, y)
lines(x, -y, col = "red")
title("Gráfico de duas funções", xlab = "Valore de X", ylab = "Valored de y")

#Exemplo 7:
#Duas séries
ano<- 2001:2009
tri1<- c(72.8, 66.2, 69.2, 65.9, 62.4, 67.8, 61.3, 68.5, 70.4)
tri2<- c(60.6, 53.7, 55.3, 56.7, 56.4, 57.8, 57.5, 59.8, 63.3)

plot(ano, tri1, type = "l", main= "Taxa de Ocupação por trimestre dos hotéis -  Município do Rio de Janeiro"
     , xlab = "Ano", ylab = "Taxa de ocupação %", col = "blue", ylim= c(50,80))
lines(ano, tri2, col = "red")

#Exemplo 8 
x <- c(18,23,25,35,65,54,34,56,72,19,23,42,18,39,37)
y <- c(202,185,187,180,156,169,174,172,153,199,193,174,198,183,178)

plot(x,y) #Gráfico
abline(lm(y~ x)) #Gráfico da regressão linear
'ou'
lm(y~x) #Regressão y com x
lm(formula = y ~ x)
lmregress <- lm(y~x)
lmregress[[1]][1]
lmregress[[1]][2]
lmregress[[5]]

#Exemplo 9:
m<- ts(matrix(rnorm(300), 100, 3), start = c(1961, 6), frequency = 12)
m

plot(m, plot.type = "single",col = 1:3)
legend("topright", legend = colnames(m), col = 1:3, lty = 1)

#Aula 04/07/2024
install.packages("readxl")
library(readxl)

getwd()
setwd("~/MeusProjetos/R_Study_cases/UFPE/Math. Laboratory")

dados<-read_xlsx("~/MeusProjetos/R_Study_cases/UFPE/Math. Laboratory/Importar Arquivo Excel.xlsx")
attach(dados)
summary(dados)

plot(AA)
lines(EE)
lines(FF)
lines(DD, col = 2, lwd = 1)
lines(BB, col = 3, lwd = 2)
lines(CC, col = 4, lwd = 3)

#Parte 2
#Distribuições de probabilidades no R
#Pdistribuição retorna a probabilidade 

#Calcule a probabilidade de não caber em assento de 15 polegadas 
#numa população normal com média 14.4, e desvio padrão 1 
pnorm(15, 14.4, 1, lower.tail = FALSE)

#Tamanho de cadeira no qual 95% da população caiba
qnorm(0.95, 14.4, 1)

#Densidade probabilistica
dnorm(12, 14.4, 1)

#Individuos aléatorios
rnorm(20, 14.4, 1)

#Aula 08/07/2024

#Série de Taylor
# a) y(x) = e^x *sen(x)
# b) y(x) = e^-x * cos(x)
# c) y(x) = 2*e^-x + e^x
# d) imprimir o polinômio encontrado
# e) Elaborar os gráficos das funções acima
# f) Elaborar os gráficos dos polinômios que se aproximam das funções indicadas

'graficos especiais  - 1'

set.seed(1234)
n<-10000
c1<-matrix(rnorm(n, mean = 0, sd = 0.5), ncol = 4)
c1
c2<-matrix(rnorm(n, mean = 3, sd = 2), ncol = 4)
c2

mydata<- rbind(c1, c2)
mydata
mydata<- as.data.frame(mydata)
names(mydata)<-c("x", "y")
mydata

with(mydata,
     plot(x, y, pch =19, main= "Scatter Plot with 10000 Observations"))

with(mydata, 
     smoothScatter(x, y, main= "Scatter Plot Colored by Smoothed Densities"))

'graficos especiais  - 2'

install.packages("hexbin")
library(hexbin)

with(mydata, 
     {bin <- hexbin(x, y, xbins = 50)
     plot(bin, main= "Hexagonal Binning with 10000 Observations")})

'graficos especiais  - 3'
install.packages("scatterplot3d")
library(scatterplot3d)
data(mtcars)
attach(mtcars)
scatterplot3d(wt, disp, mpg, main= "Basic 3D Scatter Plot")

'graficos especiais  - 4'

library(scatterplot3d)
attach(mtcars)
scatterplot3d(wt, disp, mpg,
              pch = 16,
              highlight.3d= TRUE,
              type= "h",
              main= "3D Scatter Plot with Vertical lines")

install.packages("ggplot2")
library(ggplot2)

g<- ggplot(mtcars)
g<- g+geom_point(aes(x = hp, y = mpg, color = factor(am)), size = 3)

g

g<- g+scale_color_manual("Automatic",
                        values = c("red", "blue"),
                        labels = c("No", "Yes"))
g

g<- g+labs(title = "Relação entre consumo, potência e tipo de câmbio",
           y = "Consumo",
           x = "Potência")
g

mtcars |>
  ggplot()+
  geom_point(aes(x = hp, y = mpg, color = factor(am)), size = 3)+
  scale_color_manual("Automatic",
                     values = c("red", "blue"),
                     labels = c("No", "Yes"))+
  labs(title = "Relação entre consumo, potência e tipo de câmbio",
       y = "Consumo",
       x = "Potência")

g1<- ggplot(mtcars, aes(y = mpg, x = disp))+
  geom_point()

g1
