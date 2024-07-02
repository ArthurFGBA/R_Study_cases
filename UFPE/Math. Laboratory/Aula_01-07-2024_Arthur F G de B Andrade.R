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