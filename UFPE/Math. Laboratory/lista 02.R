install.packages("tidyverse")
library(tidyverse)
install.packages("readxl")
library(readxl)

##Questão 01

Ano<-c(1970,1975,1980,1981,1982,1983,1984,1985,1986,1987,1988,1989,1990,1991,1992,1993,1994,1995)
Depositos_totais <- c(312,381.5,347.4,404.2,402.1,452.0,431.7,582.3,596.6,620.8,513.6,606.9,629.0,602.7,656.7,678.5,637.6,698.2)
PIB <- c(33027,105962,191842,212187,222354,223354,245104,273949,303496,323736,335923,362286,361909,376089,379411,384591,395478,480361)
Populacao <- c(93139037,105279615,119002706,121304828,124132901,126932107,129881714,130964997,132744121,135682832,138506432,141596301,146917459,147489931,150474909,153390844,155608189,158617875)
Renda_per_capita <- c(355,1006,1961,1749,1791,1760,1887,2092,2286,2386,2425,2559,2463,2550,2521,2507,2541,3028)

q1<-data.frame(Ano, Depositos_totais, PIB, Populacao, Renda_per_capita)

model<-lm(Depositos_totais ~ PIB+Populacao+Renda_per_capita, q1)

summary(model)

sprintf('Os resultados obtido no modelo de regressão com os dados para cada coeficiente foi intercepto: %s, beta1: %s, beta2: %s e beta3: %s.', model[[1]][1], model[[1]][2], model[[1]][3], model[[1]][4])
model[[1]][1]

#Resultados diferentes dos destacados na questão

##Questão 02
#parte 1
vet<-rnorm(1000, 500, 5)
vet<-sort(vet)

vet_500ma<-vet[501:1000]
vet_500me<-vet[001:500]

#Vetor diferença
v_dif<- vet_500ma-vet_500me

v_dif

#parte 2
vet_100ma<-vet[901:1000]
vet_100me<-vet[001:100]

#desvio padrão 100 maiores
sd(vet_100ma)

#desvio padrão 100 menores
sd(vet_100me)

##Questão 03
coef<-matrix(c(7,-3,-3,2,4,1,0,-2,-1),3,3, byrow = TRUE)
ys<-c(7,0,2)

solucao<-solve(coef,ys)

# Solução de X
solucao[1]
# Solução de Y
solucao[2]
# Solução de Z
solucao[3]

#determinante da matriz dos coeficientes
det(coef)

##Questão 04
fum<-c(52.4,55,55.2,55.2,55.5,56.2,57,57.4,58.3,58.4,59.2,59.3,59.6,59.7,60,60.5,60.6,61.2,61.6,61.9,62.1,62.2,62.4,62.7,63.5,64.1,64.7,64.8,64.9,65,65.4,66,66.9,69.1,69.2,69.8)

n_fum<-c(63.8,65.7,66.2,66.2,66.2,66.8,67.5,67.7,67.9,68,68.1,68.3,68.6,68.6,68.7,68.8,68.8,69.2,69.3,69.4,69.5,70.1,70.1,70.2,70.2,70.3,70.4,70.7,70.8,70.8,71,71.4,71.5,71.6,72.7,72.7,72.9,73.3,73.3,73.9,74.1,75.8,75.9,77.5)

#medias, desvios padrões e tamanho da amostra
mf<-round(mean(fum),2)
s1<-sd(fum)
mnf<-round(mean(n_fum),2)
s2<-sd(n_fum)
n1<-dim(array(fum))
n2<-dim(array(n_fum))

#Quantil associado a probabilidade
t1<-qt(0.975, df = n1-1)
t2<-qt(0.975, df = n2-1)

#limite inferior e superior do intervalo de confiança
mf_sup<-round((mf+t1*s1/sqrt(n1)),2)
mf_inf<-round((mf-t1*s1/sqrt(n1)),2)

mnf_sup<-round((mnf+t2*s2/sqrt(n2)),2)
mnf_inf<-round((mnf-t2*s2/sqrt(n2)),2)

#Intervalo de confiança 
#fumantes
sprintf('Fumantes - Media: %s, limite inferior: %s, limite superior: %s', mf, mf_inf, mf_sup )
sprintf('Não fumantes - Media: %s, limite inferior: %s, limite superior: %s', mnf, mnf_inf, mnf_sup )

# Por meio do intervalo de confiança para a média amostral podemos inferir 
# Que existe uma diferença no tempo de vida médio entre os fumantes e 
# não fumantes com confiança de 95%.

##Questão 05
#parte 1
val_p<-function(c,j,t){
  val<-c*(1+j)**t
  return(val)
}

ent<-c(100,200,300,400,500,600)

val_p(ent,0.12,2)

#parte 2
val_p(ent,0.04,2)

val_p(ent,0.05,2)

##Questão 06
x<-5
sum<-x

for (n in 2:1000) {
  sum<-sum+(-1)^(n+1)*(x^n)/(n*factorial(n))
}

print(sum)
