#simulado 15/07

#Questão 01
v <-rnorm(1000, 100, 50)
v <- sort(v)
v

#Vetor diferença
v_dif <- v[501:1000]-v[001:500]
v_dif

#Desvios padrão
sd_100ma<- sd(v[901:1000])
sd_100ma
sd_100me<- sd(v[001:100])
sd_100me

#Questão 02
install.packages('skimr')
library(skimr)

q2 <- data.frame(
  c('Jan/23', 'Fev/23', 'Mar/23', 'Abr/23', 'Mai/23', 'Jun/23', 
    'Jul/23', 'Ago/23', 'Set/23', 'Out/23', 'Nov/23', 'Dez/23'),
  c(124, 116, 101, 118, 118, 120, 
    110, 127, 106, 130, 120, 121),
  c(111, 101, 130, 108, 127, 129, 
    122, 103, 122, 127, 130, 120),
  c(117, 142, 121, 123, 121, 148, 
    141, 122, 139, 125, 122, 121),
  c(1.0, 1.5, 0.5, 0.8, 0.6, 0.5, 
    0.8, 0.7, 0.9, 1.1, 0.3, 0.4)
)

# Adicionando nomes às colunas para maior clareza
colnames(q2) <- c("Mes", "A", "B", "C", "IPCA")

#Estatistica descritiva de toda a base de dados
skim(q2)

#Por loja
summary(q2["A"])
sd(q2["A"])
summary(q2["B"])
sd(q2["B"])
summary(q2["C"])
sd(q2["C"])

boxplot(q2["A"], main = "Supermercado A")
boxplot(q2["B"], main = "Supermercado B")
boxplot(q2["C"], main = "Supermercado C")

boxplot(q2[, 2:4],
        names = c("Supermercado A", "Supermercado B", "Supermercado C"),
        main = "Box Plots para as Variáveis",
        xlab = "Variáveis",
        ylab = "Valores",
        col = c("lightblue", "lightgreen", "lightpink"),
        border = "darkblue")
#C)
ipca_acum<-c()
ipca_acum[1]<-ipca_b[1,1]+1
for i in 2:12 {
  ipca_acum[i]<-ipca_acum[i-1]+ipca_b[i,1]
}

ipca.acum<-matrix(data =c(ipca_acum),nrow=12, ncol=1)
colnames(ipca.acum) <- c("IPCA_Acum")
rownames(ipca.acum) <- c(mes_ano)

#Questão 03
capitalaizar <- function(c, i,t){
  M <- c*(1+i)^t
  return (round(M,2))
}

juros <- abs(round(rnorm(10, 0.09,0.04),2))
saldo <- abs(round(rnorm(10, 8000,1500),2))
juros
saldo

for(j in 1:10){
  print(paste("Para um saldo de ",saldo[j], " a uma taxa de ", juros[j]," por 5 anos, o valor final sera de : ",capitalaizar(saldo[j],juros[j],5) ))
}

#Questão 04
coob<- function(k,l,a,b){
  y= 2*(k^a)*l^b
  return(y)
}
#Produção com R$1000 e 100 horas, a= 0.8 e b=0.2
coob(1000,100,0.8,0.2)

#Produção com R$20000, 250 horas, a= 0.7 e b=1.2
coob(20000,250,0.7,1.2)

#Questão 05
x1<- 0:1000
x2<-1001:2000
x3<-2001:3000

b1<- rnorm(1000,0.10,0.02)
b2<- rnorm(1000,0.25,0.03)
b3<- rnorm(1000,0.35,0.04)

y1<-1100-b1*x1
y2<-1250-b2*x2
y3<-1590-b3*x3

xp<- 0:3000
yp<- c(y1,y2,y3)

plot(xp, yp, main = "Demanda", type = 'l', col = "darkblue", lwd =1)

#Questão 06
A<- matrix(c(4,7,3,2),2,2,byrow= T)
B<- matrix(c(2,4,2,2),2,2,byrow= T)
C<- matrix(c(1,3,1,5),2,2,byrow= T)

y<-2*A+B^2-3*C
print(y)

#Questão 07
x<-rnorm(20,15,4.5)
sum<-0
sum_cum<-c()
for (i in 1:20) {
  y1<-2+x[i]^2-0.05*x[i]^3
  y2<-2+x[i]^2-0.01*x[i]^3
  if ((sum+y1)<0){
    sum<-sum+y2
  }else{
    sum<-sum+y1
  }
  sum_cum[i]<-sum
}
print(sum)

print(sum_cum)

plot(sum_cum, type = 'n', main="Soma acumulada")
lines(sum_cum, col = 'lightblue', lwd= 2)

#Questão 08


#Questão 09
q9<- function(x) 1/(16+x^2)
integrate(q9,lower =0, upper =3 )

#XE[1;3]
integrate(q9,lower =1, upper =3 )

#Questão 10
#i)

preco<-c(3000,1000,0,2500,4200,5570,900,0,3000,10000,5000,0,3200,15000,1000,3500,5000,0,5000,3000,0,2000,0,15000,5000,10000,4000,5000,10000,8000,0,4000,2500,2000)

preco1<-sort(preco)

preco2<-sort(preco1, decreasing=T)

Consumidor<-seq(1:34)
Consumidor
plot(preco2, type = 'n')
lines(preco2, col = "darkgreen", lwd = 3)

#ii)

demanda<-lm(preco2~Consumidor)
summary(demanda)

plot(Consumidor,preco2,main= "Curva de demanda Original", xlab="Quantidade",ylab="Preço",)
abline(demanda, col = "red")

#Questão 11
#Carteira 1
install.packages(c('tidyverse', 'ggplot2', 'BatchGetSymbols'))
library(tidyverse)
library(ggplot2)
library(BatchGetSymbols)

first.date <- as.Date('2022-01-01')
last.date <- Sys.Date()
freq.data <- 'weekly'
tickers <- c('ELET3.SA','ITUB4.SA','VALE3.SA')
tickers
l.out <- BatchGetSymbols(tickers = tickers, 
                         first.date = first.date,
                         last.date = last.date, 
                         freq.data = freq.data,
                         cache.folder = file.path(tempdir(), 
                                                  'BGS_Cache') )

print(l.out$df.control)

p <- ggplot(l.out$df.tickers, aes(x = ref.date, y = price.close))
p <- p + geom_line()
p <- p + facet_wrap(~ticker, scales = 'free_y') 
print(p)


# Filtrando os retornos Eletrobrás
eletro <- l.out$df.tickers %>% 
  na.omit() %>% 
  filter(ticker == 'ELET3.SA') %>% 
  select( ret.closing.prices) %>% 
  rename( Eletrobras = ret.closing.prices) #Observe que modificamos o nome da coluna.


#Filtrando os retornos da itau
itau <- l.out$df.tickers %>% 
  na.omit() %>% 
  filter(ticker == 'ITUB4.SA') %>% 
  select( ret.closing.prices) %>% 
  rename(Itau = ret.closing.prices) #Observe que modificamos o nome da coluna.

#Filtrando os retornos do vale
vale<- l.out$df.tickers %>% 
  na.omit() %>% 
  filter(ticker == 'VALE3.SA') %>% 
  select( ret.closing.prices) %>% 
  rename(Vale = ret.closing.prices) #Observe que modificamos o nome da coluna.

#Criando os uma variável com todos os retornos
retornos <- cbind(eletro, itau, vale)

retornos


'Para começarmos vamos carregar os pacotes:'
install.packages('fPortfolio')
library(fPortfolio)

'Caso não tenha instalado o pacote, lembre-se de instalar o mesmo e depois carregar.
Vamos transformar, agora, os nossos retornos em séries temporais. Todos os retornos estão guardados na variável que criamos “Retornos”.'
#Transformando os retornos em séries de tempo
retornos <- as.timeSeries(retornos)
'Agora vamos encontrar o ponto de tangência e ver os respectivos pesos de cada ação:'

# Portfolio com a melhor relação entre risco/retorno
portfolio.eficiente <- tangencyPortfolio(retornos, spec = portfolioSpec(), constraints = "LongOnly")
portfolio.eficiente

#Carteira 2
first.date <- as.Date('2022-01-01')
last.date <- Sys.Date()
freq.data <- 'weekly'
tickers <- c('MGLU3.SA','PETR3.SA','BBAS3.SA')
tickers
l.out <- BatchGetSymbols(tickers = tickers, 
                         first.date = first.date,
                         last.date = last.date, 
                         freq.data = freq.data,
                         cache.folder = file.path(tempdir(), 
                                                  'BGS_Cache') )

print(l.out$df.control)

p <- ggplot(l.out$df.tickers, aes(x = ref.date, y = price.close))
p <- p + geom_line()
p <- p + facet_wrap(~ticker, scales = 'free_y') 
print(p)


# Filtrando os retornos MagaLu
maglu <- l.out$df.tickers %>% 
  na.omit() %>% 
  filter(ticker == 'MGLU3.SA') %>% 
  select( ret.closing.prices) %>% 
  rename( Magalu = ret.closing.prices) #Observe que modificamos o nome da coluna.


#Filtrando os retornos da Petrobrás
Petro <- l.out$df.tickers %>% 
  na.omit() %>% 
  filter(ticker == 'PETR3.SA') %>% 
  select( ret.closing.prices) %>% 
  rename(Petrobras = ret.closing.prices) #Observe que modificamos o nome da coluna.

#Filtrando os retornos do vale
bb<- l.out$df.tickers %>% 
  na.omit() %>% 
  filter(ticker == 'BBAS.SA') %>% 
  select( ret.closing.prices) %>% 
  rename(Banco_do_Brasil = ret.closing.prices) #Observe que modificamos o nome da coluna.

#Criando os uma variável com todos os retornos
retornos <- cbind(maglu, Petro, bb)

retornos


'Para começarmos vamos carregar os pacotes:'
library(fPortfolio)

'Caso não tenha instalado o pacote, lembre-se de instalar o mesmo e depois carregar.
Vamos transformar, agora, os nossos retornos em séries temporais. Todos os retornos estão guardados na variável que criamos “Retornos”.'
#Transformando os retornos em séries de tempo
retornos <- as.timeSeries(retornos)
'Agora vamos encontrar o ponto de tangência e ver os respectivos pesos de cada ação:'

# Portfolio com a melhor relação entre risco/retorno
portfolio.eficiente <- tangencyPortfolio(retornos, spec = portfolioSpec(), constraints = "LongOnly")
portfolio.eficiente

#Questão 13
x<-5
soma<-0
for (i in 1:100){
  if((i%%2)== 0 ){
    y= -(i+2)*x^i/(i+1)*factorial(i)
  } else{
    y= (i+2)*x^i/(i+1)*factorial(i)
  }
  soma<-soma+y
}

print(soma)

#Questão 14
coef<-matrix(c(1,2,-1,2,-1,1,1,1,1),3,3, byrow = TRUE)
ys<-c(2,3,6)

solucao<-solve(coef,ys)

# Solução de X
solucao[1]
# Solução de Y
solucao[2]
# Solução de Z
solucao[3]

