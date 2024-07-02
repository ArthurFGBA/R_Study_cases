#setando diretorio
setwd("C:/Users/Arthu/Downloads")
getwd()

install.packages("readxl")
install.packages("ggplot2")
install.packages("readr")
install.packages("tidyverse")
install.packages("patchwork")
install.packages("gt")
install.packages("modelsummary")
install.packages("openxlsx")
install.packages("scales")
library("scales")
library("readxl")
library("ggplot2")
library('readr')
library('tidyverse')
library('patchwork')
library('gt')
library('modelsummary')
library('stringr')
library('scales')
library('dplyr')

dados<- read_excel("Despesas_India (3).xlsx") #Regressão linear simples
print(head(dados))

mod<- lm(Despesas_Alimentacao ~ Despesa_Total,dados) 
summary(mod)
plot(mod)

ggplot(dados,
       aes(x = Despesa_Total, y = Despesas_Alimentacao)) +
  geom_point(shape = 21, fill = "white", color = "#222631",
             size = 3, stroke = 0.5) +
  geom_smooth(method = "lm", color = "#aa3f3b", se = FALSE) +
  scale_x_continuous(labels = comma_format(big.mark = ".")) +
  scale_y_continuous(labels = comma_format(big.mark = ".")) +
  labs(title = "Estudo Dirigido 01: Questão 06",
       x = "Despesas Totais",
       y = "Despesas com Alimentação") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  )
#questçao 2 estudo dirigido Comparação de regressões
pnb<-read_excel("Q2-Estudodirigido.xlsx")

print(head(pnb))
mod2<- lm(PNB ~ M1,pnb) 
summary(mod2)

ggplot(data=pnb, 
       aes(x = M1,y = PNB))+
  geom_point(shape = 21, fill = "white", color = "#222631",
             size = 3, stroke = 0.5) +
  geom_smooth(method = "lm", color = "#aa3f3b", se = FALSE) +
  scale_x_continuous(labels = comma_format(big.mark = ".")) +
  scale_y_continuous(labels = comma_format(big.mark = ".")) +
  labs(title = "Estudo Dirigido 01: Questão 06",
       x = "M1",
       y = "PNB") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  )
  
mod2<- lm(PNB ~ M2,pnb)
summary(mod2)

mod2<- lm(PNB ~ M3,pnb)
summary(mod2)

mod2<- lm(PNB ~ L,pnb)
summary(mod2)

#Questão 3 Estudo dirigido Regressões lineares multiplas
#Regressões logaritimicas multiplas
bd3<-read_excel("C:/Users/Arthu/Downloads/Q3-estudodirigido.xlsx")
print(head(bd3))

mod3<-lm(Y ~ x1t+x2t+x3t+x4t,bd3)
summary(mod3)

mod4<-lm(log(Y) ~ x1t+x2t+x3t+x4t,bd3)
summary(mod4)

loglog<-lm(log(Y) ~ log(x1t)+log(x2t)+log(x3t)+log(x4t),bd3)
summary(loglog)

#Questão 6 Regressão linear multipla
bd6<-read_excel("Q6-b1^.xlsx")
print(head(bd6))

modb11<-lm(TESTSCORE ~ STR+EL,bd6)
summary(modb11)

