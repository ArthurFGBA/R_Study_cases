#setando diretorio
setwd("C:/Users/Arthu/Downloads")
getwd()

install.packages("readxl")
install.packages("ggplot2")
library("readxl")
library("ggplot2")

dados<- read_excel("Despesas_India (3).xlsx")
dados

mod<- lm(Despesas_Alimentacao ~ Despesa_Total,dados)
summary(mod)
plot(mod)
