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

dados<- read_excel("Despesas_India (3).xlsx")
print(head(dados))

mod<- lm(Despesas_Alimentacao ~ Despesa_Total,dados)
summary(mod)
plot(mod)

ggplot(dados, aes(X = Despesa_Total, y = Despesas_Alimentacao))+
geom_point()+
geom_smooth( method= "lm" , color="red", se = FALSE)+
scale_x_continuous(labels = comma_format(big.mark = ".")) +
scale_y_continuous(labels = comma_format(big.mark = "."))+
theme_classic()


## 'geo_smooth' using formula 'y ~ x'
#ggsave(""dpi=500)

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
