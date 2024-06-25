# Pacotes necessários
install.packages("readstata13")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("broom")
install.packages("car")
install.packages("MASS")
install.packages("repr")
install.packages("lmtest")
install.packages("readr")
install.packages("readxl")

# Bibliotecas necessárias
library(repr)
library(readstata13)
library(dplyr)
library(ggplot2)
library(broom)
library(car)
library(MASS)
library(scales)
library(lmtest)
library(quantreg)
library(readr)
library("readxl")

dados1<-read_excel("~/MeusProjetos/R_Study_cases/UFPE/PET mini course/GPA-ACT.xlsx")
dados1$GPA<-as.numeric(dados1$GPA)

# Gráfico de dispersão
theme_set(theme_minimal())
options(repr.plot.width = 16, repr.plot.height = 9, repr.plot.res = 700)
ggplot(dados1, aes(x = ACT, y = GPA)) +
  geom_point(shape = 21, fill = "white", color = "#222631", size = 3,
             
             stroke = 0.5) +
  
  labs(title = "Prática 03 - Questão 1: Gráfico de Dispersão",
       x = "ACT",
       y = "GPA") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  )
# A disposição do gráfico de dispersção demonstra uma tendencia 
# a relação quase linear entre as variáveis

summary(dados1$ACT)
summary(dados1$GPA)
sd(dados1$ACT)
sd(dados1$GPA)

#As analises mostram que os dados GPA tem 0,38 de desvio padrão e média 3,21
# Ao paso que ACT possui média 25,88 e desvio padrão 2,85

model<-lm(GPA ~ ACT,dados1)
summary(model)

# O b0 não é útil pois ACT não pode assumir o valor de 0 assim impossibilitando 
# A interpretação de b0 como quando ACT for zero sendo o intercepto

# A direção acompanha o esperado de uma relação possitiva entre notas maiores
# estarem ligadas a um melhor desempenho nos ultimos 4 anos

# Gráfico de dispersão com a reta de regressão
ggplot(dados1, aes(x = ACT, y = GPA)) +
  geom_point(shape = 21, fill = "white", color = "#222631", size = 3,
             stroke = 0.5) +
  geom_smooth(method = "lm", color = "#aa3f3b", se = FALSE) +
  scale_x_continuous(labels = comma_format(big.mark = ".")) +
  scale_y_continuous(labels = comma_format(big.mark = ".")) +
  labs(title = "Prática 03 - Questão 1: Regressão Simples",
       x = "ACT",
       y = "GPA") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  )

# Prevendo o coeficiente GPA com nota ACT = 20 
predict(model, newdata = data.frame(ACT = 20))


#Questão 2
dados2<-read_excel("C:/Users/Arthu/Downloads/Despesas_India (3).xlsx")

#Regressão linear estimada
model2<-lm(Despesas_Alimentacao ~ Despesa_Total,dados2)
summary(model2)

# Gráfico de dispersão com a reta de regressão
ggplot(dados2, aes(x = Despesa_Total, y = Despesas_Alimentacao)) +
  geom_point(shape = 21, fill = "white", color = "#222631", size = 3,
             stroke = 0.5) +
  geom_smooth(method = "lm", color = "#aa3f3b", se = FALSE) +
  scale_x_continuous(labels = comma_format(big.mark = ".")) +
  scale_y_continuous(labels = comma_format(big.mark = ".")) +
  labs(title = "Prática 03 - Questão 2: Regressão Simples",
       x = "Despesas Totais",
       y = "Despesas Alimentação") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  )

# Que a Regressão linear nesse caso não consegue explicar os dados 
# da melhor forma devido a seu baixo R2 de 0,36 além de possuirem
#baixa significancia por contá dos seus desvios padrões

#III. Não pois a medida que as despesas aumentam os desvios tentem a aumentar

# Teste de Heterocedasticidade
dados2 %>%
  mutate(residuos = model2$residuals) %>%
  ggplot(data = ., aes(y = residuos, x = Despesa_Total)) +
  geom_point() +
  geom_abline(slope = 0) +
  theme_classic()
bptest(model_lm)

model2_r<-rlm(Despesas_Alimentacao ~ Despesa_Total,dados2)
summary(model2_r)

# Não se ajustou melhor pois os coeficiêntes são muito similares

ggplot(dados2, aes(x = Despesa_Total, y = Despesas_Alimentacao)) +
  geom_point(shape = 21, fill = "white", color = "#222631", size = 3,
             stroke = 0.5) +
  geom_smooth(method = "rlm", color = "#aa3f3b", se = FALSE) +
  scale_x_continuous(labels = comma_format(big.mark = ".")) +
  scale_y_continuous(labels = comma_format(big.mark = ".")) +
  labs(title = "Prática 03 - Questão 2: Regressão Robusta",
       x = "Despesas Totais",
       y = "Despesas Alimentação") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"))

# Comparando visualmente os modelos linear e robusto
ggplot(dados2, aes(x = Despesa_Total, y = Despesas_Alimentacao)) +
  geom_point(shape = 21, fill = "white", color = "#222631", size = 3,
             stroke = 0.5) +
  geom_smooth(aes(color = "Modelo Robusto"), method = "rlm", se = FALSE) +
  geom_smooth(aes(color = "Modelo Linear"), method = "lm", se = FALSE) +
  scale_x_continuous(labels = scales::comma_format(big.mark = ".")) +
  scale_y_continuous(labels = scales::comma_format(big.mark = ".")) +
  labs(title = "Prática 03: OLS vs. RLM",
       x = "Massa",
       y = "Preço") +
  scale_color_manual(name = "", values = c("Modelo Linear" = "#152a6d", "
Modelo Robusto" = "#aa3f3b"),
                     
                     labels = c("Modelo Linear", "Modelo Robusto")) +
  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "bottom")

# IV. Sim, pois a derivação da demanda em relaçao a renda é possitiva 
# ou seja aumenta de acordo com a renda e consequêntemente a relação entre
# despesas totais que simboliza a renda segue o mesmo.
