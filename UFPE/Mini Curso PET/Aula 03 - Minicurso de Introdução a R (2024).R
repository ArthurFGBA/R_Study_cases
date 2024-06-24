# Pacotes necessários
install.packages("readstata13")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("broom")
install.packages("car")
install.packages("MASS")
install.packages("repr")
install.packages("lmtest")

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

# Importando os dados
df <- read.dta13("~/MeusProjetos/R_Study_cases/UFPE/Mini Curso PET/auto.dta")
head(df)
# Gráfico de dispersão
theme_set(theme_minimal())
options(repr.plot.width = 16, repr.plot.height = 9, repr.plot.res = 700)
ggplot(df, aes(x = weight, y = price)) +
  geom_point(shape = 21, fill = "white", color = "#222631", size = 3,
             
             stroke = 0.5) +
  
  labs(title = "Prática 01: Gráfico de Dispersão",
       x = "Massa",
       y = "Preço") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  )

# Um modelo de regressão simples
model_lm <- lm(price ~ weight, data = df)
summary(model_lm)

# Gráfico de dispersão com a reta de regressão
ggplot(df, aes(x = weight, y = price)) +
  geom_point(shape = 21, fill = "white", color = "#222631", size = 3,
             stroke = 0.5) +
  geom_smooth(method = "lm", color = "#aa3f3b", se = FALSE) +
  scale_x_continuous(labels = comma_format(big.mark = ".")) +
  scale_y_continuous(labels = comma_format(big.mark = ".")) +
  labs(title = "Prática 01: Regressão Simples",
       x = "Massa",
       y = "Preço") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black")
  )

# Prevendo o preço do automóvel de Massa (4000)
predict(model_lm, newdata = data.frame(weight = 4000)) 

# Encontrando previsões para o modelo
predictions <- predict(model_lm)
head(predictions)

# Plotando os valores observados e ajustados
df$predicted <- predictions
theme_set(theme_minimal())
options(repr.plot.width = 16, repr.plot.height = 9, repr.plot.res = 700)
ggplot(df, aes(x = seq_along(price))) +
  geom_line(aes(y = price), color = "#16215b") +
  geom_line(aes(y = predicted), color = "red", linetype = "dashed") +
  labs(title = "Prática 01: Previsão",
       x = "Massa",
       y = "Preço") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"))

# Um modelo de regressão múltipla (Massa e comprimento)
model <- lm(price ~ weight + length, data = df)
summary(model)
# Um modelo de regressão múltipla (outras variáveis)
excluir <- c("foreign", "make", "predicted")
df1 <- df[, !(names(df) %in% excluir)]
model <- lm(price ~ ., data = df1, na.action = na.omit)
summary(model)

# Variáveis dummy para foreign
df <- df %>% mutate(dum_Foreign = as.factor(foreign))
df1 <- cbind(df1, model.matrix(~dum_Foreign - 1, data = df))
model <- lm(price ~ ., data = df1, na.action = na.omit)
summary(model)

# Regressão log-linear
model_ln <- lm(log(price) ~ weight + length, data = df)
summary(model_ln)

# Gráfico de dispersão com a reta de regressão
ggplot(df, aes(x = weight, y = log(price))) +
  geom_point(shape = 21, fill = "white", color = "#222631", size = 3,
             stroke = 0.5) +
  geom_smooth(method = "lm", color = "#aa3f3b", se = FALSE) +
  scale_x_continuous(labels = comma_format(big.mark = ".")) +
  scale_y_continuous(labels = comma_format(big.mark = ".")) +
  labs(title = "Prática 02: Modelos não-lineares",
       x = "Massa",
       y = "Preço") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"))

# Teste de Heterocedasticidade
df %>%
  mutate(residuos = model_lm$residuals) %>%
  ggplot(data = ., aes(y = residuos, x = weight)) +
  geom_point() +
  geom_abline(slope = 0) +
  theme_classic()
bptest(model_lm)

# Regressão robusta
model_rlm <- rlm(price ~ weight, data = df)
summary(model_rlm)

# Gráfico de dispersão com a reta de regressão
ggplot(df, aes(x = weight, y = price)) +
  geom_point(shape = 21, fill = "white", color = "#222631", size = 3,
             stroke = 0.5) +
  geom_smooth(method = "rlm", color = "#aa3f3b", se = FALSE) +
  scale_x_continuous(labels = comma_format(big.mark = ".")) +
  scale_y_continuous(labels = comma_format(big.mark = ".")) +
  labs(title = "Prática 03: Regressão Robusta",
       x = "Massa",
       y = "Preço") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"))

# Comparando visualmente os modelos linear e robusto
ggplot(df, aes(x = weight, y = price)) +
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

# Regressão Quantílica
model_quant <- rq(price ~ weight, data = df, tau = 0.5)
model_quant
# Automatizando os gráficos com uma função
rq_smooth <- function(method = "rq", se = FALSE, tau, color) {
  if (method == "rq") {
    return(geom_smooth(aes(color = color), method = method, se = se,
                       
                       method.args = list(tau = tau)))
    
  } else {
    return(geom_smooth(aes(color = color), method = method, se = se))
  }
}

# Plotando o gráfico de dispersão com diferentes retas
ggplot(df, aes(x = weight, y = price)) +
  geom_point(shape = 21, fill = "white", color = "#222631",
             
             size = 3, stroke = 0.5) +
  rq_smooth(method = "rq", se = FALSE, tau = 0.5,
            color = "Modelo Quantílico Q(50)") +
  rq_smooth(method = "rq", se = FALSE, tau = 0.1,
            color = "Modelo Quantílico Q(10)") +
  rq_smooth(method = "rq", se = FALSE, tau = 0.9,
            color = "Modelo Quantílico Q(90)") +
  geom_smooth(aes(color = "Modelo Linear"), method = "lm", se = FALSE) +
  scale_x_continuous(labels = scales::comma_format(big.mark = ".")) +
  scale_y_continuous(labels = scales::comma_format(big.mark = ".")) +
  labs(title = "Prática 04: Regressão Quantílica",
       x = "Massa",
       y = "Preço") +
  scale_color_manual(name = "",
                     
                     values = c("Modelo Linear" = "#152a6d",
                                "Modelo Quantílico Q(50)" = "#aa3f3b",
                                "Modelo Quantílico Q(10)" = "#255f00",
                                "Modelo Quantílico Q(90)" = "#330c4b"),
                     labels = c("Modelo Linear", "Modelo Quantílico Q(50)"
                                ,
                                "Modelo Quantílico Q(10)",
                                "Modelo Quantílico Q(90)")) +
  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "bottom"
  )

