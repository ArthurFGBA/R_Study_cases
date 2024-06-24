
library(tidyverse)
library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(purrr)
library(ggplot2)

#Questão 01
pib<-read.csv("C:/Users/Arthu/Downloads/PIB_nominal a preços de mercado.csv", header = TRUE)

pib<-as.tibble(pib)

str(pib)

pib<-pib %>%
  select(-X) %>%
  rename(Data = Data, 
         PIB_nominal = colnames(pib[,2])) %>%
  mutate(Data = yq(Data), PIB_nominal = as.numeric(PIB_nominal))

str(pib)

ggplot(pib, aes(x = Data, y = PIB_nominal))+
        geom_line(color = 'black', linewidth = 1, linetype = 1)+
        labs(title = "Série Histórica do PIB",
             x = "Trimestres",
             y = "PIB Nominal")+
        theme_classic()

import<-read.csv("C:/Users/Arthu/Downloads/PIB - Importações.csv", header = TRUE)

import<- as.tibble(import)

import<- import %>%
  select(-X) %>%
  rename(Data = Data,
         PIB_import = colnames(import[,2])) %>%
  mutate(Data = yq(Data), PIB_import = as.numeric(PIB_import))

export<-read.csv("C:/Users/Arthu/Downloads/PIB - Exportações.csv", header = TRUE)

export<- as.tibble(export)

export<- export %>%
  select(-X) %>%
  rename(Data = Data,
         PIB_export = colnames(export[,2])) %>%
  mutate(Data = yq(Data), PIB_export = as.numeric(PIB_export))

balanca<- inner_join(import, export, by = "Data")

balanca<- balanca %>%
  mutate( Balanca_Comercial = PIB_export-PIB_import) %>%
  select(-PIB_export,-PIB_import)

ggplot(balanca, aes(x = Data, y = Balanca_Comercial))+
  geom_line(color = 'black', linewidth= 1, linetype = 1)+
  labs(title = "Série Histórica do Saldo da Balança Comercial",
       x = "Trimestres",
       y = "Saldo da Balança Comercial")+
  theme_classic()

#Questão 02

women
women_db<-as.tibble(women)

str(women_db)

ggplot(women_db, aes(x = weight, y = height))+
  geom_line(color = 'black', linewidth = 1, linetype = 1)
labs(title = "USA Women height x weight",
     x = "weight",
     y = "height")+
  theme_classic()
# O gráfico de linha nos permite visualizar a relação peso altura dos dados 
# assim permitindo a interpretação de possivel relacionamento entre as 
# variaveis

model_lin<-lm(height ~ weight, women_db)
summary(model_lin)

model_log<-lm(log(height) ~ log(weight), women_db)
summary(model_log)

ggplot(women_db, aes(x = weight, y = height))+
  geom_point(color = 'black', shape = 5)+
  geom_smooth(method = "lm", color = 'red', se = FALSE) +
  labs(title = "USA Women height x weight",
       x = "weight",
       y = "height")+
  theme_classic()

# O gráfico de dispersão nos permite visualizar todas as cobinações registradas
# e assim podemos observar uma tendência e relação entre maior peso e maior 
# altura na população feminina

ggplot(women_db, aes(x = height))+
  geom_histogram(binwidth = 3,fill = '#4E0707' ,color = 'black')
  labs(title = "USA Women height",
       x = "weight",
       y = "Quantity")+
  theme_classic()
  
#O histograma nos revela a distribuição discreta da altura na população
# feminina por meio dele podemos observar uma concertração maior na altura
# média fugindo dos dados outliers
ggsave("Women_histogram.png", dpi= 500)
getwd()
