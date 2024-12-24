#Questão 1 
install.packages('tidyverse','ggplot2','readr','dplyr',
                 'stringr','lubridate','purrr','mFilter')
library(tidyverse)
library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(purrr)
library(ggplot2)
library(mFilter)

getwd()
setwd("C:/Users/Arthu/OneDrive/Documentos/MeusProjetos/R_Study_cases/Macroeconomics")

industria<-readr::read_csv("~/MeusProjetos/R_Study_cases/Macroeconomics/horas trabalhadas.csv"
         ,col_names = TRUE)

industria<-as_tibble(industria)

industria<- industria %>%
  select(-`...3`)%>%
  rename(Horas_Trabalhadas = `Indicadores Industriais - horas trabalhadas - indústria - índice dessazonalizado (média 2006 = 100) - - - Confederação Nacional da Indústria - CNI12_HTRABD12` ) %>%
  mutate(Data= ym(format(as.Date(paste0(Data, ".01"), format = "%Y.%m.%d"), "%Y-%m")),
         Horas_Trabalhadas = as.numeric(Horas_Trabalhadas))

industria<- industria %>%
  mutate(Data = floor_date(Data, "quarter")) %>%
  group_by(Data) %>%
  summarise(Horas_Trabalhadas = log(mean(Horas_Trabalhadas, na.rm = TRUE)))%>%
  slice(-c(1:16))

apu <- readr::read_csv("~/MeusProjetos/R_Study_cases/Macroeconomics/APU .csv",
                        col_names = TRUE)
apu<-as.tibble(apu)

apu<- apu%>%
  rename(Adm_Pub = colnames(apu[,2])) %>%
  mutate(Data = yq(Data), 
         Adm_Pub = log(as.numeric(Adm_Pub)))
plot.ts(apu$Adm_Pub, start = c(1996,1), frequency = 4)

fbcf <- readr::read_csv("~/MeusProjetos/R_Study_cases/Macroeconomics/Formação bruta de capital.csv"
                        ,col_names = TRUE)
fbcf <- as.tibble(fbcf)

fbcf <- fbcf %>%
  select(-`...3`)%>%
  rename(Capital_Fixo = colnames(fbcf[,2]))%>%
  mutate(Data  = yq(Data), Capital_Fixo = log(as.numeric(Capital_Fixo)))

fam<- readr::read_csv("~/MeusProjetos/R_Study_cases/Macroeconomics/Consumo final- Familias.csv"
                      ,col_names = TRUE)
fam<-as.tibble(fam)

fam<- fam %>%
  select(-`...3`)%>%
  rename(Cons_Familias = colnames(fam[,2]))%>%
  mutate(Data = yq(Data), Cons_Familias = log(as.numeric(Cons_Familias)))

pib<- readr::read_csv("~/MeusProjetos/R_Study_cases/Macroeconomics/PIB - preços de mercado.csv"
                      ,col_names = TRUE)
pib<-as.tibble(pib)

pib<- pib %>%
  select(-`...3`)%>%
  rename(PIB_Nominal = colnames(pib[,2]))%>%
  mutate(Data = yq(Data), PIB_Nominal = log(as.numeric(PIB_Nominal)))

hp_industria <- hpfilter(ts(industria$Horas_Trabalhadas
                            , start = c(1996,1), frequency = 4), freq = 1600)

hp_pib <- hpfilter(ts(pib$PIB_Nominal
                     , start = c(1996,1), frequency = 4), freq = 1600)

hp_fbcf<- hpfilter(ts(fbcf$Capital_Fixo
                      , start = c(1996,1), frequency = 4), freq = 1600)

hp_fam<- hpfilter(ts(fam$Cons_Familias
                     , start = c(1996,1), frequency = 4), freq = 1600)

hp_apu<- hpfilter(ts(apu$Adm_Pub
                     , start = c(1996,1), frequency = 4), freq = 1600)

#Criação de graficos comparativos entre tendência e real
#Aplicação Horas trabalhadas
industria<- industria %>%
  mutate(trend = hp_industria$trend, cycle = hp_industria$cycle)

htXtd<- industria %>%
  ggplot(aes(x = Data))+
  geom_line(aes(y= Horas_Trabalhadas, color = "Horas Trabalhadas"), linewidth= 1.2) +
  geom_line(aes(y = trend, color = "Tendência"), linetype = 'dashed', linewidth= 1.2)+
  labs(title = "Horas Trabalhadas - Industria",
       x = "Trimestre",
       y = "")+
  theme_minimal(
    base_size = 13,
    base_family = "serif"
  )+
  theme(plot.title = element_text(size = 20, hjust = 0.5))

ggsave("horas_trabalhadasXtêndencia.png", plot = htXtd, width = 8, height = 6, dpi = 300)

#Aplicação a série do PIB
pib<- pib %>%
  mutate(trend = hp_pib$trend, cycle = hp_pib$cycle)

pibXtd<- pib %>%
  ggplot(aes(x = Data))+
  geom_line(aes(y= PIB_Nominal, color = "PIB Nominal"), linewidth= 1.2) +
  geom_line(aes(y = trend, color = "Tendência"), linetype = 'dashed', linewidth= 1.2)+
  labs(title = "PIB nominal à Preços de Mercado",
       x = "Trimestre",
       y = "")+
  theme_minimal(
    base_size = 13,
    base_family = "serif"
  )+
  theme(plot.title = element_text(size = 20, hjust = 0.5))

ggsave("PIBXtêndencia.png", plot = pibXtd, width = 8, height = 6, dpi = 300)

#Apicação a série de FBCF
fbcf<- fbcf %>%
  mutate(trend = hp_fbcf$trend, cycle = hp_fbcf$cycle)

CFXtd<- fbcf %>%
  ggplot(aes(x = Data))+
  geom_line(aes(y= Capital_Fixo, color = "Capital Fixo"), linewidth= 1.2) +
  geom_line(aes(y = trend, color = "Tendência"), linetype = 'dashed', linewidth= 1.2)+
  labs(title = "Formação Bruta de Capital Fixo",
       x = "Trimestre",
       y = "")+
  theme_minimal(
    base_size = 13,
    base_family = "serif"
  )+
  theme(plot.title = element_text(size = 20, hjust = 0.5))

ggsave("FBCFXtêndencia.png", plot = CFXtd, width = 8, height = 6, dpi = 300)

#Aplicação a despesas das familias
fam<- fam %>%
  mutate(trend = hp_fam$trend, cycle = hp_fam$cycle)

DFXtd<- fam %>%
  ggplot(aes(x = Data))+
  geom_line(aes(y= Cons_Familias, color = "Consumo das Familias"), linewidth= 1.2) +
  geom_line(aes(y = trend, color = "Tendência"), linetype = 'dashed', linewidth= 1.2)+
  labs(title = "Consumo das Familias",
       x = "Trimestre",
       y = "")+
  theme_minimal(
    base_size = 13,
    base_family = "serif"
  )+
  theme(plot.title = element_text(size = 20, hjust = 0.5))

ggsave("DespFamXtêndencia.png", plot = DFXtd, width = 8, height = 6, dpi = 300)

#Aplicação a despesas públicas
apu<- apu %>%
  mutate(trend = hp_apu$trend, cycle = hp_apu$cycle)

DPuXtd<- apu %>%
  ggplot(aes(x = Data))+
  geom_line(aes(y= Adm_Pub, color = "Despesa Pública"), linewidth= 1.2) +
  geom_line(aes(y = trend, color = "Tendência"), linetype = 'dashed', linewidth= 1.2)+
  labs(title = "Despesa da Administração Pública",
       x = "Trimestre",
       y = "")+
  theme_minimal(
    base_size = 13,
    base_family = "serif"
  )+
  theme(plot.title = element_text(size = 20, hjust = 0.5))

ggsave("DPuXtêndencia.png", plot = DPuXtd, width = 8, height = 6, dpi = 300)

#Obtendo Componentes ciclicos 
#industria 
Cy_HT<- industria %>%
  ggplot(aes(x = Data))+
  geom_line(aes(y= cycle), color = 'blue', linewidth= 1.2) +
  labs(title = "Componente Ciclico das Horas Trabalhadas",
       x = "Trimestre",
       y = "")+
  theme_minimal(
    base_size = 13,
    base_family = "serif"
  )+
  theme(plot.title = element_text(size = 20, hjust = 0.5))

ggsave("Cycle_industria.png", plot = Cy_HT, width = 8, height = 6, dpi = 300)

#PIB
Cy_PIB<- pib %>%
  ggplot(aes(x = Data))+
  geom_line(aes(y= cycle), color = 'blue', linewidth= 1.2) +
  labs(title = "Componente Ciclico do PIB",
       x = "Trimestre",
       y = "")+
  theme_minimal(
    base_size = 13,
    base_family = "serif"
  )+
  theme(plot.title = element_text(size = 20, hjust = 0.5))

ggsave("Cycle_PIB.png", plot = Cy_PIB, width = 8, height = 6, dpi = 300)

#FBCF
Cy_FBCF<- fbcf %>%
  ggplot(aes(x = Data))+
  geom_line(aes(y= cycle), color = 'blue', linewidth= 1.2) +
  labs(title = "Componente Ciclico do Capital fixo",
       x = "Trimestre",
       y = "")+
  theme_minimal(
    base_size = 13,
    base_family = "serif"
  )+
  theme(plot.title = element_text(size = 20, hjust = 0.5))

ggsave("Cycle_FBCF.png", plot = Cy_FBCF, width = 8, height = 6, dpi = 300)

#Familias
Cy_Fam<- fam %>%
  ggplot(aes(x = Data))+
  geom_line(aes(y= cycle), color = 'blue', linewidth= 1.2) +
  labs(title = "Componente Ciclico do Consumo das Familias",
       x = "Trimestre",
       y = "")+
  theme_minimal(
    base_size = 13,
    base_family = "serif"
  )+
  theme(plot.title = element_text(size = 20, hjust = 0.5))

ggsave("Cycle_Cons_Fam.png", plot = Cy_Fam, width = 8, height = 6, dpi = 300)

#APU
Cy_APU<- apu %>%
  ggplot(aes(x = Data))+
  geom_line(aes(y= cycle), color = 'blue', linewidth= 1.2) +
  labs(title = "Componente Ciclico do Despesa Pública",
       x = "Trimestre",
       y = "")+
  theme_minimal(
    base_size = 13,
    base_family = "serif"
  )+
  theme(plot.title = element_text(size = 20, hjust = 0.5))

ggsave("Cycle_APU.png", plot = Cy_APU, width = 8, height = 6, dpi = 300)
