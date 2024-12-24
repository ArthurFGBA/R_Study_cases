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

plot.ts(industria$Horas_Trabalhadas)

apu <- readr::read_csv2("~/MeusProjetos/R_Study_cases/Macroeconomics/APU .csv",
                        col_names = TRUE)
apu<-as.tibble(apu)

apu<- apu%>%
  rename(Adm_Pub = colnames(apu[,2])) %>%
  mutate(Data = yq(Data), 
         Adm_Pub = log(as.numeric(Adm_Pub)))

plot.ts(apu$Adm_Pub)

fbcf <- readr::read_csv("~/MeusProjetos/R_Study_cases/Macroeconomics/Formação bruta de capital.csv"
                        ,col_names = TRUE)
fbcf <- as.tibble(fbcf)

fbcf <- fbcf %>%
  select(-`...3`)%>%
  rename(Capital_Fixo = colnames(fbcf[,2]))%>%
  mutate(Data  = yq(Data), Capital_Fixo = log(as.numeric(Capital_Fixo)))

plot.ts(fbcf$Capital_Fixo)

fam<- readr::read_csv("~/MeusProjetos/R_Study_cases/Macroeconomics/Consumo final- Familias.csv"
                      ,col_names = TRUE)
fam<-as.tibble(fam)

fam<- fam %>%
  select(-`...3`)%>%
  rename(Cons_Familias = colnames(fam[,2]))%>%
  mutate(Data = yq(Data), Cons_Familias = log(as.numeric(Cons_Familias)))

plot.ts(fam$Cons_Familias)

pib<- readr::read_csv("~/MeusProjetos/R_Study_cases/Macroeconomics/PIB - preços de mercado.csv"
                      ,col_names = TRUE)
pib<-as.tibble(pib)

pib<- pib %>%
  select(-`...3`)%>%
  rename(PIB_Nominal = colnames(pib[,2]))%>%
  mutate(Data = yq(Data), PIB_Nominal = log(as.numeric(PIB_Nominal)))

plot.ts(pib$PIB_Nominal)

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
