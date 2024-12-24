#Questão 1 
install.packages('tidyverse','ggplot2','readr','dplyr',
                 'stringr','lubridate','purrr','mFilter','corrplot')
library(tidyverse)
library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(purrr)
library(ggplot2)
library(mFilter)
library(corrplot)
library(skimr)

getwd()
setwd("C:/Users/Arthu/OneDrive/Documentos/MeusProjetos/R_Study_cases/Macroeconomics")

########Q1########
####----------####
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

#III- dispersão com PIB
#PIBxIndustria
pibxind<- merge(pib, industria, by = "Data") 

pibxind<- pibxind %>%
  ggplot(aes(x= cycle.x ,y = cycle.y))+
  geom_point()+
  geom_smooth(color= 'red', linetype='dashed')+
  labs(title = "Componente Ciclico do PIBxIndustria",
       x = "PIB",
       y = "Horas Trabalhadas")+
  theme_minimal(
    base_size = 13,
    base_family = "serif"
  )+
  theme(plot.title = element_text(size = 20, hjust = 0.5))

ggsave("Cycle_PIBxInd.png", plot = pibxind, width = 8, height = 6, dpi = 300)

#PIBxFBCF
pibxfbcf<- merge(pib, fbcf, by = "Data") 

pibxfbcf<- pibxfbcf %>%
  ggplot(aes(x= cycle.x ,y = cycle.y))+
  geom_point()+
  geom_smooth(color= 'red', linetype='dashed')+
  labs(title = "Componente Ciclico do PIBxFBCF",
       x = "PIB",
       y = "Capital Fixo")+
  theme_minimal(
    base_size = 13,
    base_family = "serif"
  )+
  theme(plot.title = element_text(size = 20, hjust = 0.5))

ggsave("Cycle_PIBxFBCF.png", plot = pibxfbcf, width = 8, height = 6, dpi = 300)

#PIBxConsumo das familias]
pibxfam<- merge(pib, fam, by = "Data") 

pibxfam<- pibxfam %>%
  ggplot(aes(x= cycle.x ,y = cycle.y))+
  geom_point()+
  geom_smooth(color= 'red', linetype='dashed')+
  labs(title = "Componente Ciclico do PIBxConsumo",
       x = "PIB",
       y = "Consumo das Familias")+
  theme_minimal(
    base_size = 13,
    base_family = "serif"
  )+
  theme(plot.title = element_text(size = 20, hjust = 0.5))

ggsave("Cycle_PIBxCons_Fam.png", plot = pibxfam, width = 8, height = 6, dpi = 300)

#PIBxDespesa Adm Publica
pibxapu<- merge(pib, apu, by = 'Data')

pibxapu<- pibxapu %>%
  ggplot(aes(x= cycle.x ,y = cycle.y))+
  geom_point()+
  geom_smooth(color= 'red', linetype='dashed')+
  labs(title = "Componente Ciclico do PIBxDespesa Pública",
       x = "PIB",
       y = "Despesa Pública")+
  theme_minimal(
    base_size = 13,
    base_family = "serif"
  )+
  theme(plot.title = element_text(size = 20, hjust = 0.5))

ggsave("Cycle_PIBxDesp_Pub.png", plot = pibxapu, width = 8, height = 6, dpi = 300)

#IV - tabela de correlações e desvios padrão
cycle<-merge(pib, industria, by = 'Data')
cycle<-merge(cycle, apu, by = 'Data')
cycle<-cycle[,c(4,7,10,13,16)]
colnames(cycle)<-c('PIB','Industria', 'FBCF','Familias', 'Adm.Publica')

cycle_cor<-cor(cycle)
cycle_cor[,1]

desvios <- cycle %>%
  select(where(is.numeric)) %>%
  summarise(across(everything(), sd, na.rm = TRUE))

desvios[2,]<- cycle_cor[1,]
desvios[3,]<- (desvios[1,]/desvios[1,1])

rownames(desvios)<-c('Desvio Padrão', 'Correlação com PIB','DP relativo ao PIB')

write.csv(desvios,'tabela ciclos.csv', row.names = TRUE)

###Q3############
####---------------########
#I- inflação
ipca<-readr::read_csv("~/MeusProjetos/R_Study_cases/Macroeconomics/IPCA- Geral.csv"
               ,col_names = TRUE)

ipca<-as.tibble(ipca)

ipca <- ipca %>%
  select(-`...3`)%>%
  rename(IPCA = colnames(ipca[,2]))%>%
  mutate(Data  = ym(format(as.Date(paste0(Data, ".01"),
                                   format = "%Y.%m.%d"), "%Y-%m")),
         IPCA = as.numeric(IPCA))

ipca<-ipca[1:516,]

ipc<- ipca %>%
  ggplot(aes(x = Data, y = IPCA))+
  geom_line(linewidth=1.2)+
  labs(title = "Var IPCA - 1980-2022",
       x = "Mês",
       y = "")+
  theme_minimal(
    base_size = 13,
    base_family = "serif"
  )+
  theme(plot.title = element_text(size = 20, hjust = 0.5))

ggsave("ipca_1980-2022.png", plot = ipc, width = 8, height = 6, dpi = 300)

per1<- ipca[1:174,] %>%
  ggplot(aes(x = Data, y = IPCA))+
  geom_line(linewidth=1.2)+
  labs(title = "Var IPCA - 1980-1994/6",
       x = "Mês",
       y = "")+
  theme_minimal(
    base_size = 13,
    base_family = "serif"
  )+
  theme(plot.title = element_text(size = 20, hjust = 0.5))

ggsave("ipca_1980-1994.png", plot = per1, width = 8, height = 6, dpi = 300)

per2<- ipca[175:516,] %>%
  ggplot(aes(x = Data, y = IPCA))+
  geom_line(linewidth=1.2)+
  labs(title = "Var IPCA - 1994/7-2022",
       x = "Mês",
       y = "")+
  theme_minimal(
    base_size = 13,
    base_family = "serif"
  )+
  theme(plot.title = element_text(size = 20, hjust = 0.5))

ggsave("ipca_1994-2022.png", plot = per2, width = 8, height = 6, dpi = 300)

#II - M2
m2<- readr::read_csv("~/MeusProjetos/R_Study_cases/Macroeconomics/M2.csv"
                     ,col_names = TRUE)

m2<-as.tibble(m2)

m2 <- m2 %>%
  select(-`...3`)%>%
  rename(Var_m2 = colnames(m2[,2]))%>%
  mutate(Data  = ym(format(as.Date(paste0(Data, ".01"),
                                   format = "%Y.%m.%d"), "%Y-%m")),
         Var_m2 = (as.numeric(Var_m2)/lag(Var_m2)-1)*100)

mescla<-merge(m2[2:72,],ipca[104:174,],by = 'Data')

mescla <- mescla %>%
  ggplot(aes(x = Data))+
  geom_line(aes(y= IPCA, color = 'IPCA'),linewidth=1.2)+
  geom_line(aes(y= Var_m2, color = 'M2'), linewidth=1.2)+
  labs(title = "Var IPCAxM2 - 1988/8-1994/6",
       x = "Mês",
       y = "")+
  theme_minimal(
    base_size = 13,
    base_family = "serif"
  )+
  theme(plot.title = element_text(size = 20, hjust = 0.5))

ggsave("mesclaQ3_1994-2022.png", plot = mescla, width = 8, height = 6, dpi = 300)

#III- Friedman

#######Q2##########
####--------########
##I - série BR
unbr<-readr::read_csv("~/MeusProjetos/R_Study_cases/Macroeconomics/desocupação.csv"
                      ,col_names = TRUE)
unbr<-as.tibble(unbr)

unbr<-unbr%>%
  rename(Desocupacao = colnames(unbr[,2]))%>%
  mutate(Data = yq(Data)
         , Desocupacao = as.numeric(Desocupacao))

hp_unbr<-hpfilter(ts(unbr$Desocupacao
                     , start = c(2012,1), frequency = 4), freq = 1600)

unbr<- unbr %>%
  mutate(trend = hp_unbr$trend, cycle = hp_unbr$cycle)

unbrXtd<- unbr %>%
  ggplot(aes(x = Data))+
  geom_line(aes(y= Desocupacao, color = "Desocupação"), linewidth= 1.2) +
  geom_line(aes(y = trend, color = "Tendência"), linetype = 'dashed', linewidth= 1.2)+
  labs(title = "Taxa de Desocupação - BR",
       x = "Trimestre",
       y = "")+
  theme_minimal(
    base_size = 13,
    base_family = "serif"
  )+
  theme(plot.title = element_text(size = 20, hjust = 0.5))

ggsave("UNBRxTrend.png", plot = unbrXtd, width = 8, height = 6, dpi = 300)

#II- relacione com fatos ocorridos

#III - serie USA
usun<-readr::read_csv("~/MeusProjetos/R_Study_cases/Macroeconomics/UNRATE.csv"
                      ,col_names = TRUE)

usun<-as.tibble(usun)

usun<- usun %>%
  rename(Data = colnames(usun[,1])) %>%
  mutate(Data= ymd(Data),
         UNRATE = as.numeric(UNRATE))

usun<- usun %>%
  mutate(Data = floor_date(Data, "quarter")) %>%
  group_by(Data) %>%
  summarise(UNRATE = mean(UNRATE, na.rm = TRUE))

hp_usun<-hpfilter(ts(usun$UNRATE
                     , start = c(1948,1), frequency = 4), freq = 1600)

usun<- usun %>%
  mutate(trend = hp_usun$trend, cycle = hp_usun$cycle)

usunXtd<- usun %>%
  ggplot(aes(x = Data))+
  geom_line(aes(y= UNRATE, color = "Un.Rate"), linewidth= 1.2) +
  geom_line(aes(y = trend, color = "Tendência"), linetype = 'dashed', linewidth= 1.2)+
  labs(title = "Taxa de Desemprego - USA",
       x = "Trimestre",
       y = "")+
  theme_minimal(
    base_size = 13,
    base_family = "serif"
  )+
  theme(plot.title = element_text(size = 20, hjust = 0.5))

ggsave("USUNxTrend.png", plot = usunXtd, width = 8, height = 6, dpi = 300)

#IV - comparação

usxbr<-data.frame(
  Data = unbr$Data,
  BR_trend = hp_unbr$trend[1:51],
  US_trend = hp_usun$trend[257:307])

usxbr<- usxbr%>%
  ggplot(aes(x= Data))+
  geom_line(aes(y= BR_trend, color = "BR_trend"),linewidth= 1.2)+
  geom_line(aes(y= US_trend, color = "US_trend"),linewidth= 1.2)+
  labs(title = "Taxa de Desemprego - BRxUSA - 2012-2024",
       x = "Trimestre",
       y = "")+
  theme_minimal(
    base_size = 13,
    base_family = "serif"
  )+
  theme(plot.title = element_text(size = 20, hjust = 0.5))

ggsave("USxBR_Trend.png", plot = usxbr, width = 8, height = 6, dpi = 300)
  