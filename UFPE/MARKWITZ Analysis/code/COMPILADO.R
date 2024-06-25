##1.a - Importar dados de dividendos e precos de empresas ----------------------

# Referência ao pipe-------------------------------
`%>%` <- magrittr::`%>%`


##Pacotes para coletar os dados dos ativos e realizar análise de performance

#Para instalar um pacote qualquer
#install.packages("quantmod")

library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(tidyverse)
library(rb3)
library(ggplot2)

##Importar os dados de interesse (Yahoo Finanças)---------------------------------------------

#Indicar quais os tickers
tickers <- c("PETR4.SA", "BBAS3.SA", "ABEV3.SA",
             "ITUB4.SA", "PRIO3.SA", "VALE3.SA", "STBP3.SA", "RAIL3.SA", "TOTS3.SA"
             ,"TAEE11.SA", "CYRE3.SA",	"EMBR3.SA",	"WEGE3.SA",	"EZTC3.SA",	"RADL3.SA"
             ,	"CSNA3.SA",	"ELET3.SA",	"BPAN4.SA",	"UGPA3.SA",	"DASA3.SA",
             	"MSFT",	"NVDA",	"INTC",	"BA",	"IBM",	"PG", "AMZN", "WBA", "UNH", "MRK"
)

#Definir a data de início de coleta

start <- as.Date("2016-01-01")

preco <- getSymbols(tickers,
                    auto.assign = TRUE,
                    warnings = FALSE,
                    from = start,
                    src = "yahoo") %>%
  map(~Cl(get(.))) %>%
  reduce(merge) %>%
  `colnames<-`(tickers)

rm(PETR4.SA, BBAS3.SA, ABEV3.SA,
   ITUB4.SA, PRIO3.SA, VALE3.SA, STBP3.SA, RAIL3.SA, TOTS3.SA,TAEE11.SA,
   CYRE3.SA,	EMBR3.SA,	WEGE3.SA,	EZTC3.SA,	RADL3.SA,	CSNA3.SA,	ELET3.SA,
   BPAN4.SA, UGPA3.SA,	DASA3.SA,
   MSFT,	NVDA,	INTC,	BA,	IBM,	PG, AMZN, WBA, UNH, MRK)


##Importar os dados de renda fixa para o Brasil -----------------------------
imab5 <- readxl::read_excel("C:/Users/Arthu/Downloads/IMAB5-HISTORICO.xlsx")
imab5 <- imab5[,2:3]
colnames(imab5) <- c("data", "imab5")

irfm1 <- readxl::read_excel("C:/Users/Arthu/Downloads/IRFM1-HISTORICO.xlsx")
irfm1 <- irfm1[,2:3]
colnames(irfm1) <- c("data", "irfm1")

imas <- readxl::read_excel("C:/Users/Arthu/Downloads/IMAS-HISTORICO.xlsx")
imas <- imas[,2:3]
colnames(imas) <- c("data", "imas")

rf <- merge(imab5,imas, by="data")
rf <- merge(rf, irfm1, by="data")
rm(imas,imab5,irfm1)

rf$date <- as.Date(rf$data, format = "YYYY-DD-MM")

rf <- xts::xts(rf[,2:4], order.by = rf$date)

##Adicionar a variável cambial e corrigir os preços internacionais

dolar <- read.csv("C:/Users/Arthu/Downloads/BRL=X.csv")
dolar <- dolar[,c(1,5)]
dolar$Date <- as.Date(dolar$Date)
colnames(dolar) <- c("data", "dolar")


euro <- read.csv("C:/Users/Arthu/Downloads/EURBRL=X.csv")
euro <- euro[,c(1,5)]
euro$Date <- as.Date(euro$Date)
colnames(euro) <- c("data", "euro")

cambio <- merge(dolar,euro, by="data")
rm(dolar,euro)

cambio <- xts::xts(cambio[,2:3], order.by = cambio$data)


###Realizar o merge entre os dados e corrigir os valores em moeda externa
assets <- merge(preco,cambio, all=FALSE)
assets <- merge(assets, rf, all=FALSE)


#Salvar os dados --------------------------------------------
saveRDS(assets, file = here::here("data", "raw", "dados_brutos.Rds"))

##1.a - Importar dados de dividendos e precos de empresas ----------------------

# Referência ao pipe-------------------------------
`%>%` <- magrittr::`%>%`

install.packages("quantmod", "PerformanceAnalytics", "PortfolioAnalytics",
                 "tidyverse", "rb3", "ggplot2")
library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(tidyverse)
library(rb3)

##Importar os dados -----------------------------------

assets <- readRDS(here::here("data", "raw", "dados_brutos.Rds"))

library(ggplot2)

xts::plot.xts(assets[,1:4], multi.panel=TRUE, yaxis.same=FALSE)
xts::plot.xts(assets[,5:8], multi.panel=TRUE, yaxis.same=FALSE)
xts::plot.xts(assets[,9:12], multi.panel=TRUE, yaxis.same=FALSE)
xts::plot.xts(assets[,12:14], multi.panel=TRUE, yaxis.same=FALSE)
xts::plot.xts(assets[,14:17], multi.panel=TRUE, yaxis.same=FALSE)
xts::plot.xts(assets[,18:21], multi.panel=TRUE, yaxis.same=FALSE)
xts::plot.xts(assets[,22:25], multi.panel=TRUE, yaxis.same=FALSE)
xts::plot.xts(assets[,26:29], multi.panel=TRUE, yaxis.same=FALSE)
xts::plot.xts(assets[,30:33], multi.panel=TRUE, yaxis.same=FALSE)

#Transformar a base diária em base mensal -
preco <- to.monthly(assets,
                    indexAt = "lastof",
                    OHLC = FALSE)

xts::plot.xts(preco, multi.panel=TRUE, yaxis.same=FALSE)
xts::plot.xts(preco[,1:4], multi.panel=TRUE, yaxis.same=FALSE)
xts::plot.xts(preco[,5:8], multi.panel=TRUE, yaxis.same=FALSE)
xts::plot.xts(preco[,9:12], multi.panel=TRUE, yaxis.same=FALSE)
xts::plot.xts(preco[,12:14], multi.panel=TRUE, yaxis.same=FALSE)
xts::plot.xts(preco[,14:17], multi.panel=TRUE, yaxis.same=FALSE)
xts::plot.xts(preco[,18:21], multi.panel=TRUE, yaxis.same=FALSE)
xts::plot.xts(preco[,22:25], multi.panel=TRUE, yaxis.same=FALSE)
xts::plot.xts(preco[,26:29], multi.panel=TRUE, yaxis.same=FALSE)
xts::plot.xts(preco[,30:33], multi.panel=TRUE, yaxis.same=FALSE)

#Corrigir o cambio dos ativos -----------------------
preco$MSFT = preco$MSFT*preco$dolar
preco$NVDA = preco$NVDA*preco$dolar
preco$INTC = preco$INTC*preco$dolar
preco$BA = preco$BA*preco$dolar
preco$IBM = preco$IBM*preco$dolar
preco$PG = preco$PG*preco$dolar
preco$AMZN = preco$AMZN*preco$dolar
preco$WBA = preco$WBA*preco$dolar
preco$UNH = preco$UNH*preco$dolar
preco$MRK = preco$MRK*preco$dolar


##Calcular a taxa de retorno -----------------------------
retorno_ativos <- Return.calculate(preco,
                                   method = "log") %>%
  na.omit()

##Plotar as séries em termos de retornos mensais ---------
xts::plot.xts(retorno_ativos[,1:4], multi.panel=TRUE, yaxis.same=FALSE)
xts::plot.xts(retorno_ativos[,5:8], multi.panel=TRUE, yaxis.same=FALSE)
xts::plot.xts(retorno_ativos[,9:12], multi.panel=TRUE, yaxis.same=FALSE)
xts::plot.xts(retorno_ativos[,12:14], multi.panel=TRUE, yaxis.same=FALSE)
xts::plot.xts(retorno_ativos[,14:17], multi.panel=TRUE, yaxis.same=FALSE)
xts::plot.xts(retorno_ativos[,18:21], multi.panel=TRUE, yaxis.same=FALSE)
xts::plot.xts(retorno_ativos[,22:25], multi.panel=TRUE, yaxis.same=FALSE)
xts::plot.xts(retorno_ativos[,26:29], multi.panel=TRUE, yaxis.same=FALSE)
xts::plot.xts(retorno_ativos[,30:33], multi.panel=TRUE, yaxis.same=FALSE)


##Realizar uma análise descritiva sobre os momentos das séries de retorno,
#considerando uma taxa livre de risco de 6,5% a.a.

table <- table.AnnualizedReturns(retorno_ativos, Rf = .005)
table[4,] <- kurtosis(retorno_ativos)
table[5,] <- skewness(retorno_ativos)

##Ibovespa
chart.Histogram(retorno_ativos$BOVA11.SA, main = "Curva de Densidade - Ibovespa",
                methods = c("add.density", "add.normal"), breaks=40)



#Título livre de risco
chart.Histogram(retorno_ativos$imab5, main = "Curva de Densidade - Tesouro IPCA+",
                methods = c("add.density", "add.normal"), breaks=40)

##Estimar o retorno em janelas móveis -----------------------------------


#Trade-off quanto ao tamanho da janela amostral (Janelas longas reduzem o efeito
# de ruídos, porém suavizam em excesso os movimentos ao longo dos ciclos de mercado)


chart.RollingCorrelation(retorno_ativos$BOVA11.SA, retorno_ativos[,3:5],
                         width = 12, legend.loc = "topleft")

chart.RollingCorrelation(retorno_ativos$BOVA11.SA, retorno_ativos[,6:8],
                         width = 12, legend.loc = "topleft")


corrplot::corrplot(corr = cor(retorno_ativos))

# Define os nomes dos ativos na especificação
retorno_ativos <- retorno_ativos[,c(1:9,10:18,19:27,27:30)]


##Modelo Full-Investment, Long-Only sem rebalanceamento - Janela 2012-2017

portfolio_spec <- portfolio.spec(assets = names(retorno_ativos))
print(portfolio_spec)

retorno_window = window(retorno_ativos, start=as.Date("2016-01-01"),
                        end = as.Date("2023-12-31"))

# Considera que a soma dos pesos será igual a 1

portfolio_spec <- add.constraint(portfolio = portfolio_spec,
                                 type = "full_investment")

# Não permite vendas a descoberto

portfolio_spec <- add.constraint(portfolio = portfolio_spec,
                                 type = "long_only")

print(portfolio_spec)

# Adiciona os objetivos - Retorno esperado através da média amostral

portfolio_spec <- add.objective(portfolio = portfolio_spec,
                                type = "return",
                                name = "mean")

# Adiciona os objetivos - Risco esperado através do desvio padrão

portfolio_spec <- add.objective(portfolio = portfolio_spec,
                                type = "risk",
                                name = "StdDev")


# Calcula a otimização do portfólio

opt <- optimize.portfolio(retorno_window,
                          portfolio = portfolio_spec,
                          optimize_method = "random",
                          trace = TRUE)

print(opt)

# Plota o gráfico de Risco x Retorno

chart.RiskReward(opt,
                 risk.col = "StdDev",
                 return.col = "mean",
                 chart.assets = TRUE,
                 main = "Risco x Retorno - Combinações dos ativos selecionados")

weights = extractWeights(opt)


r_sample = Return.portfolio(retorno_window, weights = weights)
r_outsample = Return.portfolio(window(retorno_ativos,
                                      start= as.Date("2016-01-01")),
                               weights = weights)

r_eqweight = Return.portfolio(window(retorno_ativos,
                                     start= as.Date("2016-01-01")),
                              weights = rep(1/31,31))



r_opt <- table.AnnualizedReturns(r_sample)
r_opt_out = table.AnnualizedReturns(r_outsample)
r_eqw_out <- table.AnnualizedReturns(r_eqweight)

r_opt_out

chart.Weights(opt)

colnamesport_opt <- c("port_opt", "port_eq")

##Portfolio com pesos máximos e mínimos por ativo------------------------------
port_box <- portfolio.spec(assets = names(retorno_ativos))

port_box <- add.constraint(portfolio = portfolio_spec,
                           type = "box",
                           min = .03,
                           max = .25)

print(port_box)

opt_box <- optimize.portfolio(retorno_window,
                              portfolio = port_box,
                              optimize_method = "random",
                              trace = TRUE)

weights_box = extractWeights(opt_box)


r_sample_box = Return.portfolio(retorno_window, weights = weights_box)
r_outsample_box = Return.portfolio(window(retorno_ativos,
                                          start= as.Date("2016-01-01")),
                                   weights = weights_box)

r_box <- table.AnnualizedReturns(r_sample_box)
r_box_out <- table.AnnualizedReturns(r_outsample_box)

r_box_out

chart.Weights(opt_box)
print(opt_box)

# Adiciona restrição por categoria de ativos-------------------------------------

port_groups <- add.constraint(portfolio = port_box,
                              type = "group",
                              groups = list(c(1, 5, 19), c(2, 4, 18), c(3),
                                            c(6, 16), c(7), c(8), c(9, 21, 25),
                                            c(10), c(11), c(12, 24), c(13), c(14),
                                            c(15, 28), c(17), c(20), c(22, 23),
                                            c(26), c(27), c(29), c(30)
                                            ),
                              group_min = 0.03, group_max = 0.4)

port_groups

opt_groups <- optimize.portfolio(retorno_window,
                                 portfolio = port_groups,
                                 optimize_method = "random",
                                 trace = TRUE)
chart.Weights(opt_groups)
print(opt_groups)

weights_groups = extractWeights(opt_groups)
chart.Weights(opt_groups)

r_sample_groups = Return.portfolio(retorno_window, weights = weights_groups)
r_outsample_groups = Return.portfolio(window(retorno_ativos,
                                             start= as.Date("2016-01-01")),
                                      weights = weights_groups)

r_groups = table.AnnualizedReturns(r_sample_groups)
r_groups_out <- table.AnnualizedReturns(r_outsample_groups)



r_out <- cbind(r_eqw_out, r_opt_out, r_box_out, r_groups_out)
colnames(r_out) <- c("pesos_iguais", "opt", "box", "group")
r_out

r <- cbind(r_eqweight, r_outsample, r_outsample_box, r_outsample_groups)
colnames(r) <- c("pesos_iguais", "opt", "box", "group")

charts.PerformanceSummary(r)


########Análise de Modelo com rebalanceamento periódico -----------------------

# Run the optimization
opt_rebal_base <- optimize.portfolio.rebalancing(R = retorno_ativos,
                                                 portfolio = portfolio_spec,
                                                 optimize_method = "random",
                                                 rebalance_on = "quarters",
                                                 training_period = 36,
                                                 rolling_window = 36)

# Print the results
print(opt_rebal_base)
chart.Weights(opt_rebal_base, main = "Rebalanciamento", colorset = NULL)

#Computar o retorno do portfolio
r_opt_rebal <- Return.portfolio(R = retorno_ativos,
                                weights = extractWeights(opt_rebal_base))
colnames(r_opt_rebal) <- "base"

charts.PerformanceSummary(r_opt_rebal, main = "Return")
