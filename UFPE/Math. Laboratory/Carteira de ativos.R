#https://rpubs.com/Lucas_Venturini/661262
'Buscando dados dos ativos financeiros
Dado que é complicado a importação de dados a partir do modelo copia e cola, desenvolvedores criaram um pacote que realiza essa operação para o usuário.
'
install.packages('BatchGetSymbols')
library(tidyverse)

## -- Attaching packages ------------------------------------------- tidyverse 1.3.0 --
## v ggplot2 3.3.2     v purrr   0.3.4
## v tibble  3.0.1     v dplyr   1.0.0
## v tidyr   1.1.0     v stringr 1.4.0
## v readr   1.3.1     v forcats 0.5.0
## -- Conflicts ---------------------------------------------- tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
library(ggplot2)
library(BatchGetSymbols)

## Loading required package: rvest
## Loading required package: xml2
## 
## Attaching package: 'rvest'
## The following object is masked from 'package:purrr':
## 
##     pluck
## The following object is masked from 'package:readr':
## 
##     guess_encoding
## 
# set dates
first.date <- as.Date('2024-01-01')
last.date <- Sys.Date()
freq.data <- 'weekly'
tickers <- c('BBDC3.SA','ELET3.SA','PETR4.SA','CYRE3.SA')
tickers
l.out <- BatchGetSymbols(tickers = tickers, 
                         first.date = first.date,
                         last.date = last.date, 
                         freq.data = freq.data,
                         cache.folder = file.path(tempdir(), 
                                                  'BGS_Cache') )
                                                  
print(l.out$df.control)

p <- ggplot(l.out$df.tickers, aes(x = ref.date, y = price.close))
p <- p + geom_line()
p <- p + facet_wrap(~ticker, scales = 'free_y') 
print(p)


# Filtrando os retornos Bradesco
bradesco <- l.out$df.tickers %>% 
                na.omit() %>% 
                filter(ticker == 'BBDC3.SA') %>% 
                select( ret.closing.prices) %>% 
  rename( bradesco = ret.closing.prices) #Observe que modificamos o nome da coluna.


#Filtrando os retornos da Cyrela
cyrela <- l.out$df.tickers %>% 
                na.omit() %>% 
                filter(ticker == 'CYRE3.SA') %>% 
                select( ret.closing.prices) %>% 
  rename(Cyrela = ret.closing.prices) #Observe que modificamos o nome da coluna.

#Filtrando os retornos do Iguatemi
eletrobras <- l.out$df.tickers %>% 
                na.omit() %>% 
                filter(ticker == 'ELET3.SA') %>% 
                select( ret.closing.prices) %>% 
  rename(eletrobras= ret.closing.prices) #Observe que modificamos o nome da coluna.

#Filtrando os retornos da Petrobras
petrobras <- l.out$df.tickers %>% 
                na.omit() %>% 
                filter(ticker == 'PETR4.SA') %>% 
                select( ret.closing.prices) %>% 
  rename(Petrobras = ret.closing.prices) #Observe que modificamos o nome da coluna.


#Criando os uma variável com todos os retornos
retornos <- cbind(bradesco,cyrela,eletrobras,petrobras)

retornos


'Vamos testar essas correlações com os ativos escolhidos na seção anterior. Por exemplo, a correlação entre Bradesco e Movida'
cor(bradesco, cyrela)


'Para começarmos vamos carregar os pacotes:'
library(fPortfolio)
## Loading required package: timeDate
## Loading required package: timeSeries
## Loading required package: fBasics
## Loading required package: fAssets

'Caso não tenha instalado o pacote, lembre-se de instalar o mesmo e depois carregar.
Vamos transformar, agora, os nossos retornos em séries temporais. Todos os retornos estão guardados na variável que criamos “Retornos”.'
#Transformando os retornos em séries de tempo
retornos <- as.timeSeries(retornos)
'Agora vamos encontrar o ponto de tangência e ver os respectivos pesos de cada ação:'

# Portfolio com a melhor relação entre risco/retorno
portfolio.eficiente <- tangencyPortfolio(retornos, spec = portfolioSpec(), constraints = "LongOnly")
portfolio.eficiente

'Dado os ativos que escolhemos (Bradesco, Cyrela, Eletrobras e Petrobras) o nosso portfólio que apresenta o melhor risco/retorno seria composto de 47,6% em Cyrela e 52,22% em Petrobras.
A saída Mean, apresenta o retorno esperando, que no caso seria 0.5%, com um risco da carteira sendo representado pela saída VaR, de valor 7,90%.
Agora se quisermos achar os pesos da carteira com o menor risco possivel, temos:'
# Portfolio com menor risco:
portfolio.menor.risco <- minvariancePortfolio(retornos,spec = portfolioSpec(), constraints = "LongOnly")
portfolio.menor.risco

'Observamos que para o menor peso da carteira temos Bradesco representando 52,61% e Iguatemi representando 47,39%. Ou seja, casdo fossemos investir R$ 100, cerca de 52,61 reais seriam para bradesco e 47,39 reais para o iguatemi.
Resolvido os pesos da carteira, vamos ver graficamente, para isso iremos calcular antes o ponto eficiente:'
# Obtenção da fronteira
fronteira <- portfolioFrontier(retornos)

'Feito isso, vamos ao gráfico:'

# Gráfico com a fronteira eficiente
frontierPlot(fronteira, col = c('blue', 'red'), pch = 20)

## Adicionando informações no gráfico
monteCarloPoints(fronteira, mcSteps = 5000, pch = 20, cex = 0.25 )

Resolvido os pesos da carteira, vamos ver graficamente, para isso iremos calcular antes o ponto eficiente:
# Obtenção da fronteira
fronteira <- portfolioFrontier(retornos)

