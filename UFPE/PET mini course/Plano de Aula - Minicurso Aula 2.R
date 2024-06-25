#---------------------------------------------------#
#     PLANO DE AULA COMENTADO - MINICURSO DE R      #
#               PET Economia UFPE                   #
#                 Caio França                       #
#---------------------------------------------------#

# PACOTES
# BAIXAR APENAS UMA VEZ A BIBLIOTECA
# install.packages("readr")
# install.packages("dplyr")
# install.packages("stringr")
# install.packages("lubridate")
# install.packages("purrr")
# install.packages("ggplot2")

# Importar a Bibliotecas
library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(purrr)
library(ggplot2)

# Importar da forma nativa
# read.csv2 importa um aquivo csv separado por ;
df <- read.csv2('/home/caio/Documentos/Faculdade/PET Economia UFPE/Curso R/IPCA.csv',
                header = FALSE)
df

class(df)      
View(df)        # Visualizar
str(df)         # Estrutura
nrow(df)        # Número de Linhas
ncol(df)        # Número de Colunas
dim(df)         # Dimensões
colnames(df)    # Nomes das Colunas
head(df)        # Primeiros linhas do DF
tail(df)        # Últimas linhas do DF

df[,1]          # Selecionando uma coluna (Vetor)
df[2,]          # Selecionando uma linha (Data Frame)
df$V1           # Selecionando uma coluna pelo nome (Vetor)
df["V1"]        # Selecionando uma coluna pelo nome (Data Frame)


df_tibble <- as_tibble(df)  # tibble do tidyverse
df_tibble

df_tibble[ ,1]  # Selecionando uma coluna (tibble)
df_tibble[2, ]  # Selecionando uma linha (tibble)
df_tibble$V1    # Selecionando uma coluna (vetor)
df_tibble["V1"] # Selecionando uma coluna (tibble)

# Deletando uma coluna
df <- df[, -3]                         # Forma Nativa
df_tibble <- select(df_tibble, -V3)    # Forma do tibble
df_tibble <- select(df_tibble, V1, V2) # Forma do tibble

# Deletando uma linha
df <- df[-1 ,]                                         # Forma Nativa
rownames(df) <- seq(1, nrow(df))                       # Ajuste do índice
df_tibble <- slice(df_tibble, seq(2, nrow(df_tibble))) # Forma do tibble

# Ajustar nome de colunas
colnames(df) <- c("Time", "IPCA")                      # Forma Nativa
df_tibble <- rename(df_tibble, Time = V1, IPCA = V2)   # Forma do tibble

# Ajuste Datas: Lubridate
df$Time <- ym(df$Time)                                 # Forma Nativa
df_tibble <- mutate(df_tibble, Time = ym(Time))        # Forma do tibble

# Ajuste Numeros: Stringr
df$IPCA <- as.numeric(str_replace(df$IPCA, ",", "."))  # Forma Nativa
df_tibble <- mutate(df_tibble,                         # Forma do tibble
                    IPCA = as.numeric(str_replace(IPCA, ",", "."))) 

# PIPE: magrittr
media <- 1:10 %>%
  sqrt() %>%
  log() %>%
  mean()

mean(log(sqrt(1:10)))

# Utilizando o pipe
df_tibble <- df_tibble %>%
  select(-V3) %>%
  slice(seq(2, nrow(.))) %>%
  rename(Time = V1, IPCA = V2) %>%
  mutate(Time = ym(Time), IPCA = as.numeric(str_replace(IPCA, ",", ".")))


# Importação completa, utilizando a biblioteca readr
# read_csv2 importa um aquivo csv separado por ;
df <- read_csv2(file = '/home/caio/Documentos/Faculdade/PET Economia UFPE/Curso R/IPCA.csv', 
                col_names = c("Time", "IPCA", "V3"),
                col_select = c("Time", "IPCA"),
                skip = 1,
                col_types = cols(
                  Time = col_date(format = "%Y.%m"),
                  IPCA = col_double()
                ))


lag(1:10)
lead(1:10)
cumprod(1:10)

# Aplicações
df <- df %>%
  mutate(inflacao         = IPCA / lag(IPCA),
         inflacao_2       = inflacao - 1,
         inflacao_percent = inflacao_2 * 100,
         
         inflacao         = replace(inflacao, 1, 1),
         acumulada        = cumprod(inflacao),
         inflacao         = replace(inflacao, 1, NA))


# Separando em trimestres
df_mensal <- df %>%
  filter(month(Time) %in% c(3, 6, 9, 12)) %>%
  select(Time, IPCA)

# Aplicações para trimestres
df_mensal <- df_mensal %>%
  mutate(inflacao         = IPCA / lag(IPCA),
         inflacao_2       = inflacao - 1,
         inflacao_percent = inflacao_2 * 100,
         
         inflacao = replace(inflacao, 1, 1),
         acumulada = cumprod(inflacao),
         inflacao = replace(inflacao, 1, NA))


intensi <- function(x){
  if(is.na(x)){
    return(NA)
  }else if (x > 1.1){
    return('ALTA')
  } else if(x > 1.01){
    return('MEDIANA')
  } else{
    return('BAIXA')
  }
}

?map_chr        # Utilizando para aplicar a função intensi() em um vetor

# Colocando em fator para organizar os gráficos
df <- df %>%
  mutate(intensidade = factor(map_chr(inflacao, intensi), levels = c("BAIXA", "MEDIANA", "ALTA")))

df_mensal <- df_mensal %>%
  mutate(intensidade = factor(map_chr(inflacao, intensi), levels = c("BAIXA", "MEDIANA", "ALTA")))


#---------------- Plot ----------------#

## Pré Formatações

# theme_minimal()
# theme_classic()
# theme_bw()
# theme_dark()
# theme_grey()
# theme_linedraw()
# theme_test()
# theme_gray()
# theme_void()


# Gráfico de Linha
# linewidth -> grossura da linha
# linetype  -> Tipo da linha (1,2,3,4,5 e 6)
# labs      -> Nome do título e dos eixos
# color     -> Cor da Linha (OU nome OU o código (EXEMPLO: #fd881e))
ggplot(df, aes(x = Time, y = inflacao_percent)) +
  geom_line(color = 'black', linewidth = 0.5, linetype = 1) +
  labs(title = 'Série Temporal',
       x = 'Anos',
       y = 'Inflação') +
  theme_classic()

# Histograma
# binwidth -> Tamanhos das Barras (Utilizar binwidth ou bins)
# bins     -> Número de Barras
# fill     -> Cor da Barra
# color    -> Cor do Contorno
ggplot(df, aes(x = inflacao_percent)) +
  geom_histogram(binwidth = 5, fill = 'blue', color = 'black') +
  labs(title = 'Histograma',
       x = 'Variação',
       y = 'Quantidade') +
  theme_classic()

# Gráfico de Barras
ggplot(df[-1 ,], aes(x = intensidade)) +
  geom_bar(fill = 'yellow', color = 'black') +
  labs(title = 'Gráfico de Barras',
       x = 'Intensidade',
       y = 'Quantidade') +
  theme_classic()

# Grafico de Box-Plot
df <- df %>%
  mutate(decada = as.character(year(Time)),
         decada = str_replace(decada, "\\d$", "0"),
         decada = factor(decada, levels = c("1970", "1980", "1990", "2000", "2010", "2020")))

ggplot(df[-1 ,], aes(x = decada ,y = inflacao_percent)) +
  geom_boxplot(color = "black",fill = "blue", outliers = FALSE) +
  geom_jitter(width = 0.1, color = "black", alpha = 0.5) +
  labs(title = 'Box-Plot',
       x = 'Decadas',
       y = 'Inflação') +
  theme_classic()

# Gráfico com Mais Variaveis
df_cambio <- read_csv(file = '/home/caio/Documentos/Faculdade/PET Economia UFPE/Curso R/CAMBIO.csv', 
                      col_names = c("Time", "Cambio", "V3"),
                      col_select = c("Time", "Cambio"),
                      skip = 1,
                      col_types = cols(
                        Time = col_date(format = "%Y.%m"),
                        IPCA = col_double()
                      ))

# Juntado os Datasets
df <- df %>%
  select(Time, inflacao_percent) %>%
  left_join(df_cambio, by = 'Time') %>%
  slice(seq(176, nrow(.)))

# scale_color_manual    -> Legenda por cor
# scale_linetype_manual -> Legenda por tipo de linha
ggplot(df, aes(x = Time)) +
  geom_line(aes(y = inflacao_percent, colour = 'Inflação', linetype = 'Inflação')) +
  geom_line(aes(y = Cambio, colour = 'Câmbio', linetype = 'Câmbio')) +
  scale_color_manual(
    name = "Váriáveis",
    values = c('Inflação' = 'blue', 'Câmbio' = 'red')) +
  scale_linetype_manual(
    name = "Váriáveis",
    values = c('Inflação' = 5, 'Câmbio' = 1)) +
  labs(title = '',
       x = 'Time',
       y = 'Inflação/Câmbio') +
  theme_classic()


# Gráfico de Dispersão
# shape  -> Tipos das bolinha (1,2,3,4,5,6,7,8,9,10,
#                              11,12,13,14,15,16,17,18,19
#                              20,21,22,23,24)
# color  -> Cor da linha da Regressão
# method -> Tipo da Regressão
# se     -> Intervalo de Confiança
# family -> Fonte do texto
# size   -> Tamanho do texto
ggplot(airquality, aes(x = Ozone, y = Temp)) +
  geom_point(color = 'black', shape = 6) +
  geom_smooth(method = "lm", color = 'red', se = FALSE) +
  labs(title = 'Gráfico de Dispersão',
       x = 'Nível de Ozonio',
       y = 'Temperatura') +
  theme_classic() +
  theme(
    plot.title = element_text(family = "serif", size = 20, hjust = 0.5),  
    axis.title.x = element_text(family = "sans", size = 12),  
    axis.title.y = element_text(family = "sans", size = 12),  
    axis.text.x = element_text(family = "mono", size = 10),   
    axis.text.y = element_text(family = "mono", size = 10),
    axis.line = element_line(colour = "black"),
    panel.grid = element_blank(),
    panel.background = element_blank()
  )
