#---------------------------------------------------
# MINICURSO DE INTRODUÇÃO À PROGRAMAÇÃO EM R (2024)
#               PET Economia UFPE
#                 Caio França
#---------------------------------------------------

# PACOTES
# install.packages("readr")
# install.packages("dplyr")
# install.packages("stringr")
# install.packages("lubridate")
# install.packages("purrr")
# install.packages("ggplot2") #Ctrl+Shift+C para comentar tudo
library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(purrr)
library(ggplot2)


df <- read.csv2("~/MeusProjetos/R_Study_cases/UFPE/Mini Curso PET/IPCA.csv",
                 header = FALSE)
df

class(df)
View(df)
str(df)
nrow(df)
ncol(df)
dim(df)
colnames(df)
head(df)
tail(df)

df[,1]
df[2,]
df$V1
df["V1"]

df_tibble <- as_tibble(df) #tibble é um tipo de arquivo do tidyverse que otimiza a visualização do dataframe
df_tibble

df_tibble$V1
df_tibble["V1"]
df_tibble[,1]

df <- df[, -3]              #Limpando dados
df_tibble <- select(df_tibble, -V3) # - nas colunas indesejadas e normal nas desejadas

df <- df[-1 ,]
rownames(df) <- seq(1, nrow(df)) #Ajustando indice das linhas
df_tibble <- slice(df_tibble, seq(2, nrow(df_tibble))) #Remoção de linhas indesejadas

colnames(df) <- c("Time", "IPCA")  #Adicionando Titulo correto as colunas
df_tibble <- rename(df_tibble, Time = V1, IPCA = V2) #função do tidyverse

df$Time <- ym(df$Time)  #Lubridate ym = yearmonth podese usar ymd= yearmonthday
df_tibble <- mutate(df_tibble, Time = ym(Time)) #trocar tipo das variaveis da coluna de string para data

df$IPCA <- as.numeric(str_replace(df$IPCA, ",", "."))  #Trocar ponto e virgula
df_tibble <- mutate(df_tibble, IPCA = as.numeric(str_replace(IPCA, ",", "."))) #Trocar ponto e virgula

df_tibble <- df_tibble %>%  #pipeline para tratamento de dados para facilitar o trabalho
  select(-V3) %>%  #|> pipe nativo
  slice(seq(2, nrow(.))) %>%
  rename(Time = V1, IPCA = V2) %>% #pipe para na penultima função
  mutate(Time = ym(Time), IPCA = as.numeric(str_replace(IPCA, ",", "."))) 
  

df <- read_csv2("~/MeusProjetos/R_Study_cases/UFPE/Mini Curso PET/IPCA.csv", 
                 col_names = c("Time","IPCA","V3"), 
                 col_select = c("Time","IPCA"),
                 skip = 1,
                 col_types = cols(
                  Time = col_date(format = "%y.%m"),
                  IPCA = col_double()
                  )
                )
lag(df$IPCA)

df <- df %>%
  mutate(inflacao = IPCA/lag(IPCA),
         inflacao_2 = inflacao-1,
         inflacao_percent = inflacao_2*100,
         acumulada = cumprod(inflacao))




df_mensal <- df %>%
  filter(month(Time) %in% c(3, 6, 9, 12)) %>%
  select(Time, IPCA)


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

df <- df %>%
  mutate(intensidade = factor(map_chr(inflacao, intensi), levels = c("BAIXA", "MEDIANA", "ALTA")))

df_mensal <- df_mensal %>%
  mutate(intensidade = factor(map_chr(inflacao, intensi), levels = c("BAIXA", "MEDIANA", "ALTA")))


#---------------- Plot ----------------#
# theme_minimal()
# theme_classic()
# theme_bw()
# theme_dark()
# theme_grey()
# theme_linedraw()
# theme_test()
# theme_gray()
# theme_void()

ggplot()

# Série Temporal
ggplot(df, aes(x = Time, y = inflacao_percent)) +
  geom_line(color = 'black', linewidth = 0.5, linetype = 1) +
  labs(title = 'Série Temporal',
       x = 'Anos',
       y = 'Inflação') +
  theme_classic()

# Histograma
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
         decada = factor(decada, levels = c("1970", "1980",
                                            "1990", "2000",
                                            "2010", "2020")))

ggplot(df[-1 ,], aes(x = decada ,y = inflacao_percent)) +
  geom_boxplot(color = "black",fill = "blue", outliers = FALSE) +
  geom_jitter(width = 0.2, color = "black", alpha = 0.5) +
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

geral <- df %>%
  select(Time, inflacao_percent) %>%
  left_join(df_cambio, by = 'Time') %>%
  slice(seq(176, nrow(.)))

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
ggplot(airquality, aes(x = Ozone, y = Temp)) +
        geom_point(color = 'black', shape = 6) +
        geom_smooth(method = "lm", color = 'red', se = FALSE) +
        labs(title = 'Grafico de Dispersao',
             x = 'Nível de Ozonio',
             y = 'Temperatura') +
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

# PNG, PDF, JPEG, TIFF e SVG.
ggsave("/home/caio/Documentos/Faculdade/PET Economia UFPE/Curso R/plot.pdf", plot = final, width = 5, height = 5)


#---------------------------------------------------
# EXTRA
#---------------------------------------------------
library(cowplot)
library(png)
library(showtext) 
showtext_auto()
font_add("plane" ,"~/Documentos/Faculdade/PET Economia UFPE/Curso R/Plane Crash.ttf")

img <- readPNG("~/Documentos/Faculdade/PET Economia UFPE/Curso R/poluicao.png")


grafico <- ggplot(airquality, aes(x = Ozone, y = Temp)) +
  geom_point(color = 'black',fill = "black", shape = 21) +
  geom_smooth(method = "lm", color = '#4F4F4F') +
  labs(title = 'aquecimento global',
       x = 'ozonio',
       y = 'temperatura') +
  theme(
    plot.title = element_text(family = "plane", size = 25, hjust = 0.5, margin = margin(10, 0, 10, 0)),  
    axis.title.x = element_text(family = "plane", size = 17),  
    axis.title.y = element_text(family = "plane", size = 17, margin = margin(0, 5, 0, 5)),  
    axis.text.x = element_text(family = "mono", colour = "black", size = 12),   
    axis.text.y = element_text(family = "mono", colour = "black", size = 12),
    axis.line = element_line(colour = "black"),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = alpha("grey", 0.25), color = NA),
    plot.background = element_rect(fill = alpha("grey", 0.25), color = NA)
  )

final <- ggdraw() +
  draw_image(img, scale = 1.3) +
  draw_plot(grafico)

plot(final)
