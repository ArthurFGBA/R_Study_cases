# --------------------------------------------------------------------
#                 Guia do pacote tidyverse 
# --------------------------------------------------------------------

install.packages("tidyverse")
library(tidyverse)
library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(purrr)
library(ggplot2)

# select() selecionar colunas a serem mostradas
# mutate() adicionar e calcular novas colunas
# filter() filtrar linhas a partir de critérios
# summarise() agregar dados
# arrange() ordenação dos dados

data("starwars")
nrow(starwars)

starwars %>%
  select(name,eye_color)



