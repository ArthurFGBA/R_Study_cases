library(tidyverse)
library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(purrr)
library(ggplot2)

pib<-read.csv("C:/Users/Arthu/Downloads/ipeadata[22-06-2024-05-19].csv", header = TRUE)

pib<-as.tibble(pib)

str(pib)

pib<-pib %>%
  select(-X) %>%
  rename(Data = Data, 
         PIB_nominal = colnames(pib[,2]) %>%
  mutate(Data = yq(Data), PIB_nominal = as.numeric(PIB_nominal))

