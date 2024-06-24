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
starwars #dados já presentes no R
nrow(starwars)

#Uso do pipe, deve ser repetido até a penultima operação
starwars %>%
  select(name,eye_color) #one by one
  #select(name:hair_color) #for a interval
  #select(-films,-vehicles,-starships) #Exclude columns
  #select(starts_with("h"))  #starts with
  #select(ends_with("color")) #ends with
  #select(contains("color"))  #Contain

#Filter
starwars %>%
  filter(homeworld == "Tatooine")  #
  #filter(!homeworld == "Tatooine")  # ! for not conditions
  #filter(homeworld == "Tatooine", species == "Droid") # , for and conditions
  #filter(homeworld == "Tatooine" | homeworld == "Naboo") # | for or conditions
  #filter(is.na(homeworld)) #is.na for not avalible values
  #filter(species %in% c("Human", "Droid")) # %in% for multiple values by column

#Mutate
#manipulate a column
starwars %>%
  mutate(
    height = (height/100)
  )
#create a new column
starwars %>%
  mutate(
    mass_100 = ifelse(mass > 100, "Y", "N")
  ) %>%
  select(name,mass, mass_100)

starwars %>%
  mutate(hair_color = ifelse(is.na(hair_color), "Unknow", hair_color))

#Parametro especiais: .keep all(todas), used(unica alterada), unused(não anterada), none(apneas a transformada)
starwars %>%
  mutate(hair_color = ifelse(is.na(hair_color), "Unknow", hair_color), .keep = "unused"
         )
# .before to define where do you whant the new column
starwars %>%
  mutate(
    mass_100 = ifelse(mass > 100, "Y", "N"), .before = "hair_color"
  )

#Arrange
starwars %>%
  arrange(height) # Ascendant order
  #arrange(-height)# Descendant order
  #arrange(desc(height)) # Descendant order

#Multiple columns
starwars %>%
  arrange(hair_color, height)

#Getting extreme values
starwars %>%
  filter(!is.na(height)) %>%
  arrange(height) %>%
  slice_tail(n = 3) #end of the list 
  #slice_head(n = 3) #Begning of the list

#Aleatory Sample
starwars %>%
  slice_sample(n = 10)

#Summarise 
starwars %>%
  summarise(
    personagens = n(),
    altura_media = mean(height, na.rm = TRUE), #na.rm remove Not Avalible values
    menor_massa = min(mass, na.rm = TRUE)
  )

starwars %>%
  group_by(eye_color) %>% #group by an column
  summarise(
    personagens = n()
  )

starwars %>%
  group_by(hair_color) %>%
  summarise(
    personagens = n()
  ) %>%
  arrange(-personagens)

#Rename
starwars %>%
  rename(nome = name, altura = height) #new name =  old name
