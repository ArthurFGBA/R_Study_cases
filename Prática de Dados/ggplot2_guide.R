library(tidyverse)
library(skimr)
library(moments)
library(readxl)

data(starwars)
str(starwars)
skim(starwars)

## Understand teh basic about aesthetics(aes)
# only X
starwars %>%
  ggplot(aes(x = height))

# X and Y
starwars %>%
  ggplot(aes(x = height, y = mass))

#Histogram
starwars %>%
  ggplot(aes(x = height))+
  geom_histogram()

#density chat
starwars %>%
  ggplot(aes(x = height))+
  geom_density()

#Density in difenrent levels
starwars %>%
  ggplot(aes(x = height, color = sex))+
  geom_density(linewidth = 1)

#with more parameters
starwars %>%
  ggplot(aes(x = height, color = sex))+
  geom_density(linewidth = 1)+
  labs(tittle = "Distribuição de Alturas por sexo",
       subtittle = "Conjunto de dados starwars",
       x = "Altura (cm)", 
       y = "Densidade", 
       caption = "Nota de rodapé"  #nota de rodapé
       )

#Dispension chat
starwars %>%
  ggplot(aes(x = height, y = mass))+
  geom_point()+
  labs(tittle = "Distribuição de Alturas por sexo",
       subtittle = "Conjunto de dados starwars",
       x = "Altura (cm)", 
       y = "Massa (kg)")

starwars %>%
  filter(mass < 1000) %>%
  ggplot(aes(x = height, y = mass))+
  geom_point()+
  labs(tittle = "Distribuição de Alturas por sexo",
       subtittle = "Conjunto de dados starwars",
       x = "Altura (cm)", 
       y = "Massa (kg)")

#Linear Tencency line - linear model "lm"
starwars %>%
  filter(mass < 1000) %>%
  ggplot(aes(x = height, y = mass))+
  geom_point()+
  geom_smooth(method = "lm", color = 'red')+ #method to indentify the kind of model you want
  labs(tittle = "Distribuição de Alturas por sexo",
       subtittle = "Conjunto de dados starwars",
       x = "Altura (cm)", 
       y = "Massa (kg)")

#non linear model - polinomial local method
starwars %>%
  filter(mass < 1000) %>%
  ggplot(aes(x = height, y = mass))+
  geom_point()+
  geom_smooth(method = "loess", color = 'red')+
  labs(tittle = "Distribuição de Alturas por sexo",
       subtittle = "Conjunto de dados starwars",
       x = "Altura (cm)", 
       y = "Massa (kg)")

#by sex
starwars %>%
  filter(mass < 1000) %>%
  ggplot(aes(x = height, y = mass, color = sex))+
  geom_point(size = 2)+
  labs(tittle = "Distribuição de Alturas por sexo",
       subtittle = "Conjunto de dados starwars",
       x = "Altura (cm)", 
       y = "Massa (kg)")

#By sex with tendency line
starwars %>%
  filter(mass < 1000) %>%
  ggplot(aes(x = height, y = mass, color = sex))+
  geom_point()+
  geom_smooth(method = "lm",  se = FALSE)+  #SE = FALSE for remove confidence interval
  labs(tittle = "Distribuição de Alturas por sexo",
       subtittle = "Conjunto de dados starwars",
       x = "Altura (cm)", 
       y = "Massa (kg)")

#geom_hline create a horizontal line
#geom_vline create a vertical line
starwars %>%
  filter(mass<1000) %>%
  ggplot(aes(x = height, y = mass))+
  geom_point()+
  geom_hline(yintercept = 80, linetype = "dashed")+ #linetype to view better your data
  geom_vline(xintercept = 125, linetype = "dotted")
  
#Bar chat
starwars %>%
  group_by(eye_color) %>%
  summarise(count = n()) %>%
  ggplot(aes(y = count, x = eye_color, ))+
  geom_bar(stat = "identity", fill = 'yellow', color = 'black', alpha = 0.8)+ #alpha define its transparency
  coord_flip()  #to turn it into a bar chat

#adding labels
starwars %>%
  group_by(eye_color) %>%
  summarise(count = n()) %>%
  ggplot(aes(y = count, x = eye_color, label = count))+
  geom_bar(stat = "identity")+
  geom_label(size = 3)+
  coord_flip()

#Separating by sex and 
starwars %>%
  filter(eye_color %in% c("black", "blue", "brown")) %>%
  group_by(eye_color, sex) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = eye_color, y = count, fill = sex,label = count))+
  geom_bar(stat = "identity", position = "dodge")+
  geom_label(position = position_dodge(width = 1), size = 3)

  