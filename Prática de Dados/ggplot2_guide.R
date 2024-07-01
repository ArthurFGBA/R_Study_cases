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

#Error Bar - to show variability of the data
medias<- starwars %>%
  filter(!is.na(height)) %>%
  group_by(gender) %>%
  summarise( 
    mean = mean(height), 
    sup_limit = mean(height) + sd(height),
    inf_limit = mean(height) - sd(height)
    )
medias 

medias %>%
  ggplot(aes(x = gender, y = mean, color = gender))+
  geom_bar(stat = "identity", fill = "lightblue")+
  geom_errorbar(aes( ymin = inf_limit, ymax = sup_limit), width = 0.4)

medias %>%
  ggplot(aes(x = gender, y = mean))+
  geom_pointrange(aes( ymin = inf_limit, ymax = sup_limit))

#Experiment test A/B
dados <- data.frame(
  metricas<- c("m1", "m2", "m3", "m4", "m5"),
  limite_inferior <- c(-3.5,-2.4,1,4,8),
  limite_superior <- c(1,1.5,3,7,10),
  significativo <- c("não", "não", "sim", "sim", "sim")
)
dados 

#Confidence interval
dados %>%
  ggplot(aes(x = metricas, color = significativo)) +
  geom_errorbar(aes(ymin = limite_inferior, ymax = limite_superior), width = 0.3) +
  geom_hline(yintercept = 0, linetype = "dotted")+
  labs(title = "Gráfico com Barras de Erro",
       x = "Métricas",
       y = "Valores",
       color = "Significativo") +
  theme_minimal()+
  coord_flip()

#Distribution by group
starwars %>%
  ggplot(aes(x = gender, y = height ))+
  geom_boxplot()

#Violin chart
starwars %>%
  ggplot(aes(x = gender, y = height))+
  geom_violin()

#Dispersion chat
starwars %>%
  ggplot(aes(x = gender, y = height))+
  geom_point()+

#jitter
starwars %>%
  ggplot(aes(x = gender, y = height))+
  geom_jitter(width = 0.3)

##line chats
data(economics)

economics %>%
  ggplot(aes(x = date, y = unemploy))+
  geom_line(linewidth = 1, linetype = 1)

economics %>%
  ggplot(aes(x = date, y = unemploy))+
  geom_line(linewidth = 1, linetype = 1)+
  geom_smooth(method = "loess", se = FALSE) #se =  false to remove te confidence interval
  
economics %>%
  ggplot(aes(x = date, y = unemploy))+
  geom_line(linewidth = 1, linetype = 1)+
  scale_x_date(limits = c(as.Date("1990-01-01"),as.Date("1999-12-31"))) #Filter a period of time 

install.packages("gapminder")
library(gapminder)
?gapminder
data(gapminder)

gapminder %>%
  filter(country == "Brazil") %>%
  ggplot(aes(x = year, y = pop))+
  geom_line(linewidth = 1)+
  geom_point(size = 2,color = "blue")

gapminder %>%
  filter(country  %in% c("Brazil", "Uruguay", "Argentina")) %>%
  ggplot(aes(x = year, y = pop, color = country))+
  geom_line(linewidth = 1)+
  geom_point(size = 2)

#What to do over a pie chat
genero<-  starwars %>%
  group_by(gender) %>%
  summarise(n = n())

genero %>%
  ggplot(aes(x = gender, y = n, fill = gender, label = n))+
  geom_bar(stat = "identity")+
  geom_label( position = position_dodge(width = 1), size = 3)+
  coord_flip()

genero %>%
  ggplot(aes(x = "", y = n, fill = gender, label = n))+
  geom_bar(stat = "identity", width = 3)+
  geom_label( position = position_stack(vjust = 0.5), size = 3)+
  coord_flip()

genero %>%
  ggplot(aes(x = "", y = n, fill = gender, label = n))+
  geom_bar(stat = "identity")+
  coord_polar(theta = "y", start = 0)+
  geom_label( position = position_stack(vjust = 0.5), size = 3)+
  theme_void()
