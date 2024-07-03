library(GGally)
library(tidyverse)
library(skimr)
library(moments)
library(readxl)
library(ggcorrplot)
library(gapminder)

data("gapminder")
str(gap)
?gapminder
gap<-as.tibble(gapminder)
coun<-gap %>%
  filter(country %in% c("Brazil", "Argentina"))

skim(coun)

gap %>%
  filter(country %in% c("Brazil", "Argentina")) %>%
  group_by(country) %>%
  summarise( Exp_vida_media = mean(lifeExp), Desvio_Exp_vida = sd(lifeExp),
             PIB_per_cap_medio = mean(gdpPercap), Desvio_PIB_per_cap = sd(gdpPercap))

cme<-gap %>%
  filter(continent == "Americas") %>%
  summarise(cme = mean(lifeExp))

cme<- as.double(cme)

gap %>%
  filter(country %in% c("Brazil", "Argentina")) %>%
  mutate(lifeExp = round(lifeExp,1)) %>%
  ggplot(aes(x = year, y = lifeExp, color = country, label = lifeExp))+
  geom_line(linewidth = 1.2, linetype = 1)+
  geom_hline(yintercept = round(cme), linetype = "dashed")+
  geom_point(size = 2)+
  geom_label(size = 3)+
  labs(
    title = "Expectativa de vida (1952-2007)",
    x = "Ano", 
    y = "Expectativa de vida",
    color = "País",
    caption = "Exp. vida média no continente"
  )+
  theme_minimal()

cgdp<-gap %>%
  filter(continent == "Americas") %>%
  summarise(cgdp = mean(gdpPercap))

cgdp<- as.double(cgdp)

gap %>%
  filter(country %in% c("Brazil", "Argentina"))%>%
  mutate(gdpPercap = round(gdpPercap,2)) %>%
  ggplot(aes(x = year, y = gdpPercap, color = country, label = gdpPercap))+
  geom_line(linewidth = 1.2, linetype = 1)+
  geom_hline(yintercept = round(cgdp), linetype = "dashed")+
  geom_point(size = 2)+
  geom_label(size = 3)+
  labs(
    title = "PIB per Capta (1952-2007)",
    x = "Ano", 
    y = "PIB per Capta",
    color = "País",
    caption = "PIB per capta médio no continente"
  )+
  theme_minimal()
  

gap %>%
  filter(country %in% c("Brazil", "Argentina"))%>%
  ggplot(aes(x = gdpPercap, y = lifeExp, color = country))+
  geom_point(size = 3)+
  geom_vline(xintercept = round(cgdp), linetype = "dashed")+
  geom_hline(yintercept = round(cme), linetype = "dashed")+
  labs(
    title = "PIB per capta X Expec. de vida (1952-2007)",
    x = "PIB per capta",
    y = "Expectativa de vida",
    color = "País",
    caption = "PIB per capta e Exp. de vida medios no continente"
  )+
  theme_minimal()
  
br<-gap %>%
  filter(country == "Brazil")

mod_br<- lm(lifeExp ~ gdpPercap,data = br)

summary(mod_br)

br %>%
  ggplot(aes(x = gdpPercap, y = lifeExp))+
  geom_point(size = 3, color = "darkgreen")+
  geom_smooth(method = "lm" ,se = FALSE, linetype = "dashed", color = "red")+
  labs(
    title = "PIB per capta X Expec. de vida (1952-2007) - Brasil",
    x = "PIB per capta",
    y = "Expectativa de vida",
    caption = "Reta de regressão"
  )+
  theme_minimal()


ar<-gap %>%
  filter(country == "Argentina")

mod_ar<- lm(lifeExp ~ gdpPercap,data = ar)

summary(mod_ar)

ar %>%
  ggplot(aes(x = gdpPercap, y = lifeExp))+
  geom_point(size = 3, color =  "lightblue")+
  geom_smooth(method = "lm" ,se = FALSE, linetype = "dashed", color = "red")+
  labs(
    title = "PIB per capta X Expec. de vida (1952-2007) - Argentina",
    x = "PIB per capta",
    y = "Expectativa de vida",
    caption = "Reta de regressão"
  )+
  theme_minimal()

gap %>%
  filter(country %in% c("Brazil", "Argentina")) %>%
  group_by(country) %>%
  summarise( Correlation = cor(lifeExp, gdpPercap))

br %>%
  select(lifeExp, gdpPercap)%>%
  ggpairs(title =  "Exp. de vida X PIB per Capta - Brasil")

ar %>%
  select(lifeExp, gdpPercap)%>%
  ggpairs( title =  "Exp. de vida X PIB per Capta - Argentina")
?ggpairs
