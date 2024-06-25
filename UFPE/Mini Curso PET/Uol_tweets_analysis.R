library(tidyverse)
install.packages("skimr") #descritivo dos dados 
library(skimr)
install.packages("moments")
library(moments)

db<-read.csv("C:/Users/Arthu/Downloads/tweets_uol.csv", header = TRUE)

skim(db)

db %>%
  group_by(source) %>%
  summarise(numero_tweeets = n())

db %>%
  group_by(source) %>%
  summarise(numero_tweeets = n()) %>%
  mutate(numero_tweeets_pct = numero_tweeets/sum(numero_tweeets)*100)

db %>%
  summarise(media_likes = mean(favorite_count), media_retweets = mean(retweet_count))

db %>%
  filter(favorite_count>1000) %>%
  summarise('N_tweets>1000' = n())

db %>%
  arrange(desc(retweet_count)) %>%
  slice_head(n = 3) %>%
  select(screen_name, text, retweet_count)

db %>%
  arrange(desc(favorite_count)) %>%
  slice_head(n = 3) %>%
  select(screen_name, text, favorite_count)
  
db<-as.tibble(db)

ggplot(db, aes(x = display_text_width))+
  geom_histogram(binwidth = 20,fill = '#4E0707' ,color = 'black')+
  labs(tittle = "Distribuição de comprimento de tweets", 
       x = "Comprimento do Tweet",
       y = "Quantidade")+
  theme_classic()

db %>%
  mutate(
    intervalo = cut(display_text_width, seq(0,280,20))
  ) %>%
  group_by(intervalo) %>%
  summarise(
    numero_de_tweets = n()
  )
