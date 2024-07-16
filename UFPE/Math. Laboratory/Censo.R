install.packages("devtools" , repos = "http://cran.rstudio.com/")
library(devtools)
install_github("ajdamico/lodown" , dependencies = TRUE)

install.packages('lodown')
install.packages('magrittr')
install.packages('dplyr')
install.packages('stringr')
install.packages('fs')
install.packages('SAScii')
install.packages('readr')

library(lodown) # [github::ajdamico/lodown] v0.1.0
library(magrittr) # CRAN v2.0.1
library(dplyr) # CRAN v1.0.7
library(stringr) # CRAN v1.4.0
library(fs) # CRAN v1.5.0
library(SAScii) # CRAN v1.0
library(readr) # CRAN v2.0.2
library(ggplot2)

# Buscar catálogo de microdados, aplicar filtros e baixar arquivos
catalog <- lodown::get_catalog(data_name = "censo", output_dir = "data") %>%
  dplyr::filter(year == 2010, stringr::str_detect(state, "pe")) %>%
  lodown::lodown(data_name = "censo")

# Arquivos baixados
fs::dir_tree(path = "data")

# data
# \-- 2010
#     \-- RS
#         +-- Amostra_Domicilios_43.txt
#         +-- Amostra_Emigracao_43.txt
#         +-- Amostra_Mortalidade_43.txt
#          \-- Amostra_Pessoas_43.txt

# Variáveis a serem importadas
vars_censo <- c("v0001", "v0601", "v6036", "v0010", "v0011", "v0300")

# Converte arquivo de instruções de importação SAS para o R
sas_input <- SAScii::parse.SAScii(catalog$pes_sas) %>%
  dplyr::mutate(varname = stringr::str_to_lower(varname))

# Importar arquivo TXT
raw_censo <- readr::read_fwf(
  file = catalog$pes_file,
  col_positions = readr::fwf_widths(
    widths = abs(sas_input$width),
    col_names = sas_input$varname
  ),
  col_types = paste0(
    ifelse(
      !(sas_input$varname %in% vars_censo),
      "_",
      ifelse(sas_input$char, "c", "d")
    ),
    collapse = ""
  )
)

raw_censo <- raw_censo %>%
  mutate(
    faixa_etaria = cut(v6036, 
                       breaks = c(0, 4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, Inf), 
                       labels = c('0 a 4 anos', '5 a 9 anos', '10 a 14 anos', '15 a 19 anos', '20 a 24 anos', 
                                  '25 a 29 anos', '30 a 34 anos', '35 a 39 anos', '40 a 44 anos', '45 a 49 anos', 
                                  '50 a 54 anos', '55 a 59 anos', '60 a 64 anos', '65 a 69 anos', '70 a 74 anos', 
                                  '75 a 79 anos', '80 anos ou mais'),
                       right = FALSE),
    sexo = ifelse(v0601 == "1", "Masculino", "Feminino")
  )

df_piramide <- raw_censo %>%
  group_by(faixa_etaria, sexo) %>%
  summarise(populacao = n(), .groups = "drop") %>%
  mutate(populacao = ifelse(sexo == "Masculino", -populacao, populacao))

# Criação do Gráfico de Pirâmide Etária
piramide_plot <- ggplot(df_piramide, aes(x = faixa_etaria, y = populacao, fill = sexo)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  scale_y_continuous(labels = abs, limits = max(abs(df_piramide$populacao)) * c(-1, 1)) +
  scale_fill_manual(values = c("Masculino" = "blue", "Feminino" = "red")) +  # Definir as cores para as categorias
  labs(title = "Pirâmide Etária: Pernambuco",
       subtitle = "Dados de 2010, Censo IBGE",
       x = "Idade",
       y = "Mil habitantes",
       fill = "Sexo") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = 0.5),  # Centralizar o título
        plot.subtitle = element_text(hjust = 0.5))  # Centralizar o subtítulo

print(piramide_plot)
