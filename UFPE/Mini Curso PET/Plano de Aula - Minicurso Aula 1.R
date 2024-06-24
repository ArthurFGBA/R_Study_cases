#---------------------------------------------------#
#     PLANO DE AULA COMENTADO - MINICURSO DE R      #
#               PET Economia UFPE                   #
#                 Caio França                       #
#---------------------------------------------------#

#-------------- Operacoes Matematicas --------------#
5 + 5                   # Soma
8 - 4                   # Subtração
9 * 3                   # Multiplicação
9 / 3                   # Divisãoo
5**2                    # Potenciação
5^2
225^(1/2)
sqrt(225)               # Função raiz quadrada
exp(1)                  # Função e^x
pi                      # pi

log(100)                # Função logarítmica
?log                    # Ensinar a Utilizar o help
log(100, base = 10)

32 %% 5                 # Resto da Divisao
32 %/% 5                # Quociente da Divisao

0.1 + 0.2               # Vicios do computador
(0.1 + 0.2) == 0.3
sin(pi)
cos(pi/2)

#------------------ Tipos de Variveis --------------------#
### Strings/Characters
nome <- "Caio"
sobrenome <- "Franca"
print(nome)
typeof(nome)
is.character(nome)

?paste                                 # Arg -> sep = ’ ’
nome_completo <- paste(nome, sobrenome)
print(nome_completo)

?sprintf                               # Imita o printf do C
frase_1 <- sprintf(’Meu Nome %s’, nome_completo)
frase_2 <- sprintf(’Tenho %d de idade’, 21)

nchar(nome_completo)                   # Numero de Caracteres
sub("a", "W", nome_completo)           # Substituicao do primeiro argumento
gsub("a", "W", nome_completo)          # Substituicao de todos os argumentos
strsplit(nome_completo, split = " ")   # Separador de Strings
toupper(nome_completo)                 # Letra Maiúscula
tolower(nome_completo)                 # Letra Minúscula

sprintf(’Meu Nome %s’, readline(prompt = "Digite seu nome: "))

### Doubles/Integers
idade <- 20                # Doubles
print(idade)
typeof(idade)
is.numeric(idade)
is.double(idade)

nota <- 8.75               # Doubles
print(nota)
typeof(nota)
is.numeric(nota)
is.double(nota)

idade2 <- 19L              # Integers
print(idade2)
typeof(idade2)
is.integer(idade2)
is.numeric(idade2)
is.double(idade2)

sci_not <- 3.12e15         # Doubles
typeof(sci_not)
is.numeric(sci_not)

round(nota, digits = 1)    # Quando inteiro vai para o par
ceiling(nota)              # Arredonda para cima
floor(nota)                # Arredonda para baixo
as.integer(nota)           # Transforma em Inteiro

typeof(25L + 35)           # Podemos fazer operaes com doubles e integers

### Logical/Boolean
logico <- TRUE
FALSE
typeof(logico)
is.logical(logico)

T                         # Não recomendado 
F
TRUE + TRUE               # Podem ser números tbm
FALSE + TRUE

#----------------------- Operacoes Logicas ----------------------#
num <- 10

num == 10                 # Igual
num != 10                 # Diferente
num > 10
num >= 10
num < 10
num <= 10

!num == 10                # Negao

# Tabela Verdade
num != 10 & num > 10      # Conjunção
num == 10 & num >= 10
num == 10 & num > 10

num != 10 | num > 10      # Disjunção
num == 10 | num >= 10
num == 10 | num > 10

ano_1 <- 2024
ano_2 <- 2020
(ano_1 != ano_2 & ano_1 > ano_2) | (ano_1 == ano_2 & ano_1 >= ano_2)


#----------------------- Estruturas de Dados ----------------------#
### VETORES
# Apenas um Tipo de Variável
doubles <- c(-5.0, 5.0, -15.0, 35.0, -45.0)
typeof(doubles)
inteiros <- c(-5L, 5L, 15L, 25L, 35L, 45L)
typeof(inteiros)
strings <- c("Minicurso", "de", "R", "PET", "Economia", "UPE")
typeof(strings)
logicos <- c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE)
typeof(logicos)

head(strings, 3)                      # Primeiros Valores
tail(strings, 3)                      # Últimos Valores
length(strings)                       # Quantidade de Objetos no Vetor
35 %in% doubles                       # Procura um valor no vetor
"UFPE" %in% strings

# Seleção
strings[4]                            # Metodos de Selecao
strings[4:6]
strings[c(1, 4, 5, 6)]
strings[logicos]

# Substituição
strings[6] <- ’UFPE’
strings <- replace(strings, 6, ’UFPE’)

# Adição
strings[7] <- "AULA 1"
strings <- c(strings, 17)
strings <- append(strings, 6)

# Remoção
strings <- strings[-7]
strings <- strings[!strings %in% c("17", "6")]
strings

unique(logicos)               # Valores unicos de um vetor

?sort                         # decreasing = FALSE
sort(doubles)                 # Ordenar vetor
sort(strings)
doubles <- sort(doubles, decreasing = TRUE)
doubles

## Vetores Numéricos
notas <- c(9.0, 8.5, 4.5, 5.0, 7.5, 8.5, 5.0, 9.0, 9.0)
seq(0, 100, 0.1)              # Sequência de 0 até 100 de 0.1 em 0.1
100:200                       # Sequência de 100 até 200
typeof(notas)

sum(notas)                    # Soma todos os valores de um vetor
max(notas)                    # O maior valor de um vetor
min(notas)                    # O menor valor de um vetor
mean(notas)                   # A média dos valores de um vetor
var(notas)                    # A variância dos valores de um vetor
sd(notas)                     # O desvio padrão dos valores de um vetor
summary(notas)                # Resumo de algumas estatisticas

notas[notas < 8]              # Seleção Numérica

notas <- notas + 1            # Operação com vetores Numéricos
notas
notas / 2

# Porque utilizar o inteiro
object.size(as.numeric(1:1000000)) # Double
object.size(1:1000000)             # Integer


## Vetores de Strings
nomes <- c("Pedro", "Edson", "Fabio", "Gabriele")
typeof(nomes)

nomes <- paste(nomes, "Aluno UFPE", sep = " - ") # Aplicando funções no vetor
nomes

nomes <- nomes[grepl("a", nomes)]
nomes

numeros <- c(1, 2, 3, 5, "8")         # O R dá a preferência para as strings
typeof(numeros)
numeros <- as.numeric(numeros)        # Transformando em double
typeof(numeros)
numeros <- as.integer(numeros)        # Transformando em integer
typeof(numeros)


### LISTAS
# Aceita todos os tipos de variáveis ao mesmo tempo
nome <- "Mercia"
idade <- 55L
notas <- c(10, 8.5, 7, 5, 4.5)
aluno <- list(nome, idade, notas)  # Declarar uma lista sem nome
print(aluno)
typeof(aluno)
str(aluno)

# Seleção
aluno[1]                           # [] Continua como lista
typeof(aluno[1])

aluno[[1]]                         # [[]] Retira o valor do Objeto
typeof(aluno[[1]])

aluno[[3]]                         
aluno[[3]][1]

# Substituição
aluno[2] <- 21L
aluno <- replace(aluno, 2, 21L)
aluno

# Adição
aluno[4] <- "APROVADO"
aluno <- append(aluno, "PET Economia")
aluno

# Remoção
aluno <- aluno[-4]
aluno != "PET Economia"
aluno <- aluno[aluno != "PET Economia"]
aluno

names(aluno)
names(aluno) <- c(’"nome"’, ’"idade"’, ’"notas"’)        # Colocando nome na lista
names(aluno)

aluno <- list(nome = nome, idade = idade, notas = notas) # Declarando com nome
str(aluno)

# Seleção por nome
aluno$nome
aluno["nome"]
aluno$idade
aluno["idade"]
aluno$notas
aluno$notas[1]

# Adição por nome
aluno$situacao <- "Aprovado"
aluno

# Remoção por nome
aluno$situacao <- NULL
aluno

names(aluno)                      # Nomes dos objetos da lista
unlist(aluno)                     # Transformar em vetor
unlist(aluno[-1])
as.numeric(unlist(aluno[-1]))     # Retirando os nomes

### FATORES
# Inteiro por baixo
alunos <- c("caio", "caio", "brunna", "lais")
ano <- c(2020, 2021, 2023, 2023)
typeof(alunos)
typeof(ano)
fator_1 <- factor(alunos)        # Tranformando em fator
fator_2 <- factor(ano)
fator_1
fator_2
levels(fator_1)                  # Levels de um vetor
levels(fator_2)
typeof(fator_1)
typeof(fator_2)
as.numeric(fator_1)              # Mostrando por trás
as.numeric(fator_2)

fator_1 <- factor(fator_1, levels = c("caio", "caio", "brunna", "lais")) # Declarando um fator
fator_1
as.numeric(fator_1)

levels(fator_1) <- c("Pedro", "Gaby", "Marcelo") # Mudando os nomes do fator
fator_1

# Transformando em vetor
as.character(fator_1)             # Transformando em string
as.character(fator_2)
as.numeric(as.character(fator_2)) # Transformando em numerico

summary(fator_1)                  # Conta os levels
summary(fator_2)

#--------------- Loops (Estruturas de Repetição) ---------------------#
## For e While
# Estrutura Base for -> Inicio e fim delimitado
for (variable in vector) {
  
}

for (i in seq(1:10)) {
  
  print(i)
  
}

nomes <- c("Pedro", "Giulia", "Hugo", "Celso", "Mercia")
for (i in nomes) {
  
  print(sprintf("Meu nome é %s", i))
  
}

fibonacci <- c(0, 1)
for (i in seq(3, 20)) {
  fibonacci[i] <- fibonacci[i-1] + fibonacci[i-2]
}
fibonacci

# Estrutura Base while -> Final Condicional
while (condition) {
  
}

j <- 0
while (j < 20) {
  j <- j + 1
  print(j)
}

# While infinito
j <- 0
while (j < 20) {
  j <- j - 1
  print(j)
}


#--------------- (Estruturas Condicional) ---------------------#
## If, Else e Else If
# Estrutura Base
if (condition){
  
} else if(condition){
  
} else{
  
}


# Exemplo em um linha
if(5 != 5) print("oi") else print('ola')


b <- 9
if(b < 10) {
  print("Hello World")
}

if(b > 10) {
  print("Hello World")
} else{
  print("Error")
}

if(b > 10) {
  print("Hello World")
} else if(b == 9){
  print("Try Again")
} else if(b == 8){
  print("Error")
} else {
  print('Error 2')
}

# Juntando as estruturas
aleatoria <- sample(1:20, 1)
chute <- 0
while (chute != aleatoria) {
  chute <- readline(prompt = "Diga um Valor: ")
  chute <- as.numeric(chute)
  if(chute == aleatoria) {
    print("GANHOUU!!!!")
  } else if((chute == aleatoria - 1) | (chute == aleatoria + 1)){
    print("QUASEEE")
  } else{
    print("ERRADO")
  }
}

#--------------- Funções ---------------------#
# Estrutura Base
name <- function(variables) {
  
}

# x² - 6x + 5 = 0
(-(-6) + (sqrt((-6)^2 - 4*1*5))) / 2*1
(-(-6) - (sqrt((-6)^2 - 4*1*5))) / 2*1

eq_seg_grau <- function(a = 1, b = -6, c = 5){
  
  x_1 <- (-(b) + (sqrt(((b)^2) - 4*a*c))) / (2*a)
  x_2 <- (-(b) - (sqrt(((b)^2) - 4*a*c))) / (2*a)
  
  return(c(x_1, x_2))
}

eq_seg_grau()
eq_seg_grau(1, -6, 9)
eq_seg_grau(-1, -8, 9)


notas <- sample(1:10, 200, replace = TRUE)
notas

automacao <- function(...){
  x <- c(...)
  resultado <- c()
  for (nota in x) {
    if(nota >= 7){
      resultado <- c(resultado, "Aprovado")
    } else if(nota >= 3){
      resultado <- c(resultado, "Prova Final")
    } else{
      resultado <- c(resultado, "Reprovado")
    }
  }
  return(resultado)
}

automacao(notas)
automacao(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

jogo <- function(from = 1, to = 10){
  aleatoria <- sample(from:to, 1)
  chute <- 0
  while (chute != aleatoria) {
    chute <- readline(prompt = "Diga um Valor: ")
    chute <- as.numeric(chute)
    if(chute == aleatoria) {
      print("GANHOUU!!!!")
    } else if((chute == aleatoria - 1) | (chute == aleatoria + 1)){
      print("QUASEEE")
    } else{
      print("ERRADO")
    }
  }
}

jogo(1, 10)