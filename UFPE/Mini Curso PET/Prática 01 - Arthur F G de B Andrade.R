#Questão 1 
nomes <- c("Olivia Thomas", "William White", "James Moore",
           "Mia Thompson","Charlotte White", "Sophia White", "Mason Hall",
           "Emily Robinson", "Alexander Lopez", "Lily Young", "Jackson Perez",
           "Liam Anderson", "Benjamin Martin", "Lucas Gonzalez", "Henry Allen",
           "Jacob Hernandez", "Logan Martinez", "Isabella Young",
           "Mia Anderson", "Mia Gonzalez", "Benjamin Clark", "Daniel Gonzalez",
           "Jack Martinez", "Ella Wilson", "Aiden Johnson")

notas <- c(8.75, 5.44, 2.89, 7.77, 1.30, 8.01, 1.50, 2.00, 6.90, 7.89,
           6.47, 4.01, 5.30, 7.63, 2.27, 8.34, 7.45, 6.78, 3.45, 8.90, 4.56,
           7.80, 9.50, 2.60, 5.32)
length(nomes)

situacao <- c()
i<-0

for(i in 1:25){
  if(notas[i]>7){
    situacao[i]<-"Aprovado"
  }else if(notas[i]>=3){
    situacao[i]<-"Avaliação Final"
  }else{
    situacao[i]<-"Reprovado"
  }
}

print(sprintf("Aluno: %s; Situação: %s",nomes,situacao))

#Questão 2
ctof<-function(c){
  f<-c*(9/5)+32
  print(sprintf("A temperatura em Fahrenheit é: %sº",f))
}
ctof(32)

ctok<-function(c){
  k<-c+273.15
  print(sprintf("A temperatura em Fahrenheit é: %sº",k))
}
ctok(32)

ftok<-function(f){
  k<-round((f-32)*5/9+273.15,2)
  print(sprintf("A temperatura em Fahrenheit é: %sº",k))
}
ftok(75.6)

#Questão 3
class<-0
for (i in 1:1){
tele <- readline(prompt = "Telefonou para a vítima? ")
if(toupper(tele)=="SIM"){
  class<-class+1L
}
loc <- readline(prompt = "Esteve no local do crime? ")
if(toupper(loc)=="SIM"){
  class<-class+1L
}
liv <- readline(prompt = "Mora perto da vítima? ")
if(toupper(liv)=="SIM"){
  class<-class+1L
}
debt <- readline(prompt = "Devia para a vítima? ")
if(toupper(debt)=="SIM"){
  class<-class+1L
}
job <- readline(prompt = "Já trabalhou com a vítima? ")
if(toupper(job)=="SIM"){
  class<-class+1L
}

if(class == 5){
  print("Assassino")
}else if ((class == 3) | (class == 4)){
  print("Cúmplice")
}else if(class == 2){
  print("Suspeita")
}else{
  print("Inocente")
}
class<-0
}
