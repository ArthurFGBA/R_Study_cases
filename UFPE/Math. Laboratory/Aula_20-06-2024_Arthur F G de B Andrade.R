##Matrizes
matrix(1:4,2,2) #Dados, nrows, ncol

matrix(rnorm(9,20,2),3,3) #Por meio de valores aleatórios

matrix(1:6,2,3)

matrix(c("a","b","c","j"),2,2,byrow = TRUE)

matriz<-matrix(c(1,1,1,2,2,2),2,3)
colnames(matriz)<-c("Coluna 1", "Coluna 2", "Coluna 3")
rownames(matriz)<-c("linha 1", "linha 2")
matriz

matriz1<-matrix(c(0.1,0.1,0.3,0.4,0.5,0.6),2,3)
colnames(matriz1)<-c("Coluna 1", "Coluna 2", "Coluna 3")
rownames(matriz1)<-c("linha 1", "linha 2")
matriz1

#IPTU
iptu<-0.20
iptu

#Matriz IPTU
matriziptu<- matriz*iptu
matriziptu

#Matriz Agregada multiplique elemento por elemento de cada matriz
matriz2<-matriz*matriz1*iptu
matriz2

#Matriz Demanda
matrizdemanda<-matrix(c(10,20,25,30,33,40),2,3)
colnames(matrizdemanda)<-c("Coluna 1", "Coluna 2", "Coluna 3")
rownames(matrizdemanda)<-c("linha 1", "linha 2")
matrizdemanda

#Matriz Preço
matrizpreco<-matrix(c(10,20,30,40,50,60),2,3)
colnames(matrizpreco)<-c("Coluna 1", "Coluna 2", "Coluna 3")
rownames(matrizpreco)<-c("linha 1", "linha 2")
matrizpreco

#Operações com Matriz
# A*B  Multiplicação elemento a elemento
# A%*%B  Multiplicação matricial
# t(A)   Transposta 
# diag(n)  Matriz identidade de dimensão n 
# solve(A)  Inversa
# solve(A,b) Devolve o veto x dna equação b=Ax
# 
# rowMeans(A)  Devolve o vetor das médias por linha
# rowSums(A)   Devolve o vetor das somas por linha
# colMeans(A)  Devolve o vector das médias por coluna
# colSums(A)   Devolve o vector das somas por colunamatriz e vetor 

#Selecionar valores de um vetor maiores que 3 
x1<-c(1,3,5,4,7,9,2,4.5,10)
#obter valores maiores que 3
x2<-x1[x1>3]
print(x2)


#Selecionar valores onde >3 e <9
x3<-x1[(x1>3)&(x1<9)]
print(x3)

#Selecionar Valores <3 e >9
x4<-x1[(x1<3)|(x1>9)]
print(x4)
