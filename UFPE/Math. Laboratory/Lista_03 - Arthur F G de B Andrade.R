#Lista 3

##Séries de Taylor
#1ª função
# y(x) = e^x*sen(x)
# x = 1.1
prim <- expression(exp(x)*sin(x))
prim

dev <- D(prim, 'x')
dev

x<-1.1
t1<-eval(prim)
t2<-eval(dev)
t3<-eval(D(dev,'x'))
t4<-eval(D(D(dev,'x'),'x'))

#Polinomio
sprintf("y(1.1)= %s + %s*(x-1.1) + %s/%s*(x-1.1)^2 + %s/%s*(x-1.1)^3"
        ,round(t1,2),round(t2,2),round(t3,2),factorial(2),round(t4,2),factorial(3))

rm(list = ls())
#2ª função
# y(x) = e^-x*cos(x)
prim <- expression(exp(-x)*cos(x))
prim

dev <- D(prim, 'x')
dev

x<-1.1
t1<-eval(prim)
t2<-eval(dev)
t3<-eval(D(dev,'x'))
t4<-eval(D(D(dev,'x'),'x'))

#Polinomio
sprintf("y(1.1)= %s + %s*(x-1.1) + %s/%s*(x-1.1)^2 + %s/%s*(x-1.1)^3"
        ,round(t1,2),round(t2,2),round(t3,2),factorial(2),round(t4,2),factorial(3))

rm(list = ls())

#3ª função
# y(x) = 2*e^x*+e^-x
prim <- expression(2*exp(x)+exp(-x))
prim

dev <- D(prim, 'x')
dev

x<-1.1
t1<-eval(prim)
t2<-eval(dev)
t3<-eval(D(dev,'x'))
t4<-eval(D(D(dev,'x'),'x'))

#Polinomio
sprintf("y(1.1)= %s + %s*(x-1.1) + %s/%s*(x-1.1)^2 + %s/%s*(x-1.1)^3"
        ,round(t1,2),round(t2,2),round(t3,2),factorial(2),round(t4,2),factorial(3))

rm(list = ls())

#Elaborar gráficos das funções acima
#função 1 
fun1<-function(x){
  length(x)
  for (i in 1:length(x)){
    y<- exp(x)*sin(x) 
  }
  return(y)
}
x1<-seq(0,3, by=0.1)
y1<-fun1(x1)
func1<- data.frame(x1,y1)

plot(func1, type= "n", main= 'Função 1')
lines(func1,col= 'red', lwd= 2)


#Função 2
fun2<-function(x){
  length(x)
  for (i in 1:length(x)){
    y<- exp(-x)*cos(x) 
  }
  return(y)
}
x2<-seq(0,3, by=0.1)
y2<-fun2(x2)
func2<- data.frame(x2,y2)

plot(func2, type = 'n', main= 'Função 1')
lines(func2,col= 'red', lwd= 2)


#Função 3
fun3<-function(x){
  length(x)
  for (i in 1:length(x)){
    y<- 2*exp(x)+exp(-x) 
  }
  return(y)
}
x3<-seq(0,3, by=0.1)
y3<-fun3(x3)
func3<- data.frame(x3,y3)
plot(func3, type = 'n', main= 'Função 3')
lines(func3,col= 'red', lwd= 2)


#Elaborar os gráficos dos polinômios que se aproximam das funções indicadas.
#função 1
func11<-function(x){
  length(x)
  for (i in 1:length(x)){
    y<- 2.68 + 4.04*(x-1.1) + 2.73/2*(x-1.1)^2-2.63/6*(x-1.1)^3
  }
  return(y)
}

x11<-seq(0,3, by=0.1)
y11<-func11(x11)
func11<- data.frame(x11,y11)

plot(func1, type = 'n', main= 'Função 1 x polinômio')
lines(func1,col= 'red', lwd= 2)
lines(func11,col= 'blue', lwd= 2,lty= 5)

#função 2
func22<-function(x){
  length(x)
  for (i in 1:length(x)){
    y<- 0.15-0.45*(x-1.1) + 0.59/2*(x-1.1)^2-0.29/6*(x-1.1)^3
  }
  return(y)
}

x22<-seq(0,3, by=0.1)
y22<-func22(x22)
func22<- data.frame(x22,y22)

plot(func2, type = 'n', main= 'Função 2 x polinômio')
lines(func2,col= 'red', lwd= 2)
lines(func22,col= 'blue', lwd= 3, lty= 5)

#função 3
func33<-function(x){
  length(x)
  for (i in 1:length(x)){
    y<- 6.34 + 5.68*(x-1.1) + 6.34/2*(x-1.1)^2 + 5.68/6*(x-1.1)^3
  }
  return(y)
}

x33<-seq(0,3, by=0.1)
y33<-func33(x33)
func33<- data.frame(x33,y33)

plot(func3, type = 'n', main= 'Função 3 x polinômio')
lines(func3,col= 'red', lwd= 2)
lines(func33,col= 'blue', lwd= 3, lty= 5)

##Integrais
rm(list =ls())
#a)
f<- function(x) 1/(16+x^2)
integrate(f,lower =0, upper = 3 )

#b)
f1<- function(x) 1/x
integrate(f1,lower =1, upper = 2 )

#c)
f2<- function(x) sqrt(1+x^2)
integrate(f2,lower =2, upper = 3 )

#d)
f3<- function(x) x*sqrt(4-x^2)
integrate(f3, lower = 0, upper = 2)

#e)
f4<- function(x) log(1+x^2)
integrate(f4, lower = 2, upper = 3)

#f)
f5<- function(x) 1/(16+x^2)
integrate(f5,lower =0, upper = 3 )

#g)
f6<- function(x) 1/(x+1)
integrate(f6,lower =0, upper = 1 )

