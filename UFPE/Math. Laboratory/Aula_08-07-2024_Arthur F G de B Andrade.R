#Aula 08/07/2024

#Série de Taylor
# a) y(x) = e^x *sen(x)
# b) y(x) = e^-x * cos(x)
# c) y(x) = 2*e^-x + e^x
# d) imprimir o polinômio encontrado
# e) Elaborar os gráficos das funções acima
# f) Elaborar os gráficos dos polinômios que se aproximam das funções indicadas

'graficos especiais  - 1'

set.seed(1234)
n<-10000
c1<-matrix(rnorm(n, mean = 0, sd = 0.5), ncol = 4)
c1
c2<-matrix(rnorm(n, mean = 3, sd = 2), ncol = 4)
c2

mydata<- rbind(c1, c2)
mydata
mydata<- as.data.frame(mydata)
names(mydata)<-c("x", "y")
mydata

with(mydata,
     plot(x, y, pch =19, main= "Scatter Plot with 10000 Observations"))

with(mydata, 
     smoothScatter(x, y, main= "Scatter Plot Colored by Smoothed Densities"))

'graficos especiais  - 2'

install.packages("hexbin")
library(hexbin)

with(mydata, 
     {bin <- hexbin(x, y, xbins = 50)
     plot(bin, main= "Hexagonal Binning with 10000 Observations")})

'graficos especiais  - 3'
install.packages("scatterplot3d")
library(scatterplot3d)
data(mtcars)
attach(mtcars)
scatterplot3d(wt, disp, mpg, main= "Basic 3D Scatter Plot")

'graficos especiais  - 4'

library(scatterplot3d)
attach(mtcars)
scatterplot3d(wt, disp, mpg,
              pch = 16,
              highlight.3d= TRUE,
              type= "h",
              main= "3D Scatter Plot with Vertical lines")

install.packages("ggplot2")
library(ggplot2)

g<- ggplot(mtcars)
g<- g+geom_point(aes(x = hp, y = mpg, color = factor(am)), size = 3)

g

g<- g+scale_color_manual("Automatic",
                         values = c("red", "blue"),
                         labels = c("No", "Yes"))
g

g<- g+labs(title = "Relação entre consumo, potência e tipo de câmbio",
           y = "Consumo",
           x = "Potência")
g

mtcars |>
  ggplot()+
  geom_point(aes(x = hp, y = mpg, color = factor(am)), size = 3)+
  scale_color_manual("Automatic",
                     values = c("red", "blue"),
                     labels = c("No", "Yes"))+
  labs(title = "Relação entre consumo, potência e tipo de câmbio",
       y = "Consumo",
       x = "Potência")

g1<- ggplot(mtcars, aes(y = mpg, x = disp))+
  geom_point()

g1
