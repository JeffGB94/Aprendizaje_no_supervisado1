#Código

require(ISLR)
datos <- Carseats

##1.a)

set.seed(123)
muestra <- sample(1:nrow(datos), size = floor(nrow(datos) * 0.7))
train <- datos[muestra, ]; test <- datos[-muestra, ]

##1.b)

require(tree)
require(MASS)

Reg.tree <- tree(Sales ~ ., data = datos, subset = muestra)
summary(Reg.tree)
plot(Reg.tree)
text(Reg.tree, , pretty = 0)