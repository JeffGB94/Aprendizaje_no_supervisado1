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

### MSE
MSE1 <- mean((test$Sales - predict(Reg.tree, newdata = test))^2)

##1.c)
###validación cruzada para árbol de regresión
set.seed(123)
cv_Reg <- cv.tree(Reg.tree)
D.prueba <- data.frame(cv_Reg$size, cv_Reg$dev)
D.prueba <- D.prueba[which.min(D.prueba[,2]),]
plot(cv_Reg$size, cv_Reg$dev, type = "b", xlab = "Tree seze", ylab = "MSE de validación cruzada")
points(D.prueba[,1],D.prueba[,2], pch=19, cex=2, col="red")
legend("topright", inset = .05, legend=c("size = 8", "mín MSE = 1300.186"), cex=0.8, box.lty=0)

###Poda
prune.Reg <- prune.tree(Reg.tree, best = 8)
plot(prune.Reg)
text(prune.Reg, pretty = 0)

###MSE prueba
MSE2 <- mean((test$Sales - predict(prune.Reg, newdata = test))^2)

##1.d)
###Bagging
require(randomForest)
set.seed(123)

bas.Reg <- randomForest(Sales ~ . , data = datos, subset = muestra, mtry = 10, importance = TRUE)
importance(bas.Reg)
varImpPlot(bas.Reg)
MSE3 <- mean((test$Sales - predict(bas.Reg, newdata = test))^2)

##1.e)
###Random Forest
set.seed(123)
rf.Reg <- randomForest(Sales ~ . , data = datos, subset = muestra, importance = TRUE)

###MSE de prueba
MSE4 <- mean((test$Sales - predict(rf.Reg, newdata = test))^2)

###Importancia
importance(rf.Reg)
varImpPlot(rf.Reg)

