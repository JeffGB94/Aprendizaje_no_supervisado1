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

## 2.a)
library(ISLR)
data(Auto)
# Variable binaria
Auto$mpg<-as.factor((Auto$mpg>median(Auto$mpg))*1)

## 2.b)
set.seed(2567)
muestra<-sample(1:nrow(Auto),size = nrow(Auto)*.7,replace = F)
#Conjunto de entrenamiento
train<-Auto[muestra,]
#Conjunto de Prueba
test<-Auto[-muestra,]

library(e1071)
set.seed(2567)
linear.smv<- tune(svm,mpg~.,data = train,kernel="linear",ranges = list(cost=c(0.001,0.01,0.1,1,5,10,100)))
summary(linear.smv)

lin_bm<-linear.smv$best.model
summary(lin_bm)

table(Predi<-predict(lin_bm,test),Real<-test$mpg)

##2.c)
set.seed(2567)
rad_tune<-tune(svm,mpg~.,data=train,kernel="radial",ranges =list(cost=c(0.1 ,1 ,10 ,100 ,1000),                                                     gamma=c(0.5,1,2,3,4)))
summary(rad_tune)

rad_bm<-rad_tune$best.model
summary(rad_bm)

table(pred<-predict(rad_bm,test),Real<-test$mpg)

set.seed(2567)
pol_tune<-tune(svm,mpg~.,data = train,kernel="polynomial",ranges = list(cost=c(0.01,0.1,1,10,100),
                                                                        gamma=c(0.2,0.5,1,2,3),
                                                                        degree=c(1,2,3,4,5)))
summary(pol_tune)

poly_bm <- pol_tune$best.model
summary(poly_bm)

table(Pred = predict(poly_bm, test) , Real = test$mpg)

plot(lin_bm, test, acceleration ~ horsepower)

plot(rad_bm,test,acceleration~horsepower)

plot(poly_bm,test,acceleration~horsepower)

## 3.a)

