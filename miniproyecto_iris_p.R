source("funciones_practica.R")
library(ggplot2)
library(gridExtra)
library(dplyr)
library(datasets)

set.seed(971)

#alpha = 0.001 # parametro en el descendo del gradiente
epocas = 500 # veces que se itera el proceso

entradas = 4   # cantidad de variables que se consideran en la entrada

neuronas = 3   # neuronas de salida, donde se obtienen valores en funcion 
              # de las entradas y los pesos. Hay una neurona por cada especie,
              # 1 predice que es esa especie, 0 que es otra.

pruebas = 145   # cantidad de colecciones de datos que se van a meter en las
            # entradas. 

# conjuntos de datos para los nodos de entrada y el target correspondiente

x <- sample(1:nrow(iris),pruebas)

train <- iris[x, ]
validation <- iris[-x, ]

# normalizamos valores 

train$Sepal.Length <- train$Sepal.Length/mean(iris$Sepal.Length)
train$Sepal.Width <- train$Sepal.Width/mean(iris$Sepal.Width)
train$Petal.Length <- train$Petal.Length/mean(iris$Petal.Length)
train$Petal.Width <- train$Petal.Width/mean(iris$Petal.Width)

validation$Sepal.Length <- validation$Sepal.Length/mean(iris$Sepal.Length)
validation$Sepal.Width <- validation$Sepal.Width/mean(iris$Sepal.Width)
validation$Petal.Length <- validation$Petal.Length/mean(iris$Petal.Length)
validation$Petal.Width <- validation$Petal.Width/mean(iris$Petal.Width)


#Codificamos las tres especies

train$num_setosa <- 0
train$num_versicolor <- 0
train$num_virginica <- 0

for (ii in 1:nrow(train)){
    if (train$Species[ii]=="setosa"){
      train$num_setosa[ii] <- 1
    }
    if (train$Species[ii]=="versicolor"){
      train$num_versicolor[ii] <- 1
    }
    if (train$Species[ii]=="virginica"){
      train$num_virginica[ii] <- 1
  }
}

p <- t(as.matrix(select(train,Sepal.Length:Petal.Width)))
target <- t(as.matrix(select(train,num_setosa:num_virginica)))

#pesos iniciales aleatorios, bias inicial en ceros

W <- matrix(nrow = neuronas,ncol = entradas,data = rnorm(entradas*neuronas,sd = 0.1))
W <- as.data.frame(W)
names(W) <- row.names(p)
row.names(W)<-row.names(target)
W <- as.matrix(W)

ceros <- matrix(nrow = neuronas,ncol=ncol(p),rep(0,neuronas*ncol(p)))
bias <- ceros

row.names(bias)<-row.names(W)  

cat("Pesos iniciales:")
print(W)
cat("\n")

#error_cuadratico <- as.data.frame(matrix(nrow=0,ncol = neuronas))
#names(error_cuadratico) <- row.names(W)

for (ii in 1:entradas){
  for (jj in 1:ncol(p)){
    salida <- matrix(nrow = neuronas, ncol=1,sapply(W%*%p[ ,jj]+bias[ ,jj],HardLimit))
    error <-  target[ ,jj]-salida
    W <- W + error%*%t(p[ ,jj])    
    bias[ ,jj] <- bias[ ,jj] + error
  }
  
  #if (length(which(!error==ceros))==0){
  #  print(c("Iteraciones:",i))
  #  cat("\n")
  #  break
  #}
} 

# validacion

cat("Pesos finales:")
print(W)
cat("\n")

salida<-W%*%as.matrix(t(validation[ ,1:4]))+apply(bias,1,mean)
validation <- cbind(validation,as.data.frame(t(salida)))

n <- nrow(validation)
m <- ncol(validation)

validation$predict_set <- "-"
validation$predict_ver <- "-"
validation$predict_vir <- "-"

#for (ii in 1:n){
#  temp <- valida_cerca(1, validation[ii ,(m-2):m])
#  aux <- names(validation)[temp+5]
#  validation$predict[ii] <- substr(aux,start = 5,stop = nchar(aux)) 
#}

for (jj in 1:3){
  temp <- names(validation)[5+jj]
  flor <- substr(temp,start=5,stop=nchar(temp))
  for (ii in 1:n){
      if (abs(1-validation[ii,5+jj])<abs(validation[ii,5+jj])){
        validation[ii,jj+8] <- flor
      }
      else{
        validation[ii,jj+8] <- paste("no",flor)
      }
  }

}

#ver que el train si jala bien

train[ ,6:8]<-W%*%t(train[ ,1:4])+apply(bias,1,mean)

train$predict_set <- "-"
train$predict_ver <- "-"
train$predict_vir <- "-"

for (jj in 1:3){
  temp <- names(train)[5+jj]
  flor <- substr(temp,start=5,stop=nchar(temp))
  for (ii in 1:nrow(train)){
    if (abs(1-train[ii,5+jj])<abs(train[ii,5+jj])){
      train[ii,jj+8] <- flor
    }
    else{
      train[ii,jj+8] <- paste("no",flor)
    }
  }
  
}

rm(ii,x)