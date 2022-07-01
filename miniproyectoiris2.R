source("funciones_practica.R")
print("A")

library(ggplot2)
library(gridExtra)
library(dplyr)
library(datasets)


alpha = 0.0001 # parametro en el descendo del gradiente
epocas = 20   # veces que se itera el proceso

entradas = 4   # cantidad de variables que se consideran en la entrada

neuronas = 3   # neuronas de salida, donde se obtienen valores en funcion 
              # de las entradas y los pesos. Hay una neurona por cada especie,
              # 1 predice que es esa especie, 0 que es otra.

pruebas = 130   # cantidad de colecciones de datos que se van a meter en las
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

print(dim(validation))

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

W <- matrix(nrow = neuronas,ncol = entradas,data = rnorm(entradas*neuronas))
W <- as.data.frame(W)
names(W) <- row.names(p)
row.names(W)<-row.names(target)
W <- as.matrix(W)

ceros <- matrix(nrow = neuronas,ncol=ncol(p),rep(0,neuronas))
bias <- ceros

row.names(bias)<-row.names(W)  

cat("Pesos iniciales:")
print(W)
cat("\n")

#error_cuadratico <- as.data.frame(matrix(nrow=0,ncol = neuronas))
#names(error_cuadratico) <- row.names(W)

for (ii in 1:epocas){
    for (jj in 1:nrow(matriz_error)){
        salida <- W%*%p[ ,jj]+bias[ ,jj]  
        error <- target[ ,jj]-salida
        W <- W + 2*error%*%(alpha*t(p[ ,jj])) 
        bias[ ,jj] <- bias[ ,jj] + error
        #ecm<-numeric()
        #ecm[jj] <- sum(matriz_error[jj, ]**2)/ncol(matriz_error)
    }
  
  #ecm <- as.data.frame(matrix(nrow=1,ncol=3,ecm))
  #names(ecm) <- names(error_cuadratico)
  #error_cuadratico <- rbind(error_cuadratico,ecm)
  
} 

# validacion

salida<-W%*%as.matrix(t(validation[ ,1:4]))+apply(bias,1,mean)

validation <- cbind(validation,as.data.frame(t(salida)))

n <- nrow(validation)
m <- ncol(validation)


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



rm(ii,x,jj,temp,m,n)