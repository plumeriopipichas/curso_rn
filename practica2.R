
#4 conjuntos de datos para los 2 nodos de entrada
p <- matrix(nrow=2,ncol=4,data= c(0,0,0,1,1,0,1,1))

#3 targets para cada nodo de salida
aux<-data.frame(AND=c(0,0,0,1),OR =c(0,1,1,1),NOR = c(1,0,0,0))

target<-t(as.matrix(aux))

#pesos iniciales aleatorios, bias inicial en ceros
W <- matrix(nrow=3,ncol = 2,data = rnorm(6))
row.names(W)<-names(aux)

ceros <- matrix(nrow = 3,ncol=4,rep(0,12))
bias<-ceros
row.names(bias)<-row.names(W)

#valores en los nodos de salida

salida <- matrix(nrow = 3, ncol=4,sapply(W%*%p+bias,HardLimit))
row.names(salida)<-row.names(target)

error <-  target-salida


cat("Pesos iniciales:")
print(W)
cat("\n")
cat("Salida inicial:")
print(salida)
cat("\n")
cat("Error inicial:")
print(error)
cat("\n")

for (i in 1:10){
  bias <- bias+error
  W <- W+error%*%t(p)
  salida <- sapply(W%*%p+bias,HardLimit)
  error <-  target-salida
  if (length(which(!error==ceros))==0){
    print(c("Iteraciones:",i))
    cat("\n")
    break
  }
  
}

cat("Peso final:")
print(W)
cat("\n")
cat("Salida final:")
print(salida)
cat("\n")
cat("Error final:")
print(error)
cat("\n")