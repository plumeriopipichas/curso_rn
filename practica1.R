p <- matrix(nrow=2,ncol=4,data= c(0,0,0,1,1,0,1,1))
target <- matrix(nrow = 1,ncol=4,c(0,0,0,1))
bias <- matrix(nrow = 1,ncol=4,rep(0,4))

W <- matrix(nrow=1,ncol = 2,data = rnorm(2))
 
salida <- sapply(W%*%p+bias,HardLimit)
error <-  target-salida

 cat("Pesos iniciales:")
 print(as.numeric(W))
 cat("Salida inicial:")
 print(as.numeric(salida))
 cat("Error inicial:") 
 print(as.numeric(error))
 cat("\n")
 
for (i in 1:2){
   bias <- bias+error
   W <- W+error%*%t(p)
   salida <- sapply(W%*%p+bias,HardLimit)
   error <-  target-salida
 }
 
 cat("Peso final:")
 print(W)
 cat("Salida final:")
 print(salida)
 cat("Error final:") 
 print(error)
 cat("\n")
