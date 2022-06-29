source("funciones_practica.R")
library(ggplot2)
library(gridExtra)

alpha = 0.01 # parametro en el descendo del gradiente
epocas = 5   # veces que se itera el proceso

entradas=2   # cantidad de variables que se consideran en la entrada
neuronas=3   # neuronas de salida, donde se obtienen valores en funcion 
              # de las entradas y los pesos

pruebas=4   # cantidad de colecciones de datos que se van a meter en las
            # entradas. 


# conjuntos de datos para los nodos de entrada

p<-matrix(nrow=entradas,ncol=pruebas,data= c(0,0,0,1,1,0,1,1))

# Un target (etiqueta objetivo) para cada nodo de salida

#los nombres corresponden a las neuronas

aux<-data.frame(AND=c(0,0,0,1),OR =c(0,1,1,1),NOR = c(1,0,0,0))

target<-t(as.matrix(aux))

#pesos iniciales aleatorios, bias inicial en ceros

W <- matrix(nrow=neuronas,ncol = entradas,data = rnorm(entradas*neuronas))
row.names(W)<-names(aux)

ceros <- matrix(nrow = neuronas,ncol=pruebas,rep(0,neuronas*pruebas))
bias <- ceros

row.names(bias)<-row.names(W)

#valores en los nodos de salida

cat("Pesos iniciales:")
print(W)
cat("\n")
cat("Salida inicial:")
print(W%*%p)
cat("\n")
cat("Error inicial:")
print(target-W%*%p)
cat("\n")

grafs<-grafica_rn(p,target,W,bias) 

error_cuadratico <- as.data.frame(matrix(nrow=0,ncol=3))
names(error_cuadratico) <- names(aux)
print(names(aux))

for (ii in 0:epocas){
    salida <- W%*%p+bias  
    matriz_error <- target-salida
    ecm<-numeric()
    
    for (jj in 1:nrow(matriz_error)){
      ecm[jj] <- sum(matriz_error[jj, ]**2)/ncol(matriz_error)
    }
    ecm <- as.data.frame(matrix(nrow=1,ncol=3,ecm))
    names(ecm) <- names(error_cuadratico)
    error_cuadratico <- rbind(error_cuadratico,ecm)
    
    bias <- bias + matriz_error
    W <- W + 2*matriz_error%*%(alpha*t(p)) 
}   

error_cuadratico <- cbind(data.frame(epoca=0:epocas),error_cuadratico)

cat("Peso final:")
print(W)
cat("\n")
cat("Bias final:")
print(bias)
cat("\n")
cat("Error final:")
print(matriz_error)
cat("\n")

grafs<-append(grafs,grafica_rn(p,target,W,bias))

#show(do.call("grid.arrange",c(grafs,ncol=3)))

View(error_cuadratico)

rm(aux,ecm,ii,jj)