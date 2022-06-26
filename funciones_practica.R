HardLimit<-function(valor){
  if (valor>0){
    return(1)
  } 
  else{
    return(0)
  }
}

grafica_rn<-function(entradas,target,pesos){
    base <- as.data.frame(t(entradas))
    grafs<-list()
    for (i in row.names(target)){
      base$target<-as.factor(target[i, ])
      g<-ggplot(base,aes(V1,V2))
      g<-g+geom_point(size=3.5,aes(color=target))+xlim(-0.5,1.5)+ylim(-0.5,1.5)+
          scale_color_brewer(palette="Dark2")+labs(color=i)+xlab(" ")+ylab(" ")
      grafs[[i]] <- g
    }  
    return(grafs)
}