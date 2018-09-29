kernel_Gaussian <- function(x0,x_data,h){
  u <- (x_data - x0)/h
  (2*pi)^(-1/2) * exp(-u^2/2)
}
estimate_density <- function(data,x0,factor=1,h=0){
  n <- length(data)
  if (h == 0){
    h <- (0.79*IQR(data)*n^(-1/5))*factor  #h_opt by normal reference
  }
  f_hat <- sum(kernel_Gaussian(x0, data, h)) * (n*h)^(-1)
  var_f_hat <- (n*h)^(-1) * f_hat * (2*sqrt(pi))^(-1)
  list(f_hat=f_hat, var_f_hat=var_f_hat)
}

variate.kernel<-function(x0,data,alpha){
  variate.ker.den<-0
  for(i in 1:length(alpha)){
    ki<-kernel_Gaussian(x0,data[i],alpha[i])/alpha[i]
    variate.ker.den<- variate.ker.den+ki
  }
  f.h<-variate.ker.den/length(data)
  f.h
}

var.result<-function(dataset){
  act.data<-dataset[,2]
  l<-length(act.data)
  hopt1 <- (0.79*IQR(act.data)*l^(-1/5))
  first.dens1 <- lapply(act.data, estimate_density, data=act.data)
  normal.dens<-rep(0,l)
  for(i in 1:l){
    normal.dens[i]<-first.dens1[[i]]$f_hat
  }
  alpha.normal1<-hopt1*(sqrt(normal.dens)^(-1))
  var.den<-apply(as.matrix(act.data),1,FUN=variate.kernel,data=act.data,alpha=alpha.normal1)
  result<-data.frame(dataset)
  colnames(result)<-c("id","data")
  result$density<-var.den
  result$outlier_0.01<-sapply(result$density, function(x) x<=quantile(result$density, 0.01))
  result$outlier_0.05<-sapply(result$density, function(x) x<=quantile(result$density, 0.05))
  result
}


wwp<-read.table("Data/Data_clean/wwpdf_overall.WilsonBestimate")
wwp<-wwp[order(wwp[,2]),]
wwp.var<-var.result(wwp)
write.table(wwp.var, sep="\t", file="Data/Outlier_hopt/wwpdf_overall.WilsonBestimate_outlier_h.var", row.names=F)
