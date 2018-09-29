kernel_Gaussian <- function(x0,x_data,h){
  u <- (x_data - x0)/h
  (2*pi)^(-1/2) * exp(-u^2/2)
}

find.local.dmax <- function(x.sort, x0, n0){  
  samplesize.n <- length(x.sort)
  index <- which(x.sort==x0) #find the index for x0, i.e. where is x0 in sorted data
  n <- length(index) #if there is redundant measurements, check how many
  if (n>=n0){ #when there are more repeated measurements than n0
    if (min(index)==1){ #if repeated value is the minimal value
      distance.up <- abs(x.sort[max(index)+1]-x.sort[max(index)])
      d.max <- distance.up #find the disance to the next non-repeated value
    }else if (max(index)==samplesize.n){ #if repeated value is maximal value
      distance.down <- abs(x.sort[min(index)-1]-x.sort[min(index)])
      d.max <- distance.down #find the distance to the prevous non-repeated value
    }else{ #when repeated value is neither minimal nor maximal
      distance.up <- abs(x.sort[max(index)+1]-x.sort[max(index)])
      distance.down <- abs(x.sort[min(index)-1]-x.sort[min(index)])
      if (distance.up <= distance.down){ #find the smaller distance to closest non-repeated value
        d.max <- distance.up 
      }else{
        d.max <- distance.down
      }
    } 
  }else{ #majority of the cases, when no repeated measurement or few repeated
    count <- n        #initialize count as point of the starting index itself
    index.up <- max(index) + 1     #initialize to go up/higher/right, bigger index
    index.down <- min(index) - 1    #initialize to go down/lower/left, smaller index
    upper.limit <- FALSE #marker to check whether to reach maximum
    lower.limit <- FALSE #marker to check whether to reach minimum
    while (count < n0){
      if (index.up > samplesize.n){	#consider the boundary effect, if index.up/index.down reaches boundary, extend to the other end
        index.up <- index.up - 1
        upper.limit <- TRUE
      }
      if (index.down < 1){
        index.down <- index.down + 1
        lower.limit <- TRUE
      }
      distance.up <- abs(x.sort[index.up]-x.sort[max(index)])
      distance.down <- abs(x.sort[index.down]-x.sort[min(index)])
      if(upper.limit){
        d.max <- distance.down
        index.down <- index.down - 1
      }else if(lower.limit){
        d.max <- distance.up
        index.up <- index.up + 1
      }else{
        if (distance.up <= distance.down){
          d.max <- distance.up
          index.up <- index.up + 1    	#extend the range at the side with smaller distance
        }else{
          d.max <- distance.down
          index.down <- index.down - 1
        }
      }
      count <- count + 1
    }
  }
  d.max
}

nearest_density <- function(data,x0,k=10){
  n <- length(data)
  dks <- vector(mode="numeric")
  dk <- find.local.dmax(data,x0,k) #calculate kNN window width 
  dks <- c(dks, dk) #collect all window width for inspection purose
  f_hat <- sum(kernel_Gaussian(x0, data, dk)) * (n*dk)^(-1)
  var_f_hat <- 2 * (f_hat^2/k) * (2*sqrt(pi))^(-1)
  list(f_hat=f_hat, var_f_hat=var_f_hat, dks=dks)
}


KNN.result<-function(dataset,k_value=200){
  act.data<-dataset[,2]
  den.near1<-lapply(act.data,nearest_density,data=act.data,k=k_value)
  dens.est.near1 <- sapply(den.near1, function(x) x[['f_hat']])
  result<-data.frame(dataset)
  colnames(result)<-c("id","data")
  result$density<-dens.est.near1 
  result$outlier_0.01<-sapply(result$density, function(x) x<=quantile(result$density, 0.01))
  result$outlier_0.05<-sapply(result$density, function(x) x<=quantile(result$density, 0.05))
  result
}

wwp<-read.table("Data/Data_clean/wwpdf_overall.WilsonBestimate")
wwp<-wwp[order(wwp[,2]),]
k_value_wwp<-floor(length(wwp[,2])/50)
print(k_value_wwp)
wwp.knn<-KNN.result(wwp,k=k_value_wwp)
write.table(wwp.knn, sep="\t", file="Data/Outlier_hopt/wwpdf_overall.WilsonBestimate_outlier_h.knn", row.names=F)
