## Algorithm of kernal estimate probability density of continuous variable, normal kernal only

## setwd("~/Work/Statistics/PDB_research/Outliers")
## setwd("/net/users/chenghua/Python/Projects/Outlier/R")

## Function to calculate optimal h, the IQR version of H-opt from normal distribution
## return optimal window width based on robust IQR, but the formula is derived from normal distribution
hopt <- function(data){
    n = length(data)
    hopt <- (0.79*IQR(data)*n^(-1/5))
    hopt
}

## Function of Gaussian kernel
## return pdf density of a particular point, note that this is kernel, not density estimate
## x0 is the point of interest, h is the window width
## It's same as calculate pdf with mean=x0 and sd=h
kernelGaussian <- function(x,x0=0,h=1){ 
    u <- (x - x0)/h
    f <- (2*pi)^(-1/2) * exp(-u^2/2)
    f
}

## Function of Kernel estimation f_hat by fixed-length h. 
## return density estimate (optionally return variance of the estimate)
## f_hat follows strict density property, variance function is for all region 
## x0 is the point of interest, factor is the scale parameters, default at 1.
## It seems of no use to parallel this step by using parSapply(cl, data, kernelGaussian, x0=x0, h=h), especially when calling it in another parSapply
estimateDensity <- function(data,x0,factor=1,h=0){
    n <- length(data)
    if (h == 0){
        h <- hopt(data)*factor
    }
    f_hat <- sum(kernelGaussian(data,x0,h))/(n*h) 
    f_hat
    ## var_f_hat <- (n*h)^(-1) * f_hat * (2*sqrt(pi))^(-1) # Return variance of the estimate, not used for now
    ## list(f_hat=f_hat, var_f_hat=var_f_hat)
}

## Combined function to calculate density only
## Input parameter "data" is  a vector of original data
## Return density_est is a vector of density, in the same order as that of "data"
estimateDensityAll <- function(cluster=FALSE, data, h=0){
    n <- length(data)
    if(!h) h <- (0.79*IQR(data)*n^(-1/5)) #hopt
    
    if(class(cluster)[1]=="SOCKcluster"){
        ## v_density_est <- parSapply(cluster, data, function(data, x0, h, n) sum(kernelGaussian(data,x0,h))/(n*h), data=data, h=h,n=n)
                                        #works, explit x0/data/h/n
        
        v_density_est <- parSapply(cluster, data, function(x0) sum(kernelGaussian(data,x0,h))/(n*h))
                                        #works, best solution, without explicit data/h/n, directly pass data
        
        ## v_density_est <- parSapply(cluster, data, estimateDensityNew, data=data, h=h, n=n)
                                        #works, order independent, use "data=data" can explicitly pass variable and disregard order of parameters, the one not pass will take input from sapply's 2nd parameter
        
        ## v_density_est <- parSapply(cluster, data, estimateDensityNew, data, h, n)
                                        #works, order dependent, implicitly take sapply's 2nd parameter as first input for the function, and orderly pass 2nd/3rd/4th, etc. 
    }else{
        v_density_est <- sapply(data, function(x0) sum(kernelGaussian(data,x0,h))/(n*h))
    }
    v_density_est
}

#Input DF, Output DF
estimateDensityDF <- function(cluster=FALSE, data, col_value, h=0){
    v_da <- data[[col_value]]
    v_density_est <- estimateDensityAll(cluster=cluster,v_da, h)
    data$density <- v_density_est
    data
}
