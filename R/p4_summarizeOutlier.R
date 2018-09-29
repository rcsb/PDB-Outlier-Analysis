## Calculate summary statistics
library(moments)

getNormalFit <- function(d){
    n = length(d)
    mean = mean(d)
    sd = sd(d)
    sigma_low_0.01  = mean - 2.58*sd
    sigma_high_0.01 = mean + 2.58*sd 
    sigma_low_0.05  = mean - 2.0*sd
    sigma_high_0.05 = mean + 2.0*sd
    skewness <- skewness(d)
    kurtosis <- kurtosis(d)
    data.frame(n=n, mean=mean, sd=sd,
              sigma_low_0.01=sigma_low_0.01, sigma_high_0.01=sigma_high_0.01,
              sigma_low_0.05=sigma_low_0.05, sigma_high_0.05=sigma_high_0.05,
              skewness=skewness, kurtosis=kurtosis)
}    

getIQR <- function(d){
    median = median(d)
    Q1 = quantile(d, 0.25)
    Q3 = quantile(d, 0.75)
    IQR = IQR(d)
    IQR1.5_below_Q1 = Q1 - 1.5*IQR
    IQR1.5_above_Q3 = Q3 + 1.5*IQR
    IQR1.0_below_Q1 = Q1 - 1.0*IQR
    IQR1.0_above_Q3 = Q3 + 1.0*IQR
    data.frame(median=median, Q1=Q1, Q3=Q3, IQR=IQR,
               IQR1.5_below_Q1=IQR1.5_below_Q1, IQR1.5_above_Q3=IQR1.5_above_Q3,
               IQR1.0_below_Q1=IQR1.0_below_Q1, IQR1.0_above_Q3=IQR1.0_above_Q3)
}

getQuantile <- function(d){
    min <- min(d)
    max <- max(d)
    q0.001 = quantile(d,0.001)
    q0.999 = quantile(d,0.999)
    q0.005  = quantile(d,0.005)
    q0.995 = quantile(d,0.995)
    q0.025  = quantile(d,0.025)
    q0.975 = quantile(d,0.975)
    data.frame(min=min, max=max,q0.001=q0.001,q0.999=q0.999,q0.005=q0.005,q0.995=q0.995,q0.025=q0.025,q0.975=q0.975)
}

getPRO <- function(data){
    Q1 = quantile(data$data, 0.25)
    Q3 = quantile(data$data, 0.75)
    
    d1 <- subset(data, data<Q1 & outlier_0.01) # Pick the left side density outliers
    if(length(d1$data) == 0){
        outlier_left_highest_0.01 = -999  # No outlier at the left side
    }else{
        outlier_left_highest_0.01 = max(d1$data) # Return the highest outlier at the left
    }
    d2 <- subset(data, data>Q3 & outlier_0.01)  # Pick the right side density outliers
    if(length(d2$data) == 0){
        outlier_right_lowest_0.01 = 999  # No outlier at the right side
    }else{
        outlier_right_lowest_0.01 = min(d2$data) # Return the lowest outlier at the right
    }

    d3 <- subset(data, data<Q1 & outlier_0.05) 
    if(length(d3$data) == 0){
        outlier_left_highest_0.05 = -999  
    }else{
        outlier_left_highest_0.05 = max(d3$data) 
    }
    d4 <- subset(data, data>Q3 & outlier_0.05)  
    if(length(d4$data) == 0){
        outlier_right_lowest_0.05 = 999  
    }else{
        outlier_right_lowest_0.05 = min(d4$data) 
    }
    data.frame(outlier_left_highest_0.01=outlier_left_highest_0.01,outlier_right_lowest_0.01=outlier_right_lowest_0.01,
               outlier_left_highest_0.05=outlier_left_highest_0.05,outlier_right_lowest_0.05=outlier_right_lowest_0.05)
}

getDensityMedian <- function(data){
    density_median_low = -999 #set starting value
    density_median_high = 999 #set starting value
    median_pdf = median(data$density)
    max_density_index = which.max(data$density)
    mode = data$data[max_density_index]
    for (i in max_density_index:1){
        if (data$density[i] <= median_pdf){
      	    density_median_low = data$data[i]
	    break
        }
    }
    for (i in max_density_index:length(data$data)){
        if (data$density[i] <= median_pdf){
      	    density_median_high = data$data[i]
	    break
        }
    }
    density_IQR <- density_median_high - density_median_low
    data.frame(mode=mode, density_median_low=density_median_low, density_median_high=density_median_high,density_IQR=density_IQR)
}

getSummaryOne <- function(data){
    d <- data$data
    number_obs <- length(d)
    
    df_1 <- getNormalFit(d) ## Calculate statistics for normal distribution fit
    df_2 <- getIQR(d) ## Calculate Tukey's fence statistics
    df_3 <- getQuantile(d) ## Calculate quantile based statistics
    df_4 <- getPRO(data) ## Calculate the density ranking outier statistics
    df_5 <- getDensityMedian(data)  ## Calculate the new dispersion statistics based on density outlier
    ## Measure dispersion as the smallest range between median density left and right
    df_summary_one <- cbind(df_1, df_2, df_3, df_4, df_5) 
} 

getSummary <- function(filename_item_list, data_folder=""){
    df_items <- read.table(filename_item_list)
    v_items <- as.character(df_items[,1])
    n = length(v_items)
    df_summary <- data.frame()
    for (item in v_items){
        filename_data <- paste0(item, "_outlier")
    	filepath_data <- paste0(data_folder, filename_data)
    	print(filepath_data)
    	data <- read.table(filepath_data, header=TRUE, sep="\t")
    	df_summary_one <- getSummaryOne(data)
        row.names(df_summary_one) <- item
        df_summary <- rbind(df_summary, df_summary_one)
        
    }
    filename_summary = paste0("summary.csv")
    write.csv(round(df_summary,3), file=filename_summary)
}

filename_item_list = "items.list" # File recording each variable name in a row, which is also the data file name in data folder
data_folder <- "../Data/Outlier_hopt/" # Data folder
getSummary(filename_item_list, data_folder)
