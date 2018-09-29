## Calculate MPR statistics
getMPR <- function(data, p){
    d <- data$data
    den <- data$density
    p_den = quantile(den, 1-p)
    
    max_density_index = which.max(data$density)
    MPR_low=min(d)
    MPR_high=max(d)
    for (i in max_density_index:1){
        if (den[i] <= p_den){
      	    MPR_low <- d[i]
	    break
        }
    }
    for (i in max_density_index:length(d)){
        if (den[i] <= p_den){
      	    MPR_high <- d[i]
	    break
        }
    }
    MPR <- c(p_den,MPR_low,MPR_high)
    MPR
}

getMPRs <- function(filepath_data,v_p){
    data <- read.table(filepath_data, header=TRUE, sep="\t")
    v_MPRs<-c()
    for(p in v_p){
        v_MPR <- getMPR(data, p)
        ## print("v_MPR")
        ## print(v_MPR)
        v_MPRs<-c(v_MPRs,v_MPR)
    }
    v_MPRs
}

getMPRAll <- function(filename_item_list, data_folder="",v_p){
    df_items <- read.table(filename_item_list)
    v_items <- as.character(df_items[,1])
    n = length(v_items)
    df_MPRs_All <- data.frame()
    for (item in v_items){
        filename_data <- paste0(item, "_outlier")
    	filepath_data <- paste0(data_folder, filename_data)
    	print(filepath_data)
        v_MPRs<-getMPRs(filepath_data,v_p)
        ## print("v_MPRs")
        ## print(v_MPRs)
        df_MPRs_All<-rbind(df_MPRs_All,v_MPRs)
    }
    row.names(df_MPRs_All)<-v_items
    v_names<-c()
    for(p in v_p){
        v_names<-c(v_names,paste0(p*100,"%_density"),paste0(p*100,"%_MPR_low"),paste0(p*100,"%_MPR_high"))
    }
    names(df_MPRs_All)<-v_names
    ## print(df_MPRs_All)
    write.csv(round(df_MPRs_All,3), file="MPRs.csv")
}

filename_item_list = "items.list" # File recording each variable name in a row, which is also the data file name in data folder
data_folder <- "../Data/Outlier_hopt/" # Data folder
v_p <- c(0.5,0.6,0.7,0.8,0.9,0.95)
getMPRAll(filename_item_list,data_folder,v_p)
