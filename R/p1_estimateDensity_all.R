library(parallel)
cluster <- makeCluster(detectCores()-1, type="FORK") # FORK cluster, not working in Windows
source("Util/outlier.R") # Read in outlier detection algorithm 
source("Util/sortDF.R")

writeDensity_all <- function(filename_item_list, folder_datain="", folder_dataout=""){  
    df_items <- read.table(filename_item_list) # Read each variable name in a list file, 
    v_items <- as.character(df_items[,1]) # Each row is the variable name, data file name is same as variable name

    for (item in v_items){
        filepath_data <- paste0(folder_datain, item) # Data file name is same as variable name in ../Data folder
    	print(filepath_data)
    	data_ori <- read.table(filepath_data) # All data file have two columns: 1st->ID; 2nd->data
        data <- estimateDensityDF(cluster=cluster, data_ori, 2)
        names(data) <- c("id", "data", "density")
        data <- sortDF(data, 2)
        filename_out = paste0(item, "_density")
	filepath_out = paste0(folder_dataout, filename_out)
    	write.table(data, sep="\t", file=filepath_out, row.names=FALSE) # Write result to local file
    }
}

filename_item_list = "items.list" # File recording each variable name in a row, which is also the data file name in data folder
folder_datain <- "../Data/Data_clean/"
folder_dataout <- "../Data/Density_hopt/"
writeDensity_all(filename_item_list, folder_datain, folder_dataout)

stopCluster(cluster)
