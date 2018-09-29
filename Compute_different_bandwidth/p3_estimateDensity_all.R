library(parallel)
cluster <- makeCluster(detectCores()-1, type="FORK") # FORK cluster, not working in Windows
source("../R/Util/outlier.R") # Read in outlier detection algorithm 
source("../R/Util/sortDF.R")

writeDensity_all <- function(filename_item_list, folder_datain="", folder_dataout=""){  
    df_items <- read.table(filename_item_list) # Read each variable name in a list file, 
    v_items <- as.character(df_items[,1]) # Each row is the variable name, data file name is same as variable name

    filename_h_list <- "h_fixed.list"
    data_h <- read.table(filename_h_list)
    v_h_names <- data_h[,1]

    for (item in v_items){
        filepath_data <- paste0(folder_datain, item) # Data file name is same as variable name in ../Data folder
    	print(filepath_data)
    	data_ori <- read.table(filepath_data) # All data file have two columns: 1st->ID; 2nd->data
        
        filename_item_h <- paste0(item, "_h")
        filepath_item_h <- paste0("Data/", filename_item_h)
        print(filepath_item_h)
        data_item_h <- read.table(filepath_item_h, header=T, sep="\t")
        for (h_name in v_h_names){
            h <- data_item_h[[h_name]][1]
            print(paste(h_name, h))
            data <- estimateDensityDF(cluster=cluster, data_ori, 2, h)
            names(data) <- c("id", "data", "density")
            data <- sortDF(data, 2)
            filename_out = paste0(item, "_density_", h_name)
            filepath_out = paste0(folder_dataout, filename_out)
            print(filepath_out)
            write.table(data, sep="\t", file=filepath_out, row.names=FALSE) # Write result to local file
        }
    }
}

filename_item_list <- "items.list" # File recording each variable name in a row, which is also the data file name in data folder
folder_datain  <- "Data/Data_clean/"
folder_dataout <- "Data/Density_hopt/"
writeDensity_all(filename_item_list, folder_datain, folder_dataout)

stopCluster(cluster)
