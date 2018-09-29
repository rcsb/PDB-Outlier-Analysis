sampleData <- function(filename_item_list, folder_datain="", folder_dataout=""){
    df_items <- read.table(filename_item_list) 
    v_items <- as.character(df_items[,1])
    for (item in v_items){
        filepath_data <- paste0(folder_datain, item) # Data file name is same as variable name in ../Data folder
    	print(filepath_data)
    	data_ori <- read.table(filepath_data) # All data file have two columns: 1st->ID; 2nd->data
        len <- length(data_ori[,2])
        set.seed(1)
        v_index <- sample(len, 10000)
        data_sample <- data_ori[v_index,]
        filepath_out = paste0(folder_dataout, item)
        write.table(data_sample, sep="\t", file=filepath_out, col.names=F, row.names=FALSE) # Write result to local file
    }
}

filename_item_list = "items.list" 
folder_datain  <- "../../Data/Data_clean/"
folder_dataout <- "Data/Data_clean/"
sampleData(filename_item_list, folder_datain, folder_dataout)
