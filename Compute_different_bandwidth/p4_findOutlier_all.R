pickOutlier <- function(data, percent){
    threshold <- quantile(data, percent)
    sapply(data, function(x) x<=threshold)
}

pickOutlierDF <- function(data, percents){
    for (percent in percents){
        name = paste0("outlier_", as.character(percent))
        data[[name]] <- pickOutlier(data[["density"]], percent)
    }
    data
}

pickOutlier_all <- function(filename_item_list, folder_datain="", folder_dataout=""){  
    df_items <- read.table(filename_item_list) # Read each variable name in a list file, 
    v_items <- as.character(df_items[,1]) # Each row is the variable name, data file name is same as variable name

    filename_h_list <- "h_fixed.list"
    data_h <- read.table(filename_h_list)
    v_h_names <- data_h[,1]

    for (item in v_items){
        filename_item_h <- paste0(item, "_h")
        filepath_item_h <- paste0("Data/", filename_item_h)
        print(filepath_item_h)
        data_item_h <- read.table(filepath_item_h, header=T, sep="\t")
        percents <- c(0.01, 0.05)

        for (h_name in v_h_names){
            filename_data <- paste0(item, "_density_", h_name)
            filepath_data <- paste0(folder_datain, filename_data) # Data file name is same as variable name in ../Data folder
            print(filepath_data)
            data_ori <- read.table(filepath_data, sep="\t", header=TRUE) # All data file have two columns: 1st->id; 2nd->data; 3rd->density
            
            data <- pickOutlierDF(data_ori, percents)
            filename_out = paste0(item, "_outlier_", h_name)
            filepath_out = paste0(folder_dataout, filename_out)
            write.table(data, sep="\t", file=filepath_out, row.names=FALSE) # Write result to local file
        }
    }
}

filename_item_list = "items.list" # File recording each variable name in a row, which is also the data file name in data folder
folder_datain <- "Data/Density_hopt/"
folder_dataout <- "Data/Outlier_hopt/"
pickOutlier_all(filename_item_list, folder_datain, folder_dataout)
