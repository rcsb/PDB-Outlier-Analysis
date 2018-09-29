## Generate bandwidth h-opt by other methods
library(kedd)

getHOne <- function(da){
    h.iqr <- (0.79*IQR(da)*(length(da))^(-1/5))
    h.amise <- h.amise(da)$h
    h.bcv <- h.bcv(da)$h
    h.ucv <- h.ucv(da)$h
    h.ccv <- h.ccv(da)$h
    h.mcv <- h.mcv(da)$h
    h.mlcv <- h.mlcv(da)$h
    h.tcv <- h.tcv(da)$h
    data.frame(hopt=hopt, h.amise=h.amise, h.bcv=h.bcv, h.ccv=h.ccv,
               h.mcv=h.mcv, h.mlcv=h.mlcv, h.tcv=h.tcv, h.ucv=h.ucv)
}

getHAll <- function(filename_item_list, folder_datain="", folder_dataout=""){
    df_items <- read.table(filename_item_list) 
    v_items <- as.character(df_items[,1])
    for (item in v_items){
        filepath_data <- paste0(folder_datain, item) # Data file name is same as variable name in ../Data folder
    	print(filepath_data)
    	data_ori <- read.table(filepath_data) # All data file have two columns: 1st->ID; 2nd->data
        da <-  data_ori[,2]
        df_h_one <- getHOne(da)

        filename_out = paste0(item, "_h")
        filepath_out = paste0(folder_dataout, filename_out)
        write.table(df_h_one, sep="\t", file=filepath_out, row.names=FALSE) # Write result to local file
    }
}

filename_item_list = "items.list" 
folder_datain  <- "Data/Data_clean/"
folder_dataout <- "Data/"
getHAll(filename_item_list, folder_datain, folder_dataout)

