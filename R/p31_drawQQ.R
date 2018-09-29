## Draw normal QQ plot of each data set

drawQQ <- function(filename_item_list, data_folder="", image_folder=""){
    df_items <- read.table(filename_item_list)
    v_items <- as.character(df_items[,1])

    for (item in v_items){
        filename_data <- paste0(item, "_outlier")
    	filepath_data <- paste0(data_folder, filename_data)
    	print(filepath_data)
    	data <- read.table(filepath_data, header=TRUE, sep="\t")

        ## Draw QQ plot
    	filename_image = paste0(item, "_QQ.png")
	filepath_image = paste0(image_folder, filename_image) 
    	png(filepath_image, width = 800, height = 800)
    	par(mfrow=c(1,1),mar=c(5.1, 5.1, 4.1, 2.1))
	qqnorm(data$data, cex.axis=2.5, cex.lab=2.5, cex.main=2.5, main="")
    	dev.off()
    }
}

filename_item_list = "items.list" # File recording each variable name in a row, which is also the data file name in data folder
data_folder <- "../Data/Outlier_hopt/"
image_folder <- "../Images/"
drawQQ(filename_item_list, data_folder, image_folder)
