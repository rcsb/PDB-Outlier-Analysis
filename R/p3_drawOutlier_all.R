## Function to draw density plot with outliers highlighted in color
## It seems to be of no use to for parallel operation for drawing image, tried parallel setting but of no use. 
drawDensityOne <- function(data, percent, min_cut, max_cut){
    col_outlier <- paste0("outlier_", as.character(percent))
    ## if(!min_cut) min_cut <- min(data[["data"]])
    ## if(!max_cut) max_cut <- max(data[["data"]])
    plot(data[["data"]], data[["density"]], type="n", col="black", xlab="", ylab="Density Estimate",
         cex.axis=2.5, cex.lab=2.5, xlim=c(min_cut, max_cut))
    draw <- mapply(function(x,y,z) segments(x, 0, x, y, col = if(z) "indianred" else "lightblue", lty=1, lwd=2),
                   data[["data"]], data[["density"]], data[[col_outlier]])
    ## draw <- apply(data[,c("data","density",col_outlier)], 1, function(x) segments(x[1], 0, x[1], x[2], col = if(x[3]) "red" else "blue", lty=1, lwd=2))
    rm(draw)
}

## Function to draw standard-deviation-based, IQR-based, and simple-percent-based outlier cut off line
## For standard deviation, use +-2.58 sigma that is 0.5% cut on both sides of normal distribution
## For IQR deviation, use Q1-1.5IQR and Q3+1.5IQR (Tukey) that is ~0.35% cut on both sides of normal distribution
## For simple percent cut, use 0.5% cut on both sides
drawCut001 <- function(vector){
  abline(v=mean(vector)-2.58*sd(vector),col="magenta", lty=2, lwd=3)
  abline(v=mean(vector)+2.58*sd(vector),col="magenta", lty=2, lwd=3)
  abline(v=quantile(vector,0.25)-1.5*IQR(vector),col="orange", lty=2, lwd=3)
  abline(v=quantile(vector,0.75)+1.5*IQR(vector),col="orange", lty=2, lwd=3)
  abline(v=quantile(vector, 0.005), col="darkgray", lty=2, lwd=3)
  abline(v=quantile(vector, 0.995), col="darkgray", lty=2, lwd=3)
}

## Function to draw 5% comparision, use 2.0 sigma, 1.0*IQR, and 5%
drawCut005 <- function(vector){
  abline(v=quantile(vector,0.25)-1.0*IQR(vector),col="orange", lty=2, lwd=3)
  abline(v=quantile(vector,0.75)+1.0*IQR(vector),col="orange", lty=2, lwd=3)
  abline(v=mean(vector)-2.0*sd(vector),col="magenta", lty=2, lwd=3)
  abline(v=mean(vector)+2.0*sd(vector),col="magenta", lty=2, lwd=3)
  abline(v=quantile(vector, 0.025), col="darkgray", lty=2, lwd=3)
  abline(v=quantile(vector, 0.975), col="darkgray", lty=2, lwd=3)
}

drawDensity <- function(filename_item_list, data_folder="", image_folder="", percent, compare=TRUE, horizon=TRUE, legend=TRUE){
    df_items <- read.table(filename_item_list)
    v_items <- as.character(df_items[,1])

    for (item in v_items){
        filename_data <- paste0(item, "_outlier")
    	filepath_data <- paste0(data_folder, filename_data)
    	print(filepath_data)
    	data <- read.table(filepath_data, header=TRUE, sep="\t")

     	filename_image = paste0(item, "_", percent, ".png")
	filepath_image = paste0(image_folder, filename_image) 
    	png(filepath_image, width = 800, height = 800)
    	par(mfrow=c(1,1),mar=c(5.1, 5.1, 4.1, 2.1))
        d = data[["data"]]
        if(compare){
            min_cut <- min(quantile(d, 0.001), quantile(d,0.25)-1.5*IQR(d), mean(d)-2.58*sd(d))
            max_cut <- max(quantile(d, 0.999), quantile(d,0.75)+1.5*IQR(d), mean(d)+2.58*sd(d)) 
            drawDensityOne(data, percent, min_cut, max_cut) # Draw density
            if (abs(percent-0.01)<0.000001){
                drawCut001(data$data) # Draw 2.58 sigma cut, Tukey's fence 1.5*IQR beyond Q1/Q3, 1% cut
            }else if (abs(percent-0.05)<0.000001){
                drawCut005(data$data) # Draw 2.0  sigma cut, Tukey's fence 1.0*IQR beyond Q1/Q3, 5% cut
            }
        }else{
            min_cut <- quantile(d, 0.001)
            max_cut <- quantile(d, 0.999)
            drawDensityOne(data, percent, min_cut, max_cut) # Draw density
        }
        if(legend){
            if (abs(percent-0.05)<0.000001){
                legend("topright", cex=2.5, lty=2, lwd=4, col=c("magenta","white","orange","white","darkgray","white"),
                       c(expression(paste("L:",mu,"-2.0",sigma)), expression(paste("R:",mu,"+2.0",sigma)),
                         expression(paste("L:","Q"[1],"-1.0IQR")), expression(paste("R:","Q"[3],"+1.0IQR")),
                         "L:lower 2.5%", "R:upper 2.5%"))
            }else if (abs(percent-0.01)<0.000001){
                legend("topright", cex=2.5, lty=2, lwd=4, col=c("magenta","white","orange","white","darkgray","white"),
                       c(expression(paste("L:",mu,"-2.58",sigma)), expression(paste("R:",mu,"+2.58",sigma)),
                         expression(paste("L:","Q"[1],"-1.5IQR")), expression(paste("R:","Q"[3],"+1.5IQR")),
                         "L:lower 0.5%", "R:upper 0.5%"))
            }
        }

        if(horizon){ 
            threshold <- quantile(data$density, percent)
            abline(h=threshold, lty=3)
        }
        dev.off()
    }
}

filename_item_list <- "items.list" # File recording each variable name in a row, which is also the data file name in data folder
data_folder <- "../Data/Outlier_hopt/" # Data folder
image_folder <- "../Images/" # Image folder
percent <- 0.01
drawDensity(filename_item_list, data_folder, image_folder, percent, compare=FALSE, horizon=FALSE, legend=FALSE)
