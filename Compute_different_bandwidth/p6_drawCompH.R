item = "wwpdf_overall.WilsonBestimate"
df_h <- read.csv("h.list", header=F)
v_h <- as.character(df_h[,1])
v_h <- v_h[c(1,2,3,6,9,10)]

data_folder <- "./Data/Outlier_hopt/"
color = c("red","orange","blue","magenta","black","cyan")
png("h_diff.png", width = 600, height = 600)
par(mfrow=c(1,1),mar=c(5.1, 5.1, 4.1, 2.1))
for (i in 1:1){
    h <- v_h[i]
    print(h)
    filename <- paste0(item, "_outlier_", h)
    filepath <- paste0(data_folder, filename)
    data <- read.table(filepath, header=T, sep="\t")
    da <- data[["data"]]
    density <- data[["density"]]
    plot(da, density, type="l", xlim=c(0,200), ylim=c(0,0.04), col=color[i],
         xlab="Estimated B from Wilson Plot", ylab="Density Estimate", lwd=2,
         cex.axis=2, cex.lab=2)
}

for (i in 2:4){
    h <- v_h[i]
    print(h)
    filename <- paste0(item, "_outlier_", h)
    filepath <- paste0(data_folder, filename)
    data <- read.table(filepath, header=T, sep="\t")
    da <- data[["data"]]
    density <- data[["density"]]
    lines(da, density, type="l",col=color[i],lwd=2)
}

for (i in 5:6){
    h <- v_h[i]
    print(h)
    filename <- paste0(item, "_outlier_", h)
    filepath <- paste0(data_folder, filename)
    data <- read.table(filepath, header=T, sep="\t")
    da <- data[["data"]]
    density <- data[["density"]]
    lines(da, density, type="l",col=color[i],lwd=2, lty=3)
}

legend("topright", lty=c(1,1,1,1,3,3), lwd=2,col=color, legend=v_h, cex=2)
dev.off()
