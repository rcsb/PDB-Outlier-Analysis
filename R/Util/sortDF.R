## Sort data frame for each reading of the output 
## sort_by parameter takes input as either number or name in quotations, e.g. 1, or "coloumn name"
sortDF <- function(df, sort_by){  
    index_sorted <- order(df[[sort_by]])
    df_sorted <- df[index_sorted,]
    rownames(df_sorted) <- 1:nrow(df_sorted)
    df_sorted
}
