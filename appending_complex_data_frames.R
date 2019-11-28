# Remove rows 1 and 3 from a csv file  and delete last row (TRUE or FALSE)
clean_final_df_fc <- function(file, rm_last_row = TRUE){
  
  headers = read.csv(file, skip = 1, header = F, nrows = 1, as.is = T)
  df = read.csv(file, skip = 3, header = F)
  colnames(df)= headers
  
  if (rm_last_row == TRUE){
    df <- df[-nrow(df),]
  }
  
  return(df)
}

# Add the cleaned data frames (by the function clean_final_df_fc)
# and return a list
dfs_to_list_fc <- function(file_names){
  
  df_list <- lapply(file_names, clean_final_df_fc)
  names(df_list) <- gsub("*.csv","",file_names)
  
  return(df_list)
}

# Transpose the rows of data frames in a list
transpose_df_inList <- function(list_name){
  
  tlist_name <- lapply(list_name, function(df) {
    rownames(df) <- df[,1]
    var_name <-colnames(df)[1]
    df[,1] <- NULL
    tdf <- as.data.frame(t(df))
    tdf[,var_name] <- rownames(tdf)
    return(tdf)
  })
  
  return(tlist_name)
}


file_names = c("df1.csv","df2.csv")
dfs_in_list <- dfs_to_list_fc(file_names)
tdfs_in_list <- transpose_df_inList(dfs_in_list)
