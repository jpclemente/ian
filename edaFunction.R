eda <- function(house_df){
  # Discard duplicates.
  duplicated_ids <- house_df[which(duplicated(house_df$id)),]
  house_df_order <-  house_training[order(house_training$date, decreasing = TRUE),]
  house_df <- house_df_order[!duplicated(house_df_order$id), ]
  
  # Set NAs.
  house_df$bathrooms <- house_df$bathrooms%>% dplyr::na_if(0)
  house_df$bedrooms <- house_df$bedrooms %>% dplyr::na_if(33)
  
  # Impute values
  house_df <- kNN(house_df, variable = c("bedrooms"), dist_var = c("lat", "long"), k = 5)
}