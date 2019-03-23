eda <- function(house_df){
  
  # Discard duplicates.
  duplicated_ids <- house_df[which(duplicated(house_df$id)),]
  house_df_order <- house_training[order(house_training$date, decreasing = TRUE),]
  house_df <- house_df_order[!duplicated(house_df_order$id), ]
  
  # drop id
  house_df <- house_df %>% select(-c(id))

  # Set NAs.
  house_df$bathrooms <- house_df$bathrooms %>% dplyr::na_if(0)
  house_df$bedrooms <- house_df$bedrooms %>% dplyr::na_if(33)
  house_df$yr_renovated <- house_df$yr_renovated %>% dplyr::na_if(0)
  house_df$sqft_basement <- house_df$sqft_basement %>% dplyr::na_if(0)
  
  # Impute values
  house_df <- kNN(house_df, variable = c("bedrooms"), dist_var = c("lat", "long"), k = 5)
  
  # transformations
  house_df <- mutate(house_df, price = log(price))
  house_df <- mutate(house_df, sqft_living = log(sqft_living))
  house_df <- mutate(house_df, sqft_lot = log(sqft_lot))
  house_df <- mutate(house_df, sqft_lot15 = log(sqft_lot15))
  
  # factores
  # house_training_WO$bathrooms <- factor(house_training_WO$bathrooms)
  
  return(house_df)
}
