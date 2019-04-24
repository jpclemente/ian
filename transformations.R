transform <- function(house_df){
  
  # Discard duplicates.
  duplicated_ids <- house_df[which(duplicated(house_df$id)),]
  house_df_order <- house_df[order(house_df$date, decreasing = TRUE),]
  house_df <- house_df_order[!duplicated(house_df_order$id), ]

  # Set NAs.
  house_df$bathrooms <- house_df$bathrooms %>% dplyr::na_if(0)
  house_df$bedrooms <- house_df$bedrooms %>% dplyr::na_if(33)
  # house_df$yr_renovated <- house_df$yr_renovated %>% dplyr::na_if(0)
  # house_df$sqft_basement <- house_df$sqft_basement %>% dplyr::na_if(0)
  
  # Impute values
  house_df <- kNN(house_df, variable = c("bedrooms"), dist_var = c("lat", "long"), k = 5, imp_var = FALSE)
  
  # transformations
  house_df <- mutate(house_df, price = log(price))
  house_df <- mutate(house_df, sqft_living = log(sqft_living))
  house_df <- mutate(house_df, sqft_lot = log(sqft_lot))
  house_df <- mutate(house_df, sqft_lot15 = log(sqft_lot15))
  
  # Binary
  house_df <- house_df %>% mutate(has_basement = if_else(sqft_basement == 0, 0,1))
  house_df <- house_df %>% mutate(renovated = if_else(yr_renovated == 0, 0,1))
  
  # drop unnecessary variables
  house_df <- house_df %>% select(-c(id, sqft_basement, yr_renovated))
  
  # factores
  # house_training_WO$bathrooms <- factor(house_training_WO$bathrooms)
  
  return(house_df)
}
