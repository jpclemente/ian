transform <- function(house_df){
  
  # Discard duplicates.
  duplicated_ids <- house_df[which(duplicated(house_df$id)),]
  house_df_order <- house_df[order(house_df$date, decreasing = TRUE),]
  house_df <- house_df_order[!duplicated(house_df_order$id), ]
  
  # Set NAs.
  house_df$bathrooms <- house_df$bathrooms %>% dplyr::na_if(0)
  house_df$bedrooms <- house_df$bedrooms %>% dplyr::na_if(33)
  
  # Binary variables
  house_df <- house_df %>% mutate(has_basement = if_else(sqft_basement == 0, 0,1))
  house_df <- house_df %>% mutate(renovated = if_else(yr_renovated == 0, 0,1))
  
  # Impute values
  house_df <- kNN(house_df, variable = c("bedrooms"), dist_var = c("sqft_living", "floors", "has_basement"), k = 5, imp_var = FALSE)
  #house_df <- kNN(house_df, variable = c("bathrooms"), dist_var = c("sqft_living", "floors"), k = 5, imp_var = FALSE)
  
  # logarithmic transformations
  house_df <- mutate(house_df, price = log(price))
  house_df <- mutate(house_df, sqft_living = log(sqft_living))
  house_df <- mutate(house_df, sqft_lot = log(sqft_lot))
  house_df <- mutate(house_df, sqft_lot15 = log(sqft_lot15))
  
  #dummies
  house_df$grade_range <- ifelse(house_df$grade %in% c(1,2,3), 1, 
                                ifelse(house_df$grade %in% c(4,5,6), 2,
                                ifelse(house_df$grade %in% c(7,8,9), 3, 4)))  
  
  grade <- factor(house_df$grade_range)
  house_df <- house_df %>% cbind(model.matrix(~grade)[,-1])

  # Impute values
  house_df <- kNN(house_df, variable = c("bedrooms"), dist_var = c("sqft_living", "floors", "has_basement", "grade2", "grade3", "grade4"), k = 5, imp_var = FALSE)
  #house_df <- kNN(house_df, variable = c("bathrooms"), dist_var = c("sqft_living", "floors"), k = 5, imp_var = FALSE)
  
  # drop unnecessary variables
  house_df <- house_df %>% select(-c(id, sqft_basement, yr_renovated, grade, grade_range))
  
  return(house_df)
}