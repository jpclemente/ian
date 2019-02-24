library(readr)
library(caret)
library(dplyr)
library(Hmisc)
library(ggplot2)

kc_house_data <- read_csv("data/kc_house_data.csv")
inTraining <- createDataPartition(pull(kc_house_data), p = .7, list = FALSE,times = 1)

house_training <- slice(kc_house_data, inTraining)
house_testing <- slice(kc_house_data, -inTraining)

describe(house_training)
Hmisc::describe(house_training)

# EDA
# Primer vistazo.
ids_duplicados <- house_training[which(duplicated(house_training$id)),]
View(ids_duplicados)

# Eva
# ID: hay 80 repetidos.
# price: posible re-escalado.
# bathrooms: OK. Los americanos son raros
# waterfront: OK
# sqft_lot: OK
# condition: OK
# sqft_above: Alta correlacion con sqft_living???
# yr_built: (mirar con cariño)
# zipcode: OK
# long: ok

# Javi
# date: 
# bedrooms: posible error en 33. Dato muy atípico.
#     imputar como la media
# floors: OK. Los americanos son raros
# sqft_living: OK
# view: OK
# grade: posibilidad de reducir a 3 valores.
# sqft_basement: 0 son no aplica, pues no tiene sentido.
# yr_renovated: muchos ceros que son not available.
# lat: ok
# sqft_living15: ok

describe(house_training$yr_built)

table(house_training$zipcode)

house_training %>% summarise(total.count=n())
