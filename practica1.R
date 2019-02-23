library(readr)
library(caret)
library(dplyr)
library(Hmisc)

kc_house_data <- read_csv("data/kc_house_data.csv")
inTraining <- createDataPartition(pull(kc_house_data), p = .7, list = FALSE,times = 1)

house_training <- slice(kc_house_data, inTraining)
house_testing <- slice(kc_house_data, -inTraining)

describe(house_training)

# EDA

# Primer vistazo.

# ID: hay 80 repetidos.

# - date: 

# price: posible re-escalado.

# - bedrooms: posible error en 33. Dato muy atípico.

#     imputar como la media

# bathrooms: OK. Los americanos son raros

# - floors: OK. Los americanos son raros

# waterfront: OK

# - sqft_living: OK

# sqft_lot: OK

# - view: OK

# condition: OK

# - grade: posibilidad de reducir a 3 valores.

# sqft_above: Alta correlacion con sqft_living???

# - sqft_basement: 0 son no aplica, pues no tiene sentido.

# yr_built: (mirar con cariño)

# - yr_renovated: muchos ceros que son not available.

# zipcode: OK

# - lat: ok

# long: ok

# - sqft_living15: ok

describe(house_training$yr_built)

table(house_training$zipcode)

house_training %>% summarise(total.count=n())
