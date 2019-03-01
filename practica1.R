library(readr)
library(caret)
library(dplyr)
library(Hmisc)
library(ggplot2)
library(VIM)

kc_house_data <- read_csv("data/kc_house_data.csv")
inTraining <- createDataPartition(pull(kc_house_data), p = .7, list = FALSE,times = 1)

house_training <- slice(kc_house_data, inTraining)
house_testing <- slice(kc_house_data, -inTraining)

# describe(house_training)

# EDA
# Primer vistazo.

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

#Ver los factores de cada variable
str(house_training)

# ID:

#Vemos que hay varios ids repetidos
describe(house_training$id)
ids_duplicados <- house_training[which(duplicated(house_training$id)),]
# View(ids_duplicados)

#Primero ordenamos descendenetemente todas las variables según la fecha para quedarnos con los más actuales (house_training_order)
#y eliminar los duplicados con una fecha inferior (house_training_WO)
house_training_order <-  house_training[order(house_training$date, decreasing = TRUE),] 
house_training_WO <- house_training_order[!duplicated(house_training_order$id), ] 
# View(house_training_WO)

# - date: 

# price: posible re-escalado.
describe(house_training_WO$price)
ggplot(data = house_training_WO) + geom_density(aes(price))
ggplot(data = house_training_WO) + geom_density(aes(log10(price)))

# - bedrooms: posible error en 33. Dato muy atípico.
#     imputar como la media

# bathrooms: OK. Los americanos son raros
#se han convertido todos los baños que están a 0 en N/A. No se redondea porque según los decimales indican 
#si es baño, aseo, etc.
house_training_WO$bathrooms <- house_training_WO$bathrooms%>% dplyr::na_if(0)
summary(house_training_WO$bathrooms)
# View(house_training_WO)

# - floors: OK. Los americanos son raros


# Javi
# date: aparentemente está todo bien

# bedrooms: posible error en 33. Dato muy atípico.

# imputam como la media de las casa mas cercanas.
house_training_WO <- house_training_order[!duplicated(house_training_order$id), ] 
house_training_WO$bedrooms <- house_training_WO$bedrooms %>% na_if(33)
table(house_training_WO$bedrooms)
summary(house_training_WO$bedrooms)
# kNN imputation de los 5 vecinos mas cercanos:
house1 <- kNN(house_training_WO, variable = c("bedrooms"), dist_var = c("lat", "long"), k = 5)
# se le establece el valor de 4

table(house1$bedrooms)
# floors: OK. Los americanos son raros

# sqft_living: OK

# view: OK

# grade: posibilidad de reducir a 3 valores.

# sqft_basement: 0 son no aplica, pues no tiene sentido.

# yr_renovated: muchos ceros que son not available.
table(house_training_WO$yr_renovated)
house_training_WO$yr_renovated <- house_training_WO$yr_renovated %>% dplyr::na_if(0)
table(house_training_WO$yr_renovated)
summary(house_training_WO$yr_renovated)
# lat: ok

# sqft_living15: ok

describe(house_training$yr_built)

table(house_training$zipcode)

house_training %>% summarise(total.count=n())
