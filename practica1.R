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

describe(house_training)

# EDA
# Primer vistazo.

#Ver los factores de cada variable
str(house_training)

#ver datos faltantes. No falta ninguno 
aggr_plot <- aggr(house_training[1:10], col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
                  labels=names(house_testing[1:10]), cex.axis=.7, gap=3, 
                  ylab=c("Histogram of missing data","Pattern"))
aggr_plot <- aggr(house_training[11:21], col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
                  labels=names(house_testing[11:21]), cex.axis=.7, gap=3, 
                  ylab=c("Histogram of missing data","Pattern"))

# ID:

#Vemos que hay varios ids repetidos
describe(house_training$id)
ids_duplicados <- house_training[which(duplicated(house_training$id)),]
View(ids_duplicados)

#Primero ordenamos descendenetemente todas las variables según la fecha para quedarnos con los más actuales (house_training_order)
#y eliminar los duplicados con una fecha inferior (house_training_WO)
house_training_order <-  house_training[order(house_training$date, decreasing = TRUE),] 
house_training_WO <- house_training_order[!duplicated(house_training_order$id), ] 
View(house_training_WO)

# - date: 

# price: posible re-escalado.
table(house_training_WO$price)
describe(house_training_WO$price)
ggplot(data = house_training_WO) + geom_density(aes(price))
ggplot(data = house_training_WO) + geom_density(aes(log10(price)))

# - bedrooms: posible error en 33. Dato muy atípico.
#     imputar como la media

# bathrooms: OK. Los americanos son raros
#se han convertido todos los baños que están a 0 en N/A. No se redondea porque según los decimales indican 
#si es baño, aseo, etc.
describe(house_training_WO$bathrooms)
table(house_training_WO$bathrooms)
house_training_WO$bathrooms[house_training_WO$bathrooms=='0'] <- NA
house_training_WO$bathrooms <- factor(house_training_WO$bathrooms)
ggplot(data = house_training_WO) + geom_density(aes(bathrooms))
#?? que hacer aqui?
View(house_training_WO)

# - floors: OK. Los americanos son raros

# waterfront: OK. ¿que hacer con este?
describe(house_training_WO$waterfront)
table(house_training_WO$waterfront)
hist(house_training_WO$waterfront,col="lightcyan")
house_training_WO$waterfront = factor(house_training_WO$waterfront)
levels(house_training_WO$waterfront)
# - sqft_living: OK

# sqft_lot: OK.
describe(house_training_WO$sqft_lot)
table(house_training_WO$sqft_lot)
ggplot(data = house_training_WO) + geom_density(aes(sqft_lot))
ggplot(data = house_training_WO) + geom_density(aes(log(sqft_lot)))

# - view: OK

# condition: OK
describe(house_training_WO$condition)
table(house_training_WO$condition)
ggplot(data = house_training_WO) + geom_density(aes(condition))
#?? AQUI LINEAL?

# - grade: posibilidad de reducir a 3 valores.

# sqft_above: Alta correlacion con sqft_living?? Esta se puede poner con log o sin el
describe(house_training_WO$sqft_above)
table(house_training_WO$sqft_above)
ggplot(data = house_training_WO) + geom_density(aes(sqft_above))

# - sqft_basement: 0 son no aplica, pues no tiene sentido.

# yr_built: (mirar con cariño). No entiendo qué hay de raro
describe(house_training_WO$yr_built)
table(house_training_WO$yr_built)
ggplot(data = house_training_WO) + geom_density(aes(yr_built))

Old <- house_training_WO[which(house_training_WO$yr_built < 1930),] 
OldAndNoRenovated <- Old[which(Old$yr_renovated == 0),]
View(OldAndNoRenovated)

SameSqrt <- OldAndNoRenovated[OldAndNoRenovated$sqft_living == OldAndNoRenovated$sqft_living15,]
View(SameSqrt)

ggplot(data = SameSqrt) + geom_density(aes(lat))
ggplot(data = SameSqrt) + geom_density(aes(long))


# - yr_renovated: muchos ceros que son not available.

# zipcode: OK
describe(house_training_WO$zipcode)
table(house_training_WO$zipcode)
ggplot(data = house_training_WO) + geom_density(aes(zipcode))

# - lat: ok

# long: ok
describe(house_training_WO$long)
table(house_training_WO$long)
ggplot(data = house_training_WO) + geom_density(aes(long))

# - sqft_living15: ok
describe(house_training_WO$sqft_living15)
table(house_training_WO$sqft_living15)
ggplot(data = house_training_WO) + geom_density(aes(sqft_living15))



describe(house_training$yr_built)

table(house_training$zipcode)

house_training %>% summarise(total.count=n())
