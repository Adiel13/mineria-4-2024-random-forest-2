install.packages("arules")
library(arules)
install.packages("genero")
library(genero)
install.packages("randomForest")
library(randomForest)

data <- read.csv('C:/Users/kevin/OneDrive/Documentos/data.csv', sep = ";", fileEncoding = "latin1")

data$cui <- format(data$cui, scientific =FALSE)

data$nota[data$nota == "SDE"] <- -1
data$final[data$final == "SDE"] <- -1
data$final[data$final == "NSP"] <- -1

data$nombre1 <- sapply(strsplit(data$nombre, " "), `[`, 1)
data$nombre2 <- sapply(strsplit(data$nombre, " "), `[`, 2)

genero("LUIS")
genero("EMILY")

data$genero <- genero(data$nombre1)

subset(data, is.na(data$genero))

data$genero <- ifelse(is.na(data$genero), genero(data$nombre2), data$genero)

data[77, "genero"] <- "male"
data[113, "genero"] <- "male"
data[119, "genero"] <- "female"
data[120, "genero"] <- "male"
data[179, "genero"] <- "female"
data[185, "genero"] <- "male"
data[202, "genero"] <- "male"
data[225, "genero"] <- "male"
data[250, "genero"] <- "male"
data[276, "genero"] <- "female"
data[363, "genero"] <- "female"
data[473, "genero"] <- "female"
data[487, "genero"] <- "male"
data[566, "genero"] <- "male"

data$genero <- ifelse(data$genero == "male", 1, 2)

data$anio_carne <- substr(data$carne, start=1, stop=4)

subset(data, anio_carne > 8000)

data$anio_carne <- ifelse(data$anio_carne > 8000, as.numeric(substr(data$anio_carne, 1,2))+1900, data$anio_carne)

data$edad <- as.integer(data$anio) - as.integer(data$anio_carne) +18

data$municipio <- substr(data$cui, nchar(data$cui) -1, nchar(data$cui))
data$departamento <- substr(data$cui, nchar(data$cui) -3, nchar(data$cui)-2)

data_apriori <- data[, c("lab", "zona", "final", "nota", "anio", "sem", "genero", "edad", "municipio", "departamento")]


data_apriori$gana <- ifelse(data_apriori$nota > 60, 1, 0)

data_apriori$gana <- as.factor(data_apriori$gana)

set.seed(100)
data_apriori <- data_apriori[sample(1:nrow(data_apriori)),]

index <-sample(1:nrow(data_apriori), 0.8*nrow(data_apriori))

train <- data_apriori[index,]
test <- data_apriori[-index,]

bosque <- randomForest(gana ~ genero + edad + sem,
                       data = train,
                       ntree = 100,
                       mtry = 2
)

pruebas <- predict(bosque, test)

pruebas

matriz <- table(test$gana, pruebas)

matriz

pre <- sum(diag(matriz)) / sum(matriz)
pre

plot(bosque)

data_nueva <- data.frame(
  genero=2,
  edad=19,
  sem = 2
)

prediccion <- predict(bosque, data_nueva)
prediccion
