# Instalar y cargar bibliotecas
install.packages("ggplot2")
install.packages("plotrix")
install.packages("corrplot")

library(ggplot2)
library(plotrix)
library(corrplot)


# Cargar datos
data <- read.csv("C:/Users/Daniel/Downloads/lego_data_clean.csv",encoding = "UTF-8")

### Estadisticas

summary(data)
##--- Lego cuenta con más de 1,200 sets.
##--- La media de precios de los sets es de 1,748 pesos mexicanos
##    y el set más caro es de 19,999 pesos.
##--- El lego con menos piezas tiene 1 y es un llavero, el que
##    más tiene es con 11,695 piezas
##--- 


### Dividir por colección 

colections <- unique(data[, c('colection', 'original')])
values_colection <- table(data$colection)
values <- data.frame(colection = names(values_colection), values = as.vector(values_colection))

colections_m <- merge(colections, values, by = 'colection', all.x = TRUE)
colections <- colections_m[order(colections_m$values, decreasing = TRUE), ]
colections$color_labels <- ifelse(colections$original, '#0F7CE9', '#1F9B00')

# Gr?fico de barras de colecciones
par(mar = c(9.5, 2.5, 4, 4))
barplot(colections$values, names.arg = colections$colection, col = colections$color_labels, border = "white", las = 1, main = "Colecciones LEGO", cex.names = 0.57,las=2)
legend("topright", legend = c("Originales", "Licenciados"), fill = c('#0F7CE9', '#1F9B00'),cex=0.6,box.lty=0)
  #------ Notemos que la colecci?n de Lego con mayor cantidad de sets es LEGO CITY y es original de LEGO.

# Gr?fico de pastel para Originales y Licenciados
originals <- table(data$original)
total <- sum(originals)

par(mar = c(1, 2.5, 4, 4))
pie(originals,labels = c('Licenciados', 'Originales'), col = c('#0F7CE9', '#1F9B00'), main = "Originales vs Licenciados", cex = 0.8)
 #------ La mayoria de los sets de LEGO son Licenciados, en especifico un 59% aproximadamente.

# Gr?fico de pastel para Ni?os y Adultos
for_adults <- table(data$adult)
total <- sum(for_adults)
pie(for_adults, labels = c('Ni?os', 'Adultos'), col = c('#0F7CE9', '#1F9B00'), main = "Para Ni?os vs Para Adultos", cex = 0.8)
legend("bottomright", legend = c('Ni?os', 'Adultos'), fill = c('#0F7CE9', '#1F9B00'))
 #------ M?s del 60% de los sets de Lego son de ni?os.

#### Histogramas

#Histograma de precios
par(mar = c(4, 4, 2, 1))
hist(data$price,col='darkred', xlab = 'Precio', main='Histograma de Precios')
#------ Se puede notar una distibucion exponencial o gamma. se acumulan los precios menor a 2000 pesos

#Histograma de Edad
par(mar = c(4, 4, 2, 1))
hist(data$age,col='darkblue', xlab = 'Edad', main='Histograma de Edad')
#------ Las edades se consentran entre los 5 y 10 a?os

#Histograma de Calificaci?n
par(mar = c(4, 4, 2, 1))
hist(data$calification,col='darkblue', xlab = 'Calificaci?n', main='Histograma de Calificaci?n')
#------ la calificacion se distibuye con una consentracion en calificaciones altas.


################# HIPOTESIS
####---- El precio de los sets de LEGO depender? de las piezas que trae y de la edad minima del LEGO.


# Matriz de correlaci?n y scatter plot

data["original"][data["original"] == 'True'] <- 1
data["original"][data["original"] == 'False'] <- 0

features <- c('price', 'pieces', 'age', 'discount', 'calification','original')
corr_matrix <- cor(sapply(data[features],as.numeric),use="complete.obs")
corrplot(corr_matrix, main = "Correlaciones", mar=c(0,0,1,0),addCoef.col = "black", method="color",col=colorRampPalette(c("white","lightblue","red"))(100))
#------ Notemos una correlaci?n alta entre Precio y n?mero de piezas de LEGO, as? como tambi?n la edad


#  Grafico de dispersi?n Prescio vs Piesas
data["original"][data["original"] == 1] <- 'True'
data["original"][data["original"] == 0] <- 'False'

color <- ifelse(data$original, '#0F7CE9', '#1F9B00')

par(mar = c(2.5, 4, 2, 1))
plot(data$pieces, data$price, pch = 16, col = color, xlab = "Pieces", ylab = "Price", main = "Scatter Plot: Pieces vs Price", grid = TRUE)
legend("bottomright", legend = c("Originales", "Licenciados"), fill = c('#0F7CE9', '#1F9B00'),cex=0.6,box.lty=0)
abline(coef = c(0, 1),col = "red",lwd = 2)
#------ notemos que a mayor n?mero de piezas mayor precio cas?una tendencia lineal o logaritmica.

par(mar = c(2.5, 4, 2, 1))
plot(data$age, data$price, pch = 16, col = color, xlab = "Edad", ylab = "Price", main = "Scatter Plot: Edad vs Price", grid = TRUE)
legend("bottomright", legend = c("Originales", "Licenciados"), fill = c('#0F7CE9', '#1F9B00'),cex=0.6,box.lty=0)
#------ no se ve una tendencia tan martacada ya que la edad es una variable discreta, pero los precios m?s altos corresponden a edades mayores

par(mar = c(2.5, 4, 2, 1))
plot(data$calification, data$price, pch = 16, col = color, xlab = "Cali.", ylab = "Price", main = "Scatter Plot: Calificacion vs Price", grid = TRUE)
legend("bottomright", legend = c("Originales", "Licenciados"), fill = c('#0F7CE9', '#1F9B00'),cex=0.6,box.lty=0)
#------ Notemos que la calificac?n no influye mucho en el precio, la mayoria de Legos se concentran en calificaciones altas



