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


### Dividir por coleccion 

colections <- unique(data[, c('colection', 'original')])
values_colection <- table(data$colection)
values <- data.frame(colection = names(values_colection), values = as.vector(values_colection))

colections_m <- merge(colections, values, by = 'colection', all.x = TRUE)
colections <- colections_m[order(colections_m$values, decreasing = TRUE), ]
colections$color_labels <- ifelse(colections$original, '#0F7CE9', '#1F9B00')

# Gr?fico de barras horizontales
par(mar = c(9.5, 2.5, 4, 4))
barplot(colections$values, names.arg = colections$colection, col = colections$color_labels, border = "white", las = 1, main = "Colecciones LEGO", cex.names = 0.57,las=2)
legend("topright", legend = c("Originales", "Licenciados"), fill = c('#0F7CE9', '#1F9B00'),cex=0.6,box.lty=0)


# Gr?fico de pastel para Originales y Licenciados
originals <- table(data$original)
total <- sum(originals)

par(mar = c(1, 2.5, 4, 4))
pie(originals,labels = c('Originales', 'Licenciados'), col = c('#0F7CE9', '#1F9B00'), main = "Originales vs Licenciados", cex = 0.8)


# Gr?fico de pastel para Ni?os y Adultos
for_adults <- table(data$adult)
total <- sum(for_adults)
pie(for_adults, labels = c('Ni?os', 'Adultos'), col = c('#0F7CE9', '#1F9B00'), main = "Para Ni?os vs Para Adultos", cex = 0.8)
legend("bottomright", legend = c('Ni?os', 'Adultos'), fill = c('#0F7CE9', '#1F9B00'))


#### Histogramas

#Histograma de precios
par(mar = c(4, 4, 2, 1))
hist(data$price,col='darkred', xlab = 'Precio', main='Histograma de Precios')

#Histograma de Edad
par(mar = c(4, 4, 2, 1))
hist(data$age,col='darkblue', xlab = 'Edad', main='Histograma de Edad')

#Histograma de Calificaci?n
par(mar = c(4, 4, 2, 1))
hist(data$calification,col='darkblue', xlab = 'Calificaci?n', main='Histograma de Calificaci?n')





# Matriz de correlaci?n y scatter plot

data["original"][data["original"] == 'True'] <- 1
data["original"][data["original"] == 'False'] <- 0

features <- c('price', 'pieces', 'age', 'discount', 'calification','original')
corr_matrix <- cor(sapply(data[features],as.numeric),use="complete.obs")
corrplot(corr_matrix, main = "Correlaciones", mar=c(0,0,1,0),addCoef.col = "black", method="color",col=colorRampPalette(c("white","lightblue","red"))(100))



#  Grafico de dispersi?n Prescio vs Piesas
data["original"][data["original"] == 1] <- 'True'
data["original"][data["original"] == 0] <- 'False'


color <- ifelse(data$original, '#0F7CE9', '#1F9B00')

par(mar = c(2.5, 4, 2, 1))
plot(data$pieces, data$price, pch = 16, col = color, xlab = "Pieces", ylab = "Price", main = "Scatter Plot: Pieces vs Price", grid = TRUE)
legend("bottomright", legend = c("Originales", "Licenciados"), fill = c('#0F7CE9', '#1F9B00'),cex=0.6,box.lty=0)
abline(coef = c(0, 1),col = "red",lwd = 2)


par(mar = c(2.5, 4, 2, 1))
plot(data$age, data$price, pch = 16, col = color, xlab = "Pieces", ylab = "Price", main = "Scatter Plot: Pieces vs Price", grid = TRUE)
legend("bottomright", legend = c("Originales", "Licenciados"), fill = c('#0F7CE9', '#1F9B00'),cex=0.6,box.lty=0)


par(mar = c(2.5, 4, 2, 1))
plot(data$calification, data$price, pch = 16, col = color, xlab = "Pieces", ylab = "Price", main = "Scatter Plot: Pieces vs Price", grid = TRUE)
legend("bottomright", legend = c("Originales", "Licenciados"), fill = c('#0F7CE9', '#1F9B00'),cex=0.6,box.lty=0)



