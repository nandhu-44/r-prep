# Load the iris dataset
data(iris)

# Open a PNG device to save the plot with high resolution
png("sepal_petal_scatterplot.png", width = 1600, height = 1200, res = 300)

# Create the scatterplot of Sepal.Length vs. Petal.Length
plot(iris$Sepal.Length, iris$Petal.Length, 
     xlab = "Sepal Length", 
     ylab = "Petal Length", 
     main = "Scatterplot of Sepal Length vs. Petal Length", 
     col = "blue", 
     pch = 16)

# Close the PNG device
dev.off()

cat("Plot saved as 'sepal_petal_scatterplot.png' with high resolution.\n")
