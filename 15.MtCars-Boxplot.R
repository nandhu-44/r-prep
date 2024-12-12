# data(mtcars)

# mtcars$gear <- as.factor(mtcars$gear)

# boxplot_hp <- boxplot(mtcars$hp ~ mtcars$gear,
#                       main = "Box Plot of Horsepower by Number of Gears",
#                       xlab = "Number of Gears",
#                       ylab = "Horsepower (hp)",
#                       col = "lightblue",
#                       pch = 19)

# legend("topright", legend = levels(mtcars$gear), pch = 19, title = "Number of Gears")

# outliers <- boxplot_hp$out
# outlier_positions <- which(mtcars$hp %in% outliers)
# text(rep(1:3, length.out = length(outliers)), outliers, 
#      labels = rownames(mtcars)[outlier_positions], pos = 4, col = "red")

data(mtcars)
mtcars$gear <- as.factor(mtcars$gear)

boxplot_hp <- boxplot(mtcars$hp ~ mtcars$gear,
                      main = "Box Plot of Horsepower by Number of Gears",
                      xlab = "Number of Gears",
                      ylab = "Horsepower (hp)",
                      col = "lightblue",
                      pch = 19)

legend("topright", legend = levels(mtcars$gear), pch = 19, title = "Number of Gears")

outliers <- boxplot_hp$out

# Check if there are any outliers before proceeding
if (length(outliers) > 0) {
  #Improved outlier labeling: get the correct x positions and labels
  outlier_groups <- as.numeric(gsub("mtcars\\$gear=", "", names(outliers)))
  text(outlier_groups, outliers, labels = outliers, pos = 3, col = "red")
} else {
  print("No outliers found.")
}
