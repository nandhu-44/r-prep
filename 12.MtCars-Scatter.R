# Load the mtcars dataset
data(mtcars)

# Create a scatterplot of Displacement vs. MPG with points colored by cylinder count
plot(x = mtcars$disp, y = mtcars$mpg,
     xlab = "Engine Displacement (cu.in.)",
     ylab = "Fuel Efficiency (MPG)",
     main = "MPG vs Displacement with Cylinder Differentiation",
     col = as.factor(mtcars$cyl),
     pch = 16)

# Add a smoothed line to the scatterplot
lines(lowess(mtcars$disp, mtcars$mpg), col = "darkgray", lwd = 2)

# Add a legend in the top-right corner to indicate cylinder groups
legend("topright", legend = levels(as.factor(mtcars$cyl)),
       col = 1:3, pch = 16, title = "Cylinder Count")
