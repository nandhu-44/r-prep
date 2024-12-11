# Load the required ggplot2 package
library(ggplot2)

# Load the mtcars dataset
data(mtcars)

# Create the scatterplot with color based on cyl and add a smooth trend line
ggplot(mtcars, aes(x = disp, y = mpg, color = factor(cyl))) + 
  geom_point(size = 3) +  # Scatter plot with points
  geom_smooth(method = "lm", se = FALSE, aes(color = factor(cyl))) +  # Add smooth line (linear model)
  labs(x = "Displacement (disp)", 
       y = "Miles per Gallon (mpg)", 
       title = "Scatterplot of mpg vs disp with Trend Line by cyl",
       color = "Number of Cylinders") +  # Labels and title
  theme_minimal() +  # Minimal theme
  theme(legend.position = "top")  # Position the legend at the top
