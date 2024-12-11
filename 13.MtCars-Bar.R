# Load the required ggplot2 package
library(ggplot2)

# Load the mtcars dataset
data(mtcars)

# Create the bar plot for number of cylinders (cyl) with color based on transmission type (am)
ggplot(mtcars, aes(x = factor(cyl), fill = factor(am))) + 
  geom_bar() +  # Bar plot
  labs(x = "Number of Cylinders", 
       y = "Count", 
       title = "Bar Plot of Number of Cylinders with Transmission Type",
       fill = "Transmission Type (am)") +  # Labels and title
  theme_minimal() +  # Minimal theme
  theme(legend.position = "top")  # Position the legend at the top
