# Load the mtcars dataset
data(mtcars)

# Calculate the mean and standard deviation of miles per gallon (mpg)
average_mpg <- mean(mtcars$mpg)
std_dev_mpg <- sd(mtcars$mpg)

# Create a histogram of the mpg distribution with custom color gradient
hist(mtcars$mpg,
     breaks = 10,  
     col = colorRampPalette(c("skyblue", "navy"))(10),
     main = "Miles Per Gallon (MPG) Distribution",
     xlab = "MPG (Miles Per Gallon)",
     ylab = "Count")

# Add a vertical line at the mean mpg value
abline(v = average_mpg, col = "darkred", lwd = 2)

# Annotate the mean and standard deviation on the histogram
text(average_mpg, 4, paste("Mean:", round(average_mpg, 2)), col = "darkred", pos = 4)
text(average_mpg, 3, paste("SD:", round(std_dev_mpg, 2)), col = "darkred", pos = 4)
