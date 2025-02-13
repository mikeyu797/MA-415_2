# Create the data frame
city <- data.frame(
  porto = rnorm(100),
  aberdeen = rnorm(100),
  nairobi = c(rep(NA, 10), rnorm(90)),
  genoa = rnorm(100)
)

# Multiply porto and aberdeen columns
porto_aberdeen <- city$porto * city$aberdeen

# Define a function to multiply two columns
multiply_columns <- function(x, y) {
  return(x * y)
}

# Use the function to multiply the same columns
porto_aberdeen_func <- multiply_columns(x = city$porto, y = city$aberdeen)

# Check if both results are identical
identical(porto_aberdeen, porto_aberdeen_func)

# Scatterplot of Porto vs Aberdeen
plot(city$porto, city$aberdeen, 
     main = "Scatterplot of Porto vs Aberdeen", 
     xlab = "Porto", 
     ylab = "Aberdeen", 
     pch = 19,   # Solid circle points
     col = "blue")  # Color of the points

# Set layout for 2x2 grid
par(mfrow = c(2, 2))

# Porto Histogram with Normal Curve
hist(city$porto, 
     main = "Porto Histogram with Normal Curve", 
     xlab = "Porto Values", 
     col = "lightblue", 
     freq = FALSE)  # Density scale

# Add normal curve for Porto
porto_mean <- mean(city$porto, na.rm = TRUE)
porto_sd <- sd(city$porto, na.rm = TRUE)
normal_sample_porto <- rnorm(1000, mean = porto_mean, sd = porto_sd)
lines(density(normal_sample_porto), col = "blue", lwd = 2)

# Aberdeen Histogram with Normal Curve
hist(city$aberdeen, 
     main = "Aberdeen Histogram with Normal Curve", 
     xlab = "Aberdeen Values", 
     col = "lightgreen", 
     freq = FALSE)

# Add normal curve for Aberdeen
aberdeen_mean <- mean(city$aberdeen, na.rm = TRUE)
aberdeen_sd <- sd(city$aberdeen, na.rm = TRUE)
normal_sample_aberdeen <- rnorm(1000, mean = aberdeen_mean, sd = aberdeen_sd)
lines(density(normal_sample_aberdeen), col = "darkgreen", lwd = 2)

# Nairobi Histogram with Normal Curve
hist(city$nairobi, 
     main = "Nairobi Histogram with Normal Curve", 
     xlab = "Nairobi Values", 
     col = "lightcoral", 
     freq = FALSE, 
     na.rm = TRUE)

# Add normal curve for Nairobi
nairobi_mean <- mean(city$nairobi, na.rm = TRUE)
nairobi_sd <- sd(city$nairobi, na.rm = TRUE)
normal_sample_nairobi <- rnorm(1000, mean = nairobi_mean, sd = nairobi_sd)
lines(density(normal_sample_nairobi), col = "red", lwd = 2)

# Genoa Histogram with Normal Curve
hist(city$genoa, 
     main = "Genoa with Normal Curve", 
     xlab = "Genoa Values", 
     col = "lightpink", 
     freq = FALSE)

# Add normal curve for Genoa
genoa_mean <- mean(city$genoa, na.rm = TRUE)
genoa_sd <- sd(city$genoa, na.rm = TRUE)
normal_sample_genoa <- rnorm(1000, mean = genoa_mean, sd = genoa_sd)
lines(density(normal_sample_genoa), col = "purple", lwd = 2)

