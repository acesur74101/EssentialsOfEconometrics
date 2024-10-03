# Installing packages if not already installed
install.packages("tseries")
install.packages("forecast")
install.packages("ggplot2")


install.packages("tidyverse")
# Loading the libraries
library(tseries)
library(forecast)
library(ggplot2)



# Generate 4 basic random series
set.seed(123)
series1 <- rnorm(100, mean = 50, sd = 10)  # Normal distribution
series2 <- runif(100, min = 20, max = 40)  # Uniform distribution
series3 <- rpois(100, lambda = 5)          # Poisson distribution
series4 <- rexp(100, rate = 0.1)           # Exponential distribution

# Creating a data frame
df_basic <- data.frame(series1, series2, series3, series4)


# Generating 4 time series
ts1 <- ts(rnorm(100, mean = 100, sd = 15), start = c(2020, 1), frequency = 12) # Monthly data
ts2 <- ts(runif(100, min = 50, max = 150), start = c(2020, 1), frequency = 12)
ts3 <- ts(rpois(100, lambda = 10), start = c(2020, 1), frequency = 12)
ts4 <- ts(rexp(100, rate = 0.2), start = c(2020, 1), frequency = 12)

# Creating a data frame
df_ts <- data.frame(ts1, ts2, ts3, ts4)


# Basic series
plot(df_basic$series1, type="l", main="Basic Series 1")
plot(df_basic$series2, type="l", main="Basic Series 2")
plot(df_basic$series3, type="l", main="Basic Series 3")
plot(df_basic$series4, type="l", main="Basic Series 4")

# Time series
plot(ts1, main="Time Series 1")
plot(ts2, main="Time Series 2")
plot(ts3, main="Time Series 3")
plot(ts4, main="Time Series 4")


# Linear regression on basic series
lm_basic <- lm(series1 ~ series2 + series3 + series4, data = df_basic)
summary(lm_basic)

# Linear regression on time series
lm_ts <- lm(ts1 ~ ts2 + ts3 + ts4, data = df_ts)
summary(lm_ts)



# Extract residuals
residuals <- residuals(lm_basic)
# Perform Jacque-Bera test

jb_test <- jarque.bera.test(residuals)

# Display the results of the JB test
jb_test

# Plot histogram of residuals
hist(residuals, breaks = 20, main = "Histogram of Residuals", xlab = "Residuals", col = "lightblue")


# Apply the ADF test to each time series
adf_ts1 <- adf.test(ts1)
adf_ts2 <- adf.test(ts2)
adf_ts3 <- adf.test(ts3)
adf_ts4 <- adf.test(ts4)

# Display results
adf_ts1
adf_ts2
adf_ts3
adf_ts4


# Generating x and y for 100 observations
set.seed(123)
x <- rnorm(100, mean = 50, sd = 10)  # Normal distribution
y <- x * 0.5 + rnorm(100, mean = 10, sd = 5)  # Linear relation with noise

# 1. Scatter Plot
plot(x, y, main = "Scatter Plot", xlab = "X", ylab = "Y", col = "blue", pch = 19)

# 2. Line Plot
plot(x, type = "l", main = "Line Plot for X", xlab = "Index", ylab = "X", col = "red")

# 3. Bar Plot
barplot(x[1:10], main = "Bar Plot of First 10 X", col = "lightblue", names.arg = 1:10)

# 4. Histogram
hist(x, main = "Histogram of X", xlab = "X", col = "lightgreen", breaks = 10)

# 5. Box Plot
boxplot(x, y, main = "Box Plot of X and Y", names = c("X", "Y"), col = c("lightblue", "lightpink"))

# 6. Density Plot
plot(density(x), main = "Density Plot of X", xlab = "X", col = "darkgreen", lwd = 2)

# 7. Pie Chart
pie(abs(x[1:5]), main = "Pie Chart of First 5 X", col = rainbow(5), labels = round(abs(x[1:5])))

# 8. Correlogram
library(corrplot)
cor_matrix <- cor(data.frame(x, y))
corrplot(cor_matrix, method = "circle", main = "Correlogram of X and Y")

# 9. Heatmap
heatmap(cor_matrix, main = "Heatmap of Correlation Matrix")

# 10. Time Series Plot
ts_x <- ts(x, start = c(2021, 1), frequency = 12)
plot(ts_x, main = "Time Series Plot of X", col = "purple")

# 11. Violin Plot
library(ggplot2)
df <- data.frame(x, y)
ggplot(df, aes(x = factor(0), y = x)) + geom_violin(fill = "lightblue") + labs(title = "Violin Plot of X")

# 12. Facet Grid
ggplot(df, aes(x = x, y = y)) + geom_point() + facet_grid(. ~ cut(y, 3)) + ggtitle("Facet Grid of X and Y")

# 13. Pairs Plot (Scatterplot Matrix)
pairs(data.frame(x, y), main = "Pairs Plot")

# 14. 3D Plot (interactive)
install.packages("plotly")
library(plotly)
plot_ly(x = ~x, y = ~y, z = ~rnorm(100), type = "scatter3d", mode = "markers")

# 15. Bubble Chart
symbols(x, y, circles = abs(x - mean(x)), inches = 0.3, bg = "lightblue", fg = "blue", main = "Bubble Chart of X and Y")

# 16. Geospatial Plot (Dummy Example for New York)
install.packages("ggmap")
library(ggmap)
register_google(key = "AIzaSyAUl1uTzQuAQlgW7k79jRtJYUaMP7aW9FA")
qmap("New York", zoom = 12)

# 17. Gantt Chart (Simplified)
ggplot(df, aes(x = 1:100, y = y)) + geom_segment(aes(xend = 1:100, yend = 0), col = "blue") + ggtitle("Gantt Chart")

# 18. Network Plot (Dummy Example)
install.packages("igraph")
library(igraph)
g <- erdos.renyi.game(10, 0.4)
plot(g, main = "Network Plot", vertex.color = "lightblue", edge.color = "gray")

# 19. Area Chart
plot(x, type = "n", main = "Area Chart of X", xlab = "Index", ylab = "X")
polygon(c(1:100, rev(1:100)), c(x, rep(0, 100)), col = "lightblue", border = NA)

# 20. Interactive Plot with plotly
plot_ly(x = ~x, y = ~y, type = 'scatter', mode = 'markers')



# Step 1: Install required packages
# install.packages("sf")
# install.packages("ggplot2")

# Step 2: Load libraries
library(sf)
library(ggplot2)

# Step 3: Load the shapefile
shapefile_path <- "/Users/abdulkadircesuroglu/Downloads/ne_10m_admin_2_counties/tr_10km.shp"  # Specify the path to your shapefile
shape_data <- st_read(shapefile_path)

# Step 4: Visualize the shapefile
ggplot(data = shape_data) +
  geom_sf(fill = "lightblue", color = "black") +
  ggtitle("Shapefile Map") +
  theme_minimal()

