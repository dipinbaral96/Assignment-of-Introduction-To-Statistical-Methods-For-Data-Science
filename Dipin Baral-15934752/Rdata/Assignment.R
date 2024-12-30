#installing the packages
#install.packages("ggplot2")
#install.packages("GGally")
#install.packages("dplyr")
#install.packages("reshape2")
#install.packages("plotly")
#install.packages("randomForest")
#install.packages("e1071")
#install.packages("rsample")
#install.packages("dplyr")

# Load necessary libraries
library(ggplot2)
library(GGally)
library(dplyr)
library(reshape2)
library(plotly)
library(randomForest)
library(e1071)
library(tidyverse)
library(rsample)
library(dplyr)

# Load the data
data <- read.csv("./data/data.csv")

#to know the directory
getwd()

#to view data
View(data)

# View the structure of the data
str(data)
#to View the first 6 rows of the `data` data frame
head(data)
#task 1.1
#to display a comparison of the variables x1,x3,x4 and x5.
plot_ly(data) %>%
  add_trace(x = ~time, y = ~x1, type = "scatter", mode = "lines", name = "X1", line = list(color = 'blue')) %>%
  add_trace(x = ~time, y = ~x3, type = "scatter", mode = "lines", name = "X3", line = list(color = 'red')) %>%
  add_trace(x = ~time, y = ~x4, type = "scatter", mode = "lines", name = "X4", line = list(color = 'green')) %>%
  add_trace(x = ~time, y = ~x5, type = "scatter", mode = "lines", name = "X5", line = list(color = 'orange')) %>%
  layout(
    plot_bgcolor = '#e5ecf6', 
    title = "Time series comparison of X with time", 
    xaxis = list(title = "Time"), 
    yaxis = list(title = "Input Signals")
  )

# Display the x2 column
head(data$x2)
# to store value in plot1,plot2,plot3,plot4 and plot5    
plot1 <- plot_ly(data, x=~time, y=~x1, type = "scatter", mode = "lines", name = "X1", line = list(color = 'red'))
plot3 <- plot_ly(data, x=~time, y=~x3, type = "scatter", mode = "lines", name = "X3", line = list(color = 'green'))
plot4 <- plot_ly(data, x=~time, y=~x4, type = "scatter", mode = "lines", name = "X4", line = list(color = 'orange'))
plot5 <- plot_ly(data, x=~time, y=~x5, type = "scatter", mode = "lines", name = "X5", line = list(color = 'purple'))

x_fig <- plotly::subplot(plot1, plot3, plot4, plot5, nrows = 4, shareX = TRUE, titleX = TRUE, titleY = TRUE) %>% 
  layout(plot_bgcolor='#e5ecf6', title="Time series analysis of X with time", xaxis = list(title="Time"))
x_fig

plot_ly(data, x=~time, y=~x2,type = "scatter",mode = "lines", name = "X2")

#scatter plot of data x1,x3,x4,x5
str(data)
fig1 = plot_ly(data, x=~time, y=~x1,type = "scatter",mode = "lines", name = "X1")
fig2 = plot_ly(data, x=~time, y=~x3,type = "scatter",mode = "lines", name = "X3")
fig3 = plot_ly(data, x=~time, y=~x4,type = "scatter",mode = "lines", name = "X4")
fig4 = plot_ly(data, x=~time, y=~x5,type = "scatter",mode = "lines", name = "X5")
x_figg = plotly:: subplot(fig1, fig2, fig3, fig4, nrows = 4, shareX = TRUE,titleX =TRUE, titleY = TRUE )  %>% layout(plot_bgcolor='#e5ecf6', title="Time series analysis of X with time", xaxis = list(title="Time"))
x_figg
#distribution of X
gg_x1 <- ggplot(data, aes(x = x1)) +    # Draw histogram & density
  geom_histogram(aes(y = after_stat(density)), bins = 10, fill = "#1f78b4") + 
  stat_density(geom = "line")

gg_x2 <- ggplot(data, aes(x = x2)) +    # Draw histogram & density
  geom_histogram(aes(y = after_stat(density)), bins = 10, fill = "#33a02c") + 
  stat_density(geom = "line")

gg_x3 <- ggplot(data, aes(x = x3)) +    # Draw histogram & density
  geom_histogram(aes(y = after_stat(density)), bins = 10, fill = "#e31a1c") + 
  stat_density(geom = "line")

gg_x4 <- ggplot(data, aes(x = x4)) +    # Draw histogram & density
  geom_histogram(aes(y = after_stat(density)), bins = 10, fill = "#ff7f00") + 
  stat_density(geom = "line")

gg_x5 <- ggplot(data, aes(x = x5)) +    # Draw histogram & density
  geom_histogram(aes(y = after_stat(density)), bins = 10, fill = "#6a3d9a") + 
  stat_density(geom = "line")

subplot(gg_x1, gg_x2, gg_x3, gg_x4, gg_x5, 
        nrows = 2, 
        shareX = FALSE, 
        titleX = TRUE, 
        titleY = TRUE) %>% 
  layout(plot_bgcolor = '#808080',  # Match with the blue from gg_x1
         title = "Distribution of X1, X2, X3, X4, and X5")
ggplotly(gg_x1)  %>% layout(plot_bgcolor='#e5ecf6', title="Distribution of X2", 
                            xaxis= list(title="X2 Signal"), yaxis = list(title="Density"))

# Generate the plot with more visible colors
data1 = data.frame(rbind(data.frame(values = data$x1, Inputs = "x1", fill='#FF5733'),  # Bright Red
                         data.frame(values= data$x3, Inputs = "X3", fill='#33FF57'),  # Bright Green
                         data.frame(values= data$x4, Inputs = "X4", fill='#3357FF'),  # Bright Blue
                         data.frame(values= data$x5, Inputs = "X5", fill='#FFC300'))) # Bright Yellow

p <- ggplot(data1, aes(x = values)) +
  geom_histogram(aes(x=values, y = after_stat(density), fill=Inputs), bins = 10, alpha=0.5) +
  stat_density(aes(x=values, y = after_stat(density), color=Inputs), geom="line", lwd=1) +
  geom_rug()

# Convert to plotly
fig <- ggplotly(p) %>% layout(plot_bgcolor='#808080', 
                              title="Distribution of X inputs", 
                              xaxis= list(title="X Signal"), yaxis = list(title="Density"))

fig

#task1.2
#distribution of x1,x3,x4,x5
gg_x1 <- ggplot(data, aes(x = x1)) +    # Draw histogram & density
  geom_histogram(aes(y = after_stat(density)), bins=10, fill = "#195f90") + 
  stat_density(geom="line")

gg_x3 <- ggplot(data, aes(x = x3)) +    # Draw histogram & density
  geom_histogram(aes(y = after_stat(density)), bins=10, fill = "#808080") + 
  stat_density(geom="line")

gg_x4 <- ggplot(data, aes(x = x4)) +    # Draw histogram & density
  geom_histogram(aes(y = after_stat(density)), bins=10, fill = "#FFC300") + 
  stat_density(geom="line")

gg_x5 <- ggplot(data, aes(x = x5)) +    # Draw histogram & density
  geom_histogram(aes(y = after_stat(density)), bins=10, fill = "#FF5733") + 
  stat_density(geom="line")

subplot(gg_x1, gg_x3,  gg_x4, gg_x5,  nrows = 2, shareX = FALSE,titleX =TRUE, titleY = TRUE )  %>% 
  layout(plot_bgcolor='#e5ecf6', title="Distribution of X1,X3,X4, and X5")

ggplotly(gg_x1)  %>% layout(plot_bgcolor='#e5ecf6', title="Distribution of X2", 
                            xaxis= list(title="X2 Signal"), yaxis = list(title="Density"))

#task1.3

# Corelation plot

plotX1 = plot_ly(data, x=~x1, y=~x2, type="scatter", mode="markers") %>% 
  layout(plot_bgcolor='#e5ecf6', title="Corelation of X1 input to X2 output", 
         xaxis = list(title="X1 input"), yaxis = list(title="Output Signal X2"))

plotX3 = plot_ly(data, x=~x3, y=~x2, type="scatter", mode="markers") %>% 
  layout(plot_bgcolor='#e5ecf6', title="Corelation of X3 input to X2 output", 
         xaxis = list(title="X3 input"), yaxis = list(title="Output Signal X2"))

plotX4 = plot_ly(data, x=~x4, y=~x2, type="scatter", mode="markers") %>% 
  layout(plot_bgcolor='#e5ecf6', title="Corelation of X4 input to X2 output", 
         xaxis = list(title="X4 input"), yaxis = list(title="Output Signal X2"))

plotX5 = plot_ly(data, x=~x5, y=~x2, type="scatter", mode="markers") %>% 
  layout(plot_bgcolor='#e5ecf6', title="Corelation of X5 input to X2 output", 
         xaxis = list(title="X5 input"), yaxis = list(title="Output Signal X2"))

correlation = plotly:: subplot(plotX1,plotX3,  plotX4, plotX5,  nrows = 2, shareX = FALSE,titleX =TRUE, titleY = TRUE )  %>% 
  layout(plot_bgcolor='#e5ecf6', title="Correlation of X with output")
#output of x1,x3,x4,x5
Correlation

#task 2.1 Regression
# Check and rename columns if necessary
if (!all(c("X1", "X2", "X3", "X4", "X5") %in% colnames(data))) {
  colnames(data) <- c("X1", "X2", "X3", "X4", "X5")
}
# Print column names and check data structure
print("Column names:")
print(colnames(data))
print("First rows of data:")
print(head(data))

# Convert all necessary columns to numeric
data$X1 <- as.numeric(data$X1)
data$X2 <- as.numeric(data$X2)
data$X3 <- as.numeric(data$X3)
data$X4 <- as.numeric(data$X4)
data$X5 <- as.numeric(data$X5)
# Function to generate the model matrix based on model description
generateModel1 <- function(data){
  ones <- rep(1, nrow(data))
  theta_bias <- runif(1, -1, 1) * ones
  model <- cbind(data$X4, data$X3^2, theta_bias)
  return(as.matrix(model))
}
generateModel2 <- function(data){
  ones <- rep(1, nrow(data))
  theta_bias <- runif(1, -1, 1) * ones
  model <- cbind(data$X4, data$X3^2, data$X5, theta_bias)
  return(as.matrix(model))
}
generateModel3 <- function(data){
  ones <- rep(1, nrow(data))
  theta_bias <- runif(1, -1, 1) * ones
  model <- cbind(data$X3, data$X4, data$X3^3, theta_bias)
  return(as.matrix(model))
}
generateModel4 <- function(data){
  ones <- rep(1, nrow(data))
  theta_bias <- runif(1, -1, 1) * ones
  model <- cbind(data$X4, data$X3^2, data$X5^3, theta_bias)
  return(as.matrix(model))
}
generateModel5 <- function(data){
  ones <- rep(1, nrow(data))
  theta_bias <- runif(1, -1, 1) * ones
  model <- cbind(data$X4, data$X1^2, data$X3^3, theta_bias)
  return(as.matrix(model))
}
# Function to calculate theta_hat
thetaHat <- function(model, y){
  return(solve(t(model) %*% model) %*% t(model) %*% y)
}
# Load X2 as the response/output variable
y <- as.numeric(data$X2)
# Generate and compute theta_hat for each model
model1 <- generateModel1(data)
theta1 <- thetaHat(model1, y)
print("Theta for Model 1:")
print(theta1)
model2 <- generateModel2(data)
theta2 <- thetaHat(model2, y)
print("Theta for Model 2:")
print(theta2)
model3 <- generateModel3(data)
theta3 <- thetaHat(model3, y)
print("Theta for Model 3:")
print(theta3)
model4 <- generateModel4(data)
theta4 <- thetaHat(model4, y)
print("Theta for Model 4:")
print(theta4)
model5 <- generateModel5(data)
theta5 <- thetaHat(model5, y)
print("Theta for Model 5:")
print(theta5)



# task 2.2
# function to calculate RSS
calculateRSS <- function(y, y_hat_model) {
  return(sum((y - y_hat_model)^2))
}
# creating model
# Model 1: Predicting x2 using x1, x3, x4, x5
model1 <- lm(x2 ~ x1 + x3 + x4 + x5, data = data)
y_hat_model1 <- predict(model1, data)
# Model 2: Predicting x2 using x1 + x3
model2 <- lm(x2 ~ x1 + x3, data = data)
y_hat_model2 <- predict(model2, data)
# Model 3: Predicting x2 using x1 + x4
model3 <- lm(x2 ~ x1 + x4, data = data)
y_hat_model3 <- predict(model3, data)
# Model 4: Predicting x2 using x3 + x5
model4 <- lm(x2 ~ x3 + x5, data = data)
y_hat_model4 <- predict(model4, data)
# Model 5: Predicting x2 using only x1
model5 <- lm(x2 ~ x1, data = data)
y_hat_model5 <- predict(model5, data)

# Calculating RSS for each model
RSS_Model1 <- calculateRSS(data$x2, y_hat_model1)
RSS_Model2 <- calculateRSS(data$x2, y_hat_model2)
RSS_Model3 <- calculateRSS(data$x2, y_hat_model3)
RSS_Model4 <- calculateRSS(data$x2, y_hat_model4)
RSS_Model5 <- calculateRSS(data$x2, y_hat_model5)




# Creating a table to show the RSS values for each model
rss_table <- data.frame(
  Model = c("Model 1 (x1, x3, x4, x5)", "Model 2 (x1, x3)", "Model 3 (x1, x4)", 
            "Model 4 (x3, x5)", "Model 5 (x1)"),
  RSS = c(RSS_Model1, RSS_Model2, RSS_Model3, RSS_Model4, RSS_Model5)
)
# Print the table
print(rss_table)

#task 2.3
# Assuming x2 is the output and x1, x3, x4, x5 are the inputs
output_variabl <- data$x2
input_variables <- data[, c("x1", "x3", "x4", "x5")]

# Function to calculate variance
calculateVariance <- function(N, rss_model){
  return(rss_model / (N - 1))
}
# Function to calculate likelihood
calculateLikelihood <- function(N, variance_model, rss_model) {
  return (-(N / 2) * log(2 * pi) - (N / 2) * log(variance_model) - (1 / (2 * variance_model)) * rss_model)
}
# Placeholder RSS values (replace with actual calculation)
RSS_Model1 <- sum((output_variable - predict(lm(output_variable ~ x1 + x3 + x4 + x5, data)))^2)
RSS_Model2 <- sum((output_variable - predict(lm(output_variable ~ x1 + x3 + x4, data)))^2)
RSS_Model3 <- sum((output_variabl - predict(lm(output_variabl ~ x1 + x3 + x5, data)))^2)
RSS_Model4 <- sum((output_variable - predict(lm(output_variable ~ x1 + x4 + x5, data)))^2)
RSS_Model5 <- sum((output_variable - predict(lm(output_variable ~ x3 + x4 + x5, data)))^2)




# Calculating the number of observations
N = length(output_variable)

# Calculating variances for each model
Variance_Model1 <- calculateVariance(N, RSS_Model1)
Variance_Model2 <- calculateVariance(N, RSS_Model2)
Variance_Model3 <- calculateVariance(N, RSS_Model3)
Variance_Model4 <- calculateVariance(N, RSS_Model4)
Variance_Model5 <- calculateVariance(N, RSS_Model5)

# Calculating likelihood for each model
Likelihood_1 <- calculateLikelihood(N, Variance_Model1, RSS_Model1)
Likelihood_2 <- calculateLikelihood(N, Variance_Model2, RSS_Model2)
Likelihood_3 <- calculateLikelihood(N, Variance_Model3, RSS_Model3)
Likelihood_4 <- calculateLikelihood(N, Variance_Model4, RSS_Model4)
Likelihood_5 <- calculateLikelihood(N, Variance_Model5, RSS_Model5)

# Creating a data frame to store both variance and likelihood for each model;s
results_table <- tibble(
  Model = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
  Variance = c(Variance_Model1, Variance_Model2, Variance_Model3, Variance_Model4, Variance_Model5),
  Likelihood = c(Likelihood_1, Likelihood_2, Likelihood_3, Likelihood_4, Likelihood_5)
)
# Print the results table
print(results_table)







# task 2.4
# function to calculate AIC
calculateAIC <- function(N, model_thetahat, likelihood_model){
  k_model = length(model_thetahat)
  return (2 * k_model - 2 * likelihood_model)
}
# function to calculate BIC
calculateBIC <- function(N, model_thetahat, likelihood_model){
  k_model = length(model_thetahat)
  return (k_model * log(N) - 2 * likelihood_model)
}
# fitting the models
Model1 <- lm(x2 ~ x1 + x3, data = data)
Model2 <- lm(x2 ~ x1 + x3 + x4, data = data)
Model3 <- lm(x2 ~ x1 + x3 + x4 + x5, data = data)
Model4 <- lm(x2 ~ poly(x1, 2) + x3, data = data)
Model5 <- lm(x2 ~ poly(x1, 2) + x3 + poly(x5, 2), data = data)
# Extracting log-likelihood and parameter counts
logLikelihood <- function(model) logLik(model)[1]
numParams <- function(model) length(coef(model))
N <- nrow(data)
# Calculating AIC and BIC
AIC_Model1 <- calculateAIC(N, coef(Model1), logLikelihood(Model1))
BIC_Model1 <- calculateBIC(N, coef(Model1), logLikelihood(Model1))

AIC_Model2 <- calculateAIC(N, coef(Model2), logLikelihood(Model2))
BIC_Model2 <- calculateBIC(N, coef(Model2), logLikelihood(Model2))

AIC_Model3 <- calculateAIC(N, coef(Model3), logLikelihood(Model3))
BIC_Model3 <- calculateBIC(N, coef(Model3), logLikelihood(Model3))

AIC_Model4 <- calculateAIC(N, coef(Model4), logLikelihood(Model4))
BIC_Model4 <- calculateBIC(N, coef(Model4), logLikelihood(Model4))

AIC_Model5 <- calculateAIC(N, coef(Model5), logLikelihood(Model5))
BIC_Model5 <- calculateBIC(N, coef(Model5), logLikelihood(Model5))

# Combining results into a table
results <- data.frame(
  Models = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
  AIC = c(AIC_Model1, AIC_Model2, AIC_Model3, AIC_Model4, AIC_Model5),
  BIC = c(BIC_Model1, BIC_Model2, BIC_Model3, BIC_Model4, BIC_Model5)
)
#print data value of results
print(results)

#task 2.5 Q-Q Plot
# Spliting data into training and testing sets
set.seed(42)
sample_index <- sample(1:nrow(data), 0.8 * nrow(data))
train <- data[sample_index, ]
test <- data[-sample_index, ]
# input features and target variable
X_train <- train[, c("time", "x1", "x3", "x4", "x5")]
y_train <- train$x2
X_test <- test[, c("time", "x1", "x3", "x4", "x5")]
y_test <- test$x2
# Train Model 1: Linear Regression
model1 <- lm(x2 ~ time + x1 + x3 + x4 + x5, data = train)
y_hat_model1 <- predict(model1, newdata = test)
# Train Model 2: Random Forest
model2 <- randomForest(x2 ~ time + x1 + x3 + x4 + x5, data = train, ntree = 100)
y_hat_model2 <- predict(model2, newdata = test)

# Train Model 3: Support Vector Regressor
model3 <- svm(x2 ~ time + x1 + x3 + x4 + x5, data = train, kernel = "radial")
y_hat_model3 <- predict(model3, newdata = test)
# Train Model 4: Placeholder Model 
model4 <- randomForest(x2 ~ time + x1 + x3 + x4 + x5, data = train, ntree = 50) 
y_hat_model4 <- predict(model4, newdata = test)
# Train Model 5: Placeholder Model
model5 <- lm(x2 ~ time + x1 + x3 + x4 + x5, data = train) 
y_hat_model5 <- predict(model5, newdata = test)
# function to calculate residuals
calculateError <- function(y, y_hat) {
  return(y - y_hat)
}

# Calculating residuals for each model
residuals_model1 <- calculateError(y_test, y_hat_model1)
residuals_model2 <- calculateError(y_test, y_hat_model2)
residuals_model3 <- calculateError(y_test, y_hat_model3)
residuals_model4 <- calculateError(y_test, y_hat_model4)
residuals_model5 <- calculateError(y_test, y_hat_model5)

# function to generate Q-Q plot
plotQQ <- function(model_error, title) {
  ggplot(data.frame(model_error = model_error), aes(sample = model_error)) +
    geom_qq(color = "#195f90") +
    geom_qq_line(color = "red") +
    ggtitle(title) +
    theme_minimal()
}
# generating Q-Q plots for each model
qq1 <- plotQQ(residuals_model1, "Q-Q Plot of Model 1 ")
qq2 <- plotQQ(residuals_model2, "Q-Q Plot of Model 2 ")
qq3 <- plotQQ(residuals_model3, "Q-Q Plot of Model 3 ")
qq4 <- plotQQ(residuals_model4, "Q-Q Plot of Model 4 ")
qq5 <- plotQQ(residuals_model5, "Q-Q Plot of Model 5 ")

# displaying the Q-Q plots
print(qq1)
print(qq2)
print(qq3)
print(qq4)
print(qq5)

#2.6 AIC, BIC, RSS and likelihood
# Creating a table to show the results
results_table_a <- tibble(
  Model = c("Model 1 ", "Model 2 ", "Model 3 ", 
            "Model 4", "Model 5 "),
  AIC = c(AIC_Model1, AIC_Model2, AIC_Model3, AIC_Model4, AIC_Model5),
  BIC = c(BIC_Model1, BIC_Model2, BIC_Model3, BIC_Model4, BIC_Model5),
  RSS = c(RSS_Model1, RSS_Model2, RSS_Model3, RSS_Model4, RSS_Model5),
  Likelihood = c(Likelihood_1, Likelihood_2, Likelihood_3, Likelihood_4, Likelihood_5)
)
# Print the results table
print(results_table)

#task 2.7 
#loading data agin for task 2.7
df <- read.csv('./data/data.csv')
# splitting the data into training and testing data sets
set.seed(100)  # For reproducibility
Split_Data <- initial_split(df, prop = 0.7)
training_set <- training(Split_Data)
testing_set <- testing(Split_Data)

# generating  training and testing models
X_training_model <- as.matrix(generateModel3(training_set))  # Ensuring it's a matrix
X_testing_model <- as.matrix(generateModel3(testing_set))    # Ensuring it's a matrix

# defining the thetaHat function
thetaHat <- function(X, y) {
  # Ensure X is a matrix and y is numeric
  if (!is.matrix(X)) stop("X must be a matrix")
  if (!is.numeric(y)) stop("y must be numeric")
  y <- as.matrix(y)  # Convert y to a column matrix
  if (nrow(X) != nrow(y)) stop("X and y must have the same number of rows")
  
  # Return the estimated coefficients
  solve(t(X) %*% X) %*% t(X) %*% y
}
#estimating model parameters using the training dataset
training_thetaHat <- thetaHat(X_training_model, training_set$x2)

#Computing model predictions for training and testing datasets
Y_training_hat <- X_training_model %*% training_thetaHat
Y_testing_hat <- X_testing_model %*% training_thetaHat

# computing the confidence intervals for predictions
error <- as.matrix(testing_set$x2) - Y_testing_hat  # Residuals
n <- nrow(X_testing_model)
z <- 1.96  # calculation 95% confidence level 
sd_error <- sqrt(sum(error^2) / (n - 1))  # SD of residuals
conf_interval <- z * sd_error  # Confidence interval
print(n) + print (z) + print(sd_error) + print(conf_interval) 

# creating the plots for training and testing data
# Plot for Training Data
training_plot <- ggplot(training_set, aes(x = x2)) +
  stat_density(geom = "line", color = "#195f90") +
  geom_vline(xintercept = conf_interval, linetype = "dashed", color = "red") +
  geom_vline(xintercept = -conf_interval, linetype = "dashed", color = "red") +
  ggtitle("Distribution of Training Data") +
  theme_minimal() +
  xlab("y") +
  ylab("Density")
ggplotly(training_plot) %>%
  layout(
    plot_bgcolor = '#e5ecf6',
    title = "Distribution of training data",
    xaxis = list(title = "x2"),
    yaxis = list(title = "Density")
  )
# plot for Testing Data
testing_plot <- ggplot(testing_set, aes(x = x2)) +
  stat_density(geom = "line", color = "#195f90") +
  geom_vline(xintercept = conf_interval, linetype = "dashed", color = "red") +
  geom_vline(xintercept = -conf_interval, linetype = "dashed", color = "red") +
  ggtitle("Distribution of Testing Data") +
  theme_minimal() +
  xlab("y") +
  ylab("Density")
ggplotly(testing_plot) %>%
  layout(
    plot_bgcolor = '#e5ecf6',
    title = "Distribution of testing data",
    xaxis = list(title = "y"),
    yaxis = list(title = "Density")
  )

#  adding error Bars to Predictions
testing_data_with_predictions <- data.frame(
  Actual = testing_set$x2,
  Predicted = Y_testing_hat,
  Lower_CI = Y_testing_hat - conf_interval,
  Upper_CI = Y_testing_hat + conf_interval
)
# scatter plot with Error Bars
error_bar_plot <- ggplot(testing_data_with_predictions, aes(x = Actual, y = Predicted)) +
  geom_point(color = "#195f90") +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), color = "red") +
  ggtitle("Model Predictions with Confidence Intervals") +
  xlab("Actual y") +
  ylab("Predicted y") +
  theme_minimal()

ggplotly(error_bar_plot)

#task 3
# Convert all necessary columns to numeric
data$x1 <- as.numeric(data$x1)
data$x2 <- as.numeric(data$x2)
data$x3 <- as.numeric(data$x3)
data$x4 <- as.numeric(data$x4)
data$x5 <- as.numeric(data$x5)

# Generate the regression model
# Model includes x1, x3, x4, and x5 as inputs and x2 as output
generateModel <- function(data) {
  ones <- rep(1, nrow(data))
  theta_bias <- runif(1, -1, 1) * ones
  model <- cbind(data$x1, data$x3, data$x4, data$x5, theta_bias)
  return(as.matrix(model))
}

# Calculate theta_hat
thetaHat <- function(model, y) {
  return(solve(t(model) %*% model) %*% t(model) %*% y)
}

# Define the response variable (y)
y <- as.numeric(data$x2)

# Generate the model and calculate theta_hat
model <- generateModel(data)
theta_hat <- thetaHat(model, y)

# Identify the two parameters with the largest absolute values in theta_hat
largest_indices <- order(abs(theta_hat), decreasing = TRUE)[1:2]
param1_hat <- theta_hat[largest_indices[1]]
param2_hat <- theta_hat[largest_indices[2]]

# Define ABC parameters
RSS_Model <- sum((y - model %*% theta_hat)^2)
epsilon <- RSS_Model * 2
num_samples <- 1000
param1_range <- c(param1_hat - abs(param1_hat), param1_hat + abs(param1_hat))
param2_range <- c(param2_hat - abs(param2_hat), param2_hat + abs(param2_hat))

# Perform rejection ABC
accepted_param1 <- numeric()
accepted_param2 <- numeric()

for (i in 1:num_samples) {
  param1_sample <- runif(1, param1_range[1], param1_range[2])
  param2_sample <- runif(1, param2_range[1], param2_range[2])
  
  abc_theta_hat <- theta_hat
  abc_theta_hat[largest_indices[1]] <- param1_sample
  abc_theta_hat[largest_indices[2]] <- param2_sample
  
  abc_Y_hat <- model %*% abc_theta_hat
  abc_RSS <- sum((y - abc_Y_hat)^2)
  
  if (abc_RSS <= epsilon) {
    accepted_param1 <- c(accepted_param1, param1_sample)
    accepted_param2 <- c(accepted_param2, param2_sample)
  }
}

# Create a data frame for the results
abc_results <- data.frame(Theta1 = accepted_param1, Theta2 = accepted_param2)

# Plot the joint posterior distribution
plot <- ggplot(abc_results, aes(x = Theta1, y = Theta2)) +
  geom_point(alpha = 0.5, color = "purple") +
  theme_minimal() +
  labs(title = "Joint Posterior Distribution", x = "Theta1", y = "Theta2")

# Display the plot
print(plot)

