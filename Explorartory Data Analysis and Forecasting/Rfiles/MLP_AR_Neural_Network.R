
library(readxl)
library(neuralnet)
library(Metrics)
library(MLmetrics)

DataFrame_days <- read.csv("DataFrame_AR(4).csv")
DataFrame_days <- DataFrame_days[-(1)]
str(DataFrame_days)

length_DataFrame_days <- nrow(DataFrame_days)
length_DataFrame_days

columnNumbers <- length(DataFrame_days)
columnNumbers

#Testing Data set with No scale.
Data_test_NoScale = DataFrame_days[431:length_DataFrame_days,]
str(Data_test_NoScale)
head(Data_test_NoScale)

#Normalizing the Data
normalization <- function(x) {
  (x - min(x))/(max(x) - min(x))
}

#apply Min-Max normalization.
DataFrame_days_scale <- data.frame(lapply(DataFrame_days[1:columnNumbers], normalization))
head(DataFrame_days_scale)

#Training Data set
Data_train_scale = DataFrame_days_scale[1:430,]
str(Data_train_scale)

#Testing Data set
Data_test_scale = DataFrame_days_scale[431:length_DataFrame_days,]
str(Data_test_scale)
head(Data_test_scale)


#Building the MLP neural network
set.seed(4444)
nn <- neuralnet(NextDay ~., data = Data_train_scale, hidden = c(9,5), 
                learningrate = 0.0001, act.fct = "logistic",
                linear.output = TRUE)
plot(nn)
nn$result.matrix

#Test the resulting output
temp_Data_test_scale <- Data_test_scale[1:columnNumbers-1]
head(temp_Data_test_scale)
str(temp_Data_test_scale)

#Prediction using our model
nn.results <- compute(nn, temp_Data_test_scale)
nn.results$net.result

#validation
results <- data.frame(actual = Data_test_scale$NextDay, prediction = nn.results$net.result)
results

actual = Data_test_NoScale$NextDay
head(actual)

predicted = (results$prediction * (max(Data_test_NoScale$NextDay) - min(Data_test_NoScale$NextDay))) + min(Data_test_NoScale$NextDay)
head(predicted)
head(Data_test_NoScale$NextDay)

class(actual)
class(predicted)

#calculate RMSE (Root Mean Square Error)
rmse(actual, predicted)

#calculate MAE (Mean Absolute Error)
mae(actual, predicted)

#calculate MAPE (Mean Absolute Percentage Error)
MAPE(predicted, actual)

