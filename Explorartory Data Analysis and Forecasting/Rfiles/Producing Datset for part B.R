
library(readxl)

#Loading the data set and creating the data Frame.
dataFrame <- read_excel("UoW_load.xlsx")
str(dataFrame)

#"Day2_9","Day2_10","Day2_11","Day3_9","Day3_10","Day3_11","Day4_9","Day4_10","Day4_11",
columns= c("Day1_9","Day1_10","Day1_11","NextDay_9","NextDay_10","NextDay_11")

# pass this vector length to ncol parameter
# and nrow with 0
DataFrame_days = data.frame(matrix(nrow = 0, ncol = length(columns)))

# assign column names
colnames(DataFrame_days) = columns

#, dataFrame$Time9[i+2], dataFrame$Time10[i+2], dataFrame$Time11[i+2], dataFrame$Time9[i+3], dataFrame$Time10[i+3], dataFrame$Time11[i+3], dataFrame$Time9[i+4], dataFrame$Time10[i+4], dataFrame$Time11[i+4]
i <- 1
while(i<500){
  values <- data.frame(dataFrame$Time9[i], dataFrame$Time10[i], dataFrame$Time11[i], dataFrame$Time9[i+1], dataFrame$Time10[i+1], dataFrame$Time11[i+1])
  #Naming the Data Frame - Step 2
  names(values) <- c("Day1_9","Day1_10","Day1_11","NextDay_9","NextDay_10","NextDay_11")
  DataFrame_days <- rbind(DataFrame_days, values)
  i = i +1
}

str(DataFrame_days)
write.csv(DataFrame_days, "test_DataFrame_NARX(1).csv")
