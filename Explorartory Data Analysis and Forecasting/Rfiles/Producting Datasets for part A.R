
library(readxl)

#Loading the data set and creating the data Frame.
dataFrame <- read_excel("UoW_load.xlsx")
str(dataFrame)


columns= c("Day1","Day2","Day3","Day4", "NextDay")

# pass this vector length to ncol parameter
# and nrow with 0
DataFrame_days = data.frame(matrix(nrow = 0, ncol = length(columns)))

# assign column names
colnames(DataFrame_days) = columns


i <- 1
while(i<497){
  values <- data.frame(dataFrame$Time11[i], dataFrame$Time11[i+1], dataFrame$Time11[i+2], dataFrame$Time11[i+3], dataFrame$Time11[i+4])
  #Naming the Data Frame - Step 2
  names(values) <- c("Day1","Day2","Day3","Day4", "NextDay")
  DataFrame_days <- rbind(DataFrame_days, values)
  i = i + 1
}

str(DataFrame_days)
write.csv(DataFrame_days, "test_DataFrame_AR(4).csv")
