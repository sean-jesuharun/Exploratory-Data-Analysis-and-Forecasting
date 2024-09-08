library(MASS)
library(NbClust)
library(factoextra)
library(ClusterR)
library(cluster)
library(caret)
library(dplyr)
library(psych)
library(readxl)

###########################################Loading the Data set##############################################################
wine_DataFrame <- read_excel("C:\\Users\\SEAN\\Desktop\\MachineLearningandDataMining\\MLCW\\Whitewine_v2.xlsx")
wine_DataFrame <- data.frame(wine_DataFrame)
str(wine_DataFrame)

#######################################Visualization of Outliers in each features of the data set############################
columnNames <- names(wine_DataFrame)
oldpar <- par(mfrow = c(2,6))
for (i in 1:11){
  boxplot(wine_DataFrame[i], main = columnNames[i], col = "#DD3300")
}
par(oldpar)

################################################visualizing how the distribution is looks like###############################
oldpar <- par(mfrow = c(2,6))
for (i in 1:12){
  truehist(wine_DataFrame[[i]], main = columnNames[i], col = blues9)
  lines(density(wine_DataFrame[[i]]), col= 2, lwd=2)
}
par(oldpar)

######################################################Removal of Outliers####################################################
# #The returned stats variable is a vector of length 5, containing the extreme of the lower whisker, the lower 'hinge', the median, the upper 'hinge' and the extreme of the upper whisker.
for (i in 1:11){
  stats <- boxplot.stats(wine_DataFrame[[i]], coef = 1.4)$stats
  #print(stats)
  lower_boundry <- stats[1]
  #print(lower_boundry)
  upper_boundry <- stats[5]
  #print(upper_boundry)
  wine_DataFrame <- wine_DataFrame[!(wine_DataFrame[i] < lower_boundry | wine_DataFrame[i] > upper_boundry),]
  #str(dataFrame1)
}

str(wine_DataFrame)

#############################Visualization of each feature after outlier removal of the data set##############################
oldpar <- par(mfrow = c(2,6))
for (i in 1:11){
  boxplot(wine_DataFrame[i], main = columnNames[i], col = "#4DB3E6")
}
par(oldpar)

###################################visualizing how the distribution is looks like after Outlier Removal#######################
oldpar <- par(mfrow = c(2,6))
for (i in 1:11){
  truehist(wine_DataFrame[[i]], main = columnNames[i], col = blues9)
  lines(density(wine_DataFrame[[i]]), col= 2, lwd=2)
}
par(oldpar)

str(tail(wine_DataFrame))


##################################################Normalizing the Data########################################################

#define Min-Max normalization function
normalization <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}


#apply Min-Max normalization to first eleven columns in wine data-set.
wine_DataFrame_without_quality <- data.frame(lapply(wine_DataFrame[1:11], normalization))
str(wine_DataFrame_without_quality)

#combining the quality with the normalized data.
quality <- wine_DataFrame$quality
wine_DataFrame_norm <- cbind(wine_DataFrame_without_quality, quality)
str(wine_DataFrame_norm)


#############################Visualization of each feature after Normalization of the data set##############################
oldpar <- par(mfrow = c(2,6))
for (i in 1:11){
  boxplot(wine_DataFrame_norm[i], main = columnNames[i], col = "#4DC6E6")
}
par(oldpar)


################################################Elbow Plot################################################################
# Initialize total within sum of squares error: wss
wss <- 0

# For 1 to 20 cluster centers
for (i in 1:10) {
  km.out <- kmeans(wine_DataFrame_norm[,-12], centers = i)
  # Save total within sum of squares to wss variable
  wss[i] <- km.out$tot.withinss
}
print(wss)

# Plot total within sum of squares vs. number of clusters
plot(1:10, wss, type = "b",
     xlab = "Number of Clusters",
     ylab = "Within groups sum of squares")



####################################################K-Means clustering Model############################################ 
set.seed(444)
KM <- kmeans(wine_DataFrame_norm[,-12], centers = 4, nstart = 25)
KM


################################################Visualizing the clustering algorithm results###############################
str(wine_DataFrame_norm)
temp_dataFrame <- wine_DataFrame_norm[,-12]
str(temp_dataFrame)

KM.clusters <- KM$cluster
rownames(temp_dataFrame)<- paste(wine_DataFrame_norm$quality, 1:dim(wine_DataFrame_norm)[1], sep = "_")
str(temp_dataFrame)
tail(temp_dataFrame)

fviz_cluster(list(data = temp_dataFrame, cluster = KM.clusters))


#############################################Setting the labels############################################################
wine_dataFrame_labels <- wine_DataFrame_norm$quality
table(wine_dataFrame_labels)
tail(wine_dataFrame_labels)

########################################to find which cluster is more describing the labels################################
table(KM.clusters, wine_dataFrame_labels)

############################################### Confusion matrix ###########################################################
cluster_wine_DataFrame_norm <- wine_DataFrame_norm
str(cluster_wine_DataFrame_norm)


cluster_wine_DataFrame_norm['quality'][cluster_wine_DataFrame_norm['quality'] == 5] <- 1
cluster_wine_DataFrame_norm['quality'][cluster_wine_DataFrame_norm['quality'] == 6] <- 2
cluster_wine_DataFrame_norm['quality'][cluster_wine_DataFrame_norm['quality'] == 7] <- 3
cluster_wine_DataFrame_norm['quality'][cluster_wine_DataFrame_norm['quality'] == 8] <- 4

str(cluster_wine_DataFrame_norm)

#add cluster assigment to original data.
final_data <- cbind(cluster_wine_DataFrame_norm, cluster = KM$cluster)

#view final data
head(final_data)


#Creates vectors having data points
expected_value <- factor(final_data$quality)
predicted_value <- factor(final_data$cluster)

#Creating confusion matrix
confusion_Matrix <- confusionMatrix(data=predicted_value, reference = expected_value)
confusion_Matrix

#################################################### PCA ###################################################
# str(dataFrame2_scale)
# 
# 
#Standadizing the Data set
# applying scale function
str(wine_DataFrame)
dataFrame1_scale <- as.data.frame(scale(wine_DataFrame[1 : 11]))
str(dataFrame1_scale)

str(wine_DataFrame_norm)

#PCA APPLYING...........
PCA_components <- prcomp(~fixed.acidity + volatile.acidity + citric.acid
                         + residual.sugar + chlorides + free.sulfur.dioxide
                         + total.sulfur.dioxide + density + pH + sulphates + alcohol, data = wine_DataFrame_norm, 
                         scale. = T)
summary(PCA_components)
#head(PCA_components)

# PCA_components$rotation
# 
# PCA_components$rotation <- -PCA_components$rotation
# PCA_components$rotation
# 
# head(PCA_components$x)
# 
# PCA_components$x <- - PCA_components$x
# head(PCA_components$x)

PCA_components$sdev

VE <- PCA_components$sdev^2
PVE <- VE / sum(VE)
round(PVE, 2)

screeplot(PCA_components, type='l', main="Screeplot for principle components")


trnfrm_PCA_components <- as.data.frame(PCA_components$x[,1:8])
#trnfrm_PCA_components <- PCA_components$x[,1:9]

str(trnfrm_PCA_components)
head(trnfrm_PCA_components)

# trnfrm_PCA_components_withQualityLabel <- cbind(trnfrm_PCA_components, quality)
# head(trnfrm_PCA_components_withQualityLabel)

# Fitting K-Means clustering Model 
# to training dataset
set.seed(1600) #Setting seed
KM.PCA <- kmeans(trnfrm_PCA_components, centers = 4, nstart = 25)
KM.PCA


############################################################################################################
#Visualizing the clustering algorithm results
KM.PCA.clusters <- KM.PCA$cluster
# rownames(trnfrm_PCA_components)<- paste(trnfrm_PCA_components_withQualityLabel$quality, 1:dim(wine_DataFrame_norm)[1], sep = "_")
# tail(trnfrm_PCA_components, 200)

fviz_cluster(list(data = trnfrm_PCA_components, cluster = KM.PCA.clusters))


##Setting the labels.
# trnfrm_PCA_components_label <- trnfrm_PCA_components_withQualityLabel$quality
# table(trnfrm_PCA_components_label)
# 
# table(KM.PCA.clusters, trnfrm_PCA_components_label)
###########################################################################################################