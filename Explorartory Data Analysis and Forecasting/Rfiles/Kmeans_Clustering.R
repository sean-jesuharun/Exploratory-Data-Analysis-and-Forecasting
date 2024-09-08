
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
wine_DataFrame <- read_excel("Whitewine_v2.xlsx")
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
summary(wine_DataFrame_without_quality)

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


########################### choosing a value for k ###########################
# Elbow method
fviz_nbclust(wine_DataFrame_norm[,-12], kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method
dataFrame3 <- sample_frac(wine_DataFrame_norm, 0.65)
str(dataFrame3)

fviz_nbclust(dataFrame3[,-12], kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap statistic
# nboot = 50 to keep the function speedy.
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
fviz_nbclust(wine_DataFrame_norm[,-12], kmeans, nstart = 25,  method = "gap_stat", nboot = 500)+
  labs(subtitle = "Gap statistic method")

# Nbclust Method
nb <- NbClust(wine_DataFrame_norm[,-12], distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "kmeans")
fviz_nbclust(nb)

####################################################K-Means clustering Model############################################ 
str(wine_DataFrame_norm)
set.seed(5005)
KM <- kmeans(wine_DataFrame_norm[,-12], centers = 4, nstart = 25)
KM
KM$tot.withinss
KM$betweenss
KM$totss

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
cluster_wine_DataFrame_norm['quality'][cluster_wine_DataFrame_norm['quality'] == 6] <- 3
cluster_wine_DataFrame_norm['quality'][cluster_wine_DataFrame_norm['quality'] == 7] <- 2
cluster_wine_DataFrame_norm['quality'][cluster_wine_DataFrame_norm['quality'] == 8] <- 4

head(cluster_wine_DataFrame_norm)

#add cluster assigment to original data.
final_data <- cbind(cluster_wine_DataFrame_norm, cluster = KM$cluster)

#view final data
head(final_data)
tail(final_data)


#Creates vectors having data points
expected_value <- factor(final_data$quality)
predicted_value <- factor(final_data$cluster)

#Creating confusion matrix
confusion_Matrix <- confusionMatrix(predicted_value, expected_value)
confusion_Matrix

#################################################### PCA ###################################################
str(wine_DataFrame)
dataFrame1_scale <- as.data.frame(scale(wine_DataFrame[1 : 11]))
str(dataFrame1_scale)

str(wine_DataFrame_norm)

#PCA APPLYING...........
PCA_components <- prcomp(~fixed.acidity + volatile.acidity + citric.acid
                         + residual.sugar + chlorides + free.sulfur.dioxide
                         + total.sulfur.dioxide + density + pH + sulphates +
                           alcohol, data = wine_DataFrame_norm, 
                         scale. = T)
summary(PCA_components)

PCA_components$sdev

VE <- PCA_components$sdev^2
PVE <- VE / sum(VE)
round(PVE, 2)

screeplot(PCA_components, type='l', main="Screeplot for principle components")


trnfrm_PCA_components <- as.data.frame(PCA_components$x[,1:8])
head(trnfrm_PCA_components)

#Setting seed
set.seed(1600) 
KM.PCA <- kmeans(trnfrm_PCA_components, centers = 4, nstart = 25)
KM.PCA
KM.PCA$tot.withinss
KM.PCA$betweenss
KM.PCA$totss

############################################################################################################
#Visualizing the clustering algorithm results
KM.PCA.clusters <- KM.PCA$cluster
fviz_cluster(list(data = trnfrm_PCA_components, cluster = KM.PCA.clusters))

###########################################################################################################