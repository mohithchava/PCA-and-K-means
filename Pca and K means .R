library(data.table)
library(cluster)
library(dplyr)
library(ggplot2)
#install.packages('factoextra')
library(factoextra)
#install.packages("devtools")
library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)
data = fread("marketing_campaign.csv")
summary(data)
str(data)

data$Customer_age <- 2021 - data$Year_Birth
today <- as.POSIXct("01-07-2021", format="%d-%m-%Y")
data$Dt_Customer <-   as.POSIXct(data$Dt_Customer, format="%d-%m-%Y")
data$MembershipDays <- round(today - data$Dt_Customer)
encode_ordinal <- function(x, order ) {
  x <- as.numeric(factor(x, levels = order, exclude = NULL))
  x
}
data$Education <- encode_ordinal(data$Education, c('HighSchool', 'Associate', 'Bachelor', 'Master', 'PhD'))

data$Marital_Status <- factor(data$Marital_Status, exclude = NULL)
data_new <- model.matrix(~.-1, data = data[, c("Marital_Status")],
                       contrasts.arg = list(
                         Marital_Status = contrasts(data$Marital_Status, contrasts = FALSE)))
data_new <- as.data.frame(data_new)                         
data <- cbind(data,data_new)                         
data <- subset(data, select = -c( ID, Year_Birth, Dt_Customer, Marital_Status))
data_scaled <- data %>% mutate_all(~(scale(.) %>% as.vector))
pca_data = prcomp(data_scaled[1:500,], center = TRUE, scale = TRUE)
summary(pca_data)
ggbiplot(pca_data)
pca_data = prcomp(data_scaled, center = TRUE, scale = TRUE)

data_transform = as.data.frame(-pca_data$x[,1:2])
fviz_nbclust(data_transform, kmeans, method = 'wss')
k = 3
kmeans_data = kmeans(data_transform, centers = k, nstart = 50)
fviz_cluster(kmeans_data, data = data_transform)
boxplot(data$Customer_age ~ kmeans_data$cluster,
        xlab='Cluster', ylab='Customer_age',
        main='Customer_age by Cluster')
hist(data$Education ~ kmeans_data$cluster,
        xlab='Cluster', ylab='Education',
        main='Education by Cluster')

ggplot() +
  geom_histogram(aes(x=data$Education, fill=kmeans_data$cluster),binwidth=0.01)


