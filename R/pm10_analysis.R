######## PM10 data import and management ##########################
# Import the PM10 data
data_pm10 <- read.csv('data/class_pm10_data.csv', sep=',')

# Check the class of each column
sapply(data_pm10, class)

# Remove the first column because it is not useful
data_pm10 <- data_pm10[,-1]

# Format the date column into the date format 
date <- data_pm10$date
# Preview the date
head(date)

date <- strptime(date, format = "%m/%d/%y", tz ="Asia/Kuala_Lumpur")
# Remove the existing date column
data_pm10 <- data_pm10[,-1]
# Combine the new date with the data
data_pm10 <- cbind(date,data_pm10)

# Separate the data into two stations, 1 and 2
data_pm10_s1 <- data_pm10[which(data_pm10$station == 1), ]
data_pm10_s2 <- data_pm10[which(data_pm10$station == 2), ]

# Remove the station column
data_pm10_s1 <- data_pm10_s1[,c(-15)]
data_pm10_s2 <- data_pm10_s2[,c(-15)]

# Remove intermediate dataframes
rm(data_pm10,date)



#### Weather data import and management #########

# Import the weather data
data_weather <- read.csv('data/mes_data_WMKB.csv', sep=',')
# Check the class of each column
sapply(data_weather, class)


# Use only numeric data, remove the non-numeric data
data_weather <- data_weather[ , c(-1, -2, -12, -14, -15, -16, -17)]

# Recheck the class of each column
sapply(data_weather, class)

# Format the date column into the date format 
date <- data_weather$Time
# Preview the date
head(date)

date <- strptime(date, format = "%Y-%m-%d %H:%M:%S", tz ="Asia/Kuala_Lumpur")
# Re-preview the date
head(date)

# Remove the existing date column
data_weather <- data_weather[,-1]

# Combine the new date with the data
data_weather <- cbind(date,data_weather)

# Remove intermediate dataframes
rm(date)



#### Merging the data #########

# Merge the data for the two stations

# Average the weather data so that it has the same sampling frequency as the PM10 data
# Use the "openair" package so that it is easier to average it; install the package first if you have
# not installed it yet using the command: install.packages('openair')
library(openair)
data_weather_avg <- timeAverage(data_weather, avg.time = 'day')

# Merge the data with the station 1 data
data_pm10_s1 <- merge(data_pm10_s1, data_weather_avg, by = 'date')

# Merge the data with the station 2 data
data_pm10_s2 <- merge(data_pm10_s2, data_weather_avg, by = 'date') 

# Remove intermediate dataframes
rm(data_weather, data_weather_avg)


#### Analysis through visualization ####

# Distribution analysis
hist(data_pm10_s1$pm10, xlab = "PM10", main = 'PM10 Distrubtion at Station 1')


boxplot(data_pm10_s1$pm10)

# Categorical analysis
barplot(table(data_pm10_s1$pm10,data_pm10_s1$monsoon), main = "PM 10 at Station 1 by Monsoon")

# Calculating the total count of PM10 values per monsoon
colSums(table(data_pm10_s1$pm10,data_pm10_s1$monsoon))

barplot(table(data_pm10_s1$pm10,data_pm10_s1$day), main = "PM10 at Station 1 by Day")

# Calculating the total count of PM10 values per day
colSums(table(data_pm10_s1$pm10,data_pm10_s1$day))

# Trend analysis
plot(data_pm10_s1$date, data_pm10_s1$pm10, type = 'l', xlab = "Date", ylab = "PM10")

#### Analysis through descriptive statistics ####
# Descriptive statistics of the data at Station 1
summary(data_pm10_s1)

#### Analysis through association and model development ####
# Correlation between all numeric parameters to develop a regression model using the function
# in the package 'openair'
corPlot(data_pm10_s1[ , c(-1,-13,-14)])

# Building a PM10 and visibility model due to high correlation between PM10 and vsby
lmPM10_vsby <- lm(pm10 ~ vsby, data = data_pm10_s1)

# The linear regression model details and R-squared. Notice that the p-value < 0.001 for vsby, which means
# that it is statistically significant. The model also fits the data by 25%.
summary(lmPM10_vsby)


# Visualize the model
plot(data_pm10_s1$vsby, data_pm10_s1$pm10, pch = 19, xlab = "Visibility", ylab = "PM10")
abline(lmPM10_vsby, col = 'red', lwd = 2)


#### HCA #########################################

# Looking at the Station 1 data as a whole using Cluster Analysis 
# Remove the columns that are non-numeric and change it into a matrix for dist calculation
data_pm10_s1_hca <- as.matrix(data_pm10_s1[ , c(-1,-13,-14, -15, -16)])
# Add the locations to the matrix so that the cluster analysis plot can be labelled by monsoon
row.names(data_pm10_s1_hca) <- paste0(as.character(data_pm10_s1[,13]), 1:nrow(data_pm10_s1)) 
# Scale and normalize the data because some parameters are much higher in magnitudes than others
scaled_data <- scale(data_pm10_s1_hca)
# Run Cluster Analysis using the canberra method for the dist and the complete method for the hclust
# The following are the available methods for hclust and and dist, respectively.
# hclust method = "ward.D", "ward.D2", "single", "complete", "average" (= UPGMA), "mcquitty" (= WPGMA), 
# "median" (= WPGMC) or "centroid" (= UPGMC).
# dist method = "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski"
hca_pm10_s1 <- hclust(dist(data_pm10_s1_hca,
                           method ="canberra"),
                      method = "complete") 


# Visualize the dendrogram
plot(hca_pm10_s1)

# Determining if the HCA is able to separate the data by monsoon (4 groups)
hca_pm10_s1_cut <- cutree(hca_pm10_s1, 4)
# The result shows that it cannot separate the data into 4 groups but can separate into 2 groups at the most
table(hca_pm10_s1_cut, data_pm10_s1$monsoon)

# Below is the way to visualize the heatmap using the same method as above
# Need to declare the functions first before you can use them in the heatmap function
dist.s1 <- function(x) dist(x, method = "canberra")
hclust.s1 <- function(x) hclust(x, method="complete")
# Plot the heatmap and scale the data according to columns
heatmap(data_pm10_s1_hca, distfun = dist.s1, hclustfun = hclust.s1,
        scale = c("column"))

#### PCA ###########################################
# Load the package 'car' to use the dataEllispe() function later; install it first, install.packages('car')
library(car) 
# Looking at the Station 2 data as a whole using PCA
# Remove the non-numeric data and skyl4 (column 27) because it contains too many NaN values
data_pm10_s1_pca <- data_pm10_s1[ , c(-1,-14, -15, -16, -27)]
# Omit all NA and NaN values if there are any
data_pm10_s1_pca <- na.omit(data_pm10_s1_pca)
# Remove Be because it contains only '0's
data_pm10_s1_pca <- data_pm10_s1_pca[ , -9]
# Remove column Monsoon and put it into another variable to label the data later by monsoon
monsoon_s1 <- data_pm10_s1_pca$monsoon
data_pm10_s1_pca <- data_pm10_s1_pca[ , -11]

# Run PCA with the data centered and scaled
pm10_s1.pca <- prcomp(data_pm10_s1_pca, # Remove the first column
                      center = TRUE, # Center the data, similar to scaling
                      scale. = TRUE) # Need to be scaled because of magnitude diff.



# Visualize the PC1 against PC2 trend
plot(pm10_s1.pca$x[,1:2], # Plot only the first and second PCs
     pch = 20,            # The marker 
     col = as.factor(monsoon_s1))    # Color the point by monsoon

# Draw circles to group the data by monsoon
dataEllipse(pm10_s1.pca$x[,1],      # PC1
            pm10_s1.pca$x[,2],      # PC2
            groups = as.factor(monsoon_s1), # Groups
            lwd = 1,            # line width
            group.labels = c('FTM','NEM','STM','SWM'), 
            plot.points = FALSE, # Do not want to redraw points on top of old points
            levels = 0.3,      # 30% confidence level, but this is a low level
            add = TRUE,
            fill=TRUE, 
            fill.alpha = 0.02,
            col = c('black', 'red', 'green', 'yellow'))

# Draw the legend
legend('bottomright', c('FTM','NEM','STM','SWM'), pch = c(20,20,20),
       col = c('black', 'red', 'green', 'yellow'))

# The PCA show that the variation of the parameters for the monsoons STM and SWM are similar while the variation 
# in monsoon FTM is different than STM and SWM. The variation of NEM shows similarities with STM, FEM and SWM. 
