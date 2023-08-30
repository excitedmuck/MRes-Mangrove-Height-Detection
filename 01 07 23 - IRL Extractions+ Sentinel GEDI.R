
###### Loading and making first layers ####
library(readxl)
library(caret)
library(randomForest)
library(raster)
library(openxlsx)
library(Metrics)
library(rgdal)
library(dplyr)

## Using the data to calculate the NDVI index
####

gedi_file <- "GEDI_2021.shp"
tiff_file <- "S2_1764_2021_allbands.tiff"

#performRegression <- function(gedi_file, tiff_file, bands) {
# Read GEDI data
GEDI_data <- readOGR(gedi_file)


# Read TIFF image data
img_data <- stack(tiff_file)
bands <- rh98~.

GEDI_data <- spTransform(GEDI_data, CRS(projection(img_data)))

# Extract image values at plot locations
img_val <- extract(img_data, GEDI_data, fun = mean, na.rm = TRUE)
#img_val <- extract(img_data, GEDI_data)#, fun = mean, na.rm = TRUE)
ht_data <- as.data.frame(cbind(GEDI_data, img_val))

# Remove the last two columns of coordinates
htData <- ht_data[, -c((ncol(ht_data) - 1):ncol(ht_data))]

# Select only one height data (e.g., rh98) and image data columns
htData <- select(htData, rh98, colnames(img_val))

# Assuming htData contains the necessary columns (Red and NIR bands)
# htData$NDVI <- (htData$NIR - htData$Red) / (htData$NIR + htData$Red)

# Assuming htData contains the necessary columns (Red and NIR bands)
htData$NDVI <- (htData$B8 - htData$B4) / (htData$B8 + htData$B4)

## NDVI can be tddhought of as the difference between the NIR 
## and Red because it would correspond to more water so makes more sense. 
### HtData in the stuff. 

mean_y <- mean(htData[, 1])
sd_y <- sd(htData[, 1])

# Set a threshold for outlier detection (e.g., 3 times the standard deviation)
threshold <- 0.5 * sd_y

# Identify the indices of the anomalous points
anomalous_indices <- which(abs(htData[, 1] - mean_y) > threshold)

# Remove the anomalous points from the data frame
clean_frac <- htData[-anomalous_indices, ]

frac <- createDataPartition(clean_frac$rh98, p = .65, list = FALSE, group = 10)

## New variable weight

# Histogram of densities RH98
ggplot(clean_frac, aes(x = rh98)) +
  geom_density()

d <- density(clean_frac$rh98)
plot(d, main="Kernel Density 98th Relative Height Metric")
polygon(d, col="red", border="blue")

hist(htData$rh98)

#### The distribution of the various bands 
library(ggplot2)

ggplot(clean_frac) +
  #geom_density(aes(x = rh98), color = "blue", fill = "lightblue", alpha = 0.5, show.legend = TRUE) +
  geom_density(aes(x = B7), color = "red", fill = "red", alpha = 0.5, show.legend = TRUE) +
  geom_density(aes(x = NDVI), color = "green", fill = "green", alpha = 0.5, show.legend = TRUE) +
  geom_density(aes(x = B9), color = "blue", fill = "blue", alpha = 0.5, show.legend = TRUE) +
  geom_density(aes(x = B8), color = "blue", fill = "blue", alpha = 0.5, show.legend = TRUE) +
  xlab("Variable") +
  ylab("Density") +
  ggtitle("Kernel Density Plots") +
  theme_minimal()

# Testing and training split. 
training <- htData[frac, ]
testing <- htData[-frac, ]

# Regression modeling
fitControl <- trainControl(method = "cv", number = 5, savePredictions = TRUE)
rfGrid <- expand.grid(mtry = c(1:6))

set.seed(1)
rfFit1 <- train(
  bands,
  data = training,
  method = "rf",
  trControl = fitControl,
  verbose = FALSE,
  importance = TRUE,
  tuneGrid = rfGrid
)

# Plot variable importance
rfImp <- varImp(rfFit1, scale = FALSE)
plot(rfImp, main = "Variable Importance")

# Evaluate model performance
predictedHt <- predict(rfFit1, testing[-1])
correlation <- cor(predictedHt, testing[, 1])^2
rmse <- sqrt(mean((predictedHt - testing[, 1])^2))

# Plot predicted vs actual heights
plot(testing[, 1], predictedHt, pch = 16, cex.axis = 1.5, cex.lab = 1.5, col = "blue",
     main = "GEDI vs Model Predicted Height\n RF Regression\n",
     xlim = c(0, 15), ylim = c(0, 15), xlab = "GEDI height (m)", ylab = "Model height (m)")
abline(1.123e-15, 1)

# Print RMSE
print(paste("rmse: ",rmse))
print(paste("R2: ",correlation))
# Access the p-value
cor_test <- cor.test(testing[, 1], predictedHt)
p_value <- cor_test$p.value

# print(paste("Correlation test: ",cor_test))
# Check the statistical significance at a significance level (e.g., alpha = 0.05)

if (p_value < 0.05) {
  print(paste("The correlation is statistically significant since p-value ", p_value, "is less than 0.05."))
} else {
  print(paste("The correlation is not statistically significant p-value =", p_value))
}

### Finding which of the Bands has the highest importance score
rfImp <- varImp(rfFit1, scale = FALSE)
plot(rfImp, main = "Variable Importance")


# Read the Excel file
file_path <- "23 06 23 - Extrapolated Height Data.xlsx"
file_path <- "24 06 23 - Diameter and Heights.xlsx"
file_path <- "25 03 23 - 2019Heights.xlsx"
data_actuals <- read_excel(file_path)
actual_values <- as.data.frame(data_actuals)


#### Loading Actual Data ####

### Merging all the shapefiles into one. 

# List of shapefile paths
shapefile_paths <- c('2015PSP_1764.shp', '2016PSP_1764.shp', '2017PSP_1764.shp', '2018PSP_1764.shp')

# Read the shapefiles individually
data_list <- lapply(shapefile_paths, function(file) readOGR(file))

# Drop the FolderPath variable from all frames except data_2018
for (i in seq_along(data_list[1:3])) {
  data_list[[i]] <- data_list[[i]][, -which(names(data_list[[i]]) == "FolderPath")]
}

# Assign individual dataframes
data_2015 <- data_list[[1]]
data_2016 <- data_list[[2]]
data_2017 <- data_list[[3]]
data_2018 <- data_list[[4]]
# data_2018<- data_2018[, -which(names(data_2018) == "FolderPath")]

# Merge the shapefiles
merged_shapefile <- rbind(data_2015, data_2016, data_2017, data_2018)
merged_shapefile@data$"Ht (m)" <- actual_values$`2019` * 0.01
merged_shapefile@data$"Ht (m)" <- round(merged_shapefile@data$"Ht (m)", 2)

#writeOGR(merged_shapefile, dsn = "Actual Heights 2019 with height", layer = "Actual_ht_2019", driver = "ESRI Shapefile", overwrite_layer = TRUE)
#writeOGR(merged_shapefile, dsn = "Actual Heights 2019 with height.geojson", layer = "Actual_ht_2019", driver = "GeoJSON", overwrite_layer = TRUE)


### Renaming colnames for export
colnames(merged_shapefile@data)[colnames(merged_shapefile@data) == "Name"] <- "namess"
colnames(merged_shapefile@data)[colnames(merged_shapefile@data) == "Ht (m)"] <- "Name"
writeOGR(merged_shapefile, dsn = "Actual Heights 2019 with height.geojson", layer = "Actual_ht_2019_f", driver = "GeoJSON", overwrite_layer = TRUE)



actual_data_transformed <- spTransform(merged_shapefile, crs_img)
ex_ht_All  <- extract(img_data,actual_data_transformed)
ex_ht_All <- as.data.frame(ex_ht_All)
ex_ht_All$NDVI <- (ex_ht_All$B8 - ex_ht_All$B4) / (ex_ht_All$B8 + ex_ht_All$B4)
predicted_heights_all <- predict(rfFit1, ex_ht_All)
frame <- as.data.frame(predicted_heights_all)
### Plottting actual Vs real heights. 
plot(actual_values$`2019`*0.01,predicted_heights_all)


### Actual 2015 Subplot
shapefile_2015 <- '2015PSP_1764.shp'
shapefile_2018 <- '2018PSP_1764.shp'
ht_data_15 <- readOGR(shapefile_2015)
ht_data_18 <- readOGR(shapefile_2018)

#### Measuring the heights of 2015
shp_data_2015 <- spTransform(ht_data_15, crs_img)
ex_ht_2015 <- as.data.frame(extract(img_data,shp_data_2015))
ex_ht_2015$NDVI <- (ex_ht_2015$B8 - ex_ht_2015$B4) / (ex_ht_2015$B8 + ex_ht_2015$B4)
predicted_heights_2015 <- predict(rfFit1, ex_ht_2015)
actual_2015_ht <- actual_values[actual_values$Year==2015,]$`2019`
plot((actual_2015_ht+0.48)*.01,predicted_heights_2015)

## Underestimation of Heights ###
((actual_2015_ht+0.48)*.01)-predicted_heights_2015



####### Filter data for 2017 ####

shapefile_2015 <- '2015PSP_1764.shp'
shapefile_2016 <- '2016PSP_1764.shp'
shapefile_2017 <- '2017PSP_1764.shp'
shapefile_2018 <- '2018PSP_1764.shp'

ht_data_15 <- readOGR(shapefile_2015)
ht_data_16 <- readOGR(shapefile_2016)
ht_data_17 <- readOGR(shapefile_2017)
ht_data_18 <- readOGR(shapefile_2018)

#### Measuring the heights of 2015 ####
shp_data_2015 <- spTransform(ht_data_15, crs_img)
ex_ht_2015 <- as.data.frame(extract(img_data,shp_data_2015))
ex_ht_2015$NDVI <- (ex_ht_2015$B8 - ex_ht_2015$B4) / (ex_ht_2015$B8 + ex_ht_2015$B4)
predicted_heights_2015 <- predict(rfFit1, ex_ht_2015)
actual_2015_ht <- actual_values[actual_values$Year==2015,]$`2019`
plot((actual_2015_ht+0.48)*.01,predicted_heights_2015)

## Underestimation of Heights ###
((actual_2015_ht+0.48)*.01)-predicted_heights_2015

### Code to investigate the height values .######

### Plotting the GEDi distributions ####

hist((GEDI_data@data$rh98)^(1/3))
hist(GEDI_data@data$rh98)


df_heights <- data.frame(
  'source'= c(rep('actual', length(actual_values$`2019`)),
              rep('gedi', length(GEDI_data@data$rh98))),
  'height' = c((actual_values$`2019` +0.48) * 0.01, GEDI_data@data$rh98)
)

df_heights %>% 
  ggplot(aes(x = height, fill = source)) +
  geom_density() +
  theme_minimal()+
  ggtitle("Density")+
  theme(plot.title=element_text(hjust=0.5))

## Linear model of the data. 
lm(actual_2015~predicted_heights_2015)
plot(((actual_2015 +0.48) * 0.01)~predicted_heights_2015)
#abline(a)


#### Measuring heights for 2016 ####

shp_data_2016 <- spTransform(ht_data_16, crs_img)
ex_ht_2016 <- as.data.frame(extract(img_data,shp_data_2016))
ex_ht_2016$NDVI <- (ex_ht_2016$B8 - ex_ht_2016$B4) / (ex_ht_2016$B8 + ex_ht_2016$B4)
predicted_heights_2016 <- predict(rfFit1, ex_ht_2016)
actual_2016_ht <- actual_values[actual_values$Year==2016,]$`2019`
plot((actual_2016_ht+0.48)*.01,predicted_heights_2016)

# Underestimation 
((actual_2016_ht+0.48)*.01)-predicted_heights_2016

#### Measuring heights for 2017 ####

shp_data_2017 <- spTransform(ht_data_17, crs_img)
ex_ht_2017 <- as.data.frame(extract(img_data,shp_data_2017))
ex_ht_2017$NDVI <- (ex_ht_2017$B8 - ex_ht_2017$B4) / (ex_ht_2017$B8 + ex_ht_2017$B4)
predicted_heights_2017 <- predict(rfFit1, ex_ht_2017)
actual_2017_ht <- actual_values[actual_values$Year==2017,]$`2019`
plot((actual_2017_ht+0.48)*.01,predicted_heights_2017)

# Underestimation 
((actual_2017_ht+0.48)*.01)-predicted_heights_2017

### 2018 ####
shp_data_2018 <- spTransform(ht_data_18, crs_img)
ex_ht_2018 <- as.data.frame(extract(img_data,shp_data_2018))
ex_ht_2018$NDVI <- (ex_ht_2018$B8 - ex_ht_2018$B4) / (ex_ht_2018$B8 + ex_ht_2018$B4)
predicted_heights_2018 <- predict(rfFit1, ex_ht_2018)
actual_2018_ht <- actual_values[actual_values$Year==2018,]$`2019`
plot((actual_2018_ht+0.48)*.01,predicted_heights_2018)

# Underestimation 
((actual_2018_ht+0.48)*.01)-predicted_heights_2018


### Plotting a subplot of all 4 plots ####


# Set the layout for the subplots
par(mfrow = c(2, 2))

# Plot 2015
plot((actual_2015_ht + 0.48) * 0.01, predicted_heights_2015, main = "2015")

# Plot 2016
plot((actual_2016_ht + 0.48) * 0.01, predicted_heights_2016, main = "2016")

# Plot 2017
plot((actual_2017_ht + 0.48) * 0.01, predicted_heights_2017, main = "2017")

# Plot 2018
plot((actual_2018_ht + 0.48) * 0.01, predicted_heights_2018, main = "2018")

# Reset the layout to default
par(mfrow = c(1, 1))


### 
plot(((actual_2018_ht+0.48)*.01)-predicted_heights_2018)


plot.new()
title(main = main_title, line = -1, cex.main = 1.5)

# Set the layout for the plots
par(mfrow = c(2, 2), main = "Differences in heights, Actual vs Predicted")

# Plot 2015

plot(((actual_2015_ht + 0.48) * 0.01) - predicted_heights_2015, main = "2015", ylab = "")


# Plot 2016
plot(((actual_2016_ht + 0.48) * 0.01) - predicted_heights_2016, main = "2016",ylab = "")

# Plot 2017
plot(((actual_2017_ht + 0.48) * 0.01) - predicted_heights_2017, main = "2017",ylab = "")

# Plot 2018
plot(((actual_2018_ht + 0.48) * 0.01) - predicted_heights_2018, main = "2018", ylab = "")

# Reset the layout to default
#main title for the overall plot
main_title <- "Differences observed"


# Reset the layout to default
par(mfrow = c(1, 1))

plot(((actual_2018_ht + 0.48) * 0.01) , predicted_heights_2018, main = "2018", ylab = "")

lm(((actual_2018_ht + 0.48) * 0.01) ~ predicted_heights_2018, main = "2018", ylab = "")

plot(((actual_2016_ht + 0.48) * 0.01) , predicted_heights_2016, main = "2016", ylab = "")

lm(((actual_2016_ht + 0.48) * 0.01) ~ predicted_heights_2016, main = "2016", ylab = "")



######### Plotting the average difference in predictions ####
# Read TIFF image data

tiff_file_2019 <- "S2_1764_2019-29-11-2019_allbands.tiff"
img_data_2019 <- stack(tiff_file_2019)
merged_shapefile <- rbind(data_2015, data_2016, data_2017, data_2018)
actual_data_transformed <- spTransform(merged_shapefile, crs_img)
ex_ht_2019  <- extract(img_data_2019,actual_data_transformed)
ex_ht_2019 <- as.data.frame(ex_ht_2019)
ex_ht_2019$NDVI <- (ex_ht_2019$B8 - ex_ht_2019$B4) / (ex_ht_2019$B8 + ex_ht_2019$B4)
predicted_heights_2019_tiff <- predict(rfFit1, ex_ht_2019)
frame <- as.data.frame(predicted_heights_2019_tiff)

plot(actual_values$`2019`*0.01,predicted_heights_2019_tiff)
plot(actual_values$`2019`*0.01-predicted_heights_2019_tiff)


stdev <- sd(actual_values$`2019` * 0.01 - predicted_heights_2019_tiff)
# Calculate the average
average <- mean(actual_values$`2019` * 0.01 - predicted_heights_2019_tiff)

# Plot the line
plot(actual_values$`2019` * 0.01 - predicted_heights_2019_tiff, type = "l", ylim = c(min(actual_values$`2019` * 0.01 - predicted_heights_2019_tiff), max(actual_values$`2019` * 0.01 - predicted_heights_2019_tiff)), ylab = "Difference", xlab = "Index")
abline(h = average, col = "red")


#### Training the model using 2019 GEDI heights

############## ACTUAL ######################
##### Using 2019 data #####


gedi_file <- "1764_GEDI_2019.shp"
tiff_file <- "S2_1764_2019-29-11-2019_allbands.tiff"

#performRegression <- function(gedi_file, tiff_file, bands) {
# Read GEDI data
GEDI_data <- readOGR(gedi_file)

######### TIFF image data ####
GEDI_data <- readOGR(gedi_file)


# Read TIFF image data
img_data <- stack(tiff_file)
bands <- rh98~.

# Extract image values at plot locations
img_val <- extract(img_data, GEDI_data)
ht_data <- as.data.frame(cbind(GEDI_data, img_val))

# Remove the last two columns of coordinates
htData <- ht_data[, -c((ncol(ht_data) - 1):ncol(ht_data))]

# Select only one height data (e.g., rh98) and image data columns
htData <- select(htData, rh98, colnames(img_val))

# Assuming htData contains the necessary columns (Red and NIR bands)
# htData$NDVI <- (htData$NIR - htData$Red) / (htData$NIR + htData$Red)

# Assuming htData contains the necessary columns (Red and NIR bands)
htData$NDVI <- (htData$B8 - htData$B4) / (htData$B8 + htData$B4)

## NDVI can be tddhought of as the difference between the NIR 
## and Red because it would correspond to more water so makes more sense. 
### HtData in the stuff. 

mean_y <- mean(htData[, 1])
sd_y <- sd(htData[, 1])

# Set a threshold for outlier detection (e.g., 3 times the standard deviation)
threshold <- 1 * sd_y

# Identify the indices of the anomalous points
anomalous_indices <- which(abs(htData[, 1] - mean_y) > threshold)

# Remove the anomalous points from the data frame
clean_frac <- htData[-anomalous_indices, ]

frac <- createDataPartition(clean_frac$rh98, p = .65, list = FALSE, group = 10)

## New variable weight

# Histogram of densities RH98
ggplot(clean_frac, aes(x = rh98)) +
  geom_density()

d <- density(clean_frac$rh98)
plot(d, main="Kernel Density 98th Relative Height Metric")
polygon(d, col="red", border="blue")

hist(htData$rh98)

#### The distribution of the various bands 
library(ggplot2)

ggplot(clean_frac) +
  #geom_density(aes(x = rh98), color = "blue", fill = "lightblue", alpha = 0.5, show.legend = TRUE) +
  geom_density(aes(x = B7), color = "red", fill = "red", alpha = 0.5, show.legend = TRUE) +
  geom_density(aes(x = NDVI), color = "green", fill = "green", alpha = 0.5, show.legend = TRUE) +
  geom_density(aes(x = B9), color = "blue", fill = "blue", alpha = 0.5, show.legend = TRUE) +
  geom_density(aes(x = B8), color = "blue", fill = "blue", alpha = 0.5, show.legend = TRUE) +
  xlab("Variable") +
  ylab("Density") +
  ggtitle("Kernel Density Plots") +
  theme_minimal()

# Testing and training split. 
training <- htData[frac, ]
testing <- htData[-frac, ]

# Regression modeling
fitControl <- trainControl(method = "cv", number = 5, savePredictions = TRUE)
rfGrid <- expand.grid(mtry = c(1:6))

set.seed(1)
rfFit1 <- train(
  bands,
  data = training,
  method = "rf",
  trControl = fitControl,
  verbose = FALSE,
  importance = TRUE,
  tuneGrid = rfGrid
)

# Plot variable importance
rfImp <- varImp(rfFit1, scale = FALSE)
plot(rfImp, main = "Variable Importance")

# Evaluate model performance
predictedHt <- predict(rfFit1, testing[-1])
correlation <- cor(predictedHt, testing[, 1])^2
rmse <- sqrt(mean((predictedHt - testing[, 1])^2))

# Plot predicted vs actual heights
plot(testing[, 1], predictedHt, pch = 16, cex.axis = 1.5, cex.lab = 1.5, col = "blue",
     main = "GEDI vs Model Predicted Height\n RF Regression\n",
     xlim = c(0, 15), ylim = c(0, 15), xlab = "GEDI height (m)", ylab = "Model height (m)")
abline(1.123e-15, 1)

# Print RMSE
print(paste("rmse: ",rmse))
print(paste("R2: ",correlation))
# Access the p-value
cor_test <- cor.test(testing[, 1], predictedHt)
p_value <- cor_test$p.value

# print(paste("Correlation test: ",cor_test))
# Check the statistical significance at a significance level (e.g., alpha = 0.05)

if (p_value < 0.05) {
  print(paste("The correlation is statistically significant since p-value ", p_value, "is less than 0.05."))
} else {
  print(paste("The correlation is not statistically significant p-value =", p_value))
}

### Finding which of the Bands has the highest importance score
rfImp <- varImp(rfFit1, scale = FALSE)
plot(rfImp, main = "Variable Importance")



#### Calculations for the 
tiff_file_2019 <- "S2_1764_2019-29-11-2019_allbands.tiff"
img_data_2019 <- stack(tiff_file_2019)
merged_shapefile <- rbind(data_2015, data_2016, data_2017, data_2018)
actual_data_transformed <- spTransform(merged_shapefile, crs_img)
ex_ht_2019  <- extract(img_data_2019,actual_data_transformed)
ex_ht_2019 <- as.data.frame(ex_ht_2019)
ex_ht_2019$NDVI <- (ex_ht_2019$B8 - ex_ht_2019$B4) / (ex_ht_2019$B8 + ex_ht_2019$B4)
predicted_heights_2019_tiff <- predict(rfFit1, ex_ht_2019)
frame <- as.data.frame(predicted_heights_2019_tiff)

plot(actual_values$`2019`*0.01,predicted_heights_2019_tiff)
plot(actual_values$`2019`*0.01-predicted_heights_2019_tiff)


# Calculate the average
average <- mean(actual_values$`2019` * 0.01 - predicted_heights_2019_tiff)

stdev_2019 <- sd(actual_values$`2019` * 0.01 - predicted_heights_2019_tiff)
# Plot the line
plot(actual_values$`2019` * 0.01 - predicted_heights_2019_tiff, type = "l", ylim = c(min(actual_values$`2019` * 0.01 - predicted_heights_2019_tiff), max(actual_values$`2019` * 0.01 - predicted_heights_2019_tiff)), ylab = "Difference", xlab = "Index")
abline(h = average, col = "red")


####### Coding ######
gedi_file_water <- "1764_GEDI_2021_Sea.shp"
gedi_file <- "1764_GEDI_2021.shp"
tiff_file <- "S2_1764_2021_allbands.tiff"

#performRegression <- function(gedi_file, tiff_file, bands) {
# Read GEDI data
GEDI_data <- readOGR(gedi_file)
GEDI_data_water <- readOGR(gedi_file_water)


# Read TIFF image data
img_data <- stack(tiff_file)


library(sf)

# Read the vector file using sf
gedi_data_water <- st_read(gedi_file_water)


# Check the data
head(gedi_data)