
libraries <- c("readxl", "caret", "randomForest", "raster", "openxlsx", "Metrics", "rgdal", "dplyr", "rgeos", "ggplot2", "reshape2", "sf", "gridExtra", "xgboost", "gbm")
lapply(libraries, library, character.only = TRUE)

library(rgdal)
library(sf)
library(raster)

# Read the shapefiles
vcs <- "VCS1764_PAA_UTM46N_2018.shp"
VCS1764 <- st_read(vcs)

gedi_file <- "1764_GEDI_2019_All_rh_150823_box.shp"
tiff_file <- "S2_1764_2019_Jan-Aug_Cl_15.tif"
img_data <- stack(tiff_file)
GEDI_data <- st_read(gedi_file)
plot(GEDI_data)
# Convert VCS1764 to the CRS of GEDI_data
VCS1764_transformed <- st_transform(VCS1764, st_crs(GEDI_data))

# Crop GEDI_data to VCS1764 region
GEDI_cropped <- st_intersection(GEDI_data, VCS1764_transformed)

plot(GEDI_cropped)

# Transform GEDI_cropped (which is an sf object) to the CRS of img_data
GEDI_transformed <- st_transform(GEDI_cropped, crs(img_data))

# Convert the transformed sf object back to an sp object for compatibility with raster::extract
GEDI_sp <- as(GEDI_transformed, "Spatial")

# Extract values from the raster stack for the locations in GEDI_sp
band_vals <- raster::extract(img_data, GEDI_sp)
# Convert the extracted values into a data frame
extracted_values_df <- as.data.frame(band_vals) # 1299 shots

# Add the columns of actual_data_transformed back to the data frame
ex_ht_All_df <- cbind(GEDI_transformed, extracted_values_df)

# ex_ht_All_df now contains the extracted raster values along with all the columns of actual_data_transformed
ex_ht_All_df$NDVI <- (ex_ht_All_df$B8 - ex_ht_All_df$B4) / (ex_ht_All_df$B8 + ex_ht_All_df$B4)

filtered_df <- ex_ht_All_df %>%
  filter(sensitivit > 0.95 & quality_fl == 1)

#columns_to_remove <- c("Latitude", "OID_", "Name", "Year","Unit","Longitude", "MSK_CLDPRB", "MSK_SNWPRB", "QA10", "0A20", "0A60")
columns_to_remove <- c( "MSK_SNWPRB", "QA10", "QA20", "QA60")

# Remove the specified columns
ex_ht_All_df_rm <- filtered_df[, !(names(filtered_df) %in% columns_to_remove)]
size <-  dim(ex_ht_All_df_rm) # 850 shots 

vari <- c("rh10" , "rh20","rh30","rh40", "rh50", "rh60","rh70","rh80","rh90","rh95","rh98","rh100")
predictors <- c("B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B8A", "B9", "B11", "B12", "WVP", "SCL", "TCI_R", "TCI_G", "TCI_B", "NDVI")
### Fig. 21: Plotting for the other correlations ####


cols_to_keep <- c(vari, predictors)
ex_ht_All_df_rm <- ex_ht_All_df_rm[, cols_to_keep]

remove_anomalies <- function(data, columns) {
  for (col in columns) {
    Q1 <- quantile(data[[col]], 0.25, na.rm = TRUE)
    Q3 <- quantile(data[[col]], 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    data <- data[data[[col]] >= lower_bound & data[[col]] <= upper_bound, ]
  }
  return(data)
}


set.seed(123)  # Setting seed for reproducibility
train_indices <- sample(1:nrow(ex_ht_All_df_rm), 0.7 * nrow(ex_ht_All_df_rm))

ex_ht_All_df_rm_df <- as.data.frame(ex_ht_All_df_rm)
ex_ht_All_df_rm_df$geometry <- NULL

cor_matrix <- cor(ex_ht_All_df_rm_df)
# 2. Create the heatmap
heatmap.2(cor_matrix, 
          main="Correlation Heatmap", 
          notecol="black", # color of cell labels
          density.info="none", # turns off density plot inside color legend
          trace="none", # turns off trace lines inside the heatmap
          margins = c(11, 11) # adjust margin for better visualization
)

#### Fig. 91: X coordinate Correlations, leaving out all other GEDI variables ####
# 16/08/23
library(ggplot2)
library(reshape2)

# Assuming cor_matrix is your correlation matrix
cor_melted <- melt(cor_matrix)

vari <- c("rh10" , "rh20","rh30","rh40", "rh50", "rh60","rh70","rh80","rh90","rh95","rh98","rh100")
Var1 <- vari
Var2 <- predicted
# Remove quality_fl and degrade_fl from Var2

# Filter the melted data to exclude rows with Var2 values of quality_fl and degrade_fl
cor_melted_filtered <- cor_melted[cor_melted$Var1 %in% vari & cor_melted$Var2 %in% Var2, ]

# Set the order for Var1 and Var2
cor_melted_filtered$Var1 <- factor(cor_melted_filtered$Var1, levels = vari)
cor_melted_filtered$Var2 <- factor(cor_melted_filtered$Var2, levels = unique(cor_melted_filtered$Var2))

# Plot heatmap
ggplot(cor_melted_filtered, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, 
                       name = "Correlation", limits = c(-1, 1)) +
  theme_minimal() +
  labs(title = paste("Correlation Heatmap for GEDI shots in VCS1764 region, N =",size[1]), x = "GEDI Rh metric", y = "Variable") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

#### SCL ####
count_SCL <- table(ex_ht_All_df_rm_df$SCL)
print(count_SCL)

train_data <- ex_ht_All_df_rm_df[train_indices, ]
test_data <- ex_ht_All_df_rm_df[-train_indices, ]
train_data <- na.omit(train_data)
test_data <- na.omit(test_data)
#train_data <- train_data[complete.cases(train_data), ]
#test_data <- test_data[complete.cases(test_data), ]

# Splitting data into training and testing sets (70% train, 30% test)
# train_data <- ex_ht_All_df_rm[train_indices, ]
# test_data <- ex_ht_All_df_rm[-train_indices, ]
# train_data <- train_data[complete.cases(train_data), ]
# test_data <- test_data[complete.cases(test_data), ]


# Remove anomalies from the training and testing data
train_data_clean <- remove_anomalies(train_data, vari)
test_data_clean <- remove_anomalies(test_data, vari)

#### Fig. 10  Simple random forest model #####
predictors <- c("B1","B2","B3","B4" , "B5" , "B6" ,  "B7", "B8","B8A",   "B9",  "B11" ,"B12", "WVP", "SCL", "TCI_R",    "TCI_G",      "TCI_B",  "NDVI")
# Function to train, predict, and plot
cube_root_transform <- function(x) {
  sign(x) * abs(x)^(1/3)
}

train_data_clean[vari] <- apply(train_data_clean[vari], 2, cube_root_transform)
test_data_clean[vari] <- apply(test_data_clean[vari], 2, cube_root_transform)

predictors <- c("B1","B2","B3","B4" , "B5" , "B6" ,  "B7", "B8","B8A",   "B9",  "B11" ,"B12", "WVP", "SCL", "TCI_R",    "TCI_G",      "TCI_B",  "NDVI")
# Function to train, predict, and plot

train_predict_plot <- function(response_var, train_data, test_data) {
  
  # Set up 5-fold cross-validation
  ctrl <- trainControl(method = "cv", number = 5)
  
  # Fine-tuning grid for Random Forest
  # Adjust the values as needed
  rf_grid <- expand.grid(mtry = c(2, 3, 4, 5, 6, 7, 8, 9, 10))
  
  # Train the Random Forest model with cross-validation and fine-tuning
  rf_model <- train(as.formula(paste(response_var, "~", paste(predictors, collapse = "+"))), 
                    data = train_data, 
                    method = "rf",
                    trControl = ctrl,
                    tuneGrid = rf_grid,
                    ntree = 500)
  
  # Predict on the testing set
  predictions <- predict(rf_model, newdata = test_data)
  
  
  # Evaluate the model's performance
  pred_mean <- mean(predictions)
  mae_val <- mae(test_data[[response_var]], predictions)
  rmse_val <- rmse(test_data[[response_var]], predictions)
  cor_val <- cor(test_data[[response_var]], predictions)
  R2 <- cor_val^2
  p_val <- cor.test(test_data[[response_var]], predictions)$p.value
  
  # Plotting Predicted vs Actual values
  #plot_data <- data.frame(Actual = test_data[[response_var]], Predicted = predictions)
  plot_data <- data.frame(Actual = test_data[[response_var]]^3, Predicted = predictions^3)
  
  p <- ggplot(plot_data, aes(x = Actual, y = Predicted)) +
    geom_point(alpha = 0.5) +
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
    theme_minimal() +
    labs(title = paste(gsub("rh", "Rh", response_var)),
         subtitle = paste("R^2 =", round(R2, 2), 
                          "| RMSE =", round(rmse_val, 2),
                          "| Mean =", round(pred_mean, 2), 
                          "\n Correlation =", round(cor_val, 2),
                          "| p-value =", format(p_val, scientific = TRUE, digits = 3)),
         x = "GEDI Rh (m)", y = "Predicted Ht (m)") +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = rel(0.8), color = "darkblue"),
      axis.title.x = element_text(hjust = 0.5, size = 8),
      axis.title.y = element_text(hjust = 0.5, vjust = 0.5, size = 8)
    )
  
  result <- list(
    model = rf_model,
    predictions = predictions,
    performance_metrics = list(
      mean = pred_mean,
      mae = mae_val,
      rmse = rmse_val,
      correlation = cor_val,
      R2 = R2,
      p_value = p_val, 
      plot = p
    )
  )
  return(result)
}
##### returns RF model 

#vari <- c("rh60")

vari <- c("rh10" , "rh20","rh30","rh40", "rh50", "rh60","rh70","rh80","rh90","rh95","rh98","rh100")

results_all <- lapply(vari, function(v) {
  result <- train_predict_plot(v, train_data_clean, test_data_clean)
})

saveRDS(results_all, "Models, summarymetrics_predictions_2019_s_cropped.rds")

#### Plot
plots <- lapply(results_all, function(res) res$performance_metrics$plot)

grid.arrange(grobs = plots, ncol = 2)


##saveRDS(results_all, "27_08_23_2019_models-2019.rds")


#do.call(grid.arrange, c(plot_list_with_titles, ncol = 2, top = "Training with GEDI shots - Testing set"))


#### Testing on the testing set. ####

library(caret)
library(randomForest)


tiff_file <- "S2_1764_2019_Jan-Aug_Cl_15.tif" ### replace with 2018 Tiff file
file2019 <- "Actual Heights 2019 with height.geojson"
data2019 <- st_read(file2019)
#predictions_df$Field_ht <- data2019$Name

img_data <- stack(tiff_file)
real_transformed <- st_transform(data2019, crs(img_data))
band_vals  <- raster::extract(img_data,real_transformed)

# Convert the extracted values into a data frame
extracted_values_df <- as.data.frame(band_vals) # 1299 shots

# ex_ht_All_df now contains the extracted raster values along with all the columns of actual_data_transformed
extracted_values_df$NDVI <- (extracted_values_df$B8 - extracted_values_df$B4) / (extracted_values_df$B8 + extracted_values_df$B4)
#####


# Predict using the saved models for each variable in 'vari'
predictions_list <- lapply(results_all, function(result) {
  # Retrieve the saved model from the result
  model <- result$model
  
  # Predict using the model on extracted_values_df
  predict(model, newdata = extracted_values_df)^3
})

#####

# Convert the list of predictions to a data frame
predictions_df <- data.frame(predictions_list)
colnames(predictions_df) <- vari

# Merge the predictions with the actual values in extracted_values_df
#write.csv(predictions_df, "Rh60-70-Predictions.csv", row.names = FALSE)

# Add the Field_Height values to predictions_df
library(tidyverse)

# Assuming predictions_df contains the predictions for each variable in 'vari'
predictions_df$Field_ht <- data2019$Name


# Convert the data to long format
predictions_long <- gather(predictions_df, key = "rh_variable", value = "value")

# Adjust the rh_variable levels
vari_levels <- gsub('rh', 'RH', vari)
vari_levels <- c(vari_levels, "Field_ht")

predictions_long$rh_variable <- factor(gsub('rh', 'RH', predictions_long$rh_variable), levels = vari_levels)

#### Fig. 10: The field height distribution on the testing set ####
### Fig. 23: Distribution of predicted heights ####
ggplot(data = predictions_long, aes(x = rh_variable, y = value)) +
  geom_boxplot(aes(fill = (rh_variable == "Field_ht"))) + 
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "grey50")) +
  theme_minimal() +
  labs(title = "Distribution of Predicted Rh Values",
       x = "rh Variable", y = "Height (m)") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(hjust = 0.5),
    axis.title.y = element_text(hjust = 0.5, vjust = 0.5),
    legend.position = "none"  # Hide the legend
  )

library(dplyr)

# Extract Field_ht values
field_ht_values <- predictions_long %>% 
  filter(rh_variable == "Field_ht") %>% 
  pull(value)

# Calculate metrics for each Rh metric
performance_metrics <- predictions_long %>%
  filter(rh_variable != "Field_ht") %>%
  group_by(rh_variable) %>%
  summarise(
    RMSE = sqrt(mean((value - field_ht_values)^2, na.rm = TRUE)),
    MAE = mean(abs(value - field_ht_values), na.rm = TRUE),
    r = cor(value, field_ht_values, use = "complete.obs"),
    R2 = r^2,
    
    p_value = cor.test(value, field_ht_values, method = "pearson")$p.value
  )

print(performance_metrics)


library(tidyr)

long_metrics <- performance_metrics %>%
  gather(key = "Metric", value = "Value", -rh_variable)


library(ggplot2)

ggplot(data = long_metrics, aes(x = rh_variable, y = Value)) +
  geom_bar(stat = "identity", aes(fill = rh_variable), position = "dodge") +
  facet_wrap(~ Metric, scales = "free_y", ncol = 1) +
  theme_minimal() +
  labs(title = "Performance Metrics for Rh Predicted Heights",
       x = "rh Variable") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(hjust = 0.5),
    axis.title.y = element_text(hjust = 0.5, vjust = 0.5),
    legend.position = "none"
  )

#### Fig. 17: Plotting the final completed files #####

library(gridExtra)
actual_data <- predictions_df$Field_ht
# Define a function to calculate metrics
calculate_metrics <- function(actual, predicted) {
  cor_val <- cor(actual, predicted, use = "complete.obs")
  R2 <- cor_val^2
  rmse_val <- sqrt(mean((actual - predicted)^2))
  p_val <- cor.test(actual, predicted)$p.value
  
  return(list(R2 = R2, rmse_val = rmse_val, cor_val = cor_val, p_val = p_val))
}


# Function to predict and plot for each variable
predict_and_plot <- function(v, model_result) {
  # Retrieve the saved model from the result
  model <- model_result$model
  
  # Predict using the model on extracted_values_df
  predictions <- predict(model, newdata = extracted_values_df)
  
  # Calculate metrics
  metrics <- calculate_metrics(actual_data, predictions)
  
  # Plotting Predicted vs Actual values
  plot_data <- data.frame(Actual = actual_data, Predicted = predictions)
  
  p <- ggplot(plot_data, aes(x = Actual, y = Predicted)) +
    geom_point(alpha = 0.5) +
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
    theme_minimal() +
    labs(title = paste(gsub("rh", "Rh", v)),
         subtitle = paste("R^2 =", round(metrics$R2, 3), 
                          "| RMSE =", round(metrics$rmse_val, 3),
                          "| Mean =", round(mean(predictions),2),
                          "\n Correlation =", round(metrics$cor_val, 3),
                          "| p-value =", format(metrics$p_val, scientific = TRUE, digits = 3)),
         x = "Field Ht (m)", y = "Predicted Ht (m)") +
    theme(
      plot.title = element_text(hjust = 0.5, size = 8, face = "bold"),
      plot.subtitle = element_text(hjust = 0.4, size = 8, color = "darkblue"),
      axis.title.x = element_text(hjust = 0.5, size =6 ),
      axis.title.y = element_text(hjust = 0.5, vjust = 0.5, size = 6)
    )
  
  return(p)
}

# Apply the function to each variable in 'vari' using the corresponding model result
predictions_list <- mapply(predict_and_plot, vari, results_all, SIMPLIFY = FALSE)

# Display the plots together
# Fig.10L final 
do.call(grid.arrange, c(predictions_list, ncol = 2, top = "Using the Trained RF model to predict heights in 32 Field Plots"))


#### Plotting the region again