
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

### Fig. 21: Plotting for the other correlations ####

ex_ht_All_df_rm #wihtin mangrove region

# Assuming ex_ht_All_df_rm is your data frame
all_vars <- names(ex_ht_All_df_rm)
exclude_vars <- c(vari, "AOT", "quality_fl", "degrade_fl")  # Variables to exclude
other_vars <- setdiff(all_vars, exclude_vars)  # All variables in ex_ht_All_df_rm excluding vari and the specified columns

set.seed(123)  # Setting seed for reproducibility
train_indices <- sample(1:nrow(ex_ht_All_df_rm), 0.7 * nrow(ex_ht_All_df_rm))

ex_ht_All_df_rm_df <- as.data.frame(ex_ht_All_df_rm)
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
