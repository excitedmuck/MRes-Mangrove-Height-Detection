## Goal is to carry out some sort of spatio temporal clustering
### Point is to run a machine leanring model to show the nature of any change in this region. 
# Date: 22 08 23 - Plotting the band values
### This isnt working
libraries <- c("readxl", "caret", "randomForest", "raster", "openxlsx", "Metrics", "rgdal", "dplyr", "rgeos", "ggplot2", "reshape2", "sf", "gridExtra", "xgboost", "gbm")
lapply(libraries, library, character.only = TRUE)

### For the year 2019
tiff_file <- "S2_1764_2019_Jan-Aug_Cl_15.tif"
img_data <- stack(tiff_file)
model_2020<- readRDS("Models, summarymetrics_predictions_2020_cropped.rds")
model_2019_s = readRDS("Models, summarymetrics_predictions_2019_s_cropped.rds")
model_2019 = readRDS("Models, summarymetrics_predictions_2019_cropped.rds")

vari <- c("rh10" , "rh20","rh30","rh40", "rh50", "rh60","rh70","rh80","rh90","rh95","rh98","rh100")
### Creating the True testing set. 
# creating the Independent set to be predicted. 
tiff_file <- "S2_1764_2019_Jan-Aug_Cl_15.tif" ### replace with 2018 Tiff file
file2019 <- "Actual Heights 2019 with height.geojson"
data2019 <- st_read(file2019)

img_data <- stack(tiff_file)
GEDI_transformed <- st_transform(data2019, crs(img_data))
band_vals  <- raster::extract(img_data,GEDI_transformed)

# Convert the extracted values into a data frame
extracted_values_df <- as.data.frame(band_vals) # 1299 shots

# ex_ht_All_df now contains the extracted raster values along with all the columns of actual_data_transformed
extracted_values_df$NDVI <- (extracted_values_df$B8 - extracted_values_df$B4) / (extracted_values_df$B8 + extracted_values_df$B4)
trueheights <- data2019$Name

all_models <- list(model_2019 = model_2019, model_2019_s = model_2019_s, model_2020 = model_2020)


##### Fig. 20: The code that worked. ####

library(caret)
library(dplyr)

calculate_metrics <- function(actual, predicted) {
  rmse_val <- sqrt(mean((actual - predicted)^2))
  mae_val <- mean(abs(actual - predicted))
  cor_val <- cor(actual, predicted)
  R2 <- cor_val^2
  p_val <- cor.test(actual, predicted)$p.value
  mean_val <- mean(predicted)
  return(list(rmse = rmse_val, mae = mae_val, p_val = p_val, mean = mean_val, R2 = R2, r = cor_val))
}

# Your existing code for populating all_metrics_df
all_metrics_df <- data.frame()
for(model_name in names(all_models)) {
  model_list <- all_models[[model_name]]
  for(i in 1:length(vari)) {
    v <- vari[i]
    model <- model_list[[i]]$model
    predictions <- predict(model, newdata = extracted_values_df)
    metrics <- calculate_metrics(trueheights, predictions)
    temp_df <- data.frame(
      Model = model_name,
      Variable = v,
      RMSE = metrics$rmse,
      MAE = metrics$mae,
      P_Value = metrics$p_val,
      Mean = metrics$mean,
      R2 = metrics$R2,
      R = metrics$r
    )
    all_metrics_df <- rbind(all_metrics_df, temp_df)
  }
}

#### Fig.10: Each model on Field data #####
# Convert to long format
all_metrics_long <- gather(all_metrics_df, Metric, Value, -Model, -Variable)

# Set the levels of the factor according to 'vari'
all_metrics_long$Variable <- factor(all_metrics_long$Variable, levels = vari)

# Custom labels
custom_labeller <- as_labeller(c(MAE = "MAE (m)", R = "r", R2 = "R2", P_Value = "P_Value", Mean = "Mean (m)", RMSE = "RMSE (m)"))


### Fig 10: Final R squared comparison ####
ggplot(all_metrics_long, aes(x = Variable, y = Value, color = Model)) +
  geom_point() +
  facet_wrap(~ Metric, scales = "free_y", labeller = custom_labeller) +
  labs(title = "Performance Metrics of Each Model on Field data",
       x = "Rh value",
       y = "Metric Value") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5))


##### Fig. 11 ####
# Adjust the metrics such that higher values are better
scaled_metrics <- all_metrics_long %>%
  group_by(Metric) %>%
  mutate(
    adjusted_value = case_when(
      Metric %in% c("MAE", "RMSE", "P_Value") ~ -1 * Value,  # For metrics where lower is better
      TRUE ~ Value  # For R2, as higher is better
    ),
    z_value = scale(adjusted_value)  # Scale the adjusted values
  ) %>%
  ungroup()

# Assign weights to the metrics
weights <- c(R2 = 0.7, MAE = 0.4, RMSE = 0.4, P_Value = 0.4)

# Calculate the weighted composite score
composite_score <- scaled_metrics %>% 
  filter(Metric %in% names(weights)) %>% 
  mutate(weight = weights[Metric]) %>% 
  group_by(Model, Variable) %>% 
  summarise(composite_score = sum(z_value * weight, na.rm = TRUE)) %>% 
  ungroup()

#FIgl 100…L  Plot the composite scores
library(ggplot2)

ggplot(composite_score, aes(x = Variable, y = Model)) +
  geom_tile(aes(fill = composite_score), color = "white") +
  scale_fill_gradient(low = "red", high = "steelblue") +
  labs(title = "Composite Scores Heatmap - Field data",
       x = "Rh Variable",
       y = "Model",
       fill = "Score") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

##### Fig. 12: Final Heatmap #####

# Set the levels of the factor according to 'vari'
all_metrics_long$Variable <- factor(all_metrics_long$Variable, levels = vari)

# Assuming all_metrics_long is your data frame containing the metrics
ggplot(all_metrics_long, aes(x = Variable, y = Model)) +
  geom_tile(aes(fill = Value), color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  facet_wrap(~ Metric, scales = "free") +
  labs(title = "Metrics Heatmap on Field data",
       x = "Rh Variable",
       y = "Model",
       fill = "Metric Value") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 70, vjust = 0.5))


#####
### Fig. 11: Heatmpa of hte metris
# Custom labeller function
custom_labeller <- function(variable, value) {
  new_labels <- c(
    "MAE" = "MAE (m)",
    "Mean" = "Mean (m)",
    "P_Value" = "P Value",
    "R" = "R",
    "R2" = "R^2",
    "RMSE" = "RMSE (m)"
  )
  return(new_labels[value])
}

ggplot(all_metrics_long, aes(x = Variable, y = Model)) +
  geom_tile(aes(fill = Value), color = "white") +
  scale_fill_gradient2(low = "red", mid = "white", high = "steelblue", midpoint = 0) +
  facet_wrap(~ Metric, scales = "free", labeller = custom_labeller) +
  labs(title = "Metrics Heatmap on Field data",
       x = "Rh Variable",
       y = "Model",
       fill = "Value") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 70, vjust = 0.5))

### Fig. 10: Normalised metrics

# Normalize the Value column for each Metric
all_metrics_long <- all_metrics_long %>%
  group_by(Metric) %>%
  mutate(Normalized_Value = (Value - min(Value)) / (max(Value) - min(Value))) %>%
  ungroup()

# Plot the heatmap using the normalized values
ggplot(all_metrics_long, aes(x = Variable, y = Model)) +
  geom_tile(aes(fill = Normalized_Value), color = "white") +
  scale_fill_gradient2(low = "red", mid = "white", high = "steelblue", midpoint = 0.5) +
  facet_wrap(~ Metric, scales = "free") +
  labs(title = "Normalized Metrics Heatmap on Field data",
       x = "Rh Variable",
       y = "Model",
       fill = "Normalized Metric Value") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 70, vjust = 0.5))


#### Fig. 11:  Z score normlaised metrics on Field data ####

# Z-score normalization for the Value column for each Metric
all_metrics_long <- all_metrics_long %>%
  group_by(Metric) %>%
  mutate(Normalized_Value = (Value - mean(Value)) / sd(Value)) %>%
  ungroup()

# Plot the heatmap using the normalized values
ggplot(all_metrics_long, aes(x = Variable, y = Model)) +
  geom_tile(aes(fill = Normalized_Value), color = "white") +
  scale_fill_gradient2(low = "red", mid = "white", high = "steelblue", midpoint = 0) +
  facet_wrap(~ Metric, scales = "free") +
  labs(title = "Z-score Normalized Metrics Heatmap on Field data",
       x = "Rh Variable",
       y = "Model",
       fill = "Normalized Metric Value") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 70, vjust = 0.5))





