## Objective is to compare All the results we've got.  ####
library(sf)
library(raster)
library(raster)


simradd <- "Mangrove_hmax95_myanmar_clipped.tif"
potapov <- "clipped_forest_height.tif"
lang <- "ETH_GlobalCanopyHeight_10m_2020_N15E093_Map.tif"

#tiff_file_lang <- raster(lang)

file2019 <- "Actual Heights 2019 with height.geojson"
data2019 <- st_read(file2019)
#ex_ht_All_df_rm$Field_ht <- data2019$Name


tiff_file_simradd <- raster(simradd)
tiff_file_potapov <- raster(potapov)
tiff_file_lang <- raster(lang)

extract_and_join <- function(data, tiff_file1, tiff_file2, tiff_file3) {
  # Extract raster values
  values1 <- raster::extract(tiff_file1, st_transform(data, crs(tiff_file1)))
  values2 <- raster::extract(tiff_file2, st_transform(data, crs(tiff_file2)))
  values3 <- raster::extract(tiff_file3, st_transform(data, crs(tiff_file3)))
  # Add extracted values to the original dataframe
  data$simradd_values <- values1
  data$potapov_values <- values2
  data$lang_values <- values3
  return(data)
}
result <- extract_and_join(data2019, tiff_file_simradd, tiff_file_potapov, tiff_file_lang)


result2 <- as.data.frame(result)
# Assuming rh_long is a vector with the same length as nrow(result2)
result2$rh60 <- rh_long$value

# View the desired columns
result2[,c("OID_", "Name", "simradd_values", "potapov_values", "lang_values", "rh60")]

result2[,c("OID_","Name","simradd_values","potapov_values","lang_values")]

cols_to_replace <- c("simradd_values", "potapov_values", "lang_values")

result2[cols_to_replace] <- lapply(result2[cols_to_replace], function(col) {
  ifelse(col == 101, NA, col)
})

final <- result2[,c("namess","Name","simradd_values","potapov_values","lang_values","rh60")]
final <- final %>%
  rename(
    field_data = Name,
    simrad = simradd_values,
    potapov = potapov_values,
    lang = lang_values,
  )
final$namess <- NULL


library(tidyr)

### Fig. 10;  Plot preliminary ####
final_long <- final %>%
  pivot_longer(cols = c(field_data, simrad, potapov, lang), 
               names_to = "variable", 
               values_to = "value")
final_long$index <- rep(1:nrow(final), each = 4)

library(ggplot2)
ggplot(final_long, aes(x = index, y = value, color = variable)) +
  geom_point() +
  labs(title = "Values by Index",
       x = "Index",
       y = "Height (m)") +
  theme_minimal() +
  scale_color_discrete(name = "Variables", breaks = c("field_data", "simrad", "potapov","lang"))



### Fig. 10 Bar PLot  ####

final_summary <- final_long %>%
  group_by(variable) %>%
  summarise(mean = mean(value, na.rm = TRUE), 
            sd = sd(value, na.rm = TRUE))

library(ggplot2)
library(dplyr)

# Sample data creation (you can skip this if you have your own data)

### Fig. 122 ####
final_summary <- final_long %>%
  group_by(variable) %>%
  summarise(mean = mean(value, na.rm = TRUE), 
            sd = sd(value, na.rm = TRUE))

ggplot(final_summary, aes(x = variable, y = mean, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.25, position = position_dodge(0.9)) +
  geom_text(aes(label = sprintf("Mean: %.2f\nSD: %.2f", mean, sd), y = mean + sd), 
            hjust = -0.1, vjust = 1.5, size = 3.5) +
  theme_minimal()



### Fig. 12: Violin plot showing the results from the datasets.  ####
final_summary <- final_long %>%
  group_by(variable) %>%
  summarise(mean = mean(value, na.rm = TRUE), 
            sd = sd(value, na.rm = TRUE))

# Custom labels for x-axis
custom_labels <- c("field_data" = "Field Data", "rh60" = "Rh60-2019", "potapov" = "Potapov-2019", "simrad" = "Simard-2000", "lang" = "Lang-2020")

# Plot using ggplot
# Plot using ggplot with adjusted width for violin and jitter
ggplot(combined_data, aes(x = variable, y = value, fill = variable)) +
  geom_violin(width = 0.8) +  # Increase width of violin
  geom_jitter(width = 0.1, size = 1, alpha = 0.5) +  # Reduce width of jitter
  geom_text(data = final_summary_combined, 
            aes(label = sprintf("Mean: %.2f\n  SD: %.2f", mean, sd), y = -5),  
            hjust = 0.5, vjust = 0, size = 3.5) +
  labs(title = "Comparison of Heights in the 32 Sample Plots of VCS1764", 
       x = "Model", 
       y = "Height (m)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 13, face = "bold"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        axis.text.x = element_text(face = "bold")) +
  scale_x_discrete(labels = custom_labels) +
  theme(legend.title = element_blank())  

library(ggbeeswarm)


# Assuming final_summary_combined contains the statistics for each variable
ggplot(combined_data, aes(x = variable, y = value, color = variable)) +
  geom_beeswarm() +
  geom_text(data = final_summary_combined, 
            aes(label = sprintf("Mean: %.2f\n  SD: %.2f", mean, sd), y = -5),  
            hjust = 0.5, vjust = 0, size = 3.5) +
  labs(title = "Comparison of Heights in the 32 Sample Plots of VCS1764", 
       x = "", 
       y = "Height (m)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 13, face = "bold"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        axis.text.x = element_text(face = "bold")) +
  scale_x_discrete(labels = custom_labels) +
  theme(legend.title = element_blank())


### Fig. 11:  Black labels 
ggplot(combined_data, aes(x = variable, y = value, fill = variable)) +
  geom_violin(width = 0.8) +
  geom_jitter(aes(color = variable), width = 0.1, size = 1, alpha = 0.5) +  # Color by class
  geom_text(data = final_summary_combined, 
            aes(label = sprintf("Mean: %.2f\n  SD: %.2f", mean, sd), y = -5),  
            hjust = 0.5, vjust = 0, size = 3.5) +
  labs(title = "Comparison of Heights in the 32 Sample Plots of VCS1764", 
       x = "Model", 
       y = "Height (m)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 13, face = "bold"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        axis.text.x = element_text(face = "bold")) +
  scale_x_discrete(labels = custom_labels) +
  theme(legend.title = element_blank())




## Density plot

ggplot(final_long, aes(x = value, fill = variable)) +
  geom_density(alpha = 0.5) +
  theme_minimal()

## Box plot

ggplot(final_long, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  theme_minimal()


# Plot the combined data using a violin plot
final_summary_combined <- combined_data %>%
  group_by(variable) %>%
  summarise(mean = mean(value, na.rm = TRUE), 
            sd = sd(value, na.rm = TRUE))

# Set the order for the variable column
combined_data$variable <- factor(combined_data$variable, 
                                 levels = c("field_data", "rh60", "potapov", "simrad","lang"))

# Plot using ggplot
#### Fig. 10: Plot of the distributions ####
ggplot(combined_data, aes(x = variable, y = value, fill = variable)) +
  geom_violin() +
  geom_jitter(width = 0.2, size = 1, alpha = 0.5) +
  geom_text(data = final_summary_combined, 
            aes(label = sprintf("Mean: %.2f\n  SD: %.2f", mean, sd), y = -5),  # Adjust the y value as needed
            hjust = 0.5, vjust = 0, size = 3.5) +
  labs(title = "Comparison of heights in the 32 sample plots of VCS1764", 
       x = NULL, 
       y = "Height (m)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 13)) +
  theme(legend.title = element_blank())  # This line removes the legend title


### Joining all the tables 

final$rh60 <- rh_predictions$rh60

####
final_rh60 <- final
#final_rh70 <- final
# Remove rows with NA in simrad or field_data
final_clean_simrad <- final[!is.na(final$simrad),]
final_clean_potapov <- final[ !is.na(final$potapov) ,]
final_clean_lang <- final[ !is.na(final$lang) ,]

# List of models to evaluate
models <- c("simrad", "potapov", "rh60", "lang")

# Function to calculate metrics
calculate_metrics <- function(model_name, data) {
  clean_data <- data[!is.na(data[[model_name]]),]
  rmse_val <- sqrt(mean((clean_data$field_data - clean_data[[model_name]])^2, na.rm = TRUE))
  r2_val <- cor(clean_data$field_data, clean_data[[model_name]], use = "complete.obs")^2
  mae_val <- mean(abs(clean_data$field_data - clean_data[[model_name]]), na.rm = TRUE)
  return(data.frame(model = model_name, RMSE = rmse_val, R2 = r2_val, MAE = mae_val))
}

# Calculate metrics for each model
metrics_list <- lapply(models, calculate_metrics, data = final)

# Combine into a single data frame
final_metrics <- do.call(rbind, metrics_list)

print(final_metrics)

library(ggplot2)
library(tidyr)

# Convert metrics to long format
metrics_long <- final_metrics %>%
  pivot_longer(cols = c(RMSE, R2, MAE), names_to = "metric", values_to = "value")

# Plot
ggplot(metrics_long, aes(x = metric, y = value, fill = metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = sprintf("%.2f", value)), vjust = -0.5, position = position_dodge(0.9), size = 3) +
  facet_wrap(~model, scales = "free_y") +
  labs(title = "Performance Metrics for Each Model", y = "Value") +
  theme_minimal() +
  theme(legend.position = "none")


####


# Convert the data to long format for plotting
final_long <- final %>%
  pivot_longer(cols = c(simrad, potapov, rh60, lang), names_to = "model", values_to = "predicted")

# Scatter plot with performance metrics
ggplot(final_long, aes(x = field_data, y = predicted, color = model)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  facet_wrap(~model, scales = "free") +
  labs(title = "Actual vs. Predicted Heights", x = "Actual Height (m)", y = "Predicted Height (m)") +
  theme_minimal() +
  geom_text(data = metrics, 
            aes(label = sprintf("RMSE: %.2f\nR2: %.2f\nMAE: %.2f", RMSE, R2, MAE), 
                x = Inf, y = Inf), 
            hjust = "right", vjust = "top", size = 3, inherit.aes = FALSE)+
  theme(plot.title = element_text(hjust = 0.5))
#####
### Loess function ####

#### Final metrics ####
final_metrics$label <- sprintf("RMSE: %.2f\nR2: %.2f\nMAE: %.2f", final_metrics$RMSE, final_metrics$R2, final_metrics$MAE)

# Remove NA values from final_long
final_long_clean <- final_long[!is.na(final_long$predicted),]

# Scatter plot with performance metrics
ggplot(final_long_clean, aes(x = field_data, y = predicted, color = model)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = 'loess', se = TRUE, alpha = 0.2) +  # Add this line for the "fan"
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  facet_wrap(~model, scales = "free") +
  labs(title = "Actual vs. Predicted Heights", x = "Actual Height (m)", y = "Predicted Height (m)") +
  theme_minimal() +
  geom_text(data = final_metrics, 
            aes(label = sprintf("RMSE: %.2f\nR2: %.2f\nMAE: %.2f", RMSE, R2, MAE), 
                x = Inf, y = Inf), 
            hjust = "right", vjust = "top", size = 3, inherit.aes = FALSE) +
  theme(plot.title = element_text(hjust = 0.5))


simrad_length <- nrow(final_long[final_long$model == "simrad",])
print(paste("Number of rows for simrad: ", simrad_length))
#####

### Fig. 10:  Final heights ####

# Scatter plot with performance metrics
ggplot(final_long_clean, aes(x = field_data, y = predicted, color = model)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = 'lm', se = TRUE, alpha = 0.2) +  # Use 'lm' for linear model
  facet_wrap(~tools::toTitleCase(ifelse(model == "simrad", "simard", model)), scales = "free") +  # Change spelling of simrad to simard
  labs(title = "Field vs. Predicted Heights by Model", 
       x = "Field Height (m)", 
       y = "Predicted Height (m)") +
  theme_minimal() +
  geom_text(data = final_metrics, 
            aes(label = sprintf("RMSE: %.2f\nR2: %.2f\nMAE: %.2f", RMSE, R2, MAE), 
                x = Inf, y = Inf), 
            hjust = "right", vjust = "top", size = 3, inherit.aes = FALSE) +
  theme(plot.title = element_text(hjust = 0.5),  # Make the title bold
        strip.text = element_text(face = "bold"),  # Make facet titles bold
        axis.title.x = element_text(),  # Make x-axis title bold
        axis.title.y = element_text()) +  # Make y-axis title bold
  scale_color_discrete(labels = function(x) ifelse(x == "simrad", "Simard", tools::toTitleCase(x)))  # Change spelling of simrad to simard in legend

#### Adding Tidal uncertainty


final_long_clean$upper_bound <- ifelse(final_long_clean$model == "rh60", final_long_clean$predicted + 0.3, NA)
final_long_clean$lower_bound <- ifelse(final_long_clean$model == "rh60", final_long_clean$predicted - 0.3, NA)


ggplot(final_long_clean, aes(x = field_data, y = predicted, color = model)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = 'lm', se = TRUE, alpha = 0.2) +
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound, fill = "Uncertainty"), alpha = 0.2) +
  scale_fill_manual(values = c("Uncertainty" = "grey50")) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = 'lm', se = TRUE, alpha = 0.2) +  # Use 'lm' for linear model
  facet_wrap(~tools::toTitleCase(ifelse(model == "simrad", "simard", model)), scales = "free") +  # Change spelling of simrad to simard
  labs(title = "Field vs. Predicted Heights by Model", 
       x = "Field Height (m)", 
       y = "Predicted Height (m)") +
  theme_minimal() +
  geom_text(data = final_metrics, 
            aes(label = sprintf("RMSE: %.2f\nR2: %.2f\nMAE: %.2f", RMSE, R2, MAE), 
                x = Inf, y = Inf), 
            hjust = "right", vjust = "top", size = 3, inherit.aes = FALSE) +
  theme(plot.title = element_text(hjust = 0.5),  # Make the title bold
        strip.text = element_text(face = "bold"),  # Make facet titles bold
        axis.title.x = element_text(),  # Make x-axis title bold
        axis.title.y = element_text()) +  # Make y-axis title bold
  scale_color_discrete(labels = function(x) ifelse(x == "simrad", "Simard", tools::toTitleCase(x)))  # Change spelling of simrad to simard in legend


###### Fig. 11: Tidal effects ####


# Calculate upper and lower bounds for rh60
final_long_clean$upper_bound <- ifelse(final_long_clean$model == "rh60", final_long_clean$predicted + 0.3, NA)
final_long_clean$lower_bound <- ifelse(final_long_clean$model == "rh60", final_long_clean$predicted - 0.3, NA)

# Scatter plot with performance metrics
ggplot(final_long_clean, aes(x = field_data, y = predicted, color = model)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = 'lm', se = TRUE, alpha = 0.2) +
  #geom_line(aes(y = upper_bound, color = "Uncertainty"), alpha = 0.2) +
  #geom_line(aes(y = lower_bound, color = "Uncertainty"), alpha = 0.2) +
  geom_smooth(aes(y = upper_bound, color = "Tidal effects"), method = "loess", se = FALSE) +
  geom_smooth(aes(y = lower_bound, color = "Tidal effects"), method = "loess", se = FALSE) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = 'lm', se = TRUE, alpha = 0.2) +  # Use 'lm' for linear model
  facet_wrap(~tools::toTitleCase(ifelse(model == "simrad", "simard", model)), scales = "free") +  # Change spelling of simrad to simard
  labs(title = "Field vs. Predicted Heights by Model", 
       x = "Field Height (m)", 
       y = "Predicted Height (m)") +
  theme_minimal() +
  geom_text(data = final_metrics, 
            aes(label = sprintf("RMSE: %.2f\nR2: %.2f\nMAE: %.2f", RMSE, R2, MAE), 
                x = Inf, y = Inf), 
            hjust = "right", vjust = "top", size = 3, inherit.aes = FALSE) +
  theme(plot.title = element_text(hjust = 0.5),  # Make the title bold
        strip.text = element_text(face = "bold"),  # Make facet titles bold
        axis.title.x = element_text(),  # Make x-axis title bold
        axis.title.y = element_text()) +  # Make y-axis title bold
  scale_color_discrete(labels = function(x) ifelse(x == "simrad", "Simard", tools::toTitleCase(x)))  # Change spelling of simrad to simard in legend

####
### Fig. 10: Finally worked yay ####

# Scatter plot with performance metrics
library(ggplot2)
library(dplyr)

# Calculate smoothed values for rh60 model
rh60_data <- subset(final_long_clean, model == "rh60")
smoothed_values <- predict(loess(predicted ~ field_data, data = rh60_data))

# Add upper and lower bounds to the data frame
rh60_data$upper_bound <- smoothed_values + 0.3
rh60_data$lower_bound <- smoothed_values - 0.3

# Scatter plot with performance metrics
ggplot(final_long_clean, aes(x = field_data, y = predicted, color = model)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = 'lm', se = TRUE, alpha = 0.2) +
  geom_ribbon(data = rh60_data, aes(ymin = lower_bound, ymax = upper_bound, fill = "Tidal effects"), alpha = 0.1, fill = "#D8BFD8", color = "NA") +
  geom_smooth(data = rh60_data, aes(y = upper_bound, color = "Tidal effects"), method = "loess", se = FALSE, linetype = "dotted") +
  geom_smooth(data = rh60_data, aes(y = lower_bound, color = "Tidal effects"), method = "loess", se = FALSE, linetype = "dotted") +
  facet_wrap(~tools::toTitleCase(ifelse(model == "simrad", "simard", model)), scales = "free") +
  labs(title = "Field vs. Predicted Heights by Model", x = "Field Height (m)", y = "Predicted Height (m)") +
  theme_minimal() +
  geom_text(data = final_metrics, aes(label = sprintf("RMSE: %.2f\nR2: %.2f\nMAE: %.2f", RMSE, R2, MAE), x = Inf, y = Inf), hjust = "right", vjust = "top", size = 3, inherit.aes = FALSE) +
  theme(plot.title = element_text(hjust = 0.5), strip.text = element_text(face = "bold"), axis.title.x = element_text(), axis.title.y = element_text()) +
  scale_color_discrete(labels = function(x) ifelse(x == "simrad", "Simard", tools::toTitleCase(x)))


####

# Fig. 12: Finally a figure
facet_labels <- c("lang" = "Lang-2020", "potapov" = "Potapov-2019", "simrad" = "Simard-2000", "rh60" = "Rh60-2019")
library(forcats)

ggplot(final_long_clean, aes(x = field_data, y = predicted, color = model)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = 'lm', se = TRUE, alpha = 0.2) +
  geom_ribbon(data = rh60_data, aes(ymin = lower_bound, ymax = upper_bound, fill = "Tidal effects"), alpha = 0.1, fill = "#D8BFD8", color = "NA") +
  geom_smooth(data = rh60_data, aes(y = upper_bound, color = "Tidal effects"), method = "loess", se = FALSE, linetype = "dotted") +
  geom_smooth(data = rh60_data, aes(y = lower_bound, color = "Tidal effects"), method = "loess", se = FALSE, linetype = "dotted") +
  #facet_wrap(~tools::toTitleCase(ifelse(model == "simrad", "simard", model)), scales = "free") +
  facet_wrap(~ fct_relabel(model, ~ facet_labels[.]), scales = "free") +
  labs(title = "Field vs. Predicted Heights by Model", x = "Field Height (m)", y = "Predicted Height (m)") +
  theme_minimal() +
  geom_text(data = final_metrics, aes(label = sprintf("RMSE: %.2f\nR2: %.2f\nMAE: %.2f", RMSE, R2, MAE), x = Inf, y = Inf), hjust = "right", vjust = "top", size = 3, inherit.aes = FALSE) +
  theme(plot.title = element_text(hjust = 0.5), strip.text = element_text(face = "bold"), axis.title.x = element_text(), axis.title.y = element_text()) +
  scale_color_discrete(labels = function(x) ifelse(x == "simrad", "Simard", tools::toTitleCase(x)))


