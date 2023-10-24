### Model Gatherer

model_2020<- readRDS("Models, summarymetrics_predictions_2020_cropped.rds")
model_2019_s = readRDS("Models, summarymetrics_predictions_2019_s_cropped.rds")
model_2019 = readRDS("Models, summarymetrics_predictions_2019_cropped.rds")


### Plot testing ####
extract_plot_with_title <- function(result, variable) {
  plot <- result$performance_metrics$plot
  plot + ggtitle(variable)
}

### Fig. 10 Plotting the testing set### 
vari <- c("rh10" , "rh20","rh30","rh40", "rh50", "rh60","rh70","rh80","rh90","rh95","rh98","rh100")

plot_list_with_titles <- mapply(extract_plot_with_title, model_2019, vari, SIMPLIFY = FALSE)
do.call(grid.arrange, c(plot_list_with_titles, ncol = 2, top = "GEDI Testing set - Model 2019"))


plot_list_with_titles <- mapply(extract_plot_with_title, model_2019_s, vari, SIMPLIFY = FALSE)
do.call(grid.arrange, c(plot_list_with_titles, ncol = 2, top = "GEDI Testing set - Model 2019_s"))


plot_list_with_titles <- mapply(extract_plot_with_title, model_2020, vari, SIMPLIFY = FALSE)
do.call(grid.arrange, c(plot_list_with_titles, ncol = 2, top = "GEDI Testing set - Model 2020"))


###
#### 25/08/23.  - Stuff there ####

model_summaries <- list()
for(i in 1:12) {
  model_summaries[[i]] <- summary(model_2020[[i]]$model)
}

### Some A 
predictions <- list()
for(i in 1:12) {
  predictions[[i]] <- predict(model_2020[[i]]$model, newdata = extracted_values_df)
}

#### 26/08/23 ####
aperformance_metrics <- list()
for(i in 1:12) {
  aperformance_metrics[[i]] <- model_2020[[i]]$performance_metrics
}


##### Testing
predictions <- list()
for(i in 1:12) {
  predictions[[i]] <- model_2020[[i]]$predictions
}

####

#### Fig. 12 Trying a performance ###
# Initialize an empty data frame to store all performance metrics
all_metrics_df <- data.frame()

# List of all models
all_models <- list(model_2020 = model_2020, model_2019 = model_2019, model_2019_s = model_2019_s)

# Assuming vari contains the Rh values in the order you want
#vari_ordered <- c("rh10", "Rh20", "Rh30", "Rh40", "Rh50", "Rh60", "Rh70", "Rh80", "Rh90", "Rh95","Rh98","Rh100")

vari_ordered <- c("rh10" , "rh20","rh30","rh40", "rh50", "rh60","rh70","rh80","rh90","rh95","rh98","rh100")

# Loop through each model
for(model_name in names(all_models)) {
  model <- all_models[[model_name]]
  
  # Loop through each Rh variable (assuming 12 Rh variables in each model)
  for(i in 1:12) {
    rh_name <- vari_ordered[i]
    metrics <- model[[i]]$performance_metrics
    
    # Create a temporary data frame for the current Rh variable and model
    temp_df <- data.frame(
      Model = model_name,
      Rh = rh_name,
      Mean = metrics$mean,
      MAE = metrics$mae,
      RMSE = metrics$rmse,
      R = metrics$correlation,
      R2 = metrics$R2,
      P_Value = metrics$p_value
    )
    
    # Append to the main data frame
    all_metrics_df <- rbind(all_metrics_df, temp_df)
  }
}



all_metrics_long <- gather(all_metrics_df, Metric, Value, -Model, -Rh)
all_metrics_long$Rh <- factor(all_metrics_long$Rh, levels = vari_ordered)


# Custom labels
custom_labeller <- as_labeller(c(MAE = "MAE (m)", R = "r", R2 = "R2", P_Value = "P_Value", Mean = "Mean (m)", RMSE = "RMSE (m)"))

# Plot
ggplot(all_metrics_long, aes(x = Rh, y = Value, color = Model)) +
  geom_point() +
  facet_wrap(~ Metric, scales = "free_y", labeller = custom_labeller) +
  labs(title = "Performance Metrics for Each Model on Test data",
       x = "Rh value",
       y = "Metric Value") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5))

#### Mkaing the P value not look distored


#### Making p value not look distorted #####
ggplot(all_metrics_long, aes(x = Rh, y = Value, color = Model)) +
  geom_point() +
  facet_wrap(~ Metric, scales = "free_y", labeller = custom_labeller) +
  labs(title = "Performance Metrics for Each Model on Test data",
       x = "Rh value",
       y = "Metric Value") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5)) +
  scale_y_continuous(trans = "log10", breaks = scales::trans_breaks("log10", function(x) 10^x),
                     labels = scales::trans_format("log10", scales::math_format(10^.x)))



#######

filtered_metrics_long <- all_metrics_long %>% 
  filter(!(Model == "model_2019" & Metric == "P_Value" & Value > 0.3))

filtered_metrics_long <- filtered_metrics_long %>% 
  filter(!(Model == "model_2019_s" & Metric == "P_Value" & Value > 0.004))


ggplot(filtered_metrics_long, aes(x = Rh, y = Value, color = Model)) +
  geom_point() +
  facet_wrap(~ Metric, scales = "free_y") +
  labs(title = "Performance Metrics of Each Model and Rh Variable",
       x = "Rh Variable",
       y = "Metric Value") +
  theme_minimal()

####

### Fig. 11: Heatmpa

# Assuming all_metrics_df contains the metrics for the testing data

# Z-score normalization for the Value column for each Metric
all_metrics_long <- all_metrics_long %>%
  group_by(Metric) %>%
  mutate(Normalized_Value = (Value - mean(Value)) / sd(Value)) %>%
  ungroup()

# Plot the heatmap using the normalized values
ggplot(all_metrics_long, aes(x = Rh, y = Model)) +
  geom_tile(aes(fill = Normalized_Value), color = "white") +
  scale_fill_gradient2(low = "red", mid = "white", high = "steelblue", midpoint = 0) +
  facet_wrap(~ Metric, scales = "free") +
  labs(title = "Z-score Normalized Metrics Heatmap on Test Data",
       x = "Rh Variable",
       y = "Model",
       fill = "Normalized Metric Value") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 70, vjust = 0.5))


######


### Fig. 11: Plotting the field and the test data ####

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

ggplot(all_metrics_long, aes(x = Rh, y = Model)) +
  geom_tile(aes(fill = Value), color = "white") +
  scale_fill_gradient2(low = "red", mid = "white", high = "steelblue", midpoint = 0) +
  facet_wrap(~ Metric, scales = "free", labeller = custom_labeller) +
  labs(title = "Metrics Heatmap on Test data",
       x = "Rh Variable",
       y = "Model",
       fill = "Value") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 70, vjust = 0.5))


