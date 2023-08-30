# Date: 16 08 23 - Plotting the heatmaps values ####
# Code to plot the correlations between the data sets

## Part 1: Pots heatmap of correlation - Sole GEDI ####
## Followed by 
## 16 08 23 - Part 2: Training a RF model- RH models
library(readxl)
library(caret)
library(randomForest)
library(raster)
library(openxlsx)
library(Metrics)
library(rgdal)
library(dplyr)
library(rgeos)
library(ggplot2)
library(reshape2)
library(dplyr)
library(rgdal)
library(ggplot2)
library(sf)
library(gridExtra)
library(grid)
### 15/08/23: First plotting the correlation in the whole region. 

gedi_file <- "1764_GEDI_2019_All_rh_150823_box.shp"
#clipped_gedi <- "Clipped_GEDI_data_2019_150823.shp"
tiff_file <- "S2_1764_2019_Jan-Aug_Cl_15.tif"

img_data <- stack(tiff_file)

#gedi_file <- clipped_gedi
GEDI_data <- readOGR(gedi_file)
GEDI_transformed <- spTransform(GEDI_data, crs(img_data))
band_vals  <- raster::extract(img_data,GEDI_transformed)

# Convert the extracted values into a data frame
extracted_values_df <- as.data.frame(band_vals) # 1299 shots

# Add the columns of actual_data_transformed back to the data frame
ex_ht_All_df <- cbind(GEDI_transformed@data, extracted_values_df)

# ex_ht_All_df now contains the extracted raster values along with all the columns of actual_data_transformed
ex_ht_All_df$NDVI <- (ex_ht_All_df$B8 - ex_ht_All_df$B4) / (ex_ht_All_df$B8 + ex_ht_All_df$B4)

filtered_df <- ex_ht_All_df %>%
  filter(sensitivit > 0.95 & quality_fl == 1)

vari <- c("rh10" , "rh20","rh30","rh40", "rh50", "rh60","rh70","rh80","rh90","rh95","rh98","rh100")

#columns_to_remove <- c("Latitude", "OID_", "Name", "Year","Unit","Longitude", "MSK_CLDPRB", "MSK_SNWPRB", "QA10", "0A20", "0A60")
columns_to_remove <- c( "MSK_SNWPRB", "QA10", "QA20", "QA60","rh5")

# Remove the specified columns
ex_ht_All_df_rm <- filtered_df[, !(names(filtered_df) %in% columns_to_remove)]
size <-  dim(ex_ht_All_df_rm) # 850 shots 

### Fig. 21: Plotting for the other correlations 

ex_ht_All_df_rm #wihtin mangrove region

# Assuming ex_ht_All_df_rm is your data frame
all_vars <- names(ex_ht_All_df_rm)
exclude_vars <- c(vari, "AOT", "quality_fl", "degrade_fl")  # Variables to exclude
other_vars <- setdiff(all_vars, exclude_vars)  # All variables in ex_ht_All_df_rm excluding vari and the specified columns


# Splitting data into training and testing sets (70% train, 30% test)
set.seed(123)  # Setting seed for reproducibility


vari <- c("rh10" , "rh20","rh30","rh40", "rh50", "rh60","rh70","rh80","rh90","rh95","rh98","rh100")

# Assuming ex_ht_All_df_rm is your data frame
all_vars <- names(ex_ht_All_df_rm)
exclude_vars <- c("MSK_CLDPRB","MSK_SNWPRB","QA10","QA20","QA60")  # Variables to exclude
other_vars <- setdiff(all_vars, exclude_vars)  # All variables in ex_ht_All_df_rm excluding vari and the specified columns

subset_df <- ex_ht_All_df_rm[, other_vars]


# Check for non-numeric columns and remove them
numeric_vars <- sapply(subset_df, is.numeric)
subset_df <- subset_df[, numeric_vars]

# Identify columns with standard deviation of zero
constant_vars <- sapply(subset_df, function(col) sd(col, na.rm = TRUE) == 0)

# Remove columns with standard deviation of zero
subset_df <- subset_df[, !constant_vars]

# Calculate the correlation matrix
cor_matrix <- cor(subset_df, use = "complete.obs")

#### Fig. 11: Plotting the Correlation matrix ####

# Print the correlation matrix
print(cor_matrix)


cor_matrix_GEDI_o <- cor_matrix
# Melt the matrix for ggplot
cor_melted <- melt(cor_matrix)

# Label of the size
size <- dim(ex_ht_All_df_rm)

## Fig.900: Correlation in clipped region 15/08/23

# Plot heatmap
ggplot(cor_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, 
                       name = "Correlation", limits = c(-1, 1)) +
  theme_minimal() +
  labs(title = paste("Correlation Heatmap within VCS1764, N =",size[1]), x = "GEDI Rh metric", y = "Sentinel-2 Variable") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))


#### Another correlation map
# Fig. 12: Correaltion matricL Plot the network graph
qgraph(cor_matrix, layout = "spring", theme = "colorblind", title = "Network Graph of Correlations within VCS1764, N = 126")


###
# Using the gplots package
#install.packages("gplots")
library(gplots)

heatmap.2(cor_matrix, 
          main = "Heatmap with Hierarchical Clustering within VCS1764, N = 146", 
          trace="none", 
          margins = c(13, 13))


#### NAother method

# Using the FactoMineR and factoextra packages
install.packages(c("FactoMineR", "factoextra"))
library(FactoMineR)
library(factoextra)

# Fig.10: Plotting a PCA circle plot
res.pca <- PCA(subset_df, graph = FALSE)
# Correlation Circle Plot
fviz_pca_var(res.pca, 
             col.var="contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, 
             title = "Correlation Circle Plot within VCS1764, N = 631")

####

library(qgraph)

# Threshold the correlation matrix to only show strong correlations
threshold <- 0.5
cor_matrix[abs(cor_matrix) < threshold] <- 0

# Plot the correlation network
qgraph(cor_matrix, 
       layout = "spring", 
       theme = "colorblind", 
       title = "Correlation Network within VCS1764, N = 631")

#
### Fig. 99:  Zooming in on the NDVI #####
# 16/08/23  

# Load necessary libraries
library(ggplot2)
library(tidyr)

# Convert the data frame from wide to long format
long_data <- ex_ht_All_df_rm %>%
  gather(key = "rh_metric", value = "value", starts_with("rh"))


### Plotting the mean and standard deviation

# Calculate mean and variance for each rh_metric with 3 significant figures
summary_stats <- long_data %>%
  group_by(rh_metric) %>%
  summarize(
    `mean (m)` = signif(mean(value, na.rm = TRUE), 2),
    `variance (m)` = signif(var(value, na.rm = TRUE), 3)
  )

# Plot the data
### Fig. 98 GGplot of the rh values ####
vari
long_data$rh_metric <- factor(long_data$rh_metric, levels = vari)

p <- ggplot(long_data, aes(x = rh_metric, y = value)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Distribution of Rh metrics N = 850", y = "Height (m)", x = "RH Metric") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))

# Create a table of the summary statistics with 3 significant figures
table_grob <- tableGrob(long_data, rows = NULL)

# Display the graph and table side by side
grid.arrange(p, table_grob, ncol = 2)


### Fig. 112: Plotting the correlation matrix horizontal for all 

# Calculate correlations
cor_matrix <- matrix(NA, nrow = length(vari), ncol = length(other_vars),
                     dimnames = list(vari, other_vars))

for (v in vari) {
  for (ov in other_vars) {
    cor_value <- cor(ex_ht_All_df_rm[[v]], ex_ht_All_df_rm[[ov]], use = "complete.obs")
    cor_matrix[v, ov] <- cor_value
  }
}

# Melt the matrix for ggplot
cor_melted <- melt(cor_matrix)

vari <- c("rh10" , "rh20","rh30","rh40", "rh50", "rh60","rh70","rh80","rh90","rh95","rh98","rh100")
Var1 <- vari
all_vars <- names(extracted_values_df)
exclude_vars <- c("MSK_CLDPRB","MSK_SNWPRB","QA10","QA20","QA60","quality_fl","degrade_fl")  # Variables to exclude
Var2 <- setdiff(all_vars, exclude_vars)  # All variables in ex_ht_All_df_rm excluding vari and the specified columns

  
ggplot(cor_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, 
                       name = "Correlation", limits = c(-1, 1)) +
  theme_minimal() +
  labs(title = paste("Correlation Heatmap for GEDI shots in VCS1764 region, N =",size[1])) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

#### Fig. 91: X coordinate Correlations, leaving out all other GEDI variables ####
# 16/08/23
library(ggplot2)
library(reshape2)

# Assuming cor_matrix is your correlation matrix
cor_melted <- melt(cor_matrix)

vari <- c("rh10", "rh20", "rh30", "rh40", "rh50", "rh60", "rh70", "rh80", "rh90", "rh95", "rh98", "rh100")
vari <- c("rh10" , "rh20","rh30","rh40", "rh50", "rh60","rh70","rh80","rh90","rh95","rh98","rh100")
Var1 <- vari

all_vars <- names(extracted_values_df)
exclude_vars <- c("MSK_CLDPRB","MSK_SNWPRB","QA10","QA20","QA60","quality_fl", "degrade_fl")  # Variables to exclude
Var2 <- setdiff(all_vars, exclude_vars)  # All variables in ex_ht_All_df_rm excluding vari and the specified columns

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


correlation_matrix_GEDI_rh <- cor_melted_filtered
# Yes! Finally got it to work. C'est Fantastique

### PLotting the NDVI
#Fig. 20: 16/08/23
# Question is, can i connect the ground correlations on the same map?

# Convert the data frame from wide to long format
long_data <- ex_ht_All_df_rm %>%
  gather(key = "variable", value = "value", -NDVI)

vari <- c("rh10" , "rh20","rh30","rh40", "rh50", "rh60","rh70","rh80","rh90","rh95","rh98","rh100")

# Create a list to store the scatter plots
# ... [your previous code]

# Create a list to store the scatter plots
plot_list <- lapply(vari, function(var) {
  data_subset <- subset(long_data, variable == var)
  
  # Calculate correlation and R^2
  cor_value <- cor(data_subset$value, data_subset$NDVI, use = "complete.obs")
  R2 <- cor_value^2
  
  ggplot(data_subset, aes(x = value, y = NDVI)) +
    geom_point(alpha = 0.5) +
    theme_minimal() +
    labs(title = var, x = var, y = "NDVI") +
    geom_text(aes(x = Inf, y = Inf, label = paste("R^2 =", round(R2, 3))),
              hjust = 1.1, vjust = 9, size = 4, colour = "red") +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlim(0, 60)  # Set x-axis limit here
})

grid.arrange(grobs = plot_list, ncol = 4, top = textGrob(paste("Correlation Heatmap for GEDI shots in VCS1764 region, N =", size[1]), gp=gpar(fontsize=12)))


# Part 2: Ground correlation data  ####

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

extracted_values_df <- cbind(GEDI_transformed, extracted_values_df)

# Assuming ex_ht_All_df_rm is your data frame
all_vars <- names(extracted_values_df)
exclude_vars <- c("MSK_CLDPRB","MSK_SNWPRB","QA10","QA20","QA60","geometry","namess","OID_","Latitude","Longitude")  # Variables to exclude
other_vars <- setdiff(all_vars, exclude_vars)  # All variables in ex_ht_All_df_rm excluding vari and the specified columns
subset_df <- extracted_values_df[, other_vars]

# Check for non-numeric columns and remove them
numeric_vars <- sapply(subset_df, is.numeric)
subset_df <- subset_df[, numeric_vars]
library(sf)

subset_df <- st_set_geometry(subset_df, NULL)
library(dplyr)

subset_df <- subset_df %>%
  rename(Field_ht = Name)

# Calculate the correlation matrix
cor_matrix <- cor(subset_df, use = "complete.obs")
cor_matrix_field <- cor_matrix
print(cor_matrix)
# Melt the matrix for ggplot
cor_melted <- melt(cor_matrix)
# Label of the size
size <- dim(extracted_values_df)

## Fig.900: Correlation in clipped region 15/08/23
sentinel <-  c("B1","B2" , "B3","B4",     "B5", "B6", "B7",   "B8", "B8A",   "B9",  "B11"    ,   "B12"   ,    "AOT"   ,    "WVP"    ,  "SCL" , "TCI_R"  ,   "TCI_G", "TCI_B", "NDVI")
Var1 <- c("B1","B2" , "B3","B4",     "B5", "B6", "B7",   "B8", "B8A",   "B9",  "B11"    ,   "B12"   ,    "AOT"   ,    "WVP"    ,  "SCL" , "TCI_R"  ,   "TCI_G", "TCI_B", "NDVI")
Var2 <- c("Field_ht")
# Plot heatmap
ggplot(cor_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, 
                       name = "Correlation", limits = c(-1, 1)) +
  theme_minimal() +
  labs(title = paste("Correlation Heatmap within VCS1764, N =",size[1]), x = "GEDI Rh metric", y = "Sentinel-2 Variable") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

cor_melted$Var1 <- factor(cor_melted$Var1, levels = sentinel)
cor_melted$Var2 <- factor(cor_melted$Var2, levels = unique(cor_melted$Var2))

# Plot heatmap
ggplot(cor_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, 
                       name = "Correlation", limits = c(-1, 1)) +
  theme_minimal() +
  labs(title = paste("Correlation Heatmap for GEDI shots in VCS1764 region, N =",size[1]), x = "GEDI Rh metric", y = "Variable") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

#### Plotting the corerlation matrix for the field height
Field_ht <- cor_matrix_field[, "Field_ht"]
Field_hts <- Field_ht[-1]

# Trying to plot correlations between field data
vari <- c("rh10" , "rh20","rh30","rh40", "rh50", "rh60","rh70","rh80","rh90","rh95","rh98","rh100")

rows_to_remove <- c("MSK_CLDPRB","MSK_SNWPRB","QA10","QA20","QA60","solar_elev","sensitivit","digital_el","elevation_","quality_fl","degrade_fl")
rows_to_remove <- rows_to_remove[rows_to_remove %in% rownames(cor_matrix_GEDI_o)]

filtered_matrix <- cor_matrix_GEDI_o[!rownames(cor_matrix_GEDI_o) %in% rows_to_remove, ]

aa <- filtered_matrix[,vari]
aa <- aa[!rownames(aa) %in% vari, ]
###

aa <- as.data.frame(aa)
aa$Field_hts <- Field_hts[rownames(aa)]


library(tidyr)
library(tibble)

melted_aa <- aa %>% 
  as.data.frame() %>%
  rownames_to_column(var = "Band") %>%
  pivot_longer(cols = -Band, names_to = "Metric", values_to = "Correlation")

library(ggplot2)

ggplot(data = melted_aa, aes(x = Metric, y = Correlation, color = Band, group = Band)) +
  
  geom_point(size = 2) +
  theme_minimal() +
  labs(title = "Correlations between GEDI Rh metrics and Sentinel-2 bands",
       x = "GEDI Rh metric", y = "Correlation") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  scale_color_brewer(palette = "Set1")


#### Fig. 20: Plotting a melted correlation plots ####

# Reorder the levels of the Metric factor
melted_aa$Metric <- factor(melted_aa$Metric, 
                           levels = c("Field_hts", 
                                      setdiff(unique(melted_aa$Metric), c("Field_hts", "rh100")), 
                                      "rh100"))


ggplot(data = melted_aa, aes(x = Metric, y = Correlation, fill = Band)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Correlations across Sentinel-2 bands") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)
          ,plot.title = element_text(hjust = 0.5))


#### Fig.30 Plotting the correlation plots ####
library(corrplot)
corrplot(cor(aa), method = "pie")


### To summarise, this code gets the correlation plots of the input GEDI, compares them to the ground data. 
