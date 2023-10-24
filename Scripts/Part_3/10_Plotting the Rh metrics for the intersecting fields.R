# Load ggplot2 package
library(ggplot2)
library(tidyr)
library(dplyr)

# Your data
my_data <- data.frame(
  ID = c("M", "rh10", "rh20", "rh30", "rh40", "rh50", "rh60", "rh70", "rh80", "rh90", "rh95", "rh98", "rh100"),
  A = c(1.03, -2.059999943, -1.230000019, -0.670000017, -0.180000007, 0.289999992, 0.819999993, 1.419999957, 2.170000076, 3.220000029, 4, 4.789999962, 6.550000191),
  G = c(0.27, -1.610000014, -1.120000005, -0.709999979, -0.370000005, -0.07, 0.25999999, 0.589999974, 0.970000029, 1.49000001, 1.909999967, 2.24000001, 2.690000057)
)

# Convert to long format
long_data <- gather(my_data, key = "Type", value = "Energy_Return", -ID)

# Extract numerical part from ID for sorting
long_data$ID_num <- as.numeric(gsub("rh", "", long_data$ID))

# Sort data by ID_num
long_data <- long_data %>% arrange(ID_num)

# Create the plot

# Extract the 'true height' value from the M row
true_height <- unique(long_data$Energy_Return[long_data$ID == "M"])


# Create the plot
p <- ggplot(long_data, aes(x = Energy_Return, y = ID_num)) +
  geom_point(aes(color = Type), size = 3) +
  
  # Add vertical lines for "A"
  geom_vline(data = subset(long_data, ID == "M" & Type == "A"), 
             aes(xintercept = Energy_Return, color = Type), 
             linetype = "dashed", size = 1) +
  
  # Add vertical lines for "G"
  geom_vline(data = subset(long_data, ID == "M" & Type == "G"), 
             aes(xintercept = Energy_Return, color = Type), 
             linetype = "dashed", size = 1) +
  
  geom_point(data = subset(long_data, ID == "M"), aes(color = Type), size = 3) +
  facet_wrap(~ Type, scales = "free", ncol = 2) +
  ggtitle("Rh metrics within 30 m of Field plots in 2019") +
  xlab("Height (m)") +
  ylab("Rh metric") +
  scale_color_manual(values = c("blue", "red"))+
  theme(plot.title = element_text(hjust = 0.5))

# Print the plot
print(p)


######


# Create a data frame for true mean annotations
annotation_data <- data.frame(
  Type = c("A", "G"),
  mean_value = c(1.03, 0.27),
  y_position = c(max(long_data$ID_num, na.rm = TRUE), max(long_data$ID_num, na.rm = TRUE)) # put text at the max y value
)

# Create the plot
p <- ggplot(long_data, aes(x = Energy_Return, y = ID_num)) +
  geom_point(aes(color = Type), size = 3) +
  
  # Add vertical lines for 'A'
  geom_vline(data = subset(long_data, ID == "M" & Type == "A"), 
             aes(xintercept = Energy_Return, color = "black"), 
             linetype = "dashed", size = 1) +
  
  # Add vertical lines for 'G'
  geom_vline(data = subset(long_data, ID == "M" & Type == "G"), 
             aes(xintercept = Energy_Return, color = "black"), 
             linetype = "dashed", size = 1) +
  
  # Add true mean as text
  geom_text(data = annotation_data, aes(x = mean_value, y = y_position, label = paste("Field ht:", round(mean_value, 2)), color = Type),
            hjust = 0.5, vjust = 0.5, size = 4) +
  
  geom_point(data = subset(long_data, ID == "M"), aes(color = Type), size = 3) +
  facet_wrap(~ Type, scales = "free", ncol = 2) +
  ggtitle("Rh metrics of shots within 30 m of Field plots in 2019") +
  xlab("Height (m)") +
  ylab("Rh metric") +
  scale_color_manual(values = c("blue", "green", "red"))+
  theme(plot.title = element_text(hjust = 0.5))

# Print the plot
print(p)
####


# Create the plot
p <- ggplot(long_data, aes(y = Energy_Return, x = ID_num)) +
  geom_point(aes(color = Type), size = 3) +
  
  # Add vertical lines for 'A'
  geom_hline(data = subset(long_data, ID == "M" & Type == "A"), 
             aes(yintercept = Energy_Return, color = "black"), 
             linetype = "dashed", size = 1) +
  
  # Add vertical lines for 'G'
  geom_hline(data = subset(long_data, ID == "M" & Type == "G"), 
             aes(yintercept = Energy_Return, color = "black"), 
             linetype = "dashed", size = 1) +
  
  # Add true mean as text
  geom_text(data = annotation_data, aes(x = mean_value, y = y_position, label = paste("Field ht:", round(mean_value, 2)), color = Type),
            hjust = 0.5, vjust = 0.5, size = 4) +
  
  geom_point(data = subset(long_data, ID == "M"), aes(color = Type), size = 3) +
  facet_wrap(~ Type, scales = "free", ncol = 2) +
  ggtitle("Energy Return by Rh Metric") +
  xlab("Height (m)") +
  ylab("% Energy Return") +
  ylim(-0.5, 7)+
  scale_color_manual(values = c("blue","black" , "red"))+
  theme(plot.title = element_text(hjust = 0.5))

# Print the plot
print(p)



