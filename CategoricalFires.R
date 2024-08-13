df_trial = read.csv("forestfires.csv")

# Define the breaks and labels
breaks <- c(0, 2.14, 15.42, 1091.00)
labels <- c("small", "median", "large")

# Create new column with categories
area_category <- cut(df_trial$area, breaks = breaks, labels = labels, include.lowest = TRUE)

# Combine the area data with the new category column
df_trial <- data.frame(df_trial, area_category)

# Display the result
head(df_trial)

df_trial <- subset(df_trial, select = -area) 

head(df_trial)

hist()