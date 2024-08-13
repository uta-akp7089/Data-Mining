library("ggplot2")                     # Load ggplot2 package
library("GGally")                      # Load GGally package
library(MASS)                          # Load MASS package for stepwise selection

# Reading the data into the data frame
df = read.csv("forestfires.csv")
head(df)

# Adding small value for 0.0 values.
df$area = df$area + 0.01

# Model to see if geograohy matters
model_geo = lm(area~temp+wind, data=df)
summary(model_geo)

# Remove outliers (239, 416, 480)
df = df[-c(239, 416, 480), ]

# Creating linear regression 
model1 = lm(area~., data=df)
summary(model1)

# Box cox transformation
bc = boxcox(area~., data=df)
lambda = bc$x[which.max(bc$y)]


# Apply the Box-Cox Transformation with the identified lambda
df$transformed_area <- log(df$area)
df <- subset(df, select = -area) 
# Update the model with the transformed area
model_transformed <- lm(transformed_area ~ ., data = df)

summary(model_transformed)

# Plots for new model assumptions
par(mfrow=c(1, 1)) # To plot all diagnostic plots in one window
plot(model_transformed)

# Histogram of the transformed area
hist(model_transformed$residuals)

head(df)


# Variable selection, both forward and backward
step.model_both = step(model_transformed, direction="both", trace=FALSE)
summary(step.model_both)

# Forward variable selection
step.model_forward = step(model_transformed, direction="forward", trace=FALSE)
summary(step.model_forward)

# Backward variable selection
step.model_backward = step(model_transformed, direction="backward", trace=FALSE)
summary(step.model_backward)


# Model for the geography of the area