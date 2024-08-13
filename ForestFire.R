library("ggplot2")                     # Load ggplot2 package
library("GGally")                      # Load GGally package

# Reading the data into the data frame
df = read.csv("forestfires.csv")
head(df)

boxplot(df$area)

df$area <- df$area + 0.0000000001
head(df)
# Creating lisnear regression 
model1 = lm(area~., data=df)
summary(model1)

# Using it for forward and backward selection
library(MASS)

# Variable selection, both forward and backward
step.model_both = step(model1, direction="both", trace=FALSE)
summary(step.model_both)

# Forward variable selection
step.model_forward = step(model1, direction="forward", trace=FALSE)
summary(step.model_forward)

# Backward variable selection
step.model_backward = step(model1, direction="backward", trace=FALSE)
summary(step.model_backward)

plot(step.model_backward)
hist(step.model_backward$residuals)

bc = boxcox(area~X+DMC+RH, data=df)
lambda = bc$x[which.max(bc$y)]
lambda


# Apply the Box-Cox Transformation
df$transformed_area <- (df$area^lambda -1)/lambda
df2 = subset(df, select = c("X", "DMC", "RH", "transformed_area"))
df2

model2 = lm(transformed_area~., data=df2)
summary(model2)


