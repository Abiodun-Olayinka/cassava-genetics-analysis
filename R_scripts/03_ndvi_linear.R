# NDVI Linear Prediction Model
# Data file: data/ONNEMODIFIED.csv
############################################################

rm(list = ls())
# Display current objects in the environment
print(ls())

# Remove all objects from the environment
rm(list = ls())

# Verify that the environment is now empty
print(ls())

library (rms) # for calibration and validation functions
library (calibrate) # for calibration plots
library (caret) # for macDMne learning models
library (ggplot2) # for data visualization
library(boot)      # for bootstrapping


# read your data from a csv file
data <- read.csv ("ONNENDVI.csv")


# check the structure and summary of your data
str (data)
colnames(data)
summary (data)


# plot the relationsDMp between NDVI and each DM at different time points
ggplot (data, aes (x = NDVI3, y = DM)) +
  geom_point () +
  facet_grid (time ~ DM)


# set the seed for reproducibility
set.seed (123)

# remove any rows with missing values
data <- na.omit (data)

# create an index for splitting the data
split_index <- createDataPartition (data$NDVI3, p = 0.8, list = FALSE)


# create an index for splitting the data
#split_index <- createDataPartition (data$NDVI3, p = 0.8, list = FALSE, na.rm = TRUE)

# create an index for splitting the data
#split_index <- createDataPartition (data$NDVI3, p = 0.8, list = FALSE)

# split the data into training (80%) and testing (20%) sets
train_data <- data[split_index, ]
test_data <- data[-split_index, ]


# load the rms package
library (rms)

# build your model using cph or other functions
#model <- cph (Surv (time, status) ~ NDVI3, data = train_data)

# load the rms package
library (rms)

# build your model using lm or other functions
model <- lm (DM ~ NDVI3, data = train_data)

# calibrate your model using bootstrapping
#DM_cal <- calibrate (model, method = "boot", B = 100)


# plot the calibration results
#plot (DM_cal)



# build a linear regression model for plant DM using NDVI as predictor
DM_model <- lm (DM ~ NDVI3, data = train_data)

# check the model summary and diagnostics
summary (DM_model)
#plot (DM_model)

# calibrate the model using bootstrapping
DM_cal <- calibrate (DM_model, method = "boot", B = 100)

# print and plot the calibration results
print (DM_cal)
#plot (DM_cal)


# predict the plant DM using the testing data
DM_pred <- predict (DM_model, newdata = test_data)

# calculate the R-squared value
R2 <- cor (test_data$DM, DM_pred)^2

# calculate the root mean squared error
RMSE <- sqrt (mean ((test_data$DM - DM_pred)^2))

# calculate the mean absolute error
MAE <- mean (abs (test_data$DM - DM_pred))

# print the validation results
cat ("R-squared:", R2, "\n")
cat ("RMSE:", RMSE, "\n")
cat ("MAE:", MAE, "\n")




############################################################
