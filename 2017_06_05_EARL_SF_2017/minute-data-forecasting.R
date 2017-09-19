# This script generates plots for data set (Twitter AnomalyDetection package) for the 
# Business Science EARL 2017 Presentation. The source of the AnomalyDection
# plot is https://github.com/twitter/AnomalyDetection.
#
# This script generates three plots:
#   1) Linear Regression on timeseries signature
#   2) Second order polynomial added as external regressor, then lm() on signature
#   3) Third order polynomial added as external regressor, then lm() on signature

# Load libraries
library(tidyquant)
library(timekit)
library(AnomalyDetection) # devtools::install_github("twitter/AnomalyDetection")

# Get data
clicks <- tibble(
    timestamp = raw_data$timestamp %>% as.POSIXct(),
    count = raw_data$count
)
clicks

# EDA -----

# Inspect summary
clicks %>%
    tk_index() %>%
    tk_get_timeseries_summary()

# visualize clicks
g <- clicks %>%
    ggplot(aes(x = timestamp, y = count)) +
    geom_point(color = palette_light()[[1]], alpha = 0.2) +
    geom_smooth() +
    labs(title = "AnomalyDetection: raw_data",
         subtitle = '"A data frame containing a time series with headings timestamp and count."',
         x = "", y = "User Clicks") +
    theme() +
    theme_tq()

g

ggsave(filename = "./img/minute-fcast-0-eda-data-viz.png", g, height = 5, width = 7)

# Prep for ML -----

# Augment clicks
clicks_augmented <- clicks %>%
    tk_augment_timeseries_signature() %>%
    mutate(hour = factor(hour),
           minute = factor(minute))
clicks_augmented

# Make future timeseries 4000 periods
future_idx <- clicks %>%
    tk_index() %>%
    tk_make_future_timeseries(n = 4000) 

# Augment future timeseries (needed for prediction newdata)
future_augmented <- future_idx %>%
    tk_get_timeseries_signature() %>%
    mutate(hour = factor(hour),
           minute = factor(minute))


# Attempt 1 - Use ML to get patterns -----

# Fit linear regression model
att1_fit_lm <- lm(count ~ . + minute:hour + hour:index.num + minute:index.num, data = clicks_augmented[,c(-1)])

# Predict using future augmented data
att1_pred_lm <- predict(fit_lm, newdata = future_augmented[,-1])

# Add fitted values to clicks augmented
att1_clicks_augmented_lm <- clicks_augmented %>%
    add_column(yhat.lm = c(NA, att1_fit_lm$fitted.values))

# Add predicted values to future augmented
att1_future_augmented_lm <- future_augmented %>%
    add_column(yhat.lm = att1_pred_lm)

# Visualize
g <- att1_clicks_augmented_lm %>%
    ggplot(aes(x = timestamp, y = count)) +
    geom_point(color = palette_light()[[1]], alpha = 0.2) +
    # geom_point(aes(y = yhat.lm), color = palette_light()[[2]], alpha = 0.05) +
    geom_point(aes(x = index, y = yhat.lm), color = palette_light()[[2]], alpha = 0.2, data = att1_future_augmented_lm) +
    geom_smooth() +
    labs(title = "Forecasting Minute Data Using Machine Learning",
         subtitle = "Using timeseries signature only",
         x = "", y = "User Clicks") +
    theme() +
    theme_tq()

g

ggsave(filename = "./img/minute-fcast-1-ML.png", g, height = 5, width = 10)

# Training accuracy
att1_rmse_lm <- mean((att1_fit_lm$residuals)^2, na.rm = T)^0.5
att1_rmse_lm



# Attempt 2 - Use 2nd order polynomial as guide for ML algorithm -----

# Step 1 - Get poly line that follows gradual slope trend

# Fit model and get predicted path
att2_fit_poly <- lm(count ~ poly(index.num, degree = 2), data = clicks_augmented)
att2_pred_poly <- predict(att2_fit_poly, newdata = future_augmented[,"index.num"])

# Add fitted and predicted values to data frames, prep for ML
att2_clicks_augmented_poly <- clicks_augmented %>%
    add_column(yhat.poly = att2_fit_poly$fitted.values) %>%
    mutate(hour   = factor(hour),
           minute = factor(minute))

att2_future_augmented_poly <- future_augmented %>%
    add_column(yhat.poly = att2_pred_poly) %>%
    mutate(hour   = factor(hour),
           minute = factor(minute))

# Step 2 - Apply ML with poly line incorporated into model

# Ensemble poly line with regression
att2_fit_lm <- lm(count ~ . + minute:hour + hour:index.num + minute:index.num, data = att2_clicks_augmented_poly[,c(-1)])
att2_pred_lm <- predict(att2_fit_lm, newdata = att2_future_augmented_poly)

# Add fitted and predicted values to data frames
att2_clicks_augmented_lm <- att2_clicks_augmented_poly %>%
    add_column(yhat.lm = c(NA, att2_fit_lm$fitted.values)) 

att2_future_augmented_lm <- att2_future_augmented_poly %>%
    add_column(yhat.lm = att2_pred_lm) 

# Visualize results
g <- att2_clicks_augmented_lm %>%
    ggplot(aes(x = timestamp, y = count)) +
    geom_point(color = palette_light()[[1]], alpha = 0.2) +
    geom_point(aes(x = index, y = yhat.lm), color = palette_light()[[2]], alpha = 0.2, data = att2_future_augmented_lm) +
    geom_line(aes(y = yhat.poly), color = "blue", size = 1) +
    geom_line(aes(x = index, y = yhat.poly), color = "blue", size = 1, data = att2_future_augmented_lm) +
    labs(title = "Forecasting Minute Data Using Machine Learning",
         subtitle = "Using timeseries signature only",
         x = "", y = "User Clicks") +
    theme() +
    theme_tq()

g

ggsave(filename = "./img/minute-fcast-3-Poly3.png", g, height = 5, width = 10)

# Training accuracy
att2_rmse_lm <- mean((att2_fit_lm$residuals)^2, na.rm = T)^0.5
att2_rmse_lm




# Attempt 3 - Use 3rd order polynomial as guide for ML algorithm -----

# Step 1 - Get poly line that follows gradual slope trend

# Fit model and get predicted path
att3_fit_poly <- lm(count ~ poly(index.num, degree = 3), data = clicks_augmented)
att3_pred_poly <- predict(att3_fit_poly, newdata = future_augmented[,"index.num"])

# Add fitted and predicted values to data frames, prep for ML
att3_clicks_augmented_poly <- clicks_augmented %>%
    add_column(yhat.poly = att3_fit_poly$fitted.values) %>%
    mutate(hour   = factor(hour),
           minute = factor(minute))

att3_future_augmented_poly <- future_augmented %>%
    add_column(yhat.poly = att3_pred_poly) %>%
    mutate(hour   = factor(hour),
           minute = factor(minute))

# Step 2 - Apply ML with poly line incorporated into model

# Ensemble poly line with regression
att3_fit_lm <- lm(count ~ . + minute:hour + hour:index.num + minute:index.num, data = att3_clicks_augmented_poly[,c(-1)])
att3_pred_lm <- predict(att3_fit_lm, newdata = att3_future_augmented_poly)

# Add fitted and predicted values to data frames
att3_clicks_augmented_lm <- att3_clicks_augmented_poly %>%
    add_column(yhat.lm = c(NA, att3_fit_lm$fitted.values)) 

att3_future_augmented_lm <- att3_future_augmented_poly %>%
    add_column(yhat.lm = att3_pred_lm) 

# Visualize results
g <- att3_clicks_augmented_lm %>%
    ggplot(aes(x = timestamp, y = count)) +
    geom_point(color = palette_light()[[1]], alpha = 0.2) +
    geom_point(aes(x = index, y = yhat.lm), color = palette_light()[[2]], alpha = 0.2, data = att3_future_augmented_lm) +
    geom_line(aes(y = yhat.poly), color = "blue", size = 1) +
    geom_line(aes(x = index, y = yhat.poly), color = "blue", size = 1, data = att3_future_augmented_lm) +
    labs(title = "Forecasting Minute Data Using Machine Learning",
         subtitle = "Using timeseries signature only",
         x = "", y = "User Clicks") +
    theme() +
    theme_tq()

g

ggsave(filename = "./img/minute-fcast-3-Poly3.png", g, height = 5, width = 10)

# Training accuracy
att3_rmse_lm <- mean((att3_fit_lm$residuals)^2, na.rm = T)^0.5
att3_rmse_lm
