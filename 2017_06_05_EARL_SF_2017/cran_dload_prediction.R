# This script generates the models used to predict the future daily 
# downloads from the CRAN respository. Four models are developed:
#
#    1) Prophet with linear growth
#    2) Prophet with logistic growth
#    3) timekit with linear regression on timeseries signature
#    4) timekit with spline added as external regressor, then perform lm() on signature


# Libraries
library(tidyquant)
library(timekit)
library(splines)
library(prophet)
library(cranlogs)

# Import ----
cran_data <- cran_downloads(from = "2014-01-01", to = "2017-05-31")
cran_data <- cran_data %>%
    as_tibble()
cran_data

# EDA / Tidy -----

# Visualize
cran_data %>%
    ggplot(aes(x = date, y = count)) +
    geom_point(color = palette_light()[[1]], alpha = 0.5) +
    labs(title = "Daily downloads of CRAN packages", x = "", y = "Number of downloads",
         subtitle = "Using cranlogs with date range 2014-01-01 to 2017-05-31") +
    scale_y_continuous(labels = scales::comma) +
    theme_tq()

# Transform
cran_data_log <- cran_data %>%
    mutate(count = log(count))
cran_data_log

# Visualize
cran_data_log %>%
    ggplot(aes(x = date, y = count)) +
    geom_point(color = palette_light()[[1]], alpha = 0.5) +
    labs(title = "Log transform of daily CRAN downloads", x = "", y = "log(Number of downloads)",
         subtitle = "Using cranlogs with date range 2014-01-01 to 2017-05-31") +
    scale_y_continuous(labels = scales::comma) +
    theme_tq()

# Outliers: Replace -Inf with previous days observation
cran_data_log_outlier <- cran_data_log %>%
    mutate(count = ifelse(count == -Inf, NA, count)) %>%
    fill(count, .direction = "down") %>%
    fill(count, .direction = "up")

# Visualize
cran_data_log_outlier %>%
    ggplot(aes(x = date, y = count)) +
    geom_point(color = palette_light()[[1]], alpha = 0.5) +
    labs(title = "Log transform of daily CRAN downloads", x = "", y = "log(Number of downloads)",
         subtitle = "Using cranlogs with date range 2014-01-01 to 2017-05-31") +
    scale_y_continuous(labels = scales::comma) +
    theme_tq()



# Model ----

train <- cran_data_log_outlier %>%
    filter(year(date) < 2017)
test <- cran_data_log_outlier %>%
    filter(year(date) == 2017)



# prophet -----
train_prophet <- train %>%
    rename(ds = date,
           y  = count)

# Model 1: Prophet linear growth
fit_prophet_linear <- prophet(train_prophet)

future_prophet_linear <- make_future_dataframe(fit_prophet_linear, periods = 365)

forecast_prophet_linear <- predict(fit_prophet_linear, future_prophet_linear)

yhat_prophet_linear <- forecast_prophet_linear$yhat

yhat_prophet_linear_train <- yhat_prophet_linear[1:nrow(train)]
yhat_prophet_linear_test <- yhat_prophet_linear[(nrow(train) + 1):(nrow(train) + nrow(test))]
yhat_prophet_linear_future <- tail(yhat_prophet_linear, length(yhat_prophet_linear) - (nrow(train) + nrow(test)))

resid_prophet_linear_train <- train$count - yhat_prophet_linear_train
resid_prophet_linear_test <- test$count - yhat_prophet_linear_test

mean(resid_prophet_linear_train^2)^0.5
mean(resid_prophet_linear_test^2)^0.5

full_data_prophet_linear <- bind_rows(train, test) %>%
    add_column(yhat = c(yhat_prophet_linear_train, yhat_prophet_linear_test),
               model = "prophet.linear")
full_data_prophet_linear

full_data_prophet_linear %>%
    ggplot(aes(x = date, y = count)) +
    geom_point(color = palette_light()[[1]], alpha = 0.5) +
    geom_point(aes(x = date, y = yhat), color = palette_light()[[2]], alpha = 0.25, data = filter(full_data_prophet_linear, year(date) == 2017)) +
    labs(title = "prophet: linear model", x = "", y = "log(Number of downloads)") +
    scale_y_continuous(labels = scales::comma) +
    theme_tq(base_size = 18)


# Model 2: Prophet logistic growth with carrying capacity = 17
train_prophet_logistic <- train_prophet %>%
    mutate(cap = 17)

fit_prophet_logistic <- prophet(train_prophet_logistic, growth = "logistic")

future_prophet_logistic <- make_future_dataframe(fit_prophet_linear, periods = 365)
future_prophet_logistic$cap <- 17

forecast_prophet_logistic <- predict(fit_prophet_logistic, future_prophet_logistic)

yhat_prophet_logistic <- forecast_prophet_logistic$yhat

yhat_prophet_logistic_train <- yhat_prophet_logistic[1:nrow(train)]
yhat_prophet_logistic_test <- yhat_prophet_logistic[(nrow(train) + 1):(nrow(train) + nrow(test))]
yhat_prophet_logistic_future <- tail(yhat_prophet_logistic, length(yhat_prophet_logistic) - (nrow(train) + nrow(test)))

resid_prophet_logistic_train <- train$count - yhat_prophet_logistic_train
resid_prophet_logistic_test <- test$count - yhat_prophet_logistic_test

mean(resid_prophet_logistic_train^2)^0.5
mean(resid_prophet_logistic_test^2)^0.5

full_data_prophet_logistic <- bind_rows(train, test) %>%
    add_column(yhat = c(yhat_prophet_logistic_train, yhat_prophet_logistic_test),
               model = "prophet.logistic") 
full_data_prophet_logistic

full_data_prophet_logistic %>%
    ggplot(aes(x = date, y = count)) +
    geom_point(color = palette_light()[[1]], alpha = 0.5) +
    geom_point(aes(x = date, y = yhat), color = palette_light()[[2]], alpha = 0.25, data = filter(full_data_prophet_logistic, year(date) == 2017)) +
    labs(title = "prophet: logistic model w/ carrying capacity = 17", x = "", y = "log(Number of downloads)") +
    scale_y_continuous(labels = scales::comma) +
    theme_tq(base_size = 18)


# timekit ----

train_augmented <- train %>%
    tk_augment_timeseries_signature() %>%
    mutate(
        day.fctr = factor(day)
    )

test_augmented <- test %>%
    tk_augment_timeseries_signature() %>%
    mutate(
        day.fctr = factor(day)
    )

future_augmented <- train %>%
    tk_index() %>%
    tk_make_future_timeseries(n_future = 365) %>%
    tk_get_timeseries_signature() %>%
    mutate(
        day.fctr = factor(day)
    )

# Model 3: timekit linear regression 

fit_timekit_lm <- lm(count ~ . + month.lbl:day.fctr, data = train_augmented[,-1])

yhat_timekit_lm <- predict(fit_timekit_lm, newdata = future_augmented)

yhat_timekit_lm_train <- c(NA, fit_timekit_lm$fitted.values)
yhat_timekit_lm_test <- yhat_timekit_lm[1:nrow(test)]
yhat_timekit_lm_future <- yhat_timekit_lm[(nrow(test) + 1):length(yhat_timekit_lm)]

resid_timekit_lm_train <- train$count - yhat_timekit_lm_train
resid_timekit_lm_test <- test$count - yhat_timekit_lm_test

mean(resid_timekit_lm_train^2, na.rm = T)^0.5
mean(resid_timekit_lm_test^2)^0.5

full_data_lm <- bind_rows(train, test) %>%
    add_column(yhat = c(yhat_timekit_lm_train, yhat_timekit_lm_test),
               model = "timekit.lm")
full_data_lm

full_data_lm %>%
    ggplot(aes(x = date, y = count)) +
    geom_point(color = palette_light()[[1]], alpha = 0.5) +
    geom_point(aes(y = yhat), color = palette_light()[[6]], alpha = 0.25, data = filter(full_data_lm, year(date) == 2017)) +
    labs(title = "timekit: lm model only", x = "", y = "log(Number of downloads)") +
    scale_y_continuous(labels = scales::comma) +
    theme_tq(base_size = 18)


# Model 4: timekit spline + linear regression 
knots <- ymd_hms("2014-07-01 00:00:00", "2016-10-01 00:00:00") %>%
    as.numeric()
fit_spline <- lm(count ~ ns(index.num, knots = knots), data = train_augmented)

yhat_spline <- predict(fit_spline, future_augmented)

train_augmented_spline <- train_augmented %>%
    add_column(yhat.spline = fit_spline$fitted.values)
future_augmented_spline <- future_augmented %>%
    add_column(yhat.spline = yhat_spline)

fit_timekit_lm_spline <- lm(count ~ . + month.lbl:day.fctr, data = train_augmented_spline[,-1])

yhat_timekit_lm_spline <- predict(fit_timekit_lm_spline, newdata = future_augmented_spline)

yhat_timekit_lm_spline_train <- c(NA, fit_timekit_lm_spline$fitted.values)
yhat_timekit_lm_spline_test <- yhat_timekit_lm_spline[1:nrow(test)]
yhat_timekit_lm_spline_future <- yhat_timekit_lm_spline[(nrow(test) + 1):length(yhat_timekit_lm_spline)]

resid_timekit_lm_spline_train <- train$count - yhat_timekit_lm_spline_train
resid_timekit_lm_spline_test <- test$count - yhat_timekit_lm_spline_test

mean(resid_timekit_lm_spline_train^2, na.rm = T)^0.5
mean(resid_timekit_lm_spline_test^2)^0.5

full_data_lm_spline <- bind_rows(train, test) %>%
    add_column(yhat = c(yhat_timekit_lm_spline_train, yhat_timekit_lm_spline_test),
               model = "timekit.spline.lm")
full_data_lm_spline

full_data_lm_spline %>%
    ggplot(aes(x = date, y = count)) +
    geom_point(color = palette_light()[[1]], alpha = 0.5) +
    geom_point(aes(y = yhat), color = palette_light()[[6]], alpha = 0.25, data = filter(full_data_lm_spline, year(date) == 2017)) +
    labs(title = "timekit: spline + lm", x = "", y = "log(Number of downloads)") +
    scale_y_continuous(labels = scales::comma) +
    theme_tq(base_size = 18)


# Ensemble ----

full_data <- bind_rows(
    full_data_prophet_linear,
    full_data_prophet_logistic,
    full_data_lm,
    full_data_lm_spline
)
full_data 

full_data_ensemble <- full_data %>%
    filter(!(date == "2014-01-01")) %>%
    spread(key = model, value = yhat) %>%
    mutate(
        yhat = 0.25*prophet.linear + 0.25*prophet.logistic + 0.25*timekit.lm + 0.25*timekit.spline.lm,
        .resid = count - yhat,
        type = ifelse(year(date) == 2017, "test", "train"))
full_data_ensemble

full_data_ensemble %>%
    filter(type == "train") %>%
    .$.resid %>%
    .^2 %>%
    mean(.) %>%
    .^0.5

full_data_ensemble %>%
    filter(type == "test") %>%
    .$.resid %>%
    .^2 %>%
    mean(.) %>%
    .^0.5

full_data_ensemble %>%
    ggplot(aes(x = date, y = count)) +
    geom_point(color = palette_light()[[1]], alpha = 0.5) +
    geom_point(aes(y = yhat), color = palette_light()[[6]], alpha = 0.25, data = filter(full_data_ensemble, year(date) == 2017)) +
    labs(title = "Ensemble of prophet and timekit models", x = "", y = "log(Number of downloads)") +
    scale_y_continuous(labels = scales::comma) +
    theme_tq(base_size = 18)

# Visualize 

forecast_ensemble <- future_augmented %>%
    select(index) %>%
    add_column(
        yhat.prophet.linear    = c(yhat_prophet_linear_test, yhat_prophet_linear_future),
        yhat.prophet.logistic  = c(yhat_prophet_logistic_test, yhat_prophet_logistic_future),
        yhat.timekit.lm        = c(yhat_timekit_lm_test, yhat_timekit_lm_future),
        yhat.timekit.lm.spline = c(yhat_timekit_lm_spline_test, yhat_timekit_lm_spline_future)
    ) %>%
    mutate(
        yhat.log = 0.25*yhat.prophet.linear + 0.25*yhat.prophet.logistic + 0.25*yhat.timekit.lm + 0.25*yhat.timekit.lm.spline,
        yhat = exp(yhat.log),
        yhat.best.individual = exp(yhat.prophet.linear)
    )
forecast_ensemble

cran_data_log_outlier %>%
    ggplot(aes(x = date, y = count)) +
    geom_point(color = palette_light()[[1]], alpha = 0.5) +
    geom_point(aes(x = index, y = yhat.log), color = palette_light()[[6]], alpha = 0.25, data = forecast_ensemble) +
    # geom_point(aes(x = index, y = yhat.prophet.linear), color = palette_light()[[2]], alpha = 0.25, data = forecast_ensemble) +
    labs(title = "Forecast of log transform of daily CRAN downloads", x = "", y = "log(Number of downloads)",
         subtitle = "Ensemble of prophet and timekit models") +
    scale_y_continuous(labels = scales::comma) +
    theme_tq()



cran_data %>%
    ggplot(aes(x = date, y = count)) +
    geom_point(color = palette_light()[[1]], alpha = 0.5) +
    geom_point(aes(x = index, y = yhat), color = palette_light()[[6]], alpha = 0.25, data = forecast_ensemble) +
    # geom_point(aes(x = index, y = yhat.best.individual), color = palette_light()[[2]], alpha = 0.25, data = forecast_ensemble) +
    labs(title = "Forecasting Daily downloads of CRAN packages", x = "", y = "Number of downloads",
         subtitle = "Ensemble of prophet and timekit models") +
    scale_y_continuous(labels = scales::comma) +
    theme_tq()



