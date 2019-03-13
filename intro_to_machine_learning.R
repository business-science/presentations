# LEARNING LAB 05 ----
# INTRODUCTION TO MACHINE LEARNING
# Hands On with parsnip R Package

# 1.0 Libraries ----

# Core packages
library(tidyverse)

# Visualization
library(tidyquant)
library(plotly)

# Modeling packages
library(parsnip)
library(rsample)
library(yardstick)
library(broom)

# Connector packages
library(rpart)
library(rpart.plot)
library(xgboost)

# Pull in functions
source("2019_03_13_Learning_Lab_05_Intro_to_Machine_Learning/scripts/plot_price_vs_weight.R")
source("2019_03_13_Learning_Lab_05_Intro_to_Machine_Learning/scripts/calc_metrics.R")
source("2019_03_13_Learning_Lab_05_Intro_to_Machine_Learning/scripts/plot_predictions.R")

# 2.0 Data Setup ----

# Price, Model, Product Category, and Weight
# - Web Scraped from https://www.cannondale.com/en/USA
price_vs_weight_tbl <- read_csv("2019_03_13_Learning_Lab_05_Intro_to_Machine_Learning/data/price_vs_weight_tbl.csv") 

price_vs_weight_tbl

plot_price_vs_weight()

# Engineered Features from Model Description
# - Feature Engineering: 101, Week 3 - Feature Engineering 
engineered_features_tbl <- read_csv("2019_03_13_Learning_Lab_05_Intro_to_Machine_Learning/data/engineered_features.csv")

engineered_features_tbl

# Join data, and remove Product Families with low counts
# - Data Manipulation: 101, Week 2 - Data Manipulation
pricing_model_tbl <- price_vs_weight_tbl %>%
    left_join(engineered_features_tbl, by = "row_id") %>%
    filter(!(ProductFamily %in% c("Trail", "TT and TRI")))

pricing_model_tbl



# 3.0 Data Split ----
# - Splitting Data: Business Analysis with R (101), Week 6 - Machine Learning

set.seed(1)
split_obj <- rsample::initial_split(pricing_model_tbl, 
                                    prop   = 0.8, 
                                    strata = "ModelBase")

train_tbl <- split_obj %>% training() 
test_tbl  <- split_obj %>% testing() 

# 4.0 Machine Learning ----
# - ML Algorithms: Business Analysis with R (101), Week 6 - Machine Learning
# - Cross Validation & Grid Search: Data Science for Business with R (201), Week 5 - H2O AutoML

# 4.1 Linear Regression (No Engineered Features) ----
?linear_reg

# Specify Model, Set Engine, & Fit Model to Data
model_01_lm <- linear_reg("regression") %>%
    set_engine("lm") %>%
    fit(Price_num ~ Category + ProductFamily + Weight_lb, 
        data = train_tbl %>% select(-row_id, Model))

# Prediction
model_01_lm %>% predict(new_data = test_tbl)

# Visualize Results
# - Custom Functions: 101 Week 5
# - Visualization: 101 Weeks 4 & 7
model_01_lm %>% plot_predictions(new_data = test_tbl)

# Calculate Performance
# - Custom Functions with Tidy Eval: Covered in 201, Week 1
model_01_lm %>% calc_metrics(new_data = test_tbl, truth = Price_num)



# 4.2 Linear Regression w/ Engineered Features ----
model_02_lm <- linear_reg("regression") %>%
    set_engine("lm") %>%
    fit(Price_num ~ ., data = train_tbl %>% select(-row_id, -Model))

model_02_lm %>% predict(new_data = test_tbl)

model_02_lm %>% plot_predictions(new_data = test_tbl)

model_02_lm %>% calc_metrics(test_tbl, truth = Price_num)

# Explanation
model_02_lm$fit %>%
    broom::tidy() %>%
    arrange(p.value)
    


# 4.3 Decision Tree ----
# - Parameters: 101, Week 6
# - Cross Validation: 201, Week 5
model_03_rpart <- decision_tree(
        mode = "regression", 
        cost_complexity = 0.001,
        tree_depth = 5, 
        min_n = 6) %>%
    set_engine("rpart") %>%
    fit(Price_num ~ ., data = train_tbl %>% select(-row_id, -Model))

model_03_rpart %>% calc_metrics(test_tbl, truth = Price_num)

model_03_rpart %>% plot_predictions(new_data = test_tbl)

# Explanation
model_03_rpart$fit %>%
    rpart.plot(
        fallen.leaves = FALSE, 
        extra = 101, 
        roundint = FALSE, 
        main = "Model 03: Decision Tree",
        cex = 0.8
    )


# 4.4 XGBoost ----
# - Parameters: 101, Week 6
# - Cross Validation: 201, Week 5
model_04_xgboost <- boost_tree(
        mode = "regression", 
        mtry = 30, 
        trees = 500, 
        min_n = 2, 
        tree_depth = 6,
        learn_rate = 0.35, 
        loss_reduction = 0.0001) %>%
    set_engine("xgboost") %>%
    fit(Price_num ~ ., data = train_tbl %>% select(-row_id, -Model))

model_04_xgboost %>% calc_metrics(test_tbl, truth = Price_num)

model_04_xgboost %>% plot_predictions(new_data = test_tbl)

# Explanation
model_04_xgboost$fit %>%
    xgb.importance(model = .) %>%
    xgb.plot.importance(main = "XGBoost Feature Importance")


# 5.0 Conclusions ----

# - Best Model - Linear Regression with Engineered Features, XGBoost 2nd
# - Features 
#   - "Hi-Mod", "Weight", "Model Base", "Category Road" seem to be globally important
#   - What about locally? --> LIME 201, Week 7
