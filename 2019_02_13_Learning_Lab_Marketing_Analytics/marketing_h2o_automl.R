# BUSINESS SCIENCE LEARNING LABS
# EPISODE 4
# MARKETING ANALYTICS: SCALABLE AUTOMATIC MACHINE LEARNING WITH H2O

# Setup instructions: 
# - Please update to the latest version of h2o using install.packages("h2o") 
#   to access the latest functionality and performance features


# Libraries
library(tidyverse)
library(readxl)
library(h2o)


# 1.0 READ EXCEL SHEETS ----
path   <- "2019_02_13_Learning_Lab_Marketing_Analytics/data/bank_term_deposit_marketing_analysis.xlsx"
sheets <- excel_sheets(path)


# 2.0 INVESTIGATE DATA FOR EACH SHEET ----
sheets %>%
    map(~ read_excel(path  = path, sheet = .)) %>%
    set_names(sheets)


# 3.0 PERFORM VLOOKUP EQUIVALENT ----
data_joined_tbl <- sheets[4:7] %>%
    map(~ read_excel(path = path, sheet = .)) %>%
    reduce(left_join)

data_joined_tbl 


# 4.0 START H2O CLUSTER ----
h2o.init()


# 4.1 H2O DATA PREP ----

# Convert string columns to factor (aka "enum") type
data_joined_tbl <- data_joined_tbl %>% 
    mutate_if(is.character, as.factor)

# The training set (convert to an H2OFrame)
# If you have large data, set this option for a speed-up: options("h2o.use.data.table" = TRUE)
train <- as.h2o(data_joined_tbl)

# Take a look at the training set
h2o.describe(train)

# Identify the response column
y <- "TERM_DEPOSIT"

# If our binary response column was encoded as 0/1 then we would have to convert it to a factor
# in order to tell H2O to perform classification (instead of regression).  Since our response
# is already a factor/enum type, there's nothing to do, but in the 0/1 case you just do:
#train[,y] <- as.factor(train[,y])

# Identify the predictor columns (remove response and ID column)
x <- setdiff(names(train), c(y, "ID"))


# 4.2 H2O AutoML Training ----

# Execute an AutoML run for 10 models
aml <- h2o.automl(
    y = y, 
    x = x, 
    training_frame = train,
    project_name = "term_deposit",
    max_models = 10,
    seed = 1)


# 4.3 H2O AutoML Leaderboard ----

# Next, we will view the AutoML Leaderboard.  Since we did not specify a `leaderboard_frame` in the `h2o.automl()` 
# function for scoring and ranking the models, the AutoML leaderboard uses cross-validation metrics to rank the models.  

# A default performance metric for each machine learning task (binary classification, multiclass classification, 
# regression) is specified internally and the leaderboard will be sorted by that metric.  In the case of binary 
# classification, the default ranking metric is Area Under the ROC Curve (AUC). 

# The leader model is stored at `aml@leader` and the leaderboard is stored at `aml@leaderboard`.
lb <- aml@leaderboard

# Now we will view a snapshot of the top models.  Here we should see the two Stacked Ensembles 
# at or near the top of the leaderboard.  Stacked Ensembles can almost always outperform a single model.
print(lb)

# To view the entire leaderboard, specify the `n` argument of the `print.H2OFrame()` 
# function as the total number of rows:
print(lb, n = nrow(lb))


# 4.4 Ensemble Exploration ----

# To understand how the ensemble works, let's take a peek inside the Stacked Ensemble "All Models" model.  
# The "All Models" ensemble is an ensemble of all of the individual models in the AutoML run.  
# This is often the top performing model on the leaderboard.

# Get model ids for all models in the AutoML Leaderboard
model_ids <- as.data.frame(aml@leaderboard$model_id)[,1]
# Get the "All Models" Stacked Ensemble model
se <- h2o.getModel(grep("StackedEnsemble_AllModels", model_ids, value = TRUE)[1])
# Get the Stacked Ensemble metalearner model
metalearner <- h2o.getModel(se@model$metalearner$name)

# Examine the variable importance of the metalearner (combiner) algorithm in the ensemble.  
# This shows us how much each base learner is contributing to the ensemble. The AutoML Stacked Ensembles 
# use the default metalearner algorithm (GLM with non-negative weights), so the variable importance of the 
# metalearner is actually the standardized coefficient magnitudes of the GLM. 
h2o.varimp(metalearner)

# We can also plot the base learner contributions to the ensemble.
h2o.varimp_plot(metalearner)


# 4.5 Variable Importance ----

# Now let's look at the variable importance on the training set using the top XGBoost model
# (Stacked Ensembles don't have variable importance yet)
xgb <- h2o.getModel(grep("XGBoost", model_ids, value = TRUE)[1])

# Examine the variable importance of the top XGBoost model
h2o.varimp(xgb)

# We can also plot the base learner contributions to the ensemble.
h2o.varimp_plot(xgb)


