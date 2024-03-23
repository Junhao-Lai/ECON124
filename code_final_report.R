# Final Paper for ECON 124
setwd("C:/Users/laix1/Desktop/final/ECON124")
knitr::opts_knit$set(root.dir = 'C:/Users/laix1/OneDrive/Desktop/final/ECON124')
library(haven)
covid_data <- data.frame(read_dta("C19CS Data.dta"))

#ESTIMATE a model 

OLS_model <- glm(t12_satisfy ~ t12_drink + t12_reside, data = covid_data)
coef(OLS_model)

# Post-lasso 3-fold
# Post-lasso algorithm 
# 1. Using lasso, regress Y on controls X
# 2. Using lasso, regress D on controls x
# 3. Collect all the covariates W that have non-zero coefficients in either of the two regressions 
# 4. Run an OLS regression of Y on D and W 

# Step #1
library(glmnetUtils)
y_model <- glmnetUtils::cv.glmnet(t12_satisfy ~ (t12_drink + t12_reside)^2, data = covid_data, nfold = 3, use.model.frame = TRUE)

# Step #2
