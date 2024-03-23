# Final Paper for ECON 124
setwd("C:/Users/laix1/Desktop/final/ECON124")
knitr::opts_knit$set(root.dir = 'C:/Users/laix1/OneDrive/Desktop/final/ECON124')
library(haven)
covid_data <- data.frame(read_dta("C19CS Data.dta"))
selected_data <- covid_data[, 1728:1867]

#ESTIMATE a model 
#selected_data <- na.omit(selected_data[selected_data$t12_reside, ])
selected_data <- na.omit(selected_data[selected_data$t12_drink, ])

#selected_data$t12_reside <- factor(selected_data$t12_reside)
#selected_data$t12_drink <- factor(selected_data$t12_drink)

#selected_data <- na.omit(selected_data[selected_data$t12_satisfy, ])
#selected_data <- na.omit(selected_data[selected_data$t12_slfhlth, ])
#selected_data <- na.omit(selected_data[selected_data$t12_isolate, ])


OLS_model <- glm(t12_satisfy ~ t12_drink + t12_reside, data = selected_data)
coef(summary(OLS_model))

# Post-lasso 3-fold
# Post-lasso algorithm 
# 1. Using lasso, regress Y on controls X
# 2. Using lasso, regress D on controls x
# 3. Collect all the covariates W that have non-zero coefficients in either of the two regressions 
# 4. Run an OLS regression of Y on D and W 

# Step #1
library(glmnetUtils)
y_model <- glmnetUtils::cv.glmnet(t12_satisfy ~ (t12_slfhlth + t12_slfmem + poly(t12_isolate, 2, raw = TRUE))^2, data = selected_data, nfold = 3, use.model.frame = TRUE)

# Step #2
d_model <- glmnetUtils::cv.glmnet(t12_drink ~ (t12_slfhlth + t12_slfmem + poly(t12_isolate, 2, raw = TRUE))^2, data = selected_data, nfold = 3, use.model.frame = TRUE)

#Step #3 
# Step 3
# Grab the indices of the control var that lasso didn't zero out in either step 1 or 2 
y_nonzero_coef_ind <- which(coef(y_model, s = "lambda.min")[-1] !=0)
d_nonzero_coef_ind <- which(coef(d_model, s = "lambda.min")[-1] !=0)
nonzero_coef_ind <- union(y_nonzero_coef_ind, d_nonzero_coef_ind)


#Step 4
controls = model.matrix(~(t12_slfhlth + t12_slfmem + poly(t12_isolate, 2, raw = TRUE))^2, data = selected_data)[,-1]

length(nonzero_coef_ind)
cat(length(nonzero_coef_ind), "controls were included out of", ncol(controls))

data2 <- data.frame(t12_satisfy = selected_data$t12_satisfy,  t12_drink = selected_data$t12_drink, controls[, nonzero_coef_ind])




##
post_lasso <- glm(t12_satisfy ~., data = data2)

coef(summary(post_lasso))["livingalone",]

