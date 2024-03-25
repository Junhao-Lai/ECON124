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


#Data section 
# Regress self-satisfaction rate on alcohol drinking rate
naive_OLS <- glm(t12_satisfy ~ t12_drink, data = selected_data)
summary(naive_OLS)
# one unit increase in the alcoholic drinks in the past week is associated with a 4.27% increased in the satisfaction in life now 

# Create scatterplot with larger point size
plot(selected_data$t12_drink, selected_data$t12_satisfy,
     xlab = "Number of Drinks",
     ylab = "Satisfaction Score",
     main = "Scatterplot of Drinks vs. Satisfaction",
     pch = 16, 
     cex = 1.5) 

#----------------------------------------------------------------------------
#Interesting variables 
#1 Worry about the COVID-19 pandemic vs. Overall life satisfaction
#selected_data$t12_satisfy <- factor(selected_data$t12_satisfy)
#selected_data$t12_worry <- factor(selected_data$t12_worry)

#---------------------------------------------------------------------
#1 
OLS_1 <- glm(t12_worry ~ t12_satisfy, data = selected_data)
summary(OLS_1)
# one unit increase in the worry about covid-19 in the past week is associated with a 0.14%  decreased in the satisfaction in life

# Create a scatterplot
plot(selected_data$t12_worry, selected_data$t12_satisfy,
     xlab = "t12_worry about covid",
     ylab = "Satisfaction Score",
     main = "Scatterplot of worry about covid vs. life-Satisfaction",
     pch = 16, 
     cex = 1.5)
#----------------------------------------------------------------------
#2 
#Number of face-to-face contacts vs. worry about covid-19
OLS_2 <- glm(t12_contact_tot ~ t12_worry, data = selected_data)
summary(OLS_2)

# Create a histogram
plot(selected_data$t12_contact_tot,selected_data$t12_worry,
     xlab = "contact with others without mask",
     ylab = "Worry about covid",
     main = "Scatterplot of contact vs. worry about covid",
     pch = 16, 
     cex = 1.5)

# one unit increase in the contact with others without masks in the past week is associated with a 1.78% decreased in worry about covid


#------------------------------------------------------------------------------
#COVID-19 symptoms vs. Vaccination status
OLS_3 <- glm(t12_cov_ff_t ~ t12_vac_mo, data = selected_data)
summary(OLS_3)
# one unit increase in the covid symptoms in the past week is associated with a 1.59% increased in the likelihood of getting the vaccine

# Create a histogram
plot(selected_data$t12_test_ff,selected_data$t12_vac_mo,
     xlab = "Had covid symptoms",
     ylab = "Vaccination status",
     main = "Scatterplot of COVID-19 symptoms vs. Vaccination status",
     pch = 16, 
     cex = 1.5)
---------------------------------------

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
y_model <- glmnetUtils::cv.glmnet(t12_satisfy ~ (t12_slfhlth + t12_slfmem + poly(t12_pla_tot, 2, raw = TRUE))^2, data = selected_data, nfold = 3, use.model.frame = TRUE)

# Step #2
d_model <- glmnetUtils::cv.glmnet(t12_drink ~ (t12_slfhlth + t12_slfmem + poly(t12_pla_tot, 2, raw = TRUE))^2, data = selected_data, nfold = 3, use.model.frame = TRUE)

#Step #3 
# Step 3
# Grab the indices of the control var that lasso didn't zero out in either step 1 or 2 
y_nonzero_coef_ind <- which(coef(y_model, s = "lambda.min")[-1] !=0)
d_nonzero_coef_ind <- which(coef(d_model, s = "lambda.min")[-1] !=0)
nonzero_coef_ind <- union(y_nonzero_coef_ind, d_nonzero_coef_ind)


#Step 4
controls = model.matrix(~(t12_slfhlth + t12_slfmem + poly(t12_pla_tot, 2, raw = TRUE))^2, data = selected_data)[,-1]

length(nonzero_coef_ind)
cat(length(nonzero_coef_ind), "controls were included out of", ncol(controls))

data2 <- data.frame(t12_satisfy = selected_data$t12_satisfy,  t12_drink = selected_data$t12_drink, controls[, nonzero_coef_ind])

##
post_lasso <- glm(t12_satisfy ~., data = data2)
coef(summary(post_lasso))["t12_drink",]


# Counter-factual prediction 
cfact_data <- data2
cfact_data$t12_drink <- 5
mean(predict(post_lasso, cfact_data))

# Compare with current value
mean(selected_data$t12_satisfy)

#-----------------------------------------------------
#2nd post lasso
# Find out how enjoyed life affect the intensity of exercise in a week

OLS_model2 <- glm(t12_exerc ~ t12_cesd6+t12_slfhlth, data = selected_data)
coef(summary(OLS_model2))

y_model <- glmnetUtils::cv.glmnet(t12_exerc ~ (t12_slfmem + t12_promis1 + t12_promis2 + t12_promis3 + t12_promis4 + t12_promis4 + t12_promis5)^2, data = selected_data, nfold = 3, use.model.frame = TRUE)

d_model <- glmnetUtils::cv.glmnet(t12_cesd6 ~ (t12_slfmem + t12_promis1 + t12_promis2 + t12_promis3 + t12_promis4 + t12_promis4 + t12_promis5)^2, data = selected_data, nfold = 3, use.model.frame = TRUE)


y_nonzero_coef_ind <- which(coef(y_model, s = "lambda.min")[-1] !=0)
d_nonzero_coef_ind <- which(coef(d_model, s = "lambda.min")[-1] !=0)
nonzero_coef_ind <- union(y_nonzero_coef_ind, d_nonzero_coef_ind)

controls = model.matrix(~(t12_slfmem + t12_promis1 + t12_promis2 + t12_promis3 + t12_promis4 + t12_promis4 + t12_promis5)^2, data = selected_data)[,-1]

length(nonzero_coef_ind)
cat(length(nonzero_coef_ind), "controls were included out of", ncol(controls))

data2 <- data.frame(t12_exerc = selected_data$t12_exerc, t12_cesd6 = selected_data$t12_cesd6, controls[, nonzero_coef_ind])

post_lasso <- glm(t12_exerc ~., data = data2)

coef(summary(post_lasso))["t12_cesd6",]

# Counter-factual prediction 
cfact_data <- data2
cfact_data$t12_cesd6 <- 1 # if the person is enjoying life 
mean(predict(post_lasso, cfact_data))

# Compare with current value
mean(selected_data$t12_exerc)
