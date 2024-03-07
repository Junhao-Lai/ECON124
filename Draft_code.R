

setwd("/Users/laix1/OneDrive/Desktop/Final_Proposal") #Alter when needed 
library(haven)

covid_data <- data.frame(read_dta("C19CS Data.dta"))

View(covid_data)


SM_data <- na.omit(covid_data[,c(201,851),]) #暂时先要这2个变量
head(SM_data)

#最幼稚的regression between 抽烟者 和 Covid-19 阳性
naive_SM_OLS <- glm(t5_cov_self_t ~ t0_smoke, data = SM_data)
summary(naive_SM_OLS)

# A person what has an active smoking status is associated with a 2.4% increased in the chance of tested positive for COVID-19 past month.