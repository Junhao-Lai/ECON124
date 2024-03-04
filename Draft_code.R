

setwd("/Users/laix1/OneDrive/Desktop/Final_Proposal")
library(haven)

covid_data <- data.frame(read_dta("C19CS Data.dta"))

View(covid_data)
