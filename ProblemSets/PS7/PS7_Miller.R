#DS for Econ PS7 William Miller

#load libraries
library(tidyverse)
library(mice)
library(modelsummary)

#load data
df <- read.csv("https://raw.githubusercontent.com/willmiller6/DScourseS25/refs/heads/master/ProblemSets/PS7/wages.csv")
view(df)

summary(df)

#remove NA values in hgc and tenure
df <- df %>% 
  drop_na(hgc, tenure)

#convert college and married to factors

df <- df %>%
  mutate(
    college_grad = as.numeric(college == "college grad"),
    married_dummy = as.numeric(married == "married")
  )
lapply(df, class)
#summary table using modelsummary with title that reads "Summary Table"
datasummary_skim(df, title = "Summary Table")
#25% of logwage is missing

sum(is.na(df$logwage))
#this works out to 560 observations

mod <- list()

#first regression - complete cases

df_complete_cases <- df %>% 
  drop_na(logwage)

mod[["Listwise Deletion"]] <- lm(logwage ~ hgc + college_grad + tenure + I(tenure^2) + age + married_dummy, data = df_complete_cases)


#second regression - mean imputation

mean(df$logwage, na.rm = TRUE)

df_mean_imputation <- df %>% 
  mutate(logwage = if_else(is.na(logwage), mean(logwage, na.rm = TRUE), logwage))

#validation
view(df_mean_imputation)
sum(is.na(df_mean_imputation))
#looks good

mod[["Mean Imputation"]] <- lm(logwage ~ hgc + college_grad + tenure + I(tenure^2) + age + married_dummy, data = df_mean_imputation)


#third regression - fitted values
fit1 <- lm(logwage ~ hgc + college_grad + tenure + I(tenure^2) + age + married_dummy, data = df_complete_cases)
df_fitted_values <- df %>% 
  mutate(logwage = if_else(is.na(logwage), predict(fit1, newdata = .), logwage))
view(df_fitted_values)
mod[["Fitted Dependent Variable"]] <- lm(logwage ~ hgc + college_grad + tenure + I(tenure^2) + age + married_dummy, data = df_fitted_values)
summary(mod[["Fitted Dependent Variable"]])


#fourth regression - multiple imputation
df_mice <- mice(df, m = 5, printFlag = FALSE)

mod[["Multiple Imputation"]] <- with(df_mice, lm(logwage ~ hgc + college_grad + tenure + I(tenure^2) + age + married_dummy))

mod[["Multiple Imputation"]] <- mice::pool(mod[["Multiple Imputation"]])

#modelsummary with all four regressions
modelsummary(mod, out="latex")

plot(density(df_complete_cases$logwage))