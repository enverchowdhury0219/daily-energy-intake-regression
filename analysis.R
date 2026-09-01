# determinants of daily energy intake in u.s. adults
# STA302 final project — converted from project.Rmd (knitr::purl style)
# full write-up and results: see README.md and poster.pdf
#
# libraries used: tidyverse, dplyr, broom, knitr, car, leaps, kableExtra
# ------------------------------------------------------------------

# run this script from the repo root so read.csv("diet.csv") resolves correctly

# getting a first look at our dataset
library(tidyverse)
diet <- read.csv("diet.csv")
dim(diet)
head(diet)

# choosing the columns we want to use in our preliminary model, also cleaning the dataset by removing NAs and renaming columns to make them easily understandable
library(dplyr)
diet_clean <- diet %>%
  select(DR1TKCAL,  # our response variable: total daily calories
         DR1TPROT,  # protein intake
         DR1TCARB,  # carbohydrates intake
         DR1TTFAT,  # total fat intake
         DR1TSUGR,  # total sugar intake
         DR1TFIBE,  # fiber intake
         DR1TALCO,  # alcohol intake
         DR1TCHOL,  # cholesterol
         DR1TSODI,  # sodium intake
         DR1TPOTA,  # potassium intake
         DRQSDIET)  # diet type (our categorical variable)

diet_clean <- na.omit(diet_clean)

diet_clean <- diet_clean %>%
  rename(
    Calories = DR1TKCAL,
    Protein = DR1TPROT,
    Carbs = DR1TCARB,
    Fat = DR1TTFAT,
    Sugar = DR1TSUGR,
    Fiber = DR1TFIBE,
    Alcohol = DR1TALCO,
    Cholesterol = DR1TCHOL,
    Sodium = DR1TSODI,
    Potassium = DR1TPOTA,
    DietType = DRQSDIET
  )

# removing the NAs of the categorical variable (9 in this case) and adjusting our categorical variable factor levels accordingly
diet_clean <- diet_clean %>%
  filter(DietType != 9)

diet_clean$DietType <- factor(diet_clean$DietType,
                              levels = c(1, 2),
                              labels = c("OnDiet", "NotOnDiet"))

# confirming results of categoarical variable cleaning
str(diet_clean$DietType)
table(diet_clean$DietType)

# looking at what our cleaned data looks like
glimpse(diet_clean)
summary(diet_clean)
dim(diet_clean)

# fitting our preliminary model and displaying the summaries of all the variables
model1 <- lm(
  Calories ~ Protein + Carbs + Fat + Sugar + Fiber +
    Alcohol + Cholesterol + Sodium + Potassium +
    DietType + Protein:DietType,
  data = diet_clean
)

summary(model1)

# creating a easy to read table of the results of our model
library(broom)
library(knitr)

coef_tab <- tidy(model1, conf.int = TRUE, conf.level = 0.95) %>%
  select(term, estimate, std.error, conf.low, conf.high, p.value)

kable(coef_tab,
      caption = "Preliminary linear model: Calories ~ nutrients + DietType + Protein×DietType",
      digits = 3, align = "lrrrrr")

# displaying model fit statistics (R^2 and adjusted R^2)
glance(model1) %>% select(r.squared, adj.r.squared)

# arranging our residual plots in a 2 by 2 grid for document display
par(mfrow = c(2, 2))
plot(model1)
par(mfrow = c(1, 1))

# Check linearity assumption using Residuals vs Fitted plot
plot(model1, which = 1)
# no violation

# Check independence by plotting residuals in observation order
plot(residuals(model1), type = "l",
     main = "Residuals vs Observation Order",
     xlab = "Observation Index",
     ylab = "Residuals")
# no violation

# Check homoskedasticity using the Scale-Location plot
plot(model1, which = 3)
# Funnel shape indicating violation of constant variance so before applying transformation, trying to find which predictor is causing issue
res <- residuals(model1)
plot(diet_clean$Protein, res)
plot(diet_clean$Carbs, res)
plot(diet_clean$Fat, res)
plot(diet_clean$Sugar, res)
plot(diet_clean$Fiber, res)
plot(diet_clean$Alcohol, res)
plot(diet_clean$Cholesterol, res)
plot(diet_clean$Sodium, res)
plot(diet_clean$Potassium, res)
boxplot(res ~ diet_clean$DietType)

# Alcohol is causing the issue with a funnel shape.
# log transform alcohol
lnAlcohol <- log(diet_clean$Alcohol + 1)
model2 <- lm(Calories ~ Protein + Carbs + Fat + Sugar + Fiber + lnAlcohol + Cholesterol + Sodium + Potassium + DietType
             + Protein:DietType, data = diet_clean)
plot(model2, which = 3)

# Still shows mild heteroskedacity
hist(diet_clean$Protein)
hist(diet_clean$Carbs)
hist(diet_clean$Fat)
hist(diet_clean$Sugar)
hist(diet_clean$Alcohol)
hist(diet_clean$Cholesterol)
hist(diet_clean$Sodium)
hist(diet_clean$Potassium)
#Remaining variance differences were small and not expected to substantially affect model inference.

# Check normality of the residuals using the Q-Q plot
plot(model1, which = 2)
# Mild deviations in the lower tail are unlikely to materially affect inference for the model coefficients
plot(model2, which = 2)

# T-test
summary(model2)

# ANOVA test
anova(model2)

# Finding range - leads to more problematic points in large datasets
boxplot(diet_clean$Calories)
summary(diet_clean$Calories)
quantile(diet_clean$Calories, probs = c(0.95, 0.99))

# Multicollinearity check
#install.packages("car")
library(car)
# vif(model1, type = "predictor")
vif(model2, type = "predictor")

# Remove Cholesterol 
model3 <- lm(Calories ~ Protein + Carbs + Fat + Sugar + Fiber + lnAlcohol +
               Sodium + Potassium + DietType + Protein:DietType,
             data = diet_clean)
summary(model3)
anova(model3)
vif(model3, type = "predictor")

# All possible subset selection
install.packages("leaps")
library(leaps)
library(car)
 
best <- regsubsets(Calories ~ Protein + Carbs + Fat + Sugar + Fiber + lnAlcohol +
             Sodium + Potassium + DietType + Protein:DietType, data = diet_clean,
             nbest = 1, nvmax = 11)
summary(best)

model4 <- lm(Calories ~ Protein + Carbs + Fat + lnAlcohol, data = diet_clean)
summary(model4)

# Checking for problematic points
n <- nrow(diet_clean)
p <- length(coef(model4))-1
diet_clean$lnAlcohol <- log(diet_clean$Alcohol + 1)


# leverage cutoff
hcut <- 2*(p+1)/n
h_ii <- hatvalues(model4) # leverage statistic
which(h_ii > hcut) # which observations are leverage points?

# cook's distance cutoff
cookcut <- qf(0.5, p+1, n-p-1)
D_i <- cooks.distance(model4) # cook's distance
which(D_i > cookcut) # which observations are influential by cook's distance?

# dffits cutoff
fitcut <- 2*sqrt((p+1)/n)
dffits_i <- dffits(model4) # dffits
which(abs(dffits_i) > fitcut) # which observations are influential by dffits?

# dfbeta cutoff
betacut <- 2/sqrt(n)
dfbetas_i <- dfbetas(model4) # dfbetas
# which observations are influential by dfbetas?
for(i in 1:ncol(dfbetas_i)){
  print(paste0("Beta ", i-1))
  print(which(abs(dfbetas_i[,i]) > betacut))
  }# this checks all betas in a loop

# standardized residuals
r_i <- rstandard(model4)
which(r_i > 4 | r_i < -4) # which observations are regression outliers?

# Sanity check to see if removing problematic points improves the model significantly
prob_idx <- which(abs(r_i) > 4 | abs(dffits_i) > fitcut)

model5_sens <- lm(Calories ~ Protein + Carbs + Fat + lnAlcohol + DietType,
                  data = diet_clean[-prob_idx, ])
summary(model5_sens)
summary(model4)

# Recheck assumptions and diagnostics for model4
plot(model4, which = 1)
plot(model4, which = 2)
plot(model4, which = 3)

vif(model4)

anova(model4)

library(broom)
library(knitr)

coef_tab <- tidy(model4, conf.int = TRUE, conf.level = 0.95) %>%
  select(term, estimate, std.error, conf.low, conf.high, p.value)

kable(coef_tab,
      caption = "Final model: Calories ~ Proteins + Carbs + Fats + lnAlcohol + DietType",
      digits = 3, align = "lrrrrr")

library(broom)
library(dplyr)
library(knitr)
library(kableExtra)

## Tidy coefficients for Model 4
coef_tab <- tidy(model4) %>%
  mutate(
    Predictors = case_when(
      term == "(Intercept)"        ~ "(Intercept)",
      term == "Protein"            ~ "Protein",
      term == "Carbs"              ~ "Carbs",
      term == "Fat"                ~ "Fat",
      term == "lnAlcohol"          ~ "lnAlcohol",
      term == "DietTypeNotOnDiet"  ~ "DietType NotOnDiet",
      TRUE                         ~ term
    ),
    p = if_else(p.value < 0.001, "<0.001",
                sprintf("%.3f", p.value)),
    Estimates = sprintf("%.3f", estimate)
  ) %>%
  select(Predictors, Estimates, p)

## Model summary rows
mod_sum <- glance(model4)
extra_rows <- tibble(
  Predictors = c("Observations", "R^2 / R^2 adjusted"),
  Estimates = c(
    as.character(mod_sum$nobs),
    sprintf("%.3f / %.3f", mod_sum$r.squared, mod_sum$adj.r.squared)
  ),
  p = c("", "")
)

coef_tab_full <- bind_rows(coef_tab, extra_rows)

## Create three identical model columns
wide_tab <- coef_tab_full %>%
  select(Predictors) %>%
  mutate(
    Est1 = coef_tab_full$Estimates,
    p1   = coef_tab_full$p,
    Est2 = coef_tab_full$Estimates,
    p2   = coef_tab_full$p,
    Est3 = coef_tab_full$Estimates,
    p3   = coef_tab_full$p
  )

## Produce a horizontal, 3-column table
wide_tab %>%
  kable(
    digits = 3,
    align = c("l", "r", "r", "r", "r", "r", "r"),
    caption = "Final Model Displayed in Three Side-by-Side Panels"
  ) %>%
  add_header_above(c(" " = 1,
                     "Model 4A" = 2,
                     "Model 4B" = 2,
                     "Model 4C" = 2)) %>%
  kable_styling(full_width = FALSE)

