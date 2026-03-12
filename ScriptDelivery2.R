#We load  the necessary libraries
library(tidyverse)
library(tidyr)


#We load both of the datasets
life_expectancy <- read.csv("LifeExpectancyDataset.csv")
economic_data <- read.csv("economic_data.csv")


#We select only the years of data that we are interested in
economic_data_filtered <- economic_data %>%
  filter(year >= 2010 & year <= 2015) %>%
  select(-Public.Debt....of.GDP., -GDP.per.Capita..Current.USD.)

#And we normalize all the country names
country_map <- c(
  "bahamas" = "bahamas, the",
  "bolivia (plurinational state of)" = "bolivia",
  "côte d'ivoire" = "cote d'ivoire",
  "congo"= "congo, rep.",
  "democratic republic of the congo" = "congo, dem.rep.",
  "democratic people's republic of korea" = "korea, dem. people's rep.",
  "egypt" = "egypt, arab rep.",
  "gambia" = "gambia, the",
  "iran (islamic republic of)" = "iran, islamic rep.",
  "kyrgyzystan" = "kyrgyz republic",
  "lao people's democratic republic" = "lao pdr",
  "micronesia (federated states of)" = "micronesia, fed. sts.",
  "republic of moldova" = "moldova",
  "republic of korea" = "korea, rep.",
  "slovakia" = "slovak republic",
  "united kingdom of great britain and northern ireland" = "united kingdom",
  "united states of america" = "united states",
  "swaziland" = "eswatini",
  "turkey" = "turkiye",
  "the former yugoslav republic of macedonia" = "north macedonia",
  "venezuela (bolivarian republic of)" = "venezuela, rb",
  "yemen" = "yemen rep."
)

#We apply the country name map and eliminate the GDP column which was found twice in the dataset
life_expectancy_filtered <- life_expectancy %>% 
  mutate(Country = ifelse(Country %in% names(country_map),
                          country_map[Country], Country))%>%
  select(-GDP)


#We merge both datasets by the name of the country and the year of the data
merged <- inner_join(life_expectancy_filtered, economic_data_filtered, by=c("Country" = "country_name", "Year" = "year"))


#We check for NAs
sum(is.na(merged))

#We clean the column names of the dataset
names(merged)

new_names <- c("Country",                         "Year",                            "Status",                         
               "LifeExpectancyMen",           "LifeExpectancyWomen",          "AdultMortalityMen",          
               "AdultMortalityWomen",         "InfantDeaths",                   "Alcohol",                        
               "PercentageExpenditure",          "HepatitisBMen",                "HepatitisBWomen",             
               "Measles",                         "BMI",                             "UnderFiveDeaths",              
               "Polio",                           "TotalExpenditure",               "Diphtheria",                     
               "HIV",                        "Population",                      "ThinnessTeens",           
               "ThinnessKids",              "IncomeComposition", "Schooling",                      
               "country_id",                      "InflationCPI",               "GDPCurrentUSD",              
               "UnemploymentRate",         "InterestRateReal",        
               "InflationGDPDeflator",     "GDPGrowthAnnual",           "CurrentAccountBalanceGDP",
               "GovernmentExpenseOfGDP",   "GovernmentRevenueOfGDP",   "Tax.RevenueOfGDP",
               "GrossNationalIncomeUSD")

#Apply the new names
merged <- setNames(merged, new_names)

#Converting the status column into a logical one, FALSE meaning the country is developing while TRUE means it is developed
merged$Status[merged$Status == "Developing"] = FALSE
merged$Status[merged$Status == "Developed"] = TRUE
merged$Status = as.logical(merged$Status)


#==============================================================================
#Backward elimination in order to find good predictors for the thinness in teens
#==============================================================================


#First of all we select the numeric values from the merged dataset
merged_numeric <- merged[,sapply(merged, is.numeric)]

#Here we eliminate the ThinnessKids column because it doesnt make sense having it in the model
#As well as the year column
merged_numeric <- merged_numeric %>%
  select(-ThinnessKids, -Year) %>%
  drop_na()

#We define the model with all the possible covariates
mod1 <- lm(ThinnessTeens ~ ., data = merged_numeric)
summary(mod1)
plot(mod1, 5)
plot(mod1, 1)
plot(mod1, 2)

#As in the residuals plot we see a lot of heterosdacity, we will use the log(ThinnessTeens)
#In order to fix this heterodascity

mod12 <- lm(log(ThinnessTeens) ~ ., data = merged_numeric)
summary(mod12)
plot(mod12, 5)
plot(mod12, 1)
plot(mod12, 2)

#Now that we can see that we fixed the heterosdascity a lot, we see that the values
#310, 311, 275 are giving problems in the residual plot and in the qq-plot so we are 
#going to discard them as they are outliers

merged_numeric_noutliers <- merged_numeric[-c(310, 311, 275),]

mod12 <- lm(log(ThinnessTeens) ~ ., data = merged_numeric_noutliers)
summary(mod12)
plot(mod12, 5)
plot(mod12, 1)
plot(mod12, 2)

#Now the points giving problems are 274, 273, 246 so we eliminate them aste previous ones

merged_numeric_noutliers2 <- merged_numeric_noutliers[-c(274, 273, 246),]

mod122 <- lm(log(ThinnessTeens) ~ ., data = merged_numeric_noutliers2)
summary(mod12)
plot(mod122, 5)
plot(mod122, 1)
plot(mod122, 2)


#Now that the heterosdascity problem is fixed, and also that the distribution
#has been normalized, we are ready to start with the backward elimination to 
#find good predictors for ThinnesTeens

model_after_elimination <- step(mod122, direction = "backward")

summary(model_after_elimination)

plot(model_after_elimination, 1)
plot(model_after_elimination, 2)

model_after_elimination$terms

#So after the elimination we ended up with a r-squared value of 0.6742 which is large enough and
#we also get 15 covariates that work well to predict ThinnessTeens.
#We also get an adjusted r^2 of 0.6587 which ends up having a really small difference
#with the r^2 just 0.0155


#==============================================================================
#Backward elimination in order to find good predictors for the IncomeComposition
#==============================================================================

income_modelo <- lm(IncomeComposition ~ GDPCurrentUSD + HIV + 
                        AdultMortalityMen + InfantDeaths + Alcohol + 
                        BMI + TotalExpenditure + UnemploymentRate + 
                        Status + ThinnessTeens + Population + 
                        InflationCPI + Measles + Polio,
                      data = merged)

summary(income_modelo)
plot(income_modelo, 5)

# R-squared is = 0.7242 and Adjusted R-squared is = 0.7184
# We also didnt find any outliers in the dataset
# Problem: many variables are not significant (p > 0.05)

# We remove AdultMortalityMen (p = 0.634, not significant)
income_modelo1 <- lm(IncomeComposition ~ GDPCurrentUSD + HIV + 
                         InfantDeaths + Alcohol + 
                         BMI + TotalExpenditure + UnemploymentRate + 
                         Status + ThinnessTeens + Polio,
                       data = merged)

summary(income_modelo1)
plot(income_modelo1, 5)

# R-squared is = 0.6687 and Adjusted R-squared is = 0.6642
# InfantDeaths still not significant (p = 0.124)

#Remove InfantDeaths (p = 0.124, not significant)
income_modelo2 <- lm(IncomeComposition ~ GDPCurrentUSD + 
                         Alcohol + 
                         BMI + TotalExpenditure + UnemploymentRate + 
                         Status + ThinnessTeens + Polio,
                       data = merged)

summary(income_modelo2)
plot(income_modelo2, 5)

#R-squared is = 0.6189 and Adjusted R-squared is = 0.6148
# All variables are now significant (p < 0.125) but R-squared went down compared to model 1

#For this we try the log transformations
# Some variables have skewed distributions
# Log makes them more normal and improves the model
merged_transform <- merged %>%
  mutate(log_GDP = log(GDPCurrentUSD),
         log_HIV = log(HIV),
         log_Polio = log(Polio),
         log_TotalExpenditure = log(TotalExpenditure),
         log_Alcohol = log(Alcohol),
         log_BMI = log(BMI),
         log_UnemploymentRate = log(UnemploymentRate),
         log_ThinnessTeens = log(ThinnessTeens)
  )

#We do a model with log transformations
modelo_log <- lm(IncomeComposition ~ log_GDP + log_HIV + log_Alcohol + 
                   log_BMI + log_TotalExpenditure + log_UnemploymentRate + 
                   log_ThinnessTeens + log_Polio,
                 data = merged_transform)

summary(modelo_log)

# R-squared is = 0.7361 and Adjusted R-squared is = 0.7332 
#It is the best one until now

# Compare all models
AIC(income_modelo, income_modelo1, income_modelo2, modelo_log)
BIC(income_modelo, income_modelo1, income_modelo2, modelo_log)
# modelo_log also has the HIGHEST Adjusted R-squared = 0.7332

#==============================================================================
#CONFIDENCE INTERVALS
#==============================================================================

#we find the confidence intervals for the "winner" model, the one with the highest r-squared
confint(model_after_elimination, level = 0.95)
shapiro.test(residuals(model_after_elimination)) #therefore we should reject the null hypothesis (allegedly)
plot(model_after_elimination, 1) #residuals vs fitted
plot(model_after_elimination, 2) #qqplot
plot(model_after_elimination, 5) #residuals vs leverage

#==============================================================================
#PREDICTION
#==============================================================================

set.seed(123) #to make sure that we get the same results after randomising

prediction_indexes <- sample(1:nrow(merged_numeric_noutliers2), size = 0.8 * nrow(merged_numeric_noutliers2)) #we select a 80/20 distribution

trainingdt <- merged_numeric_noutliers2[prediction_indexes,] #80% for the training part

testdt <- merged_numeric_noutliers2[-prediction_indexes,] #20% for the testing part


modeltraining <- lm(model_after_elimination, data = trainingdt) 

#we adjust the model with the mentioned 80%, with the same variables as the ThinnessTeens model we have previously used 
predictiontest <- predict(modeltraining, testdt) #we predict the other 20%


valoresreales <- log(testdt$ThinnessTeens)

correlacion <- cor(predictiontest, valoresreales, use = "complete.obs") #it gives us a correlation of about 0.92 

r2test <- correlacion^2 #it gives us a r^2 of approximately 0.61, which is really close to the one of the model(0.6587)
r2test

#Now, to find the confidence and prediction intervals
newcountry <- testdt[1,] #we select the first country from our test group

confidintr <- predict(modeltraining, newdata = newcountry, interval = "confidence")
exp(confidintr)
#this would get us the confidence interval for the regular value of ThinnessTeens

predicintr <- predict(modeltraining, newdata = newcountry, interval = "prediction")
exp(predicintr)
#this would get us the prediction interval for the regular value of ThinnessTeens

