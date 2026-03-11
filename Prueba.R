#We load all the necessary libraries
library(tidyverse)
library(tidyr)

life_expectancy <- read.csv("LifeExpectancyDataset.csv")
economic_data <- read.csv("economic_data.csv")

economic_data_filtered <- economic_data %>%
  filter(year >= 2010 & year <= 2015) %>%
  select(-Public.Debt....of.GDP., -GDP.per.Capita..Current.USD.)

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

life_expectancy_filtered <- life_expectancy %>% 
  mutate(Country = ifelse(Country %in% names(country_map),
                          country_map[Country], Country))%>%
  select(-GDP)


merged <- inner_join(life_expectancy_filtered, economic_data_filtered, by=c("Country" = "country_name", "Year" = "year"))

sum(is.na(merged))

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

merged <- setNames(merged, new_names)

merged$Status[merged$Status == "Developing"] = FALSE
merged$Status[merged$Status == "Developed"] = TRUE
merged$Status = as.logical(merged$Status)
#####
######Deliverable 2 - Zirriborrue

#backward eliminationen prueba
df_modelo <- merged %>%
  select(Schooling, Alcohol, BMI, HIV, GDPCurrentUSD,
         AdultMortalityMen, InfantDeaths, IncomeComposition,
         UnemploymentRate, InflationCPI) %>%
  drop_na()

full_model <- lm(Schooling ~  Alcohol+ BMI+ HIV+ GDPCurrentUSD+
                 AdultMortalityMen+ InfantDeaths+ IncomeComposition+
                 UnemploymentRate+ InflationCPI, data = merged)
summary(full_model)
plot(full_model,5)
#mrsquared 0.8414, ars 0.8394, quitamos gdp al ser el pvalor mas alto
modelo2 <- lm(Schooling ~  Alcohol+ BMI+ HIV+
                AdultMortalityMen+ InfantDeaths+ IncomeComposition+
                UnemploymentRate+ InflationCPI, data = merged)
summary(modelo2)
plot(modelo2, 5)
# mismo mrs, ars 0.8396, unemployment fuera
modelo3 <- lm(Schooling ~  Alcohol+ BMI+ HIV+
                AdultMortalityMen+ InfantDeaths+ IncomeComposition+
                InflationCPI, data = merged)
summary(modelo3)
plot(modelo3, 5)
#hemen ya mrs 0.8112, ars 0.8095,
which.max(cooks.distance(modelo3))
#al ver que ha bajado, identificamos los outliers (nos ha llamado la atención el 349 en está iteración, y el 108 que esta en todas)
df_sin_outliers <- merged[-c(108,349), ]
modelo31 <- lm(Schooling ~  Alcohol+ BMI+ HIV+
                AdultMortalityMen+ InfantDeaths+ IncomeComposition+
                InflationCPI, data = df_sin_outliers)
summary(modelo31)
plot(modelo31, 5)
#aquí ya vemos q sube mrs a 0.8543, ars 0.8529 (máximo hasta ahora). ahora vemos aumento de pvalor en BMI, quitamos
modelo4 <- lm(Schooling ~  Alcohol+ HIV+
                 AdultMortalityMen+ InfantDeaths+ IncomeComposition+
                 InflationCPI, data = df_sin_outliers)
summary(modelo4)
plot(modelo4, 5)
#tiramos con AIC y BIC
AIC(full_model, modelo2, modelo3, modelo31, modelo4)
BIC(full_model, modelo2, modelo3, modelo31, modelo4)



#Backward elimination in order to find good predictors for the thinnes in teens

merged_numeric <- merged[,sapply(merged, is.numeric)]

merged_numeric <- merged_numeric %>%
  select(-ThinnessKids) %>%
  drop_na()

mod1 <- lm(ThinnessTeens ~ ., data = merged_numeric)
summary(mod1)
plot(mod1)

#As the value of r^2 is so high, we can say that we can find good predictors
#First of all we are going to eliminate the columns that dont make sense
#and eliminate the outliers that we can see in the qq plot

merged_numeric <- merged_numeric[-c(190, 147),]

merged_numeric <- merged_numeric %>%
  select(-Year)

ss12 <- lm(ThinnessTeens^2 ~ ., data = merged_numeric)
summary(ss12)
plot(ss12, 5)
plot(ss12, 1)

<<<<<<< HEAD
#Backward elimination in order to find good predictors for the thinnes in teens
=======

#==============================================================================
#Backward elimination in order to find good predictors for the thinness in teens
#==============================================================================
>>>>>>> 61500ddb98b8e9b1e6b59024d1a807290f0e206d

#First of all we select the numeric values from the merged dataset
merged_numeric <- merged[,sapply(merged, is.numeric)]

#Here we elimante the thinnesKids column because it doesnt make sense to having it
#As well as the year column
merged_numeric <- merged_numeric %>%
  select(-ThinnessKids, -Year) %>%
  drop_na()

<<<<<<< HEAD
=======
mod1 <- lm(ThinnessTeens^2 ~ ., data = merged_numeric)
summary(mod1)
plot(mod1, 5)
>>>>>>> 61500ddb98b8e9b1e6b59024d1a807290f0e206d

#As the value of r^2 is so high, we can say that we can find good predictors
#First of all we are going to eliminate the columns that dont make sense
#and eliminate the outliers that we can see in the qq plot

merged_numeric <- merged_numeric[-c(190, 147),]

<<<<<<< HEAD
# merged_numeric <- merged_numeric %>%
#   select(-Year)
=======
#merged_numeric <- merged_numeric %>%
#  select(-Year)
>>>>>>> 61500ddb98b8e9b1e6b59024d1a807290f0e206d

ss12 <- lm(ThinnessTeens^2 ~ ., data = merged_numeric)
summary(ss12)
plot(ss12, 5)


#This done, we are going to start iterating in the model with the backwad elimination method
#with the step function which computes the backward elimination method based on the AIC method

model_after_elimination <- step(ss12, direction = "backward")

summary(model_after_elimination)

plot(model_after_elimination, 5)

#So we get that the covariates that work as predictor for the thinnes in teens are:
#InfantDeaths, Alcohol, Measles, UnderFiveDeaths, TotalExpenditure, Pôpulation, ThinnessKids(of course), GDPCurrentUSD, and the GrossNationalIncomeUSD

plot(ss12, 5)

<<<<<<< HEAD

=======
######ZABAN PARTIE
>>>>>>> 61500ddb98b8e9b1e6b59024d1a807290f0e206d
incomen_modelue <- lm(IncomeComposition ~ GDPCurrentUSD + HIV + 
                      AdultMortalityMen + InfantDeaths + Alcohol + 
                      BMI + TotalExpenditure + UnemploymentRate + 
                      Status + ThinnessTeens + Population + 
                      InflationCPI + Measles + Polio,
                    data = merged)

summary(incomen_modelue)

# R-squared is = 0.7242 and Adjusted R-squared is = 0.7184
# Problem: many variables are not significant (p > 0.05)

# We remove AdultMortalityMen (p = 0.634, not significant)
incomen_modelue1 <- lm(IncomeComposition ~ GDPCurrentUSD + HIV + 
                        InfantDeaths + Alcohol + 
                        BMI + TotalExpenditure + UnemploymentRate + 
                        Status + ThinnessTeens + Polio,
                      data = merged)

summary(incomen_modelue1)

# R-squared is = 0.6687 and Adjusted R-squared is = 0.6642
# InfantDeaths still not significant (p = 0.124)

#Remove InfantDeaths (p = 0.124, not significant)
incomen_modelue2 <- lm(IncomeComposition ~ GDPCurrentUSD + 
                         Alcohol + 
                         BMI + TotalExpenditure + UnemploymentRate + 
                         Status + ThinnessTeens + Polio,
                       data = merged)

summary(incomen_modelue2)
#@R-squared is = 0.6676 and Adjusted R-squared is = 0.6636
# All variables are now significant (p < 0.05) but R-squared went down compared to model 1

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
AIC(incomen_modelue, incomen_modelue1, incomen_modelue2, modelo_log)
BIC(incomen_modelue, incomen_modelue1, incomen_modelue2, modelo_log)
# modelo_log also has the HIGHEST Adjusted R-squared = 0.7332


#CONFIDENCE INTERVALS
#we find the confidence intervals for the "winner" model, the one with the highest r-squared
confint(model_after_elimination, level = 0.95)
shapiro.test(residuals(model_after_elimination)) #therefore we should reject the null hypothesis (allegedly)
plot(model_after_elimination, 1) #residuals vs fitted
plot(model_after_elimination, 2) #qqplot
plot(model_after_elimination, 5) #residuals vs leverage

#PREDICTION
set.seed(123) #to make sure that we get the same results after randomising
prediction_indexes <- sample(1:nrow(merged_numeric), size = 0.8 * nrow(merged_numeric)) #we select a 80/20 distribution
trainingdt <- merged_numeric[prediction_indexes,] #80% for the training part
testdt <- merged_numeric[-prediction_indexes,] #20% for the testing part

modeltraining <- lm(ThinnessTeens^2 ~ InfantDeaths + Alcohol + PercentageExpenditure
                     + BMI + UnderFiveDeaths + TotalExpenditure + HIV + 
                      IncomeComposition + InflationCPI + UnemploymentRate +
                    InterestRateReal + InflationGDPDeflator + GDPGrowthAnnual +
                      CurrentAccountBalanceGDP + GovernmentExpenseOfGDP +
                      GovernmentRevenueOfGDP  + Tax.RevenueOfGDP, data = trainingdt) 
#we adjust the model with the mentioned 80%, with the same variables as the ThinnessTeens model we have previously used 
predictiontest <- predict(modeltraining, testdt) #we predict the other 20%

valoresreales <- testdt$ThinnessTeens^2
correlacion <- cor(predictiontest, valoresreales, use = "complete.obs") #it gives us a correlation of about 0.92 
r2test <- correlacion^2 #it gives us a r^2 of approximately 0.85

#Now, to find the confidence and prediction intervals
newcountry <- testdt[1,] #we select the first country from our test group

confidintr <- predict(modeltraining, newdata = newcountry, interval = "confidence")
confidintr 

predicintr <- predict(modeltraining, newdata = newcountry, interval = "prediction")
predicintr

sqrt(predicintr) #this would get us the prediction interval for the regular value of ThinnessTeens
