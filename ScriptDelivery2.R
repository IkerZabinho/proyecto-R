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

#Converting the status column into a logical one, FALSE meaning the country is developing
#while TRUE means it is developed
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

#As in the residuals plot we see a lot of heteroscedasticity, we will use the log(ThinnessTeens)
#In order to fix this heteroscedasticity

mod12 <- lm(log(ThinnessTeens) ~ ., data = merged_numeric)
summary(mod12)
plot(mod12, 5)
plot(mod12, 1)
plot(mod12, 2)

#Now that we can see that we fixed the heteroscedasticity a lot, we see that the values
#310, 311, 275 are giving problems in the residual plot and in the qq-plot so we are 
#going to discard them because they are outliers

merged_numeric_noutliers <- merged_numeric[-c(310, 311, 275),]

mod12 <- lm(log(ThinnessTeens) ~ ., data = merged_numeric_noutliers)
summary(mod12)
plot(mod12, 5)
plot(mod12, 1)
plot(mod12, 2)

#Now the points giving problems are 274, 273, 246 so we eliminate them as the previous ones

merged_numeric_noutliers2 <- merged_numeric_noutliers[-c(274, 273, 246),]

mod122 <- lm(log(ThinnessTeens) ~ ., data = merged_numeric_noutliers2)
summary(mod12)
plot(mod122, 5)
plot(mod122, 1)
plot(mod122, 2)

shapiro.test(residuals(mod122))

#Now that the heterosdascity problem is fixed, and also that the distribution
#has been normalized as we can see a 0.7982 p-value in the shapiro test
#and the Q-Q plot seems to follow a normal distribution, we are ready to
#start with the backward elimination to find good predictors for ThinnesTeens

model_after_elimination <- step(mod122, direction = "backward")

summary(model_after_elimination)

plot(model_after_elimination, 1)
plot(model_after_elimination, 2)

model_after_elimination$terms

#So after the elimination we ended up with a r-squared value of 0.6742 which is large enough and
#as we are dealing with health relaated data andwe also get 15 covariates that work well 
#to predict ThinnessTeens. We also get an adjusted r^2 of 0.6587 which ends up having a 
#really small difference with the r^2 just 0.0155 of difference


#==============================================================================
#Backward elimination in order to find good predictors for the IncomeComposition
#==============================================================================

income_model <- lm(IncomeComposition ~ GDPCurrentUSD + HIV + 
                        AdultMortalityMen + InfantDeaths + Alcohol + 
                        BMI + TotalExpenditure + UnemploymentRate + 
                        Status + ThinnessTeens + Population + 
                        InflationCPI + Measles + Polio,
                      data = merged)

summary(income_model)
plot(income_model, 5)
plot(income_model, 2)
plot(income_model, 1)

# R-squared is = 0.7242 and Adjusted R-squared is = 0.7184
#We can find one outlier with the Q-Q and residual plots which is 108, we eliminate it 

# We remove AdultMortalityMen (p = 0.634, not significant)
income_model1 <- lm(IncomeComposition ~ GDPCurrentUSD + HIV + 
                         InfantDeaths + Alcohol + 
                         BMI + TotalExpenditure + UnemploymentRate + 
                         Status + ThinnessTeens + Polio,
                       data = merged)

summary(income_model1)
plot(income_model1, 5)
plot(income_model1, 2)
plot(income_model1, 1)

# R-squared is = 0.6748 and Adjusted R-squared is = 0.6704
# InfantDeaths still not significant (p = 0.124)

#Remove InfantDeaths (p = 0.124, not significant)
income_model2 <- lm(IncomeComposition ~ GDPCurrentUSD + 
                         Alcohol + 
                         BMI + TotalExpenditure + UnemploymentRate + 
                         Status + ThinnessTeens + Polio,
                       data = merged)

summary(income_model2)
plot(income_model2, 5)
plot(income_model2, 2)
plot(income_model2, 1)

#R-squared is = 0.6384 and Adjusted R-squared is = 0.6345
# All variables are now significant (p < 0.125) but R-squared went down compared to model 1

income_model3 <- lm(IncomeComposition ~ GDPCurrentUSD + 
                  Alcohol + BMI + TotalExpenditure + UnemploymentRate + 
                  Status + ThinnessTeens + Polio,
                 data = merged)

summary(income_model3)
plot(income_model3, 5)
plot(income_model3, 2)
plot(income_model3, 1)

#Here there are some ouliers which end up having a value >3 in the residuals vs leverage plot
#so we get rid of them

noutlier <- merged[!(rownames(merged) %in% c("900", "393", "78", "108", "640")), ]

#We clearly see in the Q-Q plot and in the residuals plot that there are 2 outliers
#the 900 and the 108 so we are going to eliminate them

final_model <- lm(IncomeComposition ~ GDPCurrentUSD + 
                  Alcohol + BMI + TotalExpenditure + UnemploymentRate + 
                  Status + ThinnessTeens + Polio,
                data = noutlier)

summary(final_model)
plot(final_model, 5)
plot(final_model, 2)
plot(final_model, 1)

shapiro.test(residuals(final_model))

#After the removal of the outliers we see that the Q-Q plot has estabilized
#and as in the shapiro test we get a value of 0.4152 we cannot reject that
#this doesnt follow a normal distribution
# R-squared is = 0.7361 and Adjusted R-squared is = 0.7332 
#It is the best one until now

# Compare all models
AIC(income_model, income_model1, income_model2, income_model3, final_model)
BIC(income_model, income_model1, income_model2, income_model3, final_model)
# model_log also has the HIGHEST Adjusted R-squared = 0.7332

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



summary(model_after_elimination)


library(ggplot2)
library(tidyverse)
library(ggrepel)
library(scales)


df_preston_full <- merged %>%
  group_by(Country) %>% filter(!(Country %in% c("Canada", "China", "Sweden", "Norway"))) %>%
  summarise(

    Life_Exp = mean(c(LifeExpectancyMen, LifeExpectancyWomen), na.rm = TRUE),
    GDP_Total = mean(GDPCurrentUSD, na.rm = TRUE),
    Schooling = mean(Schooling, na.rm = TRUE),
    Status = first(Status)
  ) %>% filter(!is.na(Life_Exp), !is.na(GDP_Total), GDP_Total > 0)

ggplot(df_preston_full, aes(x = GDP_Total, y = Life_Exp , color = Status, label = Country)) +
  geom_point(aes(size = Schooling), alpha = 0.5) + 
  scale_x_log10(labels = label_number(suffix = " B", scale = 1e-9)) + 
  geom_smooth(aes(group = 1), method = "loess", color = "black", linetype = "dashed", se = FALSE) +
  geom_text_repel(size = 2.5, max.overlaps = 15, show.legend = FALSE) +
  scale_size(range = c(1, 8)) +
  scale_color_manual(values = c("TRUE" = "#00BFC4", "FALSE" = "#F8766D"), 
                     labels = c("Desarrollado", "En Desarrollo")) +
  labs(
    title = "Preston Curve. Does money buy life?",
    subtitle = paste("(Average 2010-2015)"),
    x = "Total GDP - USD (Billions)",
    y = "Life Expectancy (Years)",
    size = "Years of Schooling",
    color = "Status"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
cor(df_preston_full$Life_Exp, df_preston_full$GDP_Total) #Slightly positive correlation which proves our point.


modelo_mortalidad <- lm(AdultMortalityMen ~ UnemploymentRate * Status, data = merged)

summary(modelo_mortalidad) #NO CORRELATION.
