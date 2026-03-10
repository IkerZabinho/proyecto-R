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
                          mapa_paises[Country], Country))%>%
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
numeric_df <- filtered_clean[, sapply(filtered_clean, is.numeric)]

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

ss1 <- lm(ThinnessTeens ~ ., data = merged_numeric)
summary(ss1)
plot(ss1, 5)

#As the value of r^2 is so high, we can say that we can find good predictors
#First of all we are going to eliminate the columns that dont make sense
#and eliminate the outliers that we can see in the qq plot

merged_numeric <- merged_numeric[-c(190, 147),]

merged_numeric <- merged_numeric %>%
  select(-Year)

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
