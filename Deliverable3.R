#We load  the necessary libraries
library(tidyverse)
library(tidyr)
library(FactoMineR)
library(factoextra)
library(tidyverse)


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


<<<<<<< HEAD
################################
=======

>>>>>>> 614e5fe5ee864606549eabd46257c5cd28453f71
df_multivariate <- merged %>%
  select(LifeExpectancyMen, AdultMortalityMen, Alcohol, Schooling, 
         GDPCurrentUSD, InflationCPI, UnemploymentRate, BMI, HIV) %>%
  drop_na()

res.pca <- PCA(df_multivariate, scale.unit = TRUE, graph = FALSE)

fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))

fviz_pca_var(res.pca, col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

fviz_pca_ind(res.pca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             geom = "point", repel = TRUE)

merged_ca <- merged %>%
  mutate(
    Schooling_Level = cut(Schooling, 
                          breaks = c(0, 10, 14, 22), 
                          labels = c("Low_School", "Mid_School", "High_School")),
    Mortality_Level = cut(AdultMortalityMen, 
                          breaks = 3, 
                          labels = c("Low_Mort", "Med_Mort", "High_Mort"))
  ) %>%
  drop_na(Schooling_Level, Mortality_Level)

tabla_contigencia <- table(merged_ca$Schooling_Level, merged_ca$Mortality_Level)

print(chisq.test(tabla_contigencia))

res.ca <- CA(tabla_contigencia, graph = FALSE)

fviz_ca_biplot(res.ca, repel = TRUE, 
               col.row = "blue", col.col = "red")

fviz_ca_row(res.ca, col.row = "contrib", 
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

<<<<<<< HEAD
=======

>>>>>>> 614e5fe5ee864606549eabd46257c5cd28453f71
