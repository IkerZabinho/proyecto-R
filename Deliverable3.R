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



################################

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

###### PCA# #######
# Selecting 7 numeric variables and a categorical one (Status)
df_pca_raw <- merged %>%
  select(LifeExpectancyMen, AdultMortalityMen, Schooling, 
         GDPCurrentUSD, Alcohol, BMI, HIV, Status) %>%
  drop_na()

# Column 8, in this case Status, is set as a supplementary qualitative variable
res.pca <- PCA(df_pca_raw, quali.sup = 8, scale.unit = TRUE, graph = FALSE)

#We want to see if the components are optimal
print("--- Eigenvalues ---")
print(res.pca$eig)
fviz_eig(res.pca, addlabels = TRUE, main = "Scree Plot")

print("--- Variable Correlations ---")
print(res.pca$var$coord)

# Dim 1 represents Social Development (Strong link to Schooling: 0.86)
# Dim 2 represents Economy and Mortality (Strong link to GDP: 0.64)

# Correlation Circle visualization
fviz_pca_var(res.pca, col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, title = "Variables - PCA") #Using this, we see the correlation among variables in a visual way

# CATEGORICAL PROJECTION (Requirement D)
# Projecting 'Status' (Developed vs Developing)
# Developed countries (TRUE) cluster on the right side (High Dim 1)
fviz_pca_ind(res.pca,
             geom.ind = "point", 
             col.ind = df_pca_raw$Status, 
             palette = c("#FC4E07", "#00AFBB"),
             addEllipses = TRUE,
             legend.title = "Status",
             title = "PCA: Countries by Development Status")

# CONTRIBUTIONS AND QUALITY
# Check which variables contribute most to Dim 1
print("Variable Contributions to Dim 1:")
sort(res.pca$var$contrib[,1], decreasing = T)

# Quality of representation (cos2)
print("Quality of representation (cos2):")
print(res.pca$var$cos2)

##############
#### CA ######

# 1. PREPARE CATEGORICAL DATA
# Create levels for Schooling and Mortality (3 categories each)
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

# 2. CONTINGENCY TABLE
# Observed frequencies between Schooling and Mortality
contingency_table <- table(merged_ca$Schooling_Level, merged_ca$Mortality_Level)
print(contingency_table)

# 3. CHI-SQUARE TEST (Independence Test)
# p-value > 0.05 means variables are independent (no strong link)
chi2_test <- chisq.test(contingency_table)
print(chi2_test)

# 4. ROW PROFILES
# Proportions of mortality for each schooling level
row_profiles <- round(prop.table(contingency_table, margin = 1), 3)
print(row_profiles)

# 5. RUN CORRESPONDENCE ANALYSIS (CA)
res.ca <- CA(contingency_table, graph = FALSE)

# 6. OPTIMAL NUMBER OF COMPONENTS
# Dim 1 explains 98.9%, so 1 component is enough
print(res.ca$eig)
fviz_eig(res.ca, addlabels = TRUE)

# 7. INTERPRETATION AND BIPLOT
# Visualizing the relationship between categories
fviz_ca_biplot(res.ca, repel = TRUE, 
               title = "CA Biplot: Schooling vs Mortality")

# 8. CONTRIBUTIONS
# 'Low_School' and 'Low_Mort' define the main axis (Dim 1)
print("Row Contributions:")
print(res.ca$row$contrib)
print("Column Contributions:")
print(res.ca$col$contrib)


write_csv(filtered_clean, "csv_merged2cat.csv")