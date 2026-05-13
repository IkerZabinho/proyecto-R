#We load all the necessary libraries
library(tidyverse)
library(tidyr)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(scales)
library(ggrepel)

set.seed(123) #for reproducibility

#We read both datasets
life <- read.csv("LifeExpectancyDataset.csv")
economic <- read.csv("economic_data.csv")

#In this part, as both datasets can possibly have the countries listed in a different way,
# we look them up manually, so that we can see the ones spelt differently and transform them
unique(life$Country) 
unique(economic$country_name)

#We convert to lowercase and remove whitespace to simplify the process
life <- life %>% mutate(Country = tolower(trimws(Country)))
economic <- economic %>% mutate(country_name = tolower(trimws(country_name)))

#We create a vector with the different country names to make them equal
mapa_paises <- c(
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
#If a country contains either of those names, we make them understand it is the same country,
# so that the data can be appropriately merged
life <- life %>% 
  mutate(Country = ifelse(Country %in% names(mapa_paises),
                          mapa_paises[Country], Country))

#We merge both datasets by Country name and Year, now that we have assured that the country
# names cannot suppose any problem.
merged <- inner_join(life, economic, by=c("Country" = "country_name", "Year" = "year"))


#Even though by doing an inner join the data that has been merged is supposedly
# the one between 2010 and 2015 (as those are the years that are coincidental
# in both datasets), we filter it just in case there are missing values or any
# false values that accidentally got in
filtered <- merged %>% 
  filter(Year>=2010 & Year<=2015)

#Variable type change in Status column, Character -> Logical

filtered$Status[filtered$Status == "Developing"] = FALSE
filtered$Status[filtered$Status == "Developed"] = TRUE
filtered$Status = as.logical(filtered$Status)

#Creation of new variable in Filtered and merged dataset: Above(TRUE)/Below(FALSE) average GDP
#This variable comes from the difficulty to categorize countries economically

average = mean(filtered$GDP, na.rm = TRUE)

filtered = filtered %>%
  mutate(above_below_average = ifelse(GDP > average, TRUE, FALSE))

#Here as we don´t want any country with NA values in neither of its variables

filtered_clean <- filtered #We had previously removed all rows with NA but we've seen it's not useful
# and it gives us more inconveniences as we have far less countries to analyse

sum(is.na(filtered_clean))

filtered_clean

#Elimination of irrelevant variable

filtered_clean$GDP.per.Capita..Current.USD. = NULL

#Make a dataset without NA-s although there are less observations it may be useful later

filtered_clean2 <- filtered_clean %>%
  drop_na()


#Creation of new variable in Filtered and merged dataset: Above(TRUE)/Below(FALSE) average GDP
#This variable comes from the difficulty to categorize countries economically
average_GDP = mean(filtered_clean$GDP..Current.USD., na.rm = TRUE)
filtered_clean = filtered_clean %>%
  mutate(above_below_average = ifelse(GDP..Current.USD. > average_GDP, TRUE, FALSE))

# Check dataset structure and variable tipes
str(filtered_clean)
sapply(filtered_clean, class)          # Gives the class of each variable
table(sapply(filtered_clean, class))   # Summarizes how many variables per type

# Histogram for Male Life Expectancy
ggplot(filtered_clean, aes(x = Life.expectancy..men. )) +
  geom_histogram(bins = 20, fill = "blue", color = "black") +
  labs(title = "Distribution of Male Life Expectancy (2010-2015)",
       x = "Male Life Expectancy (years)",
       y = "Frequency")

# Histogram for Female Life Expectancy
ggplot(filtered_clean, aes(x = Life.expectancy.women.)) +
  geom_histogram(bins = 20, fill = "pink", color = "black") +
  labs(title = "Distribution of Female Life Expectancy (2010-2015)",
       x = "Female Life Expectancy (years)",
       y = "Frequency")

# Boxplot comparing male and female life expectancy
ggplot(filtered_clean, aes(x = "Male", y = Life.expectancy..men.)) +
  geom_boxplot(fill = "blue", alpha = 0.6) +
  geom_boxplot(aes(x = "Female", y = Life.expectancy.women.),
               fill = "pink", alpha = 0.6) +
  labs(title = "Comparison of Male and Female Life Expectancy (2010–2015)",
       x = "Gender",
       y = "Life Expectancy (years)")

df_grouped <-filtered_clean %>%
  group_by(Country) %>%
  summarise(
    GDP = mean(GDP..Current.USD.),
    Life_expectancy = (mean(Life.expectancy..men.) + mean(Life.expectancy.women.))/2,
    Status = Status,
    Population = mean(Population)
  )


ggplot(df_grouped, aes(x = GDP, y = Life_expectancy, color = Status)) +
  geom_point(aes(size = Population), alpha = 0.5) + 
  geom_smooth(method = "loess", color = "black", linetype = "dashed", se = FALSE) +
  scale_x_log10(
    breaks = c(1e8, 1e9, 1e10, 1e11, 1e12),
    labels = c("0.1 B", "1.0 B", "10.0 B", "100.0 B", "1000.0 B")
  ) + 
  scale_color_manual(
    values = c("TRUE" = "cadetblue2", "FALSE" = "coral"),
    labels = c("Developed", "Developing")
  ) +
  theme_minimal() +
  labs(
    title = "Preston Curve. Does money buy life?",
    subtitle = "(Average 2010-2015)",
    x = "Total GDP - USD (Billions)",
    y = "Life Expectancy (Years)",
    color = "Status",
    size = "Population of country"
  ) +
  
  theme(legend.position = "bottom")



###########################
numeric_df <- filtered_clean[, sapply(filtered_clean, is.numeric)]

x <- model.matrix(~ Life.expectancy..men. + Life.expectancy.women.,
                  data = filtered_clean)

y <- filtered_clean$GDP

solve(t(x) %*% x) %*% t(x) %*% y

mod <- lm(y ~ x-1,
          data = filtered_clean)

mod_s <- summary(mod)
names(mod_s)


#####################################################################
#DELIVERY 2
#####################################################################

life_expectancy_filtered <- life %>% 
  mutate(Country = ifelse(Country %in% names(mapa_paises),
                          country_map[Country], Country))%>%
  select(-GDP)

#We select only the years of data that we are interested in
economic_data_filtered <- economic %>%
  filter(year >= 2010 & year <= 2015) %>%
  select(-Public.Debt....of.GDP., -GDP.per.Capita..Current.USD.)

#We merge both datasets by the name of the country and the year of the data
merged <- inner_join(life_expectancy_filtered, economic_data_filtered, by=c("Country" = "country_name", "Year" = "year"))

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

merged_numeric_noutliers <- merged_numeric[-c(310, 311, 314, 315, 275),]

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

#Now that the heterosdascity problem is fixed, we are ready to start with the 
#backward elimination to find good predictors for ThinnesTeens

model_after_elimination <- step(mod122, direction = "backward")

summary(model_after_elimination)

plot(model_after_elimination, 1)
plot(model_after_elimination, 2)

model_after_elimination$terms

shapiro.test(residuals(model_after_elimination))

#the distribution
#has been normalized as we can see a 0.7982 p-value in the shapiro test
#and the Q-Q plot seems to follow a normal distribution,

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



################################################################################
#DELIVERY 3
################################################################################

# Taken from the merged dataset of previous deliverables

new_names <- c("Country",                         "Year",                            "Status",                         
               "LifeExpectancyMen",           "LifeExpectancyWomen",          "AdultMortalityMen",          
               "AdultMortalityWomen",         "InfantDeaths",                   "Alcohol",                        
               "PercentageExpenditure",          "HepatitisBMen",                "HepatitisBWomen",             
               "Measles",                         "BMI",                             "UnderFiveDeaths",              
               "Polio",                           "TotalExpenditure",               "Diphtheria",                     
               "HIV",           "GDP",             "Population",                      "ThinnessTeens",           
               "ThinnessKids",              "IncomeComposition", "Schooling",                      
               "country_id",                      "InflationCPI",               "GDPCurrentUSD",              
               "UnemploymentRate",         "InterestRateReal",        
               "InflationGDPDeflator",     "GDPGrowthAnnual",           "CurrentAccountBalanceGDP",
               "GovernmentExpenseOfGDP",   "GovernmentRevenueOfGDP",   "Tax.RevenueOfGDP",
               "GrossNationalIncomeUSD", "PublicDebtGDP", "Above/BelowAverage")

merged <- filtered_clean2

merged <- setNames(merged, new_names)

#==============================================================================
#PCA
#==============================================================================
# We group by Country to create structural profiles
df_grouped <- merged %>%
  group_by(Country, Status) %>%
  summarise(across(where(is.numeric), function(x) mean(x, na.rm = TRUE)), .groups = "drop")

# Selecting 7 numeric variables and a categorical one (Status)
df_pca_data <- df_grouped %>% 
  select(Country, LifeExpectancyMen, AdultMortalityMen, Schooling, 
         GDPCurrentUSD, Alcohol, BMI, HIV, Status) %>%
  drop_na()

# Setting Country as row names for identification in plots
df_final <- as.data.frame(df_pca_data)
rownames(df_final) <- df_final$Country
df_final <- df_final %>% select(-Country)

# Status (column 8 in df_final) is set as a supplementary qualitative variable
res.pca <- PCA(df_final, quali.sup = 8, scale.unit = TRUE, graph = FALSE)

# Eigenvalues check. We want to see if the components are optimal ( > 1)
print("Eigenvalues")
print(res.pca$eig) 

# Check which variables contribute most to Dim 1
print("Contributions to Dimension 1")
sort(res.pca$var$contrib[,1], decreasing = TRUE) 

# Quality of representation (cos2), how well is each variable represented
print("Quality of representation (cos2)")
print(res.pca$var$cos2)

# Elbow Plot visualization
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))

# Correlation Circle visualization
fviz_pca_var(res.pca, col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)


# Projecting 'Status' (Developed vs Developing)
fviz_pca_ind(res.pca, habillage = 8, addEllipses = TRUE, repel = TRUE, label = "ind")
res.pca$var$contrib[,1]


#==============================================================================
#CORRESPONDENCE ANALISYS
#==============================================================================

# Create levels for Schooling and Mortality (3 categories each)
merged_ca <- merged %>%
  group_by(Country) %>%
  summarise(
    Schooling = mean(Schooling),
    Mortality = (mean(AdultMortalityMen) + mean(AdultMortalityWomen))/2
  )%>%
  mutate(
    Schooling_Level = cut(Schooling, 
                          breaks = c(0, 10, 14, 22), 
                          labels = c("Low_School", "Mid_School", "High_School")),
    Mortality_Level = cut(Mortality, 
                          breaks = 3, 
                          labels = c("Low_Mort", "Med_Mort", "High_Mort"))
  ) %>%
  drop_na(Schooling_Level, Mortality_Level)



# Contingency Table - Observed frequencies between Schooling and Mortality
contingency_table <- table(merged_ca$Schooling_Level, merged_ca$Mortality_Level)
print(contingency_table)

# Chi-Square Independence Test)
# p-value > 0.05 indicates independence between categories
chi2_test <- chisq.test(contingency_table)
print(chi2_test) #p-value equals 0.02951, this is lower than 0.05 so these tells 
#us that there is dependency between schooling and mortality level.

#the schooling of a country does not help us predict the mortality.

####independence table: observed - expected
tab_independencia <- chi2_test$expected

print("Table under independence (expected values):")
print(round(tab_independencia, 2))

# we see the difference
# if positive, attracted, else, repelled.
difer <- contingency_table - tab_independencia
print(round(difer, 2))

# Corrplot for the attraction/repulsion
corrplot(chi2_test$residuals, is.cor = FALSE, 
         title = "Residuals (Blue: Attraction / Red: Repulsion)",
         mar=c(0,0,1,0))





#####

# Proportions of mortality for each schooling level
row_profiles <- round(prop.table(contingency_table, margin = 1), 3)
print(row_profiles)

# Proportions of Schooling for each mortality level
col_profiles <- round(prop.table(contingency_table, margin = 2), 3)
print(col_profiles)

# RUN CORRESPONDENCE ANALYSIS (CA)
res.ca <- CA(contingency_table, graph = FALSE)

# OPTIMAL NUMBER OF COMPONENTS
#hemen esplikaziyue sartu
print(res.ca$eig)
fviz_eig(res.ca, addlabels = TRUE)

# INTERPRETATION AND BIPLOT
# Visualizing the relationship between categories
fviz_ca_biplot(res.ca, repel = TRUE, 
               title = "CA Biplot: Schooling vs Mortality")

# CONTRIBUTIONS
# 'Low_School' and 'Low_Mort' define the main axis (Dim 1)
print("Row Contributions:")
print(res.ca$row$contrib)
print("Column Contributions:")
print(res.ca$col$contrib)

#==============================================================================
#K-MEANS
#==============================================================================


# We use PCA's first two coordinates, as the previous analysis indicated us the plot was located in comp. 2.
pca_clusters_data <- res.pca$ind$coord[, 1:2] 

#We use the elbow method again, in this case to select the number of clusters
fviz_nbclust(pca_clusters_data, kmeans, method = "wss") +
  # geom_vline(xintercept = 3, linetype = 2) +
  labs(subtitle = "Elbow method") 
#we select 3 (berez 2ra aldatzie eongohuan sieso? bñ azkenien 2tan banaute ya statusekin zakeau adibidez 
#ordun ns pixket para variar)

# Método de la Silueta (Silhouette Method)
fviz_nbclust(pca_clusters_data, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method") #bazpare jartzeiat bñ ni putisima idea honek ze eiteiken ze berez
#suposatzek altuena hartzie komeniko huala ta ns 10 eo 2 komeni dituken, 10 desdeluego ezetz


set.seed(123) #for reproducibility
km_res <- kmeans(pca_clusters_data, centers = 3, nstart = 25) #as explained earlier 3 centers, and 25 iterations just in case

#to visualize the results
fviz_cluster(km_res, data = pca_clusters_data,
             palette = "jco",
             ellipse = FALSE, #ellipse.type = CONVEX jartzie ziok ta clusterran perimetrue margotu bezela eiteik
             ggtheme = theme_minimal(),
             main = "K-means Clustering on PCA Dimensions")

# df_pca_raw <- pca_clusters_data
# 
# #in which cluster is each country?
# df_pca_raw$cluster <- as.factor(km_res$cluster)
# 
# # Table with the variables used in the pca and the countries that have been clustered
# cluster_interpretation <- df_pca_raw %>%
#   group_by(cluster) %>%
#   summarise(across(where(is.numeric), mean)) %>%
#   arrange(desc(LifeExpectancyMen))
# 
# print(cluster_interpretation)



#==============================================================================
#CLUSTERING
#==============================================================================

#Take the coordinates of the oints after the pca
pca_coords <- res.pca$ind$coord[, 1:2]

#plot the silhouette and the wss plot to see what number of clusters we need to define
#for each type of clustering methods, partitioning method will use the output of the wss while
#for the hierarchical method we will use the silhouette plot
fviz_nbclust(pca_coords, kmeans, method = "wss")
fviz_nbclust(pca_coords, kmeans, method = "silhouette")

#partitioning(k-means) 
#

km_res <- kmeans(pca_coords, centers = 4, nstart = 25)

fviz_cluster(km_res, data = pca_coords, palette = "jco", 
             ellipse = FALSE, geom = c("point", "text"), repel = TRUE,
             ggtheme = theme_minimal())

df_final$cluster <- as.factor(km_res$cluster)
cluster_summary <- df_final %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean))

contingency_table <- table(merged_ca$Schooling_Level, merged_ca$Mortality_Level)
chisq.test(contingency_table)

res.ca <- CA(contingency_table, graph = FALSE)

fviz_ca_biplot(res.ca, repel = TRUE, title = "CA: Schooling vs Mortality")

#dendrogram

pca_data <- res.pca$ind$coord #we create the distance matrix
distance_matrix <- dist(pca_data, method = "euclidean")


# Then, we apply the hclust function choosing the method (complete/single/...)
hc <-  hclust(distance_matrix, method = "complete")
hc
# We can plot the dendrogram
plot(hc)

# dendrogram totxuo
fviz_dend(hc,
          k = 3,               
          cex = 0.7,           
          lwd = 0.5,           
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800"), 
          color_labels_by_k = TRUE, 
          rect = FALSE,          
          main = "Hierarchical Clustering: Country Health Profiles",
          xlab = "Countries",
          ylab = "Height (Euclidean Distance)",
          ggtheme = theme_minimal() + theme(legend.position = "none"))


# Cortamos el árbol en 3 grupos
grupos <- cutree(hc, k = 3)

# Añadimos los grupos al dataframe original para ver las medias
df_final$cluster_jerarquico <- as.factor(grupos)

# Interpretación: ¿Qué caracteriza a cada grupo?
resumen_clusters <- df_final %>%
  group_by(cluster_jerarquico) %>%
  summarise(across(where(is.numeric), mean))

print(resumen_clusters)

