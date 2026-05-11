# We load  the necessary libraries
library(tidyverse)
library(tidyr)
library(FactoMineR)
library(factoextra)

# Taken from the merged dataset of previous deliverables
merged <- read.csv("csv_merged2cat.csv") 

# We clean and standardize the column names for better accessibility
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

#Apply the new names
merged <- setNames(merged, new_names)

###### PCA #######

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


#### CORRESPONDECE ANALYSIS - CA ######

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
# p-value > 0.05 indicates independace between categories
chi2_test <- chisq.test(contingency_table)
print(chi2_test) #pvalue equals 0.2256, which indicates us that the link is really weak (independence)
#the schooling of a country does not help us predict the mortality.


# Proportions of mortality for each schooling level
row_profiles <- round(prop.table(contingency_table, margin = 1), 3)
print(row_profiles)

# RUN CORRESPONDENCE ANALYSIS (CA)
res.ca <- CA(contingency_table, graph = FALSE)

# OPTIMAL NUMBER OF COMPONENTS
# Dim 1 explains 98.9%, so 1 component is enough
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


#####K-MEANS


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



##CLUSTERING

pca_coords <- res.pca$ind$coord[, 1:2]

fviz_nbclust(pca_coords, kmeans, method = "wss")
fviz_nbclust(pca_coords, kmeans, method = "silhouette")

set.seed(123)
km_res <- kmeans(pca_coords, centers = 3, nstart = 25)

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
          k = 3,                 # El número de grupos que quieres colorear
          cex = 0.4,             # Tamaño de la fuente para los países
          lwd = 0.01,             # Grosor de las líneas (finito como pediste)
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800"), # Colores para cada cluster
          color_labels_by_k = TRUE, # Colorea también los nombres de los países
          rect = F,           # Añade el recuadro alrededor de cada grupo
          rect_fill = F,      # Rellena el fondo del recuadro (sutil)
          rect_border = "gray",  # Color del borde del recuadro
          main = "Dendrograma Jerárquico: Agrupación por Perfil de Salud",
          xlab = "Países",
          ylab = "Altura (Distancia)",
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



