
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