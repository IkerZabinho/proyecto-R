library(tidyverse)

life <- read.csv("LifeExpectancyDataset.csv")
economic <- read.csv("economic_data.csv")

#In this part, as both datasets can possibly have the countries listed in a different way,
# we look them up manually, so that we can see the ones spelt differently and transform them
unique(life$Country) 
unique(economic$country_name)

life <- life %>% mutate(Country = tolower(trimws(Country))) #We set it to lowercase to simplify the process
economic <- economic %>% mutate(country_name = tolower(trimws(country_name)))

#we create a vector with the different ones and indicate that they are the same.
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
#if a country contains either of those names, we make them understand it is the same country,
# so that the data can be appropriately merged
life <- life %>% 
  mutate(Country = ifelse(Country %in% names(mapa_paises),
                          mapa_paises[Country], Country))

#we merge both datasets by Country name and Year, now that we have assured that the country
# name cannot suppose any problem.
merged <- inner_join(life, economic, by=c("Country" = "country_name", "Year" = "year"))


#Even though by doing an inner join the data that has been merged is supposedly
# the one between 2010 and 2015 (as those are the years that are coincidental
# in both datasets), we filter it just in case there are missing values or any
# false values that accidentally got in
filtered <- merged %>% 
  filter(Year>=2010 & Year<=2015)

unique(merged[,1])
unique(life[,1])


sum(is.na(life))

#Variable type change in Status column, Character -> Logical

filtered$Status[filtered$Status == "Developing"] = FALSE
filtered$Status[filtered$Status == "Developed"] = TRUE
filtered$Status = as.logical(filtered$Status)

typeof(filtered$Status)

print(dim(filtered))
dim(merged)

#Creation of new variable in Filtered and merged dataset: Above(TRUE)/Below(FALSE) average GDP
#This variable comes from the difficulty to categorize countries economically
average = mean(filtered$GDP..Current.USD., na.rm = TRUE)

filtered = filtered %>%
  mutate(above_below_average = ifelse(GDP..Current.USD. > average, TRUE, FALSE))

# Check the structure of your dataset
str(filtered)

# Count variables by type
sapply(filtered, class)          # Gives the class of each variable
table(sapply(filtered, class))   # Summarizes how many variables per type

# Histogram for Male Life Expectancy
ggplot(filtered, aes(x = Life.expectancy..men. )) +
  geom_histogram(bins = 20, fill = "blue", color = "black") +
  labs(title = "Distribution of Male Life Expectancy (2010-2015)",
       x = "Male Life Expectancy (years)",
       y = "Frequency")

# Histogram for Female Life Expectancy
ggplot(filtered, aes(x = Life.expectancy.women.)) +
  geom_histogram(bins = 20, fill = "pink", color = "black") +
  labs(title = "Distribution of Female Life Expectancy (2010-2015)",
       x = "Female Life Expectancy (years)",
       y = "Frequency")

# Boxplot comparing male and female life expectancy
ggplot(filtered, aes(x = "Male", y = Life.expectancy..men.)) +
  geom_boxplot(fill = "blue", alpha = 0.6) +
  geom_boxplot(aes(x = "Female", y = Life.expectancy.women.),
               fill = "pink", alpha = 0.6) +
  labs(title = "Comparison of Male and Female Life Expectancy (2010–2015)",
       x = "Gender",
       y = "Life Expectancy (years)")

# Scatterplot with regression line
ggplot(filtered, aes(x = GDP, y = Life.expectancy.women.)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Preston Curve Women: Life Expectancy vs GDP",
       x = "GDP",
       y = "Average Life Expectancy (years)")

# Scatterplot with regression line
ggplot(filtered, aes(x = GDP, y = Life.expectancy..men.)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Preston Curve Men: Life Expectancy vs GDP",
       x = "GDP",
       y = "Average Life Expectancy (years)")


