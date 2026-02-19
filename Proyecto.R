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




