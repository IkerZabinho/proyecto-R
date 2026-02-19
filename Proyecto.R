library(tidyverse)

life <- read.csv("LifeExpectancyDataset.csv")
economic <- read.csv("economic_data.csv")

unique(life$Country) #herrialdiek zeñ dituken ikusteko
unique(economic$country_name)

life <- life %>% mutate(Country = tolower(trimws(Country))) #Dana miniskula pasau eta espaziyuek kendu
economic <- economic %>% mutate(country_name = tolower(trimws(country_name)))


mapa_paises <- c(
  "bolivia (plurinational state of)" = "bolivia",
  "côte d'ivoire" = "cote d'ivoire",
  "republic of korea" = "korea, rep.",
  "united kingdom of great britain and northern ireland" = "united kingdom",
  "united states of america" = "united states",
  "eswatini" = "swaziland",
  "the former yugoslav republic of macedonia" = "north macedonia",
  "venezuela (bolivarian republic of)" = "venezuela, rb"
)
#Aplicar reemplazos en minuscula
life <- life %>% 
  mutate(Country = ifelse(Country %in% names(mapa_paises),
                          mapa_paises[Country], Country))

#We merge both datasets by Country name and Year.
merged <- inner_join(life, economic, by=c("Country" = "country_name", "Year" = "year"))


#Even though by doing an inner join the data that has been merged is supposedly
# the one between 2010 and 2015 (as those are the years that are coincidental
# in both datasets), we filter it just in case there are missing values or any
# false values that accidentally got in
filtered <- merged %>% 
  filter(Year>=2010 & Year<=2015)


sum(is.na(life))

#Variable type change in Status column, Character -> Logical

filtered$Status[filtered$Status == "Developing"] = FALSE
filtered$Status[filtered$Status == "Developed"] = TRUE
filtered$Status = as.logical(filtered$Status)

typeof(filtered$Status)

print(dim(filtered))
dim(merged)
