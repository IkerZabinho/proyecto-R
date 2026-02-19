library(tidyverse)
life <- read.csv("LifeExpectancyDataset.csv")
life
economic <- read.csv("economic_data.csv")
economic

#We merge both datasets by Country name and Year.
merged <- inner_join(life, economic, by=c("Country" = "country_name", "Year" = "year"))

merged

#Even though by doing an inner join the data that has been merged is supposedly
# the one between 2010 and 2015 (as those are the years that are coincidental
# in both datasets), we filter it just in case there are missing values or any
# false values that accidentally got in
filtered <- merged %>% filter(Year>=2010 & Year<=2015)

filtered

#patatin patatero
#golaso de messiiiiii