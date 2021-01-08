starbucks_loc = read.csv('directory_df.csv')
#View(starbucks_loc)
country_code = read.csv('country_code.csv')
#View(country_code)
continent_country = read.csv('continent_country.csv')
# View(continent_country)

library(dplyr)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
#library(ggpubr) does not work for some reason
library(Hmisc) # for correlation calculation
library(stringr)
library(psych)


summary(starbucks_loc)
summary(country_code)
summary(continent_country)

starbucks_loc_by_country = starbucks_loc %>%
                           group_by(Country) %>%
                           summarise(num_of_loc = n())
#View(starbucks_loc_by_country)

# create a dataframe with the country, num of locations, continent
# loc_with_num_of_stores = full_join(starbucks_loc, starbucks_loc_by_country, by  = 'Country')
#View(loc_with_num_of_stores)
#loc_with_num_of_stores = cbind(loc_with_num_of_stores, continent_country)

# View(loc_with_num_of_stores %>%
#   group_by(Country) %>%
#     summarise(num_in_country = n()) %>%
#    group_by(State.Province) %>%
#    summarise(num_in_state = n()) %>%
#    group_by(City) %>%
#    summarise(num_in_city = n()))

#View(loc_with_num_of_stores %>%
      # group_by(Country) %>%
      # summarise(num_in_country = n()))

#View(loc_with_num_of_stores %>%
      # group_by(Country, State.Province) %>%
      # summarise(num_in_state = n()))

#View(loc_with_num_of_stores %>%
      # group_by(City) %>%
      # summarise(num_in_city = n()))

# In order to match country code with country name, I need to rename the 
# two-letter country code in continent_code.csv to Code, then join 
# continent_code.csv to country_code.csv. This way I will know what country 
# code corresponds to which country and the corresponding continent
 
 colnames(continent_country)[4] = 'Code'
country_names = full_join(continent_country, country_code, by = 'Code')
#View(country_names)

# Now I need to join country_names with starbucks_loc
# First I need to change the column name Country to Code so that I can full_joi
# by Code

colnames(starbucks_loc)[8] = 'Code'
starbucks_with_country_names = full_join(starbucks_loc, country_names, by = 'Code')
#View(starbucks_with_country_names)

starbucks_loc = starbucks_with_country_names %>%
                select(Brand, Continent_Name, Country_Name, State.Province, City,
                Store.Name, Store.Number, Street.Address, Ownership.Type) 

#View(starbucks_loc)

# Datatable for shiny dashboard. i want to display every country in each continent
# and how many stores are in each

starbucks_loc_num = starbucks_with_country_names %>%
   select(Brand, Continent_Name, Country_Name, State.Province, City,
          Store.Name, Store.Number, Street.Address, Ownership.Type) %>%
   group_by(Country_Name)



# How are stores distributed across continents? Bar graph it

starbucks_by_continent = starbucks_loc %>%
                        group_by(Continent_Name) %>%
                        summarise(count = n()) %>%
                        arrange(desc(count))
#View(starbucks_by_continent)

ggplot(starbucks_by_continent, aes(x = reorder(Continent_Name, count), 
                                   y = count)) +
 geom_bar(stat = 'identity') +
 # theme(axis.text.x = element_text(angle = 90, vjust = 0.30))
 coord_flip() +
 ylab('Number of Stores') +
 xlab('Continent') + 
 ggtitle('Continents by Store Count')

# How are stores distributed across countries?

length(unique(starbucks_loc$Country_Name)) # 254 unique countries

starbucks_by_country = starbucks_loc %>%
                       group_by(Country_Name) %>%
                       summarise(count = n()) %>%
                       arrange(desc(count))
View(starbucks_by_country)

top_15_starbucks_by_country = starbucks_by_country %>%
                              top_n(n = 15, wt = count)
#View(top_15_starbucks_by_country)

ggplot(top_15_starbucks_by_country, aes(x = reorder(Country_Name, count), 
                                        y = count)) +
 geom_bar(stat = 'identity') +
 # theme(axis.text.x = element_text(angle = 90, vjust = 0.30))
 coord_flip() +
 xlab('Country') +
 ylab('Number of Stores') + 
 ggtitle('Top 15 Countries by Store Count')

# Economic data from WEO
# economy = read.csv('WEO_Country.xls', sep = '\t')
# View(economy)

all_content = readLines("gdp_by_country.csv")
skip_second = all_content[-(1:4)]
world_gdp = read.csv(textConnection(skip_second), 
                     header = TRUE, stringsAsFactors = FALSE)
#View(world_gdp)

# My attempt to find the column with the min number of NA
#names(world_gdp)[which.min(sapply(world_gdp,MARGIN=2,min(sum(is.na))))]

#The contintent_country.csv has the three letter country code and so does 
#the world_gdp.csv, so I will join them together in order to be able to 
#compare the country's gdp with the number of stores

#First, change the column name from Three_Letter_Country_Code to Country.Code
#in in the continent_country.csv in order to match up with the world_gdp.csv 
#column name

colnames(continent_country)[5] = 'Country.Code'

#Now, that the column names match, full join by Country.Code
world_gdp = full_join(world_gdp, continent_country, by = 'Country.Code')

#View(world_gdp)


top_20_by_GDP = world_gdp %>%
                select(Continent_Name, Country_Name, Country.Name, 
                       Country.Code, X2016 ) %>%
                filter(is.na(Continent_Name) == 0 & is.na(Country_Name) == 0) %>%
                top_n(n = 20, X2016) %>%
                arrange(desc(X2016))
                
#View(top_20_by_GDP)                

View(world_gdp %>%
   select(Continent_Name, Country_Name, Country.Name, 
          Country.Code, X2016 ) %>%
   filter(is.na(Continent_Name) == 0 & is.na(Country_Name) == 0) %>%
   arrange(desc(X2016)))
 



#Compare top 15 countries by store count with top15 countries by gdp

top.15.store.top.20.gdp = inner_join(top_15_starbucks_by_country, 
                                     top_20_by_GDP, by = 'Country_Name')
top.15.store.top.20.gdp = top.15.store.top.20.gdp %>%
filter(duplicated(top.15.store.top.20.gdp[,'Country_Name'])== 0) %>%
                          arrange(desc(X2016)) %>%
                          mutate(X2016 = X2016/1e9) #in billions

#View(top.15.store.top.20.gdp)
#
#11/15 countries with the most Starbucks Stores are in the top 20 GDP from 2016
# Plot this

ggplot(top.15.store.top.20.gdp, aes(x = reorder(Country_Name, X2016), y = X2016)) + 
   geom_bar(stat = 'identity') +
   coord_flip() +
   xlab('Country') +
   ylab('GDP in Billions')

#Make a scatter plot to relate gdp and store count
ggplot(top.15.store.top.20.gdp, aes(x = X2016, y = count, color = Country_Name)) +
   geom_point() +
   xlab('GDP in Billions') +
   ylab('Store COunt')
   
# I suspect that there may be a linear relationship between GDP and Store Count
# I want to verify this by calculating the pearson coeficient
sto_count = top.15.store.top.20.gdp$count
gdp = top.15.store.top.20.gdp$X2016

GDP_StoreCount_corr = rcorr(gdp, sto_count, type = c('pearson'))
GDP_StoreCount_corr
GDP_StoreCount_corr$P # for exact p-values

# The p-value is < 0.05, so we should think they are dependent
# The correlation matrix sems to suggest 0.91 for linear dependece, 
# so my hunch about linear relationship is correct

# What about the remaining 4/15 countries in top store count
# that dont make the top 20 in gdp?
# They are: Taiwan, Philippines, Thailand, Malaysia

four_country_gdp = world_gdp %>%
                   select(Country_Name, Continent_Name,
                          Country.Name, Country.Code, X2016) %>%
                   filter(Continent_Name %in% c('Asia')) %>%
                   mutate(X2016 = X2016/1e9) %>%
                   arrange(desc(X2016))
#View(four_country_gdp)
# In Asia GDP, Thailand, Philippines, and Malaysia rank 10, 15, and 16 resp.
# Taiwan GDP is included in China GDP

# These four countries definitely contribute to Asia's second place
# in store count by continent
# Why Asia?
# Millenials drink a lot of coffee, so let's look at average age
# url = https://population.un.org/wpp/Download/Standard/Population/

# From 2015
avg_age = read.csv('average_age.csv', header = F, sep = ';')
#View(avg_age)
 
# Taiwan does not show up in this dataset, so I looked up the median age for 
# Taiwan and it was 39.7 in 2015. I added it to the dataset.

colnames(avg_age) = c('Country_Name', 'Average_Age', 'Year')
avg_age = avg_age[,1:2]

View(avg_age)
# The country names between avg_age and store count don't match, so I need to 
# cbind the column for avg age to the store count dataset

avg_avg_top_15 = c(37.7, 36, 40.5, 46.5, 40.5, 40.5, 30.1, 27.7, 39.7, 23.4,
                   38, 28.4, 28.2, 38.5, 46.3)

top_15_starbucks_by_country = cbind(top_15_starbucks_by_country, avg_avg_top_15)
View(top_15_starbucks_by_country)

# Use a scatter plot to visualize the relationshiop between avg_age and
# store count

ggplot(top_15_starbucks_by_country, aes(x = avg_avg_top_15, y = count, color = Country_Name)) +
   geom_point()

age = top_15_starbucks_by_country$avg_avg_top_15
sto_count_age = top_15_starbucks_by_country$count

rcorr(age, sto_count_age, type = c('pearson','spearman'))

# I think I need one massive data table to do the job of comparing two features. 
# I think feature comparison might be easier with one massive dataset.

colnames(world_pop)[1] = 'Country_Number'

merged_data = left_join(continent_country, world_pop, by = 'Country_Number')

world_pop = merged_data %>%
   filter(Time == 2016) %>%
   group_by(Country_Name) %>%
   mutate(Pop = sum(PopTotal)) %>%
   select(Continent_Name, Continent_Code, Country_Name, Code, Country.Code, Pop)

world_pop = full_join(continent_country, world_pop, by = 'Country.Code') %>%
            filter(duplicated(Code.x) == 0) 
           
View(world_pop)
View(starbucks_by_country)

pop_count = full_join(world_pop, starbucks_by_country)

pop_count_age = left_join(pop_count, avg_age, by = 'Country_Name')
View(pop_count_age)

pop_count_age = pop_count_age %>%
   select(Continent_Name.x, Continent_Code.x, Country_Name.x, Country_Name, Code.x,
          Country.Code, Country_Number, Pop, count, Average_Age)

col = c('Continent_Name', 'Continent_Code', 'Country_Name', )

world_gdp_small = world_gdp %>% 
   select(Continent_Name, Continent_Code, Country_Name, Country.Name, Code, 
          Country.Code, Country_Number, X2016) %>%
   filter(Continent_Name != NA)


count_by_country = full_join(starbucks_with_country_names, 
                             starbucks_by_country, by = 'Country_Name')

count_by_country = count_by_country %>%
        select(Brand, Continent_Name, Continent_Code, Country_Name,
               Three_Letter_Country_Code, Country_Number, Name, count) %>%
        filter(duplicated(Country_Number) == 0)

count_gdp = full_join(count_by_country, world_gdp_small, by = 'Country_Number')

count_gdp = count_gdp %>%
   select(Brand, Continent_Name.x, Continent_Code.x, Country_Name.x, 
          Country.Code, Country_Number, Country.Name, Name, Code,
          count, X2016)

count_gdp_pop = full_join(count_gdp, world_pop, by = 'Country_Number')

count_gdp_pop = count_gdp_pop %>%
        select(Brand, Continent_Name.x.x, Continent_Code.x.x, Country_Name.x.x,
               Country.Code.x, Country_Number, Country.Name, Name, Code, count, 
               X2016, Pop)

Shiny_data = count_gdp_pop %>%
   select(Brand, Continent_Name.x.x, Country_Name.x.x,
          count, X2016, Pop)

col = c('Brand', 'Continent.Name', 'Country.Name', 
        'Store.Count', 'GDP_(billions)', 'Population_(thousands)')

colnames(Shiny_data) = col

View(Shiny_data)


Shiny_data = Shiny_data %>%
              filter(is.na(Brand) == 0)

Shiny_data = Shiny_data %>%
        mutate(GDP = GDP/1e9)

write.csv(x = Shiny_data, file = 'Shiny_data.csv', row.names = F)

# stores per 10,000 people
# GDP in billions per cap
Shiny_data = Shiny_data %>% 
   mutate(Stores_per_cap = (Store.Count * 1e5)/Population_.thousands.) %>%
   mutate(GDP_per_cap = (GDP_.billions.*1000)/Population_.thousands.) %>%
   filter(duplicated(Country.Name) == 0)

# Remove column X and Brand
Shiny_data = Shiny_data %>%
             select(Continent.Name, Country.Name, 
                    Store.Count, Population_.thousands.,
                    GDP_.billions., GDP_per_cap, Stores_per_cap, GDP_per_cap)

# illiteracy rates
illiteracy <- read.csv("illiteracy.csv", stringsAsFactors = F)
View(illiteracy)
# Change Three_Letter_Country_Code in continent_country to join with illteracy
colnames(continent_country)[5] = 'LOCATION'

illit = full_join(illiteracy, continent_country, by = 'LOCATION')
View(illit)
colnames(illit)[12] = 'Country.Name'

illit2 = full_join(Shiny_data, illit, by = 'Country.Name') %>%
        group_by(Country.Name) %>%
        summarise(Illit.Pop = mean(Value, na.rm = T))
View(illit2)

Shiny_data = full_join(Shiny_data, illit2, by = 'Country.Name') %>%
             filter(is.na(Brand) == 0) %>%
             mutate(Illiteracy = Illit.Pop/Population_.thousands.)
             
Shiny_data = Shiny_data%>%
   select(Continent.Name, Country.Name, Store.Count,
                    GDP_.billions., Population_.thousands.,
                    Stores_per_cap, GDP_per_cap, Illiteracy) 

crime = read.csv('crime_rate.csv', stringsAsFactors = F)
View(crime)
colnames(crime)[1] = 'Country'

crime_real = full_join(crime, illit, by = 'Country') %>%
        group_by(Country.Name) %>%
        summarise(crimeIndex = mean(crimeIndex)) %>%
        filter(is.na(crimeIndex) == 0)

colnames(crime_real)[1] = 'Country.Name'

Shiny_data = full_join(Shiny_data, crime_real, by = 'Country.Name') %>%
   filter(is.na(Store.Count) == 0)

Shiny_data = Shiny_data[-(10)]
colnames(Shiny_data)[9] = 'Crime'

income = read.csv('income.csv', stringsAsFactors = F)
colnames(income)[1] = 'Country'

income2 = full_join(income, illit, by = 'Country') %>%
        group_by(Country.Name) %>%
        summarise(medianHouseholdIncome, medianPerCapitaIncome) %>%
        filter(duplicated(Country.Name) == 0)

Shiny_data = full_join(Shiny_data, income2, "Country.Name") %>%
        filter(is.na(Store.Count) == 0)

correlations = rcorr(as.matrix(Shiny_data[,3:11]))
correlations

# Stores per capita <-> Median Household Income 0.81
# Store Count <-> GDP 0.91
# Stores Per Capita <-> Median per capita income 0.63


Shiny_data = read.csv('Shiny_data.csv', stringsAsFactors = F)

# Drace Comments:
#    I noticed that you presented a RMD file, you can actually knit it as 
#    it's the "presentation" version of rmd. In addition with the echo = False 
#    flag. That being said, I think Powerpoint would still my general 
#    recommendation over markdown since slide decks tend to be industry 
#    standard =) also an "easy fix" for your Country naming issues, if you 
#    observe that the format is general "country name, blah" just trim based on 
#    where "," is

View(Shiny_data)


ggplot(Shiny_data, aes(x = medianHouseholdIncome, y = Stores_per_cap, color = Continent.Name)) +
   geom_point() +
   xlab('Median Household Income') +
   ylab('Stores per Capita')


ggplot(Shiny_data, aes(x = medianPerCapitaIncome, y = Stores_per_cap, color = Continent.Name)) +
   geom_point() +
   xlab('Median Per Capita Income') +
   ylab('Stores per Capita')






























































































