#------------------------- Report GHG ---------------------

# Project:(None) > New Project > New Directory > New Project > 
# Directory name > Browse > Create Project

# Database: 'historical_emissions.csv'
# Source: Climate Watch Historical GHG Emissions. 2022.
# Washington, DC: World Resources Institute.
# Available online at: https://www.climatewatchdata.org/ghg-emissions"

# Glossary (database):
# Country: Name of country/territory.
# Sector: Total including LUCF (Land-use Change and Forestry).
# Gas: All GHG (Including all kind of Greenhouse gases).
#Unit: MtCO2e (Metric tons of carbon dioxide equivalent)
# Data source: CAIT (Climate Watch)
# Years: 1990-2019

#Uploading packages: 
# Function 'library()' to upload installed package.
library(tidyverse)

# Como a base de dados que sera carregada esta no formato csv,
# separado por virgula, utilizaremos a funcao 'read_csv()'.
# Funcao 'read_csv()' do pacote readr
historical_emissions <- read_csv(file = "historical_emissions.csv")

# Vizualise estructure:
# Function 'glimpse()' of the package dplyr (part of tidyverse)
glimpse(historical_emissions)
# We can observe that the first columns (Country, Data source, Sector, Gas, and Unit) where properly identified
# as *character* and most of the years values as *double* a kind of numeric value. There is an exception with
# the year 1990, where data was identified as *character*.

#We must correct this whit the function 'mutate'
historical_emissions <- historical_emissions %>% 
                            mutate_at(c("1990"), as.numeric)

#To verify last change
glimpse(historical_emissions)

# Visualize dimension (column x rows)
# Funtion 'dim()'
dim(historical_emissions)
# That is, We have 195 countries/regions on this database

# Function 'nrow()' shows number of rows
nrow(historical_emissions)
# Function 'ncol()' shows number of columns
ncol(historical_emissions)
 
# Visualize names of variables into database (columns)
# Function 'colnames()'
colnames(historical_emissions)

# Visualize data base
# Function 'View()'
View(historical_emissions)

# We observed that some columns do not have relevant information
# We can retire those variables (Sector', 'Data source', 'Gas', 'Unit')
# Function 'select()' of the package dplyr
historical_emissions <-  historical_emissions %>% 
                            select(-'Sector', - 'Data source', -'Gas', -'Unit')

# Visualize database to confirm
glimpse(historical_emissions)


# To organize data by most pollutant countries
# Function 'arrange()' of package: dplyr
#Lets see the most pollutant countries in 1990
arranged1990 <- historical_emissions %>% 
                        arrange(desc(historical_emissions$`1990`))

#To observe the top 10 countries in 1990:
#As we has "World" as compiling all countries, we need to print the top 11 on the list
arranged1990 %>%
  select(c(Country, `1990`)) %>%
  print(n=11)


#Lets see the Countries organized originally (by 2019 values)
arranged2019 <- historical_emissions %>% 
                  arrange(desc(historical_emissions$`2019`))


#To observe the top 10 countries in 2019:
#As we has "World" as compiling all countries, we need to print the top 11 on the list
arranged2019 %>%
  select(c(Country, `2019`)) %>%
  print(n=11)


#Comparing both tables we observe differences between countries...


# To identify emission > 1000 in 2019 (filtering by x values...)
filtered_emissions <- historical_emissions %>% 
              filter(historical_emissions$`2019` > 1000.0) %>% 
              select(c(Country, `2019`)) %>%
              print(n=21)

# To identify emission > 1000 in 1990
filtered_emissions <- historical_emissions %>% 
                          filter(historical_emissions$`1990` > 1000.0) %>% 
                          select(c(Country, `1990`)) %>%
                          print(n=21)

# To identify a specific country (ex. Colombia)
# Function 'filter()' of package: dplyr
historical_emissions %>% 
  filter(Country == "Colombia") %>%
  select(Country, `2019`, `2018`)


# We can create a simple classification based on levels.
# Ex. we can choose year 2019 and insert a new column with levels A, B, C, D
# Being D to the higher and A to the lower.
# Funcao 'mutate()' do pacote dplyr
emissions_levels2019 <- historical_emissions %>% 
                          mutate(class_levels = 
                                   ifelse(`2019` < 100, "A", 
                                          ifelse(`2019` >= 100 & `2019` < 500, "B",
                                                 ifelse(`2019` >= 500 & `2019` < 1000, "C",   
                                                        "D")))) %>%
                          select(Country, `2019`, class_levels)

# Visualising this new change...
view(emissions_levels2019)


# Now, its is possible to visualize the number of countries on each level
emissions_levels2019 %>% 
    count(class_levels)
#So, most of the countries are in lower (A), 42 in level B, 9 in C and 9 in D

# To visualize the mean of emission for a year 
#Comparing 2019 and 1990).
# Function 'summarise()' of package dplyr
historical_emissions %>% 
  summarise(mean_2019 = mean(`2019`))

historical_emissions %>% 
  summarise(mean_2019 = mean(`1990`, na.rm = TRUE))

#In 1990 the mean was 351, while this value increased to 517.


# To show the number of countries (values) considered to calculate the mean 
# para o calculo
historical_emissions %>% 
    summarise(mean_2019 = mean(`2019`, na.rm = TRUE), n = n())
#The value of 195 match with the number of rows (Countries) into the table

# To see the mean to more years..., ex. last 8 years (and arrange into a new db)
historical_mean <- historical_emissions %>% 
                      summarise(m_2019 = mean(`2019`),
                                m_2018 = mean(`2018`),
                                m_2017 = mean(`2017`),
                                m_2016 = mean(`2016`),
                                m_2015 = mean(`2015`),
                                m_2014 = mean(`2014`),
                                m_2013 = mean(`2013`),
                                m_2012 = mean(`2012`))


# To visualize the new database
View(historical_mean)


# To observe more estatistical data for a year (2019).
#Mean, Standard dev., sum, median, variance, max, min and length values
historical_emissions %>% 
  summarise(m_2019 = mean(`2019`),
            sd_2019 = sd(`2019`),
            sum_2019 = sum(`2019`),
            median = median(`2019`),
            variance = var(`2019`),
            max_value = max(`2019`),
            min_value = min(`2019`),
            length_2019 = length(`2019`))