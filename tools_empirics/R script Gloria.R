library(readr)
library(dplyr)
library(lubridate)

#Load the dataset
World_Stock_Prices_Dataset <- read_csv("C:/Users/Gloria/Desktop/LMEC/Second Year/Machine Learning Barigozzi/World-Stock-Prices-Dataset.csv")

#Remove unnecessary colums
World_Stock_Prices_Dataset_Cleaned <- World_Stock_Prices_Dataset[ , -c(2, 3, 4, 8, 11, 12, 13)]
sort(table(World_Stock_Prices_Dataset_Cleaned$Country), decreasing = TRUE)

#Keep only USA based companies
World_Stock_Prices_Dataset_Cleaned <- World_Stock_Prices_Dataset_Cleaned[World_Stock_Prices_Dataset_Cleaned$Country == "usa", ]
World_Stock_Prices_Dataset_Cleaned %>%
  group_by(Date) %>%
  summarise(n = n()) %>%
  arrange(Date)

#Rename colums for clarity
names(World_Stock_Prices_Dataset_Cleaned)[names(World_Stock_Prices_Dataset_Cleaned) == "Brand_Name"] <- "Company"
names(World_Stock_Prices_Dataset_Cleaned)[names(World_Stock_Prices_Dataset_Cleaned) == "Close"] <- "Closing_Price"
World_Stock_Prices_Dataset_Cleaned$Date <- as.Date(World_Stock_Prices_Dataset_Cleaned$Date)

#Keep only observations from 1 January 2014 until 31 December 2024
World_Stock_Prices_Dataset_Cleaned <- World_Stock_Prices_Dataset_Cleaned[
  World_Stock_Prices_Dataset_Cleaned$Date >= as.Date("2015-01-01") &
    World_Stock_Prices_Dataset_Cleaned$Date <= as.Date("2025-01-01"), 
]

#First we make sure the total number of years is 10
total_years <- World_Stock_Prices_Dataset_Cleaned %>%
  mutate(Year = year(Date)) %>%
  pull(Year) %>%
  unique() %>%
  length()

#We keep only the companies appearing every day from 2015 until 2024
total_days <- World_Stock_Prices_Dataset_Cleaned %>% pull(Date) %>% unique() %>% length()

dataset_complete <- World_Stock_Prices_Dataset_Cleaned %>%
  group_by(Company) %>%
  filter(n_distinct(Date) == total_days) %>%
  ungroup()

View(World_Stock_Prices_Dataset_Cleaned)

#We check how many companies we have retained in the interval 2014-2024
n_companies <- dataset_complete %>% 
  pull(Company) %>% 
  n_distinct()

print(n_companies) # --> 38 companies all listed in the stock market every day for 10 years

#To help us visualize the panel data we order the companies in the same way in each block (each day will have the same companies listed in descending order) 
dataset_complete <- dataset_complete %>%
  arrange(Date, Company)

#To get the list of the names of the companies we have retained we run:
companies_list <- dataset_complete %>% 
  pull(Company) %>% 
  unique()

print(companies_list)
write.csv(dataset_complete, "dataset_complete.csv", row.names = FALSE)

