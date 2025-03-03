library(tidyverse)
library(dplyr)
library(rvest)
library(fredr)


#scraping a static webpage

#load the webpage into R
url <- "https://www.ncdc.noaa.gov/stormevents/listevents.jsp?eventType=%28C%29+Hail&beginDate_mm=01&beginDate_dd=01&beginDate_yyyy=2010&endDate_mm=11&endDate_dd=30&endDate_yyyy=2024&county=CLEVELAND%3A27&hailfilter=0.00&tornfilter=0&windfilter=000&sort=DT&submitbutton=Search&statefips=40%2COKLAHOMA"
page <- read_html(url)

# Use the CSS selector from SelectorGadget
hail_data <- page %>%
  html_element("#results") %>% 
  html_table(fill = TRUE)

#inspect what I have now
head(hail_data)

#clean the data

#remove the "in" from the Mag column using regex (I'm trying to practice with
#regex more because it seems so useful.)
hail_data$Mag <- gsub("in.", "", hail_data$Mag) %>%
  as.numeric(hail_data$Mag)#Convert to numeric here - I'm liking one-liners with 
#the pipe operator


#confirm the table looks how it should
view(hail_data)

#looks good!

#save to csv on my machine for future use
write.csv(hail_data, "hail_data.csv")

#gathering data using an API - I chose to use FRED to gather Gini Index data
#because Dr. B gave a presentation that got me thinking about how 
#income inequality has changed over time.

#setting up my API key from my .Renviron file here - NOT showing my key here
#as we discussed in class
fredr_set_key(Sys.getenv("FRED_API_KEY"))

#grabbing the Gini Index data for the United States
gini_index <- fredr::fredr_series_observations(series_id = "SIPOVGINIUSA")

#make sure the data looks how it should
view(gini_index)

#now I want Gini index data for other countries - this isnt so straightforward
#I will use the fredr_series_search_text function to search for Gini Index data
gini_series <- fredr_series_search_text(search_text = "Gini Index")
head(gini_series)
nrow(gini_series)#158 series in total

#this gave me the series ids for every country they have. This is similar to the search results for 
#the Gini Index on the FRED website. I will now use the fredr function 
#applied to the list of countries I got from the last step to get the data 
#for each country and then combine it into one dataframe

# Extract all Gini series IDs
gini_series_ids <- gini_series$id
gini_series_ids

# Define a function to get Gini data for a specific series ID - this was tricky
get_gini_data <- function(series_id) {
  fredr(series_id = series_id) %>%
    select(date, value) %>%
    mutate(country = series_id)
}

#Apply my function to all series IDs that I extracted from the text search and extract the data
gini_data_list <- lapply(gini_series_ids, get_gini_data)

#Combine results into a single dataframe and clean the country codes. This could have been multiple 
#steps but I combined them into one line with the pipe operator because I thought it was cleaner
gini_panel <- bind_rows(gini_data_list) %>%
  rename(Gini_Index = value) %>%
  mutate(country = gsub("SIPOVGINI", "", country))

#verify the data looks right
view(gini_panel)

#Looks good!

#also saving this one
write.csv(gini_panel, "gini_panel.csv")