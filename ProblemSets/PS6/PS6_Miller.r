library(tidyverse)
library(plotly)
library(countrycode)
library(lubridate)

#plots included in the final problem set

#gini scrape from FRED

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

#Gini index for the United States
USA_gini <- gini_index %>%
  filter(country == "USA") %>%
  ggplot(aes(x = date, y = Gini_Index)) +
  geom_line(color = "#51b1da", size=2) +
  labs(title = "Gini Index Over Time for the United States",
       x = "Year",
       y = "Gini Index") +
       theme_minimal() +
        geom_vline(xintercept = as.numeric(as.Date("1980-01-01")), linetype = "dashed", color="red")+
        annotate("text", x = as.Date("1973-01-01"), y = 39, label = "Reagan Elected", color = "red", size = 6)
ggsave("PS6a_Miller.png", USA_gini, width = 10, height = 6, units = "in", dpi = 300, bg="white")



#hail data scrape

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

#hail density in Norman, OK
severe_percent <- mean(hail_data$Mag > 1, na.rm = TRUE) * 100

hail_density <- ggplot(hail_data, aes(x = Mag)) +
  geom_density(fill="#51b1da", alpha=0.75) +
  geom_vline(xintercept = 1, linetype = "dotted", color="black", size=1) +
  labs(title = "Distribution of Hail Sizes in Cleveland County, OK",
       x = "Hail Size (inches)",
       y = "Density") +
    annotate("text", x = 1.685, y = 1, label = paste0(round(severe_percent, 1), "% of hail in Norman is severe"), color="black", size=4) +
  theme_minimal()
ggsave("PS6b_Miller.png", width = 10, height = 6, units = "in", dpi = 300, bg="white")

#Distribution of NBA defensive efficiency

#my attempt at scraping the website

scrape_def_eff <- function(date, season_year) {
  url <- paste0("https://www.teamrankings.com/nba/stat/defensive-efficiency?date=", date)
  
  # Read the webpage
  page <- read_html(url)
  
  # Extract the table
  table <- page %>%
    html_node("#DataTables_Table_0") %>%
    html_table(fill = TRUE)
  
  # Select only the first three columns
  table <- table %>%
    select(1:3) %>%  # Keep only Rank, Team, and Defensive Efficiency
    rename(Rank = 1, Team = 2, DefensiveEfficiency = 3) %>%
    mutate(
      Date = date, 
      Season = season_year,  # Add the season year column
      DefensiveEfficiency = as.numeric(DefensiveEfficiency) # Convert to numeric
    )
  
  return(table)
}

# Define seasons and their corresponding dates
season_info <- tibble(
  Season = c("2020-21", "2021-22", "2022-23", "2023-24", "2024-25"),
  Date = c("2021-07-21", "2022-06-17", "2023-06-13", "2024-06-18", "2025-03-11")
)


# Scrape data for multiple seasons
nba_def_eff_data <- map2_dfr(season_info$Date, season_info$Season, scrape_def_eff)

#this code did not work and I could not find a workaround so
#I collected the data, dumped it into excel, and saved to a csv file
#This part of the script will not be executable on your machine. 

NBA_data <- read_csv("Def_Eff_NBA.csv")

okc_team <- "Okla City"
okc_year <- 2024

NBA_def_eff <- ggplot(NBA_data, aes(x = "All Teams", y =    `Defensive Efficiency`, color = (Team == okc_team & Year == okc_year))) +
  geom_jitter(aes(size = (Team == okc_team & Year == okc_year)), alpha = 0.6, position = position_nudge(x = 0.05)) +  
  scale_color_manual(values = c("grey70", "#002D62")) +  # Grey for others, navy for OKC
  scale_size_manual(values = c(3, 7)) +
  geom_text(data = NBA_data %>% filter(Team == okc_team & Year == okc_year), 
            aes(label = "OKC Thunder"), 
            vjust = 3, color = "#002D62", fontface = "bold") +  # Label for OKC
  labs(title = "OKC Thunder's Defensive Dominance (2024)",
       x = "",
       y = "Defensive Efficiency (lower is better)") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Remove x-axis labels since they aren't needed
        axis.ticks.x = element_blank(),
        legend.position = "none") 
ggsave("PS6c_Miller.png", width = 8, height = 8, units = "in", dpi = 300, bg = "white")


view(NBA_data)


#plots that I didn't include but I still think are cool!

#Gini index for the UK
gini_index %>%
  filter(country == "GBR") %>%
  ggplot(aes(x = date, y = Gini_Index)) +
  geom_line(color = "#51b1da", size=2) +
  labs(title = "Gini Index Over Time for China",
       x = "Year",
       y = "Gini Index") +
       theme_minimal() +
        geom_vline(xintercept = as.numeric(as.Date("1979-01-01")), linetype = "dashed", color="red") +
        annotate("text", x = as.Date("1973-01-01"), y = 35, label = "Thatcher Elected", color = "red", size = 6)


#average global gini index over time
gini_index %>%
  group_by(date) %>%
  summarise(mean_gini = mean(Gini_Index, na.rm = TRUE)) %>%
  ggplot(aes(x = date, y = mean_gini)) +
  geom_line(color = "#51b1da", size=2) +
  labs(title = "Average Global Gini Index Over Time",
       x = "Year",
       y = "Gini Index") +
       theme_minimal() +
        geom_vline(xintercept = as.numeric(as.Date("1980-01-01")), linetype = "dashed", color="red")+
        geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype = "dashed", color="red") +
        annotate("text", x = as.Date("1972-01-01"), y = 43, label = "Rise of Neoliberalism", color = "red", size = 6) +
        annotate("text", x = as.Date("2013-01-01"), y = 41, label = "COVID Pandemic", color = "red", size = 6)

#number of hail events by year in Norman, OK
hail_data %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  mutate(Year = year(Date)) %>%
  group_by(Year) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = Year, y = n)) +
  geom_line(color = "#51b1da", size=2) +
  labs(title = "Number of Hail Events by Year in Cleveland County, OK",
       x = "Year",
       y = "Number of Events") +
       theme_minimal()


#distribution of hail sizes with a vertical line at the mean
hail_data %>%
  ggplot(aes(x = Mag)) +
  geom_density(fill="#51b1da", alpha=0.75) +
  labs(title = "Distribution of Hail Sizes in Cleveland County, OK",
       x = "Hail Size (inches)",
       y = "Density") +
       theme_minimal() +
       geom_vline(xintercept = mean(hail_data$Mag, na.rm = TRUE), linetype = "dashed", color="red") +
       annotate("text", x = 2.5, y = 0.3, label = "Mean Hail Size", color = "red", size = 6)
mean(hail_data$Mag, na.rm = TRUE)
head(hail_data)

#mean hail size by year
hail_data %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  mutate(Year = year(Date)) %>%
  group_by(Year) %>%
  summarise(mean_hail_size = mean(Mag, na.rm = TRUE)) %>%
  ggplot(aes(x = Year, y = mean_hail_size)) +
  geom_line(color = "#51b1da", size=2) +
  labs(title = "Mean Hail Size by Year in Cleveland County, OK",
       x = "Year",
       y = "Mean Hail Size (inches)") +
       theme_minimal()

#average global gini index since 2000 with a vertical line at 2008
gini_index %>%
  filter(date >= "2000-01-01") %>%
  group_by(date) %>%
  summarise(mean_gini = mean(Gini_Index, na.rm = TRUE)) %>%
  ggplot(aes(x = date, y = mean_gini)) +
  geom_line(color = "#51b1da", size=2) +
  labs(title = "Average Global Gini Index Since 2000",
       x = "Year",
       y = "Gini Index") +
       theme_minimal() +
       geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype = "dashed")

#Gini index for the United States, China, Germany, Japan,
#India, the United Kingdom, France, Italy, Brazi, and Canada
gini_index %>%
  filter(country %in% c("USA", "CHN", "DEU", "JPN", "IND", "GBR", "FRA", "ITA", "BRA", "CAN")) %>%
  ggplot(aes(x = date, y = Gini_Index, color = country)) +
  geom_line(size=1) +
  labs(title = "Gini Index Over Time for Top-10 Economies",
       x = "Year",
       y = "Gini Index") +
       theme_minimal()


#This was my attempt at an animated world map. 
#Missing data and my inexperience with the plotly 
#package made this a bit too difficult for me to complete.
#I hope to learn more about this in the future because I really
#like the idea of an animated world map.
#plot_ly(
#  data = gini_index,
#  type = 'choropleth',
#  locations = ~iso3,  
#  locationmode = 'ISO-3',
#  z = ~Gini_Index,  
#  frame = ~date,  
#  colorscale = 'Viridis',
#  text = ~paste(country, "<br>Gini Index:", Gini_Index)  # Tooltip info
#) %>%
#  layout(
#    title = "Gini Index by Country Over Time",
#    geo = list(projection = list(type = 'natural earth')),
#    updatemenus = list(
#      list(
#        type = "buttons",
#        x = 0.1, y = -0.1,
#        buttons = list(
#          list(label = "Play",
#               method = "animate",
#               args = list(NULL, list(frame = list(duration = 1000, redraw = TRUE)))),
#          list(label = "Pause",
#               method = "animate",
#               args = list(NULL, list(frame = list(duration = 0, redraw = FALSE))))
#        )
#      )
#    )
#  )


