#DS for Econ PS4_Miller First R Exercise

#load necessary packages
library(jsonlite)
library(tidyverse)

#download data from the internet in JSON format using a system command
system('wget -O dates.json "https://www.vizgr.org/historical-events/search.php?format=json&begin_date=00000101&end_date=20240209&lang=en"')

#view the data with the system command- its messy
system("cat dates.json")

#convert json into table object
mylist <- fromJSON("dates.json")

#now convert to a dataframe
mydf <- bind_rows(mylist$result[-1])

#check class of mydf
class(mydf)

#check class of the dates column of mydf
class(mydf$dates)

#view the first n rows of mydf
head(mydf)