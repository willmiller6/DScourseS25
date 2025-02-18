#DS for Econ PS4_Miller Second R Exercise

#install.packages("sparklyr", dependencies = T) - this isn't working locally or
#on oscer

#first load packages
library(tidyverse)
library(sparklyr)

#set connection
sc <- spark_connect(master = "local")
?as_tibble
#create a tibble from the iris dataframe called df1
df1 <- as_tibble(iris)

#copy the tibble to the spark cluster
df <- copy_to(sc, df1)

#verify datatypes
class(df1); class(df)

#select from within RDD
df1 %>% select(Sepal_Length, Species) %>% 
  head %>% print

#fiter from within RDD
df1 %>% filter(Sepal_Length>5.5) %>% 
  head %>% print

#combine those operations
df1 %>% select(Sepal_Length, Species) %>% 
  filter(Sepal_Length>5.5)  %>% head %>% print

#group by from within RDD
df2 <- df1 %>% group_by(Species) %>% 
  summarize(mean = mean(Sepal_Length), count = n()) %>% 
  head %>% print

#using arrange from within RDD
df2 %>% arrange(Species) %>% head %>% print
