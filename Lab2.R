#Helena Eitel
#QSS 30.05
#Lab Assignment 2
#Last Modified: 9/22/16

#open various packages to use later
library(dplyr)
library(readr)
library(tidyr)


#make sure working directory is correct
getwd()
#read IPUMS data
a <- read_csv("./Extract1.csv")
#read a file with the character conversions for race
r <- read_csv("./RacedRacec.csv")

#exclude Alaska and Hawaii altogether
aa <- a %>% filter(!(YEAR < 1960 & STATEFIP %in% c(2,15)))
#add the corresponding race characters to dataframe a in a new column
b <- inner_join(aa,r,"RACED")
#within each year and within each race category return the number of people (sum of PERWTs)
c <- b %>% group_by(YEAR,RACEC) %>%summarise(NUMBER = sum(PERWT))
#display the table such that each race is a column
e <- d %>% spread(YEAR,NUMBER)

#save the final file to the working directory
write_csv(e,"RaceTable.csv")

