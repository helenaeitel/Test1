#Helena Eitel
#QSS 30.05
#Lab 1 Assignment
#Last Modified: 9/16/16

#Create a dataframe for a bookstore
author <- c("Virginia Woolf","J.K. Rowling","Anthony Doerr","Roxane Gay","William Shakespeare","F. Scott Fitzgerald","Fyodor Dostoevsky","Aldous Huxley","Ralph Ellison","E.B. White")
title <- c("To the Lighthouse","Harry Potter and the Half-Blood Prince","All the Light We Cannot See","Bad Feminist","Hamlet","The Great Gatsby","The Brothers Karamazov
           ", "Brave New World","Invisible Man","Charlotte's Web")
year <- c(1927,2006,2014,2014,1600,1925,1880,1932,1947,1952)
stock <- c(5,12,8,16,7,20,3,2,9,17)
price <- c(6.32,7.86,6.11,12.61,9.60,15.52,9.09,5.16,7.11,4.58)
bookstore <- data.frame(author,title,year,stock,price)

#Sort bookstore by year (descending) and author first name (ascending)
library(dplyr)
sortbook <- bookstore %>% arrange(-year,author)

#Everything before 1950 is 25% off, anything with 5 copies or left is 40% off
#Anything before 1950 AND 5 copies or less left is 50% off
#Create new columns for sale (yes or no) where it is on sale if it is from 1950 or older or stock is 5 or below
sale <- sortbook %>% mutate(sale=ifelse((year<1950)|(stock<=5),"yes","no"))
#Create a new column for sale price
sale$saleprice = sale$price
#Change that column to fit the conditions for different sale prices
sale1 <- sale %>% mutate(saleprice=ifelse(year<1950,ifelse(stock<=5,price*0.5,price*0.75),ifelse(stock<=5,price*0.6,price))

#Create a subset that includes only books on sale and only author, title, and sale price
subset <- sale %>% filter(sale=="yes")
subset1 <- subset %>% select(author,title,saleprice)

