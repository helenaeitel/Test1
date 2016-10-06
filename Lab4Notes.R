#Helena Eitel
#Professor Merchant
#QSS 30.05
#Lab Assignment 4: Population Pyramids

library(readr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

a <- read_csv('./Extract 4.csv')

b <- a %>% mutate(Sex=factor(SEX,labels=c('Male','Female')))
#Add AGE
c <- b %>% mutate(Age=ifelse(AGE>=80,8,floor(AGE/10)))
#create vector of labels for age categories
agecats <- '0-9'
for (i in 1:7) {
  agecats <- c(agecats,paste(i,'0-',9,sep=''))
}
agecats <- c(agecats,'80+')

d <- c %>% mutate(Age=factor(AGE,labels=agecats))
e <- d %>% mutate(Gen=ifelse(BPL>150,'First Generation',
                             ifelse(MBPL>150,'Second Generation','Neither')))
#Add FBPL
f <- e %>% filter(YEAR>= 1960 | !(STATEFIP %in% c(2,15)))
g <- f %>% filter(Gen != 'Neither')
h <- g %>% mutate(weight=ifesle(YEAR==1940 & Gen=='Second Generation',SLWT,PERWT))
i <- h %>% group_by(Age,Sex,Gen,YEAR) %>% summarise(Number=sum(weight))
#make all of the male numbers negative so that they move left from the center of the axis
j <- i %>% mutate(Number = ifelse(Sex=='Male',Number*-1,Number))

#Create the graph
#Make it as if you were making a bargraph that you then flip on its side
ggplot(data=i,aes(x=Number,y=Age,fill=Sex)) + 
  geom_bar(data=j[H2$Sex=='Male',],stat='identity') +
  geom_bar(data=j[H2$Sex=='Female',],stat='identity') +
  coord_flip() +
  facet_grid(Gen~.~YEAR) +
  scale_y_continuous(breaks=c(-3000000,-1500000,0,1500000,3000000),
                     labels=c('3','1.5','0','1.5','3')) +
  labs(y='Population in Millions',title='Population Pyramids for Immigrants and their Children') +
  scale_fill_brewer(palette='Set1',guide=guide_legend(reverse=TRUE)) +
  theme_bw() + theme(legend.position='bottom')

#guides(fill=guide_legend(title='',title.position='left'))  
  
  

