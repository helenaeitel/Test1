#Helena Eitel
#Professor Merchant
#QSS 30.05
#9/29/2016

#Lab Assignment 3: Graphing

library(dplyr)

#only use working age (15-65) and exclude Alaska and Hawaii
a <- read.csv('./Extract 2.csv') %>% filter(AGE>=15 & AGE<=65 & !STATEFIP %in% c(2,15))

#create a new variable grouping races into four categories
b <- a %>% mutate(Race2=factor(ifelse(RACED %in% c(100,150),1,
                                    ifelse(RACED %in% c(200,210),2,
                                           ifelse(RACED==400,3,4))),
                             labels=c('white','black','Native American','Asian')))

#create a new variable grouping occupations into six categories
c <- b %>% mutate(Occupation=factor(ifelse(OCC1950<=099,6,
                                           ifelse(OCC1950<=290 | OCC1950 %in% c(810:979),2,
                                                  ifelse(OCC1950<=490,4,
                                                         ifelse(OCC1950<=690,3,
                                                                ifelse(OCC1950<=790,5,1))))),
                                    labels=c('none','farmers and farm laborers','craftsmen/operatives/laborers',
                                             'managerial/clerical/sales','service','professional')))

#recode the sex variable
d <- c %>% mutate(Sex=ifelse(SEX==1,'Male','Female'))                  

#create the dataset for graph 2
e <- d %>% group_by(YEAR,Sex,Race2) %>% summarise(Pop=sum(PERWT))

#create the dataset for graph 4
f <- d %>% group_by(YEAR,Sex,Occupation,Race2) %>% summarise(Pop2=sum(PERWT))

library(ggplot2)

#create a bargraph for graph 2
png('Figure2.png',height=500,width=1000)
ggplot(data=e,aes(x=YEAR,y=Pop,fill=Sex)) +
  geom_bar(stat='identity') +
  labs(x='Year',y='Population',fill='Sex',title='2. Population Aged 15-65 by Race, Year and Sex, 1870-1920') +
  scale_y_continuous(labels=scales::comma) +
  scale_x_continuous(breaks=c(1870,1900,1920)) +
  scale_fill_brewer(palette='Set2', guide=guide_legend(reverse=TRUE)) +
  facet_wrap(~Race2,ncol=2,scales='free_y')
dev.off()

#create a bar graph for graph 4
png('Figure4.png',height=500,width=1000)
ggplot(data=f,aes(x=YEAR,y=Pop2,fill=Occupation)) +
  geom_bar(stat='identity',position='fill') +
  labs(x='Year',y='Percent of Population',fill='Occupation',title='Occupation of Persons Aged 15-65 by Sex, Race, and Year, 1870-1920') +
  scale_y_continuous(labels=scales::percent) +
  scale_x_continuous(breaks=c(1870,1900,1920)) +
  scale_fill_brewer(palette='Set1') +
  facet_grid(Sex~.~Race2) +
  theme_bw() + theme(legend.position='bottom')
dev.off()