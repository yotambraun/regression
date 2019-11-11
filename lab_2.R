
library(dplyr)
library(ggplot2)
library(tidyverse)
library(gridExtra) #for the colors of lines at 5
library(tidyr) #for func speard on 6
diseases <- read.csv("C:/Users/Yotam braun/Desktop/R in statistic/diseases.txt")

#Part 1
1.
diseases <- mutate(diseases,rate = round(population*(count/weeks_reporting)*52/100000, 1))
as.numeric(diseases$rate)
dat <- filter(diseases, diseases$disease == "Measles")
dat <- filter(dat, dat$state != "Hawaii" & dat$state != "Alaska")

2.
dat_cali <- filter(dat, state == "California")
qplot(dat_cali$year,dat_cali$rate,geom = "line")+ geom_vline(xintercept = 1963,col ="red")

3.
continuous_y <-scale_y_log10(dat$rate)
ggplot( aes(factor(year),conditionCall()), data = dat) + geom_boxplot()

#part 2

#5.
diseases_cali <- filter(diseases, state == "California")
ggplot(data=diseases_cali,mapping = aes(x=year,y =rate,group = interaction(disease),color=disease))+geom_line()+
ggtitle("Rates of different diseases across years in California")


#6.
#why filter delete year
cali_2_diseases <- diseases_cali %>% select(disease, rate, year) %>% filter(disease== c("Hepatitis A","Rubella"))

cali_2_diseases <- diseases_cali %>% filter(disease== c("Hepatitis A","Rubella")) %>% spread(key=disease,value = rate)
ggplot(data=cali_2_diseases,mapping = aes(x =cali_2_diseases$`Hepatitis A`, y =cali_2_diseases$Rubella,group = interaction(cali_2_diseases$year))) +geom_point()

#7.

#part 3
#8.


       
                                                                            
