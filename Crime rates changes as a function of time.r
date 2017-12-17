#my R show
#You may want to consider one or more of the following types of questions when developing your submission.

#For either city, how do incidents vary by time of day?
#  Which incidents are most common in the evening?
#  During what periods of the day are robberies most common?

#For either city, how do incidents vary by neighborhood? 
#  Which incidents are most common in the city center? 
#  In what areas or neighborhoods are robberies or thefts most common?

#For either city, how do incidents vary month to month in the Summer 2014 dataset?

#For either city, which incident types tend to correlate with each other on a day-by-day basis?

#Advanced What can we infer broadly about the differences in crime patterns between Seattle and San Francisco?
#  Does one city tend to have more crime than the other, per capita?
#  Do the relative frequencies of types of incidents change materially between the two cities?
# (NOTE: The two datasets do not have the same schema, so comparisons will require some work and some assumptions. This will require extra work, but you will be working at the forefront of what is known!)

#Advanced For either city, do certain crimes correlate with environmental factors such as temperature? (To answer this kind of question, you will need to identify and use external data sources!)




#without seeing the data what intests me is:
#1) how common each crime is in seatle? 
#2)how this picture changes by time in day? 
#3)and by months?
#3)and by years?


#rm(list=ls())
#dev.off()

#main
data=read.csv(choose.files())
data[,8]=NULL
data[,10:16]=NULL

data=data[-(data$Year<2008),]
library(stringr)

# parse time variable:
tot_time=str_split_fixed(data$Occurred.Date.or.Date.Range.Start, " ", 3)
time=tot_time[,2]
note=tot_time[,3]
rm(tot_time)
library(lubridate)
library(plyr)
time = factor(time)
time = hms(as.character(time))
hours =hour(time)
min=minute(time)
time=cbind(hours,note)
time=as.data.frame(time)
time$hours=as.numeric(as.character(time$hours))
corr_time=ifelse(time[,2]=="PM",time[,1]+12, time[,1]) 
data=cbind(data,corr_time)
data$corr_time=factor(data$corr_time)
#data$Year=factor(data$Year)

#data$Month=factor(data$Month)

# remove any offenses which happend less then 50 times
new=data[!table(data$Summarized.Offense.Description)[data$Summarized.Offense.Description] < 10000,] 
new$Year=factor(new$Year)


library(ggplot2)
library(scales) 


# how common each crime is in seatle? 
# bar plot of Summarized.Offense.Description: X offence,Y count
one=ggplot(new,aes(x=Summarized.Offense.Description))
one=one+geom_histogram(aes(fill=Summarized.Offense.Description),colour="Black",stat = "count")
one=one+theme(axis.text.x =element_blank() )
one=one+ggtitle("Most Common Crimes in Seattle (happens more then 10,000 over all years)") +
  xlab("Offence Type") 
one=one+scale_y_continuous(name="Frequency Of Offence", labels = comma)
one=one+ guides(fill=guide_legend(title="Offence Type"))



# how this picture changes by time at day?
# scatter plot of Summarized.Offense.Description: X offence, Y time+jitter,color=monthes
two=ggplot(new,aes(x=corr_time))
two=two+geom_histogram(aes(fill=Summarized.Offense.Description),colour="Black",stat = "count")
two=two+ggtitle("Most Common Crimes in Seattle During Diffrent Houres") +
  xlab("Time at Day") 
two=two+scale_y_continuous(name="Frequency Of Offences", labels = comma)
two=two+ guides(fill=guide_legend(title="Offence Type"))
#TODO: there must be a way to make it clearer, seperate rows for each offence?

two_half=ggplot(new,aes(x=corr_time))
two_half=two_half+geom_histogram(aes(fill=Summarized.Offense.Description),colour="Black",stat = "count")
two_half=two_half+facet_grid(Summarized.Offense.Description~. )
two_half=two_half+theme(strip.background = element_blank(),strip.text = element_blank(),axis.text.y =element_blank())
two_half=two_half+ggtitle("Most Common Crimes in Seattle During Diffrent Houres") +
  xlab("Time at Day") 
two_half=two_half+ guides(fill=guide_legend(title="Offence Type"))
#TODO: there must be a way to make it clearer, seperate rows for each offence?


# how this picture changes by years?

three=ggplot(new,aes(x=Year))
three=three+geom_histogram(aes(fill=Summarized.Offense.Description),colour="Black",stat = "count")
three=three+scale_x_discrete(limit = c("2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017"))
three=three+ggtitle("Most Common Crimes in Seattle During Diffrent Years (since 2008)") +
  xlab("Year") 
three=three+scale_y_continuous(name="Frequency Of Offences", labels = comma)
three=three+ guides(fill=guide_legend(title="Offence Type"))

#TODO: there must be a way to make it clearer, seperate rows for each offence?















