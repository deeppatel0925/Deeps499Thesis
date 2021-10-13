# Introduction
title: "Sask Beach Test"
output: html_notebook

# load libraries
library(tidyverse)
library(readr)
library(dplyr)

setwd("/Users/Deep/Documents/University Year 4/BIOL 499/Boris Files/Exported BORIS Files")

# import main csv file
SaskBeach = read.csv("Sask Beach Full Observation Export Version 3.csv", header = TRUE, sep = ",")

# summary with structure and head
str(SaskBeach)
head(SaskBeach)


#DONE-need some marker to indicate interval end - example a point behaviour called END which gets added for all individuals at the end of each observation window

#DONE-possibly rename observation.id to video date or something or create a new column

#DONE-and then add another column for interval or observation period just so they can each be looked at separately

#DONE-keep just date, subject, behaviour, and start..s., Interval, Date
# Example: SaskBeach2<-select(SaskBeach, Subject, Behavior, Start..s.)

#put everything in chronological order (change Observation.id to date)
# Example: SaskBeach %>% .[order(SaskBeach$Observation.id, SaskBeach$Start..s.),] ->Saskordered


#DONEgroup_by individuals and observation window and then within each individual - counts the number of times a behaviour occurs 
# Example: Saskordered %>% group_by(Subject) %>% count(Behavior) -> count2

#Bat filters

Bat90F = filter(SaskBeach, Subject == "90F'")
BatB9A = filter(SaskBeach, Subject == "B9A'")
BatC4B = filter(SaskBeach, Subject == "C4B'")
Bat516 = filter(SaskBeach, Subject == "516'")
Bat7F0 = filter(SaskBeach, Subject == "7F0'")
BatD8A = filter(SaskBeach, Subject == "D8A'")
BatE77 = filter(SaskBeach, Subject == "E77'")

print(Bat90F)

#Intervals

IntervalA = filter(SaskBeach, Interval == "A")
IntervalB = filter(SaskBeach, Interval == "B")
IntervalC = filter(SaskBeach, Interval == "C")
IntervalD = filter(SaskBeach, Interval == "D")
IntervalE = filter(SaskBeach, Interval == "E")
IntervalF = filter(SaskBeach, Interval == "F")
IntervalG = filter(SaskBeach, Interval == "G")
IntervalH = filter(SaskBeach, Interval == "H")
IntervalI = filter(SaskBeach, Interval == "I")
IntervalJ = filter(SaskBeach, Interval == "J")
IntervalK = filter(SaskBeach, Interval == "K")
IntervalL = filter(SaskBeach, Interval == "L")
IntervalM = filter(SaskBeach, Interval == "M")
IntervalN = filter(SaskBeach, Interval == "N")
IntervalO = filter(SaskBeach, Interval == "O")
IntervalP = filter(SaskBeach, Interval == "P")
IntervalQ = filter(SaskBeach, Interval == "Q")
IntervalR = filter(SaskBeach, Interval == "S")
IntervalS = filter(SaskBeach, Interval == "R")
IntervalT = filter(SaskBeach, Interval == "T")
IntervalU = filter(SaskBeach, Interval == "U")
IntervalV = filter(SaskBeach, Interval == "V")
IntervalW = filter(SaskBeach, Interval == "W")
IntervalX = filter(SaskBeach, Interval == "X")
IntervalY = filter(SaskBeach, Interval == "Y")
IntervalZ = filter(SaskBeach, Interval == "Z")
IntervalAA = filter(SaskBeach, Interval == "AA")
IntervalAB = filter(SaskBeach, Interval == "AB")
IntervalAC = filter(SaskBeach, Interval == "AC")
IntervalAD = filter(SaskBeach, Interval == "AD")

print(IntervalA)

#Behavior

StillBehavior = filter(SaskBeach, Behavior == "Still")
count(StillBehavior) -> StillCount
WingStretchBehavior = filter(SaskBeach, Behavior == "WingStretch")
count(WingStretchBehavior) -> WingStretchCount
SelfgroomingBehavior = filter(SaskBeach, Behavior == "Selfgrooming")
count(SelfgroomingBehavior) -> SelfgroomingCount
LeaveclusterBehavior = filter(SaskBeach, Behavior == "Leave cluster")
count(LeaveclusterBehavior) -> LeaveclusterCount
Joincluster1Behavior = filter(SaskBeach, Behavior == "Join cluster 1")
count(Joincluster1Behavior) -> Joincluster1Count
HeadliftBehavior = filter(SaskBeach, Behavior == "Head lift")
count(HeadliftBehavior) -> HeadliftCount
ENDBehavior = filter(SaskBeach, Behavior == "END")
count(ENDBehavior) -> ENDCount
CrawloverBehavior = filter(SaskBeach, Behavior == "Crawl over")
count(CrawloverBehavior) -> CrawloverCount
BlockACUBehavior = filter(SaskBeach, Behavior == "Block ACU")
count(BlockACUBehavior) -> BlockACUCount
AvoidcrawloverBehavior = filter(SaskBeach, Behavior == "Avoid crawl over")
count(AvoidcrawloverBehavior) -> AvoidcrawloverCount
AllowcrawloverBehavior = filter(SaskBeach, Behavior == "Allow crawl over")
count(AllowcrawloverBehavior) -> AllowcrawloverCount
AllowACUBehavior = filter(SaskBeach, Behavior == "Allow ACU")
count(AllowACUBehavior) -> AllowACUCount
ACUTailfirstBehavior = filter(SaskBeach, Behavior == "ACU  Tail first")
count(ACUTailfirstBehavior) -> ACUTailfirstCount
ACUSideBehavior = filter(SaskBeach, Behavior == "ACU  Side")
count(ACUSideBehavior) -> ACUSideCount
ACUHeadfirstBehavior = filter(SaskBeach, Behavior == "ACU  Head first")
count(ACUHeadfirstBehavior) -> ACUHeadfirstCount
ActiveWalkingBehavior = filter(SaskBeach, Behavior == "Active  Walking")
count(ActiveWalkingBehavior) -> ActiveWalkingCount
ActiveFlyingBehavior = filter(SaskBeach, Behavior == "Active  Flying")
count(ActiveFlyingBehavior) -> ActiveFlyingCounts

print(StillCount)

##get the duration of behaviours - currently puts duration one row below where we want it
#Example: Saskordered %>% group_by(Subject) %>% mutate(duration=Start..s. - lag(Start..s.)) -> durationstest

SaskBeach %>%
  mutate (BehaviorDuration = (dplyr::lag(Start..s., n = 1, default = NA) - Start..s.)*(-1)) -> DurationTest

print(DurationTest)

##select just one individual to work with
##durationstestbat<-filter(durationstest, Subject=="B9A")

Duration90F = filter(DurationTest, Subject == "90F'")
DurationB9A = filter(DurationTest, Subject == "B9A'")
DurationC4B = filter(DurationTest, Subject == "C4B'")
Duration516 = filter(DurationTest, Subject == "516'")
Duration7F0 = filter(DurationTest, Subject == "7F0'")
DurationD8A = filter(DurationTest, Subject == "D8A'")
DurationE77 = filter(DurationTest, Subject == "E77'")


###count the number of blocks/ACU
#DONE? in behaviour count?



### count the number of blocks/ACU between two specific bats 
#unsure of how to go about this



###sum the total duration of a behaviour 
#shows code for "still" behaviour, how would I specify which subject is being looked at.

SaskBeach %>%
  mutate (BehaviorDurationStill = ((dplyr::lag(Start..s., n = 1, default = NA) - Start..s.)*(-1))[Subject == "B9A'"]) -> DurationTestStill
print(DurationTestStill)


###save your data frame 
write.csv(data, file="SaskBeachDataFrame.csv")

save(data, file="SaskBeachSave.RData")


#####################Data visualization ##############################
# Example: example1<-filter(count2, Subject=="B9A")
# library(ggplot2) <---- would I still need this line if I already installed tidyverse?
# keeps giving me error on how the input must be of size 1 not 76

plotStillCountExample1 <- SaskBeach %>% select(StillCount, Bat90F)
library(ggplot2)

#Behaviour Count Example

behaviours90F<-subset(SaskBeach, Subject=="90F'")
behaviourcount90F<-behaviours90F %>% group_by(Behavior) %>% count()

behavioursACUTotal<-subset(SaskBeach, Behavior=="ACU - Head first" & Behavior=="ACU - Side" & Behavior=="ACU - Tail first")
ACUCount<-behavioursACUTotal %>% group_by(Subject) %>% count()

#simple bar plot of the counts 
#added a pipe for future changes

  ggplot(data=behaviourcount90F, mapping = aes(x = Behavior, y= n)) + geom_bar(stat="identity")
  
  ggplot(data=ACUCount, mapping = aes(x = Subject, y= n)) + geom_bar(stat="identity")


##correlation of two numeric variables
#what variables would I use here?
ggplot(data=boxusedata, aes(x=temp, y=hum)) + geom_point() + xlim(0,40)

#save plot to working directory
ggsave("plot.png", width = 5,height =5)

##create new column in data frame
plotStillCountExample1$new<-plotStillCountExample1$n+1

####Julia's Data#####
##relationship strengths between two individuals 
##individual variation in connection strengths
## weighted degree - # of connections and the strength of those connections


#other code
#behaviours90F<-subset(SaskBeach, Subject=="90F'")
#behaviourcount90F<-behaviours90F %>% group_by(Behavior) %>% count()
#ggplot(data=behaviourcount90F, mapping = aes(x = Behavior, y= n)) + geom_bar(stat="identity")
