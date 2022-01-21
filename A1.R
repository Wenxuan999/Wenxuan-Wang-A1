setwd("/Users/wenxuan/Desktop/613/A1/Data")

#=============================================
# Exercise 1: 
#=============================================
#1
library(data.table)
data1=read.csv("dathh2007.csv")
length(unique(data1$idmen))

#2
data2=read.csv("dathh2005.csv")
table1=table(factor(data2$mstatus))
table1[names(table1)=="Couple, with Kids"]

#3
data3=read.csv("datind2008.csv")
length(unique(data3$idind))

#4 
data4=read.csv("datind2016.csv")
a=data4$age>=25 & data4$age<=35
table2=summary(a)
num1=table2[3]
num1

#5
data5=read.csv("datind2009.csv")
crosstable = table(data5$gender, data5$profession)
crosstable
#6
data6=read.csv("datind2005.csv")
data7=read.csv("datind2019.csv")
#for 2005
#mean
mean(data6$wage,na.rm=T)
#The standard deviation
sd(data6$wage, na.rm=TRUE) 
#IQR
IQR(data6$wage, na.rm=T)
#Gini
data6_1=data6%>%
  drop_na(wage)
u=sum(data6_1$wage*1/18767)
x = data6_1$wage
y = t(data6_1$wage)
1/(2*u)*1/18767*1/18767*sum(abs(outer(x,y,FUN="-")))

##for 2019
#mean
mean(data7$wage,na.rm=T)
#The standard deviation
sd(data7$wage,na.rm=T)
#IQR
IQR(data7$wage, na.rm=T)
#Gini 
data7_1=data7 %>%
drop_na(wage)
u=sum(data7_1$wage*1/21421)
x = data7_1$wage
y = t(data7_1$wage)
1/(2*u)*1/21421*1/21421*sum(abs(outer(x,y,FUN="-")))


#7 Plot an histogram of 2010
data8=read.csv("datind2010.csv")
hist(data8$age)
#for female
data9=filter(data8,data8$gender=="Female")
hist(data9$age)
##for male
data10=filter(data8,data8$gender=="Male")
hist(data10$age)
#Thus, there is difference between males and females

#8 Number of individuals in Paris in 2011.
datind2011=read.csv("datind2011.csv")
dathh2011=read.csv("dathh2011.csv")
merge2011=merge(datind2011, dathh2011,by="idmen")
merge2011_1=merge2011%>%filter(location=="Paris")
length(unique(merge2011_1$idind))

#=============================================
# Exercise 2: 
#=============================================
#1 Read individual datasets
options(scipen = 200)
datind2004=read.csv("datind2004.csv")
datind2005=read.csv("datind2005.csv")
datind2006=read.csv("datind2006.csv")
datind2007=read.csv("datind2007.csv")
datind2008=read.csv("datind2008.csv")
datind2009=read.csv("datind2009.csv")
datind2010=read.csv("datind2010.csv")
datind2011=read.csv("datind2011.csv")
datind2012=read.csv("datind2012.csv")
datind2013=read.csv("datind2013.csv")
datind2014=read.csv("datind2014.csv")
datind2015=read.csv("datind2015.csv")
datind2016=read.csv("datind2016.csv")
datind2017=read.csv("datind2017.csv")
datind2018=read.csv("datind2018.csv")
datind2019=read.csv("datind2019.csv")
##append all the data
Append1=rbind(datind2004,datind2005,datind2006,datind2007,datind2008,datind2009,datind2010,datind2011,datind2012,datind2013,datind2014,datind2015,datind2016,datind2017,datind2018,datind2019)

#2 Read all household datasets from 2004 to 2019. Append all these datasets.
#Read household datasets
dathh2004=read.csv("dathh2004.csv")
dathh2005=read.csv("dathh2005.csv")
dathh2006=read.csv("dathh2006.csv")
dathh2007=read.csv("dathh2007.csv")
dathh2008=read.csv("dathh2008.csv")
dathh2009=read.csv("dathh2009.csv")
dathh2010=read.csv("dathh2010.csv")
dathh2011=read.csv("dathh2011.csv")
dathh2012=read.csv("dathh2012.csv")
dathh2013=read.csv("dathh2013.csv")
dathh2014=read.csv("dathh2014.csv")
dathh2015=read.csv("dathh2015.csv")
dathh2016=read.csv("dathh2016.csv")
dathh2017=read.csv("dathh2017.csv")
dathh2018=read.csv("dathh2018.csv")
dathh2019=read.csv("dathh2019.csv")
Append2=rbind(dathh2004,dathh2005,dathh2006,dathh2007,dathh2008,dathh2009,dathh2010,dathh2011,dathh2012,dathh2013,dathh2014,dathh2015,dathh2016,dathh2017,dathh2018,dathh2019)

#3 List the variables in the individual and household datasets.
ls(dathh2019)
ls(datind2019)
#Then find the same variables
##The variables are X, year, idmen



#4Merge the individual and household datasets. Then append them in a dataset called merge1
merge2004=merge(datind2004, dathh2004,by=c('idmen','year'))
merge2005=merge(datind2005, dathh2005,by=c('idmen','year'))
merge2006=merge(datind2006, dathh2006,by=c('idmen','year'))
merge2007=merge(datind2007, dathh2007,by=c('idmen','year'))
merge2008=merge(datind2008, dathh2008,by=c('idmen','year'))
merge2009=merge(datind2009, dathh2009,by=c('idmen','year'))
merge2010=merge(datind2010, dathh2010,by=c('idmen','year'))
merge2011=merge(datind2011, dathh2011,by=c('idmen','year'))
merge2012=merge(datind2012, dathh2012,by=c('idmen','year'))
merge2013=merge(datind2013, dathh2013,by=c('idmen','year'))
merge2014=merge(datind2014, dathh2014,by=c('idmen','year'))
merge2015=merge(datind2015, dathh2015,by=c('idmen','year'))
merge2016=merge(datind2016, dathh2016,by=c('idmen','year'))
merge2017=merge(datind2017, dathh2017,by=c('idmen','year'))
merge2018=merge(datind2018, dathh2018,by=c('idmen','year'))
merge2019=merge(datind2019, dathh2019,by=c('idmen','year'))

merge1=merge(Append1,Append2, by=c("idmen","year"))

#5 Number of households in which there are more than four family members
#for each year, find the households that have more than four family members
#Remove duplicate idmen, then get the number of households in which there are more than four family members
install.packages("tidyverse")
library(tidyverse)
a1=merge2004 %>% select(idmen) %>% group_by(idmen) %>% summarise(count = n()) %>% filter(count > 4)
a2=merge2005 %>% select(idmen) %>% group_by(idmen) %>% summarise(count = n()) %>% filter(count > 4)
a3=merge2006 %>% select(idmen) %>% group_by(idmen) %>% summarise(count = n()) %>% filter(count > 4)
a4=merge2007 %>% select(idmen) %>% group_by(idmen) %>% summarise(count = n()) %>% filter(count > 4)
a5=merge2008 %>% select(idmen) %>% group_by(idmen) %>% summarise(count = n()) %>% filter(count > 4)
a6=merge2009 %>% select(idmen) %>% group_by(idmen) %>% summarise(count = n()) %>% filter(count > 4)
a7=merge2010 %>% select(idmen) %>% group_by(idmen) %>% summarise(count = n()) %>% filter(count > 4)
a8=merge2011 %>% select(idmen) %>% group_by(idmen) %>% summarise(count = n()) %>% filter(count > 4)
a9=merge2012 %>% select(idmen) %>% group_by(idmen) %>% summarise(count = n()) %>% filter(count > 4)
a10=merge2013 %>% select(idmen) %>% group_by(idmen) %>% summarise(count = n()) %>% filter(count > 4)
a11=merge2014 %>% select(idmen) %>% group_by(idmen) %>% summarise(count = n()) %>% filter(count > 4)
a12=merge2015 %>% select(idmen) %>% group_by(idmen) %>% summarise(count = n()) %>% filter(count > 4)
a13=merge2016 %>% select(idmen) %>% group_by(idmen) %>% summarise(count = n()) %>% filter(count > 4)
a14=merge2017 %>% select(idmen) %>% group_by(idmen) %>% summarise(count = n()) %>% filter(count > 4)
a15=merge2018 %>% select(idmen) %>% group_by(idmen) %>% summarise(count = n()) %>% filter(count > 4)
a16=merge2019 %>% select(idmen) %>% group_by(idmen) %>% summarise(count = n()) %>% filter(count > 4)
a0=rbind(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16)
length(unique(a0$idmen))

#6 Number of households in which at least one member is unemployed
#find the individuals that are unemployed
#get the idmen of these individuals
#Remove duplicate idmen
data_unemployed=filter(merge1,merge1$empstat=="Unemployed")
length(unique(data_unemployed$idmen))

#7 Number of households in which at least two members are of the same profession
#first drop if profession=NA
#group by year, profession, idmen
#select households in which at least two members are of the same profession
#Then get the number
c=merge1 %>%
  drop_na(profession) %>%
  group_by(year, profession,idmen)%>% 
  summarise(count = n())%>% 
  filter(count >= 2)
  length(unique(c$idmen))


#8 Number of individuals in the panel that are from household-Couple with kids
data_kids=filter(merge1,merge1$mstatus=="Couple, with Kids")
length(unique(data_kids$idind))

#9 Number of individuals in the panel that are from Paris.
data_Paris =filter(merge1,merge1$location=="Paris")
length(unique(data_Paris$idind))

#10 Find the household with the most number of family members. Report its idmen.
#first find the most number of family members is 14
Most_num=merge1%>%
  group_by(idmen, year) %>%
  count(idind)
max(Most_num[,4])
#then Report the  idme of the family members that is 14
Most_num %>%
  filter(n ==14) %>%
  arrange(idmen)


#11Number of households present in 2010 and 2011.
household2010_2011=rbind(dathh2010,dathh2011)
length(unique(household2010_2011$idmen))



#=============================================
# Exercise 3: 
#=============================================

#1
#find the earliest and latest year that each household was surveyed
move1=aggregate(merge1[,2], by=list(merge1$idmen), FUN="min")
move2=aggregate(merge1[,2], by=list(merge1$idmen), FUN="max")
move3=merge(move1,move2,by="Group.1")
#calculate the difference as the distribution of the time spent in the survey for each household.
move4=cbind(move3,move3$x.y-move3$x.x)
move4
#if "year" and "datent" are the same,  a household moved into its current dwelling at the year of survey
merge1 =merge1 %>%
  drop_na(datent)
merge1$live = ifelse(merge1$year == merge1$datent, 1, 0)
#Report the first 10 rows
merge1[1:10,]
# calculate the share of individuals
merge1 = merge1 %>%
  group_by(year) %>%
  mutate(share1 = sum(live == 1)/sum(live == 0|1))
#plot across the year
plot1=select(merge1,year,share1)
ggplot(plot1, aes(x=year, y=share1)) + geom_line()

#2
#Based on myear and move, identify whether or not household migrated at the year of survey. Report
#the first 10 rows of your result and plot the share of individuals in that situation across years.

merge2=merge1
#for 2009-2014,use myear to get individuals who have moved,
merge2$c=ifelse(merge2$year == merge2$myear,  1, 0)
#for 2015-2019,use mive to get individuals who have moved,
merge2$d =ifelse(merge2$move == 2,  1, 0)
#replace NA=0
merge2$d=replace(merge2$d,is.na(merge2$d),0)
merge2$c=replace(merge2$c,is.na(merge2$c),0)
#plus c and d to get individuals who have moved across year
merge2$e=merge2$d+merge2$c
#Report the first 10 rows
merge2[1:10,]
#calculate the share of individuals who have migrated
merge2 = merge2 %>% group_by(year) %>%
  mutate(share2 =sum(e ==1)/sum(e ==0|1)) 
#plot across the year
plot2=select(merge2,year,share2)
ggplot(merge2, aes(x=year, y=share2)) + geom_line()


#3
#Mix the two plots 
plot3=select(merge2,year,share1,share2)
ggplot(plot3, aes(x=year, y=share2)) + geom_line()
plot4= ggplot(plot3) + 
  geom_line(mapping = aes(x=year, y=share1, color = "share1")) +
  geom_line(mapping = aes(x=year, y=share2, color = "share2"))
plot4
#I think the method based on datent is better.

#4
merge2=merge2%>%
  drop_na(datent)%>%
  filter(merge2$e>0)
merge2=merge2%>%
  mutate(l_profession = lag(profession, 1, order_by = year),
       l_empstat = lag(empstat, 1, order_by = year))
count=merge2%>%
#if the year the individual moves is the same year the people profession changes, then the value is 1
    mutate(change = ifelse((l_empstat != empstat|l_profession != profession)& year==datent,1, 0))
z=count %>% select(change,idind,year) %>% filter(change == 1)
z%>%group_by(year)
table(z$year)
x=unique(z$idind) 
length(x)
#=============================================
# Exercise 4: 
#=============================================
#position20xx is to filter the same elements in the ID of the first year and next year
position2004=Reduce(intersect,list(datind2004$idind,datind2005$idind))
#calculate the number of the same elements in the ID of 2004 and 2005,the number is 7842
length(unique(position2004))
#(z1)use the total number of the individuals in 2004 minus the number of the same elements to get the individuals who leave
z1=length(datind2004$idind)-7842
#(w1)use the total number of the individuals in 2005 minus the number of the same elements to get the individuals who enter
w1=length(datind2005$idind)-7842
position2005=Reduce(intersect,list(datind2005$idind,datind2006$idind))
length(unique(position2005))
z2=length(datind2005$idind)-7997
w2=length(datind2006$idind)-7997
position2006=Reduce(intersect,list(datind2006$idind,datind2007$idind))
length(unique(position2006))
z3=length(datind2006$idind)-8518
w3=length(datind2007$idind)-8518

position2007=Reduce(intersect,list(datind2007$idind,datind2008$idind))
length(unique(position2007))
z4=length(datind2007$idind)-8472
w4=length(datind2008$idind)-8472

position2008=Reduce(intersect,list(datind2008$idind,datind2009$idind))
length(unique(position2008))
z5=length(datind2008$idind)-8678
w5=length(datind2009$idind)-8678

position2009=Reduce(intersect,list(datind2009$idind,datind2010$idind))
length(unique(position2009))
z6=length(datind2008$idind)-9058
w6=length(datind2009$idind)-9058

position2010=Reduce(intersect,list(datind2010$idind,datind2011$idind))
length(unique(position2010))
z7=length(datind2010$idind)-9313
w7=length(datind2011$idind)-9313

position2011=Reduce(intersect,list(datind2011$idind,datind2012$idind))
length(unique(position2011))
z8=length(datind2011$idind)-9822
w8=length(datind2012$idind)-9822

position2012=Reduce(intersect,list(datind2012$idind,datind2013$idind))
length(unique(position2012))
z9=length(datind2012$idind)-9405
w9=length(datind2013$idind)-9405

position2013=Reduce(intersect,list(datind2013$idind,datind2014$idind))
length(unique(position2013))
z10=length(datind2013$idind)-9179
w10=length(datind2014$idind)-9179

position2014=Reduce(intersect,list(datind2014$idind,datind2015$idind))
length(unique(position2014))
z11=length(datind2013$idind)-9351
w11=length(datind2014$idind)-9351

position2015=Reduce(intersect,list(datind2015$idind,datind2016$idind))
length(unique(position2015))
z12=length(datind2015$idind)-9385
w12=length(datind2016$idind)-9385

position2016=Reduce(intersect,list(datind2016$idind,datind2017$idind))
length(unique(position2016))
z13=length(datind2016$idind)-9134
w13=length(datind2017$idind)-9134

position2017=Reduce(intersect,list(datind2017$idind,datind2018$idind))
length(unique(position2017))
z14=length(datind2017$idind)-8846
w14=length(datind2018$idind)-8846

position2018=Reduce(intersect,list(datind2018$idind,datind2019$idind))
length(unique(position2018))
z15=length(datind2018$idind)-8683
w15=length(datind2019$idind)-8683

attrition=cbind(c(2004:2019),c(z1/w1,z2/w2,z3/w3,z4/w4,z5/w5,z6/w6,z7/w7,z8/w8,z9/w9,z10/w10,z11/w11,z12/w12,z13/w13,z14/w14,z15/w15,0))
attrition

