
#Installing Packages needed:
rm(list=ls())
library(proto)
library(gsubfn)
library(RSQLite)
library(sp)
library(countrycode)
library(dplyr)
library(lubridate)
library(tidyverse)
library(maptools)  
library(rgdal)
library(ggplot2)
library(countrycode)
library(knitr)
library(rio)
library(chron)
require(rgdal)
library(pryr)
library(plotrix)
library(sqldf)
library(wesanderson)
library(knitr) # for dynamic reporting
library(kableExtra) # create a nicely formated HTML table
library(formattable) # for the color_tile function
library(gridExtra) #viewing multiple plots together
library(tidytext) #text mining
library(wordcloud2)
library(grid)
library(naniar)
install.packages('gridExtra')
install.packages('tidytext')
install.packages('wordcloud2')
install.packages('knitr)')
install.packages('kableExtra')
install.packages('formattable')
install.packages('vioplot')
install.packages("dplyr")

install.packages('sp')
install.packages('rworldxtra')
install.packages("lubridate")
install.packages("tidyverse")
devtools::install_github("hadley/tidyverse")
install.packages('rgdal')
install.packages("rgdal", repos="http://R-Forge.R-project.org", type="source")
install.packages('rgeos',repos="http://www.stats.ox.ac.uk/pub/RWin")
install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel",
                   "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))
install.packages('ggplot2')
install.packages('countrycode')
install.packages('rio')
install_formats()
install.packages("data.table")
install.packages('knitr')
install.packages("chron")
install.packages('pryr')
install.packages("plotrix")
install.packages('pacman')
install.packages('gsubfn')
install.packages('proto')
install.packages('RSQLite')
install.packages("wesanderson")
install.packages('naniar')
'_____________________________________________________________________________________________________'
# pacman::p_load(packagesnames,x1,x1,x2,x3,x4,)
getwd()
setwd('/Users/mac/Desktop/R')
dflgbt=read.csv('LGBT_ALL_1.csv',header = TRUE,stringsAsFactors=FALSE,na.strings = TRUE)
dflgbt['notes']=NULL
dflgbt['Unnamed..0']=NULL
##check datasets summry :
str(dflgbt)
summary(dflgbt)
sum(complete.cases(dflgbt))
colSums(is.na(dflgbt))
dim(dflgbt)
#dflgbt%>%subset_lvl=levels(subset)
colMax <- function(Q4) sapply(Q4, max, na.rm = TRUE)

names(dflgbt)
dflgbt= rename(dflgbt,'Country'='CountryCode' )
dflgbt <- select(filter(dflgbt, Country != 'Average'),c("subset","question_code","question_label","answer","percentage",'Country' ))
#______________________________________________________________________________________
#Q1 How many groups in the dataset has and what t
by_subset <- dflgbt %>%
  group_by(subset) %>% 
  summarize(count = n())
by_subset
typeof(by_subset)
tbl <- with(dflgbt, table(subset))
typeof(tbl)
ggplot(as.data.frame(by_subset), aes(subset,count,fill = subset))+      
  geom_col(position = 'dodge')+
  theme(axis.text.x  = element_text(angle = 70,hjust = 1) )
#______________________________________________________________________________________
#Q2-distrubtion of each single group in each country ?
Q3=by_country <- dflgbt %>% 
  group_by(Country,subset) %>% 
  summarize(count = n())
by_country
tbl1 <- with(dflgbt, table(Country,subset))

ggplot(as.data.frame(tbl1), aes(subset, Freq, fill = Country)) +     
  geom_col(position = 'dodge')+theme(axis.text.x  = element_text(angle = 70,hjust = 1) )
#_______________________________________________________________________________________
#Q3Where did the last incident of physical / sexual attack or threat of violence happen?	
#in the concusion this doesn't mean belgim is not friendly its only mean that most

dflgbt_fa_10=select(filter(dflgbt,question_code =='fa1_10'),c('answer','Country','subset','question_code','percentage'))
colMax(dflgbt_fa_10)
Q4_fa=by_question_subset <- dflgbt_fa_10 %>%
  group_by(question_code,answer,percentage) %>% 
  summarize(count = n())
by_question_subset
colMax(by_question_subset)
Top_10_Where_attack_happend_to_whom=dflgbt_fa_10%>%top_n(30)%>%arrange(desc(percentage))
Top_10_Where_attack_happend_to_whom %>%
  select(subset,answer,percentage,Country ) %>%
  mutate(subset = color_tile("lightpink", "lightgreen")(subset)) %>%
  mutate(Country = color_tile("lightblue", "lightblue")(Country)) %>%
  kable("html", escape = FALSE, align = "c", caption = "LGBT Who's answer is Yes ") %>%
  kable_styling(bootstrap_options =
                  c("striped", "condensed", "bordered"),
                full_width = FALSE)
#Q4-In the last 12 months, in the country where you live, have you personally felt discriminated 
#against or harassed on the grounds of sexual orientation?	 
#you can check by ascending to know country with less NO mean High discrimenation 
q_5=select(filter(dflgbt, question_code=='c2_c',answer=='No'),c('answer','subset','Country','percentage','question_code'))
q_5_Y=select(filter(dflgbt, question_code=='c2_c',answer=='Yes'),c('answer','subset','Country','percentage','question_code'))

Q4=by_question_subset <-q_5 %>%
  group_by(Country,percentage,question_code,answer) %>% 
  summarize(count = n())
by_question_subset

Q4_Y=by_question_subset <-q_5_Y %>%
  group_by(Country,percentage,question_code,answer) %>% 
  summarize(count = n())
by_question_subset
plot_q4=Q4[c('Country','percentage')]
tail(q_5)
top_10_countries=Q4%>%top_n(3)%>%arrange(desc(percentage))
tail(Q4_Y)
q_Y_n = rbind(q_5,q_5_Y)
q_Y_n = (head(q_Y_n,200))

ggplot(q_Y_n, aes(Country,percentage,fill = answer))+      
  geom_col(position = 'dodge')+
  theme(axis.text.x  = element_text(angle = 70,hjust = 1) )+ggtitle("have you personally felt discriminated on the grounds of sexual orientation")

#___________________________________________________________________________________________________
#Q5
#on GenderGround 
#In the last 12 months, in the country where you live, have you personally felt discriminated against or harassed on the grounds of gender?	
q_6=select(filter(dflgbt, question_code=='c2_b',answer=='No'),c('answer','subset','Country','percentage','question_code'))
q_6_Y=select(filter(dflgbt, question_code=='c2_b',answer=='Yes'),c('answer','subset','Country','percentage','question_code'))

Q5=by_question_subset <-q_6 %>%
  group_by(Country,percentage,question_code,answer) %>% 
  summarize(count = n())
by_question_subset

Q5_Y=by_question_subset <-q_6_Y %>%
  group_by(Country,percentage,question_code,answer) %>% 
  summarize(count = n())
by_question_subset
q_Y_n_6 = rbind(q_6,q_6_Y)
q_Y_n_6 = (head(q_Y_n_6,200))

ggplot(q_Y_n_6, aes(Country,percentage,fill = answer))+      
  geom_col(position = 'dodge')+
  theme(axis.text.x  = element_text(angle = 70,hjust = 1) )+ggtitle("have you personally felt discriminated on the grounds of Gender")

#____________________________________________________________________________________________________
#Q6-Have you ever experienced any of the following situations when using or trying 
#to access healthcare services as a L, G, B or T person?
q_c10=select(filter(dflgbt, question_code =='c10'),c('answer','subset','Country','percentage','question_code'))
Q6=by_subset <- q_c10 %>%
  group_by(answer,percentage,subset,Country,question_code) %>% 
  summarize(count = n())
by_subset


Q6=by_subset[order(answer,-by_subset$percentage),]

subsettotc10=aggregate(Q6$count~Q6$subset,Q6,sum)
names(subsettotc10)=c('subset','totcount')

pct<-round(subsettotc10$totcount/sum(subsettotc10$totcount)*100)
lbls<-paste(subsettotc10$subset,"-",pct,"%",sep="")

totcpm=pie3D(subsettotc10$totcount, labels = lbls, explode = 0.4,
             main = 'Which is Most Subset Had Diffeclty Access the Healthcare Services', mar=c(1,3,3,1), 
             radius=1.5,labelcex = 1.2, col=c("red","blue","#ddaa00","pink","#dd00dd"))

#Q7-Where (In which country) did the last incident of physical / sexual attack or threat of violence happen At my home?
#What is the distribution of countries in which the answer for last incident of physical /sexual attack or threat of violence was "At my home"
q1<- sqldf("select * from dflgbt where question_code='fa1_10' AND  answer='At my home'")

ggplot(as.data.frame(q1), aes(Country,percentage))+      
  geom_col(position = 'dodge')+
  theme(axis.text.x  = element_text(angle = 70,hjust = 1) )+ggtitle("sexual attack or threat of violence was at my home")

ggplot(as.data.frame(q1), aes(Country,percentage,fill = answer))+      
  geom_col(position = 'dodge')+
  theme(axis.text.x  = element_text(angle = 70,hjust = 1) )+ggtitle("Sexual attacks where happned  at home in Eu countries ")

q1_pub<- sqldf("select * from dflgbt where question_code='fa1_10' AND  answer='In a street, square, car parking lot or other public place'")
ggplot(as.data.frame(q1_pub), aes(Country,percentage,fill = answer))+      
  geom_col(position = 'dodge')+
  theme(axis.text.x  = element_text(angle = 70,hjust = 1) )+ggtitle("attacks happened at home in countries ")

#_____________________________________________________________________________________________________________________________
#Q8-Where do you avoid being open about yourself as L, G, B or T for fear of being assaulted, threatened or harassed by others?	
e3=select(filter(dflgbt, question_code =='e3' ,answer=='Public transport'),c('answer','subset','Country','percentage','question_code'))
Q8=by_question_subset <-e3 %>%
  group_by(answer,Country,percentage,subset) %>% 
  summarize(count = n())
by_question_subset
Top_10_Where_attack_happend_to_whom=by_question_subset%>%top_n(30)%>%arrange(desc(percentage))#missing a
qplot(percentage,data = Top_10_Where_attack_happend_to_whom, geom = "histogram",
      fill = Country)

#______________________________________________________________________________________
#Q9-Which Country most daily life support LGBT 
#where exactly they felt ?
#1=was it when they looking for the job and hows that on countries
#2=was it in work enviorment
#3=was it when you looking for the appartment 
#4=was it in the universcity or college 
#5=is it from social serivce personal 
 
#looking for job 
c4_a=select(filter(dflgbt,question_code=='c4_a'),c('answer','question_code','percentage','subset','Country'))
# at work
c4_b=select(filter(dflgbt,question_code=='c4_b'),c('answer','question_code','percentage','subset','Country'))
#when you buying house or apartment
c4_c=select(filter(dflgbt,question_code=='c4_c'),c('answer','question_code','percentage','subset','Country'))
#social serivces personal
c4_e=select(filter(dflgbt,question_code=='c4_e'),c('answer','question_code','percentage','subset','Country'))
#university or school 
c4_f=select(filter(dflgbt,question_code=='c4_f'),c('answer','question_code','percentage','subset','Country'))

location_most_dis_r=rbind(c4_a,c4_b,c4_c,c4_e,c4_f)
location_most_dis_r=
  location_most_dis_r=location_most_dis_r[order(-location_most_dis_r$percentage),]
write.csv(location_most_dis_r,'file=TopCountries.csv')
location_most_dis_r_by_country=select(filter(location_most_dis_r,answer=='Yes'),c('Country','question_code','percentage','subset'))
location_most_dis_r_by_country_y=select(filter(location_most_dis_r,answer=='Yes'),c('Country','question_code','percentage','subset','answer'))


newdata=sqldf("SELECT Country,question_code,answer, Avg(percentage) AS AvgPercent 
      FROM location_most_dis_r
      GROUP BY Country,question_code,answer")

head(location_most_dis_r)
head(newdata)
czech_rep=newdata[newdata$Country =='Czech Republic',]

czech_rep <- mutate_at(czech_rep, vars('question_code', 'answer','Country'), as.factor)

str(czech_rep)
require(rgl)
czech_rep=rename(czech_rep,'Questions'='question_code','Answers'='answer')
with(czech_rep,plot3d(Questions,Answers,AvgPercent,type="s", col=as.integer(Questions)))

  #Q10 which country less supported LGBT comunity by public figures in politics, business, and ?
#b1_g_In your opinion, how widespread is public figures in politics, business, sports, etc being open about 
#themselves being lesbian, gay, bisexual and/or transgender in the country where you live?

#b2_c_What would allow you to be more comfortable living as a lesbian, gay or bisexual person in the 
#country where you live? Public figures in politics, business, sports, etc openly speaking in support of lesbian, gay and bisexual people?	
b1_g=select(filter(dflgbt,question_code=='b1_g'),c('Country','question_code','percentage','subset','answer'))
b2_c=select(filter(dflgbt,question_code=='b2_c'),c('Country','question_code','percentage','subset','answer'))
b1_g_b2_c=rbind(b1_g,b2_c)
b_fine=select(filter(b1_g_b2_c, answer==''),c('Country','question_code','percentage','subset','answer'))


newdata_=sqldf("SELECT Country,question_code,answer, Avg(percentage) AS AvgPercent 
      FROM b1_g_b2_c
      GROUP BY Country,question_code,answer")

ggplot(as.data.frame(b1_g_b2_c), aes(Country,percentage,fill = answer))+      
  geom_col(position = 'dodge')+
  theme(axis.text.x  = element_text(angle = 70,hjust = 1) )+ggtitle("attacks happened at home in countries ")













