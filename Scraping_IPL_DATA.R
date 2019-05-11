## DATE - MAY 05 2019									




############################################################################################################################
##                                  SCRAPING IPL DATA                                                        ##

## This code scraps data from the IPLT20 website to collect various stats for the previous 11 seasons (2008 to 2019)      ##
									

############################################################################################################################




############################################################################################################################
##                                SCRPAING TEAMWISE STATS                                                                 ##												
############################################################################################################################

#IMPORTING THE LIBRARIES REQUIRED
library(tidyverse)
library(rvest)
library(stringr)
library(tidytext)
library(magrittr)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(plotly)
library(ggplot2)
library(gganimate)
library(gifski)
library(png)


#SCRAPING DATA FOR 2019 FROM THE STANDINGS TABLE
webpage <- read_html("https://www.iplt20.com/stats/2019")
df<-webpage %>%
  html_nodes("table") %>% .[1] %>%   html_table(fill = TRUE)
df<-as.data.frame(df)
df$year="2019"
my_data=df[,-1]


#FUNCTION TO SCRAPE DATA FOR A GIVEN YEAR AND APPENDING IT TO THE PREVIOUS DATAFRAME
TEAM_STANDING = function(year)
{
webpage <- read_html(paste("https://www.iplt20.com/stats/",year,sep = ""))
temp<-webpage %>%
  html_nodes("table") %>% .[1] %>%   html_table(fill = TRUE)
temp<-as.data.frame(temp)
temp$year=year
my_data=rbind(my_data,temp[,-1])
return(my_data)
}

#PREVIOUS FUNCTION CALLED FOR 2018 TO 2008
my_data=TEAM_STANDING("2018")
my_data=TEAM_STANDING("2017")
my_data=TEAM_STANDING("2016")
my_data=TEAM_STANDING("2015")
my_data=TEAM_STANDING("2014")
my_data=TEAM_STANDING("2013")
my_data=TEAM_STANDING("2012")
my_data=TEAM_STANDING("2011")
my_data=TEAM_STANDING("2010")
my_data=TEAM_STANDING("2009")
my_data=TEAM_STANDING("2008")


############################################################################################################################
##                                SCRPAING PLAYERWISE STATS                                                               ##												
############################################################################################################################


#BELOW GIVEN ARE THE URLS FOR THE 4 PLAYERS CHOSEN - VIRAT KOHLI, MS DHONI, AB dEVILLERS AND ROHITH SHARMA
dhoni_URL<-"https://www.iplt20.com/teams/chennai-super-kings/squad/1/ms-dhoni"
Kohli_URL<-"https://www.iplt20.com/teams/royal-challengers-bangalore/squad/164/virat-kohli"
ABD_URL<-"https://www.iplt20.com/teams/royal-challengers-bangalore/squad/233/ab-de-villiers"
Rohith_URL<-"https://www.iplt20.com/teams/mumbai-indians/squad/107/rohit-sharma"

#SCRPAING DATA FOR MS DHONI
webpage <- read_html(dhoni_URL)
df<-webpage %>%
  html_nodes("table") %>% .[3] %>%   html_table(fill = TRUE)
df<-as.data.frame(df)
df$name="dhoni"
my_data=df[-1,]


#FUNCTION TO SCRAPE DATA FOR OTHER PLAYERS AND APPEND IT TO THE PREVIOUS DATA FRAME
player_stats= function(URL,name)
{
webpage <- read_html(URL)
t<-webpage %>%
  html_nodes("table") %>% .[3] %>%   html_table(fill = TRUE)
t<-as.data.frame(t)
t$name=name
my_data=rbind(my_data,t[-1,])
return(my_data)
}


#CALLING THE ABOVE FUNCTION TO OTHER 3 PLAYERS
my_data=player_stats(Kohli_URL,"kohli")
my_data=player_stats(ABD_URL,"ABD")
my_data=player_stats(Rohith_URL,"Rohith")

my_data$year<-as.Date(my_data$Batting.and.Fielding,"%Y")
my_data$year<-year(my_data$year)

############################################################################################################################
##                          AN ANIMATION FOR PLAYER CONSISTENCY  -  ACROSS SEASONS                                        ##												
############################################################################################################################

#PLOTTING A GRAPH FOR PLAYER AVERAGE VS TOTAL RUNS
p=ggplot(
  my_data, 
  aes(x = Runs, y=Ave, colour = name)
) +
  #geom_point(show.legend = TRUE, alpha = 0.9) +
  geom_line(show.legend = TRUE)+
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(x = "Runs", y = "Average")
p

p + transition_time(year) +
  labs(title = "Year")

#SPLITTING THE GRAPH IN TO 4 - FOR 4 DIFFERENT PLAYERS
p + facet_wrap(~name) +  geom_point() +transition_time(year) +labs(title = "Year: {frame_time}")

#CREATE A GGANIMATE VISUALISATION
p + facet_wrap(~name) + geom_line() +
  geom_point() +
  transition_reveal(year)
+ labs(title = "Year: {frame_time}")
