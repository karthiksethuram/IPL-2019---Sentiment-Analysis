

library(tidyverse)
library(rvest)
library(stringr)
library(tidytext)
library(magrittr)
library(lubridate)
library(ggplot2)
library(gridExtra)


#For 2019
webpage <- read_html("https://www.iplt20.com/stats/2019")
df<-webpage %>%
  html_nodes("table") %>% .[1] %>%   html_table(fill = TRUE)
df<-as.data.frame(df)
df$year="2019"
my_data=df[,-1]


#For 2008 to 2018
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




dhoni_URL<-"https://www.iplt20.com/teams/chennai-super-kings/squad/1/ms-dhoni"
Kohli_URL<-"https://www.iplt20.com/teams/royal-challengers-bangalore/squad/164/virat-kohli"
ABD_URL<-"https://www.iplt20.com/teams/royal-challengers-bangalore/squad/233/ab-de-villiers"
Rohith_URL<-"https://www.iplt20.com/teams/mumbai-indians/squad/107/rohit-sharma"

webpage <- read_html(dhoni_URL)
df<-webpage %>%
  html_nodes("table") %>% .[3] %>%   html_table(fill = TRUE)
df<-as.data.frame(df)
df$name="dhoni"
my_data=df[-1,]


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

my_data=player_stats(Kohli_URL,"kohli")
my_data=player_stats(ABD_URL,"ABD")
my_data=player_stats(Rohith_URL,"Rohith")

my_data$year<-as.Date(my_data$Batting.and.Fielding,"%Y")
my_data$year<-year(my_data$year)



library(plotly)
p <- my_data %>%
  plot_ly(
    x = ~Runs, 
    y = ~Ave, 
    #size = ~pop, 
    color = ~name, 
    frame = ~Batting.and.Fielding, 
    #text = ~country, 
    #hoverinfo = "text",
    type = 'scatter',
    mode = 'markers'
  ) %>%
  layout(
    xaxis = list(
      type = "log"
    )
  )

library(ggplot2)
library(gganimate)
library(gifski)
library(png)
p=ggplot(
  my_data, 
  aes(x = Runs, y=Ave, colour = name)
) +
  #geom_point(show.legend = TRUE, alpha = 0.9) +
  geom_line()+
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(x = "Runs", y = "Average")
p
p + transition_time(year) +
  labs(title = "Year")


p + facet_wrap(~name) +
  transition_time(year) +
  labs(title = "Year: {frame_time}")


p + facet_wrap(~name) + geom_line() +
  geom_point() +
  transition_reveal(year)
+ labs(title = "Year: {frame_time}")
