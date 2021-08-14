library(ggplot2)
library(dplyr) # easier data wrangling 
library(viridis) # colour blind friendly palette, works in B&W also
library(Interpol.T) #  will generate a large dataset on initial load
library(lubridate) # for easy date manipulation
library(ggExtra) # because remembering ggplot theme options is beyond me
library(tidyr) 
library(ggthemes)

#plot results for Tolsta

df_all<-compassdpm %>% select(Location, Date, Time, DPM)

df_all<- df_all %>% mutate(year = year(Date),
                   month = month(Date, label=TRUE),
                   day = day(Date))

df_all$hour<-hms(as.character(df_all$Time))
df_all$hour<-hour(df_all$hour)


#create plotting df
df_all_plot <-df_all %>% select(Location,Date,hour,DPM)%>%
  fill(DPM) #optional - see note below

##order factor levels: Location - to change order of Locations from North to South
df_all_plot  <- transform(df_all_plot, 
                      Location=factor(Location,
                                      levels = c("Tolsta","Stoer Head","Shiant Isles","Hyskeir","Garvellachs",
                                                "Stanton Bank", "Malin", 
                                                 "Middle Bank", "Skerries", "Copelands")))


p <-ggplot(df_all_plot,aes(Date,hour,fill=DPM))+
  geom_tile() + 
  scale_fill_viridis(name="DPM", option = "D",limits=c(1,60),na.value = "grey92")
p <-p + facet_grid(Location ~ .)
p <-p + scale_y_continuous(trans = "reverse", breaks = c(0,5,10,15,20),expand = c(0,0),name="Hour of the Day")+ 
  scale_x_date(breaks=date_breaks("1 month"),name="",labels=
                 function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x),
                                     paste(month(x, label=TRUE), "\n", year(x)),
                                     paste(month(x, label=TRUE))),
               limits=as.Date(c("2017-11-01","2020-06-30")), expand=c(0,0))+
  theme_few()+ 
  theme(axis.text.y=element_text(size=7)) +
  theme(axis.text=element_text(size=7))+
  theme(legend.title=element_text(size=8))+
  theme(legend.text=element_text(size=7))+
  theme(panel.spacing = unit(0.1,"lines"))+
  theme(strip.background = element_rect(colour="white"))+
  removeGrid()+#ggExtra
  theme(legend.margin=margin(0,0,0,0))
p


ggsave(p,file="all_seasonal_diel_v7.tiff")
