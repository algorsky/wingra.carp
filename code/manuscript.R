library(tidyverse)
library(patchwork)
library(lubridate)
# Data set title: North Temperate Lakes LTER: Secchi Disk Depth; Other Auxiliary Base Crew Sample Data 1981 - current.
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/31/32/d01c782e0601d2217b94dd614444bd33" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
secchi = read_csv(infile1)
# Filter for Wingra 
secchi = secchi %>% filter(lakeid == 'WI')%>%
  mutate(month = month(sampledate))%>%
  mutate(season = ifelse(month > 11, "winter",
                         ifelse(month < 3, "winter",
                                ifelse(month <5 & month > 2, "spring",
                                       ifelse(month < 12 & month > 9, "fall", "summer")))))%>%
  mutate(removal = ifelse(sampledate < as.Date("2008-03-01"), "before", "after"))

mark_secchi<- read_csv("data/secchi_mark.csv")|>
  rename(sampledate = Date)%>%
  rename(secnview = secchi_m)

secchi_all<- secchi%>%
  dplyr::select(sampledate, secnview)%>%
  rbind(mark_secchi)

secchi_summer<- secchi_all%>%
  mutate(month = month(sampledate))%>%
  mutate(year4 = year(sampledate))%>%
  filter(month > 5 & month < 9)%>%
  group_by(year4)%>%
  summarize(secchi_median = (mean(secnview,na.rm = TRUE)),
            secchi_min = (min(secnview,na.rm = TRUE)),
            secchi_max = (max(secnview,na.rm = TRUE)))%>%
  mutate(year4 = ifelse(year4 == 2008, 2008.2, year4))%>%
  mutate(removal = ifelse(year4 < 2008, "before", "after"))
ggplot(secchi_summer, aes(x = year4, y = secchi_median, fill = removal))+
  geom_vline(xintercept = 2008, linetype = "dashed")+
  geom_path(aes(x = year4, y = secchi_median))+
  geom_errorbar(aes(ymin =secchi_min, ymax =  secchi_max), width = 0)+
  geom_point(size = 4, shape = 21)+
  ylab("Water Clarity (m)")+
  xlab("")+
  scale_fill_manual(values = c("black", "white"))+
  theme_bw(base_size = 20)+
  theme(axis.title = element_text(face="bold"),legend.position = "none", 
        plot.caption = element_text(hjust = 0))
#Breakpoint
library(BreakPoints)
#Breakpoint all
bubreak_secchi<- Buishand_R(secchi_all$secnview, n_period = 10, dstr = 'self', simulations = 1000)
ptbreak_secchi<-pettit(secchi_all$secnview,n_period=10)
bubreak_secchi
ptbreak_secchi
secchi[187,] # 2008-02-18

plot(secchi$secnview)
abline(v = bubreak_secchi$breaks,lwd=6,col="blue")
abline(v = ptbreak_secchi$breaks, lty=2,col="red",lwd=4)

model_before<- lm(secnview ~ sampledate, data = subset(secchi_all, sampledate < as.Date("2008-02-18")))
model_after<- lm(secnview ~ sampledate, data = subset(secchi_all, sampledate > as.Date("2008-02-18")))

#Secchi stats
secchi_stats<- secchi_all%>%
  mutate(month = month(sampledate))%>%
  mutate(season = ifelse(month > 11, "winter",
                         ifelse(month < 3, "winter",
                                ifelse(month <5 & month > 2, "spring",
                                       ifelse(month < 12 & month > 9, "fall", "summer")))))%>%
  mutate(removal = ifelse(sampledate < as.Date("2008-03-01"), "before", "after"))%>%
  filter(year(sampledate) != 2008)%>%
  group_by(removal, season)%>%
  summarize(mean_secchi = mean(secnview, na.rm = TRUE))

secchi_all_stats<- secchi_all%>%
  mutate(month = month(sampledate))%>%
  mutate(season = ifelse(month > 11, "winter",
                         ifelse(month < 3, "winter",
                                ifelse(month <5 & month > 2, "spring",
                                       ifelse(month < 12 & month > 9, "fall", "summer")))))%>%
  mutate(removal = ifelse(sampledate < as.Date("2008-03-01"), "before", "after"))%>%
  filter(year(sampledate) != 2008)%>%
  group_by(removal, season)%>%
  summarize(mean_secchi = mean(secnview, na.rm = TRUE))

  

#Precipitation and variables
macrophyte<- read_csv("data/ntl_macrophyte.csv")|>
  filter(lakeid == "WI")%>%
  group_by(year4)%>%
  summarize(fil_algae_sum = sum(fil_algae_wt))

use_crosswalk<- tibble(transect = c(11, 2, 5, 7, 9), direction = c("west", "north", "east", "southeast", "southwest"))
macrophyte_algae<- macrophyte%>%
  group_by(transect, year4, sampledate, depth)%>%
  summarize(algae = mean(fil_algae_wt))
filamentous_algae_timeseries<- macrophyte%>%
  group_by(year4)%>%
  summarize(sum = sum(algae))
fil_algae_precip <- filamentous_algae_timeseries%>%
  left_join(sum_precip, by = "year4")%>%
  mutate(removal = ifelse(year4 < 2008, "pre", "post"))

ggplot(filamentous_algae_timeseries, aes(x = year4, y = sum))+
  geom_bar()
