library(tidyverse)


summer<- read_csv("data/summer_variables.csv")

summer_post<- summer%>%
  filter(removal == "≥ 2008")


ggplot(summer_post)+
  geom_point(aes(x = sum_precip, y = chl_median))+
  ylab("Chlorophyll-a (µg/L)")+
  xlab("Precipitation (mm)")+
  theme_bw(base_size = 14)+
  theme(legend.position = "bottom", 
        plot.caption = element_text(hjust = 0))

ggplot(summer_post)+
  geom_point(aes(x = sum_precip, y = tp_median))+
  ylab("Total phosphorus (mg/L)")+
  xlab("Precipitation (mm)")+
  theme_bw(base_size = 14)+
  theme(legend.position = "bottom", 
        plot.caption = element_text(hjust = 0))

ggplot(summer_post)+
  geom_point(aes(x = sum_precip, y = totnuf_median))+
  ylab("Total nitrogen (mg/L)")+
  xlab("Precipitation (mm)")+
  theme_bw(base_size = 14)+
  theme(legend.position = "bottom", 
        plot.caption = element_text(hjust = 0))

ggplot((summer_post_fil))+
  geom_point(aes(x = mean_sum, y = fil_algae_sum))+
  xlab("Zooplankton biomass")+
  ylab("Filamentous Algae")+
  theme_bw()

ggplot((summer_post_fil))+
  geom_point(aes(x = mean_sum, y = chl_mean, color = year4))+
  xlab("Zooplankton biomass")+
  ylab("Chlorophyll a")+
  theme_bw()

ggplot(summer_post_fil)+
  geom_point(aes(x = tp_median, y = fil_algae_sum))

summer_post_fil<- summer_post%>%
  filter(year4> 2008 & year4< 2019)

tp_fil <- lm(chl_median ~ fil_algae_sum, data = summer_post_fil)
summary(tp_fil)  
