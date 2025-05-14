library(tidyverse)

summer_macrophyte<- macrophyte_algae %>%
  filter(year4> 2006)
zoops_biomass<- read_csv('data/zooplankton.biomass_WI.csv')|>
  mutate(mg_m3 = ug_m3/1000)
summer_zoop_sampling<- zoops_biomass%>%
  mutate(month = month(sample_date))%>%
  filter(month > 5 & month < 9)%>%
  #filter(sample_date != as.Date("2010-08-31") & sample_date != as.Date("2009-08-31"))%>%
  mutate(year = year(sample_date))%>%
  group_by(year)%>%
  summarise(unique_count = n_distinct(sample_date))

july_zoop_sampling<- zoops_biomass%>%
  mutate(month = month(sample_date))%>%
  filter(month == 7 | sample_date == as.Date("2016-06-09"))%>%
  filter(sample_date != as.Date("2016-07-07"))%>%
  mutate(year = year(sample_date))

summer_biomass<- summer_zoop_sampling %>%
  group_by(year)%>%
  summarize(biomass = sum(mg_m3))%>%
  left_join(filamentous_algae_timeseries, by = c("year"= "year4"))%>%
  left_join(sum_precip, by = c("year" = "year4"))

july_daphnia<- july_zoop_sampling%>%
  filter(larger_group == "CLADOCERA")%>%
  group_by(year)%>%
  summarize(biomass = sum(mg_m3))%>%
  left_join(filamentous_algae_timeseries, by = "year")%>%
  left_join(sum_precip, by = c("year" = "year4"))
  
ggplot(filter(july_daphnia, year > 2007))+
  geom_point(aes(x = biomass, y = sum))+
  geom_smooth(aes(x =biomass , y = sum), method = "lm", color = "black")
  
ggplot(filter(summer_biomass, year > 2007))+
  geom_smooth(aes(x =sum , y = biomass), method = "lm", color = "black")+
  geom_point(aes(x =sum , y = biomass), size = 3) +
  ylab(expression(paste("Summer zooplankton biomass", " (", µ,"g ", L^-1,")")))+
  xlab("Fil. algae wet weight (g)")+
  xlab("")+
  theme_bw(base_size = 16)
lm_zoop_algae_summer <- lm(biomass~sum, data = filter(summer_biomass, year > 2007))
summary(lm_zoop_algae_summer)

ggsave("figures/defense/zoops_algae_regression.png", width = 6, height = 5, units = 'in')


macrophyte_transect<- read_csv("data/ntl_macrophyte.csv")
fil_algae<- read_csv("data/macrophyte/fil_algae.csv")
macrophyte<- macrophyte_transect%>%
  filter(transect == 2 | transect == 5 | transect == 7 | transect == 9|transect == 11)%>%
  filter(lakeid == "WI")%>%
  rbind(fil_algae)
filamentous_algae_timeseries<- macrophyte%>%
  group_by(year4)%>%
  summarize(sum = sum(fil_algae_wt))
use_crosswalk<- tibble(transect = c(11, 2, 5, 7, 9), direction = c("west", "north", "east", "southeast", "southwest"))
macrophyte_algae<- macrophyte%>%
  merge(use_crosswalk, by = "transect")%>%
  group_by(transect, year4, sampledate, depth, direction)%>%
  summarize(algae = mean(fil_algae_wt))
filamentous_algae_timeseries<- macrophyte_algae%>%
  group_by(year4)%>%
  summarize(sum = sum(algae))%>%
  rename(year = year4)
#read in datasets
sum_precip<- read_csv("data/sum_precip.csv")

summer_zoop_biomass<- summer_zoop_sampling%>%
  group_by(year)%>%
  summarize(sum_mg_m3 = sum(mg_m3))%>%
  left_join(filamentous_algae_timeseries, by = "year")%>%
  left_join(sum_precip, by = c("year" = "year4"))

ggplot(filter(summer_zoop_biomass, year > 2008))+
  geom_point(aes(x =sum_precip , y = sum)) +
  geom_smooth(aes(x =sum_precip , y = sum), method = "lm", color = "black")+
  theme_bw()

lm_zoop_algae<- lm(sum_mg_m3~sum, data = filter(summer_zoop_biomass, year > 2008))
summary(lm_zoop_algae)
# Grouped
ggplot(zoops, aes(fill=species_name, y=density, x=sample_date)) + 
  geom_bar(position="dodge", stat="identity")

zoops_biomass<- zoops%>%
  group_by(sample_date)%>%
  summarize(ug_L_biomaxss = sum(ug_L))%>%
  rename(sampledate = sample_date)

zoops_biomass_year<- zoops_biomass %>%
  mutate(year4 = year(sampledate))%>%
  group_by(year4)%>%
  summarize(ug_L_biomass_annual = sum(ug_L_biomass))

zoops_year<- zoops_biomass_year%>%
  left_join(sum_precip, by = "year4")

ggplot(zoops_year)+
  geom_smooth(aes(x = ug_L_biomass_annual, y = sum_precip), method = "lm", color = "black")+
  geom_point(aes(x = ug_L_biomass_annual, y = sum_precip))+
  theme_bw()

zoops_secchi<- zoops_biomass%>%
  left_join(secchi_join, by = c("sampledate"))


ggplot(zoops_secchi)+
  geom_point(aes(x = ug_L_biomass, y = secnview))


ggplot(filter(zoops_chla, month(sampledate)> 4 & month(sampledate)< 10))+
  geom_point(aes(x = chl_use, y = ug_L_biomass), size = 3)+#  , size = sum_precip))+
  ylab(expression(paste("Summer zooplankton biomass", " (", µ,"g ", L^-1,")")))+
 # scale_size_continuous(name = "Summer \nTotal Precip (mm)")+
  xlab("Chlorophyll a (µg/L)")+
  theme_bw(base_size = 16)

ggsave("figures/chla_zoop_relation.png", width = 8, height = 6, units = 'in')

ggplot(filter(summer_zoops_daphnia, month(sample_date)> 4 & month(sample_date)< 10))+
  geom_point(aes(x = chl_use, y = mg_m3, size = sum_precip))


ggplot(filter(zoops_chla, month(sampledate)> 4 & month(sampledate)< 10))+
  geom_point(aes(x = sum_precip, y= ug_L_biomass))+
  geom_point(aes(x = sum_precip, y = chl_use * 75), color = "darkgreen")
ggplot()+
  geom_line(data= zoops_biomass, aes(x = sampledate, y= ug_L_biomass))+
  geom_line(data = filter(data, year4 > 2007), aes(x = sampledate, y = chl_use * 100), color = "darkgreen")

daphnia<- zoops%>%
  filter(grepl("daphnia", species_name))

cladocera<- zoops%>%
  filter(larger_group == "CLADOCERA")

daphnia_sum <- zoops%>%
  group_by(sample_date)%>%
  summarize(mg_m3 = sum(mg_m3))

ggplot(filter(zoops, grepl("daphnia",species_name)))+
  geom_point(aes(x = yday(sample_date), y = mg_m3))

ggplot(data)+
  geom_point(aes(x = sampledate, y = chl_use))+
  geom_vline(xintercept = as.Date("2012-04-01"))

summer_zoops_daphnia<- data%>%
  rename(sample_date = sampledate)%>%
  left_join(daphnia_sum, by = "sample_date")%>%
  mutate(month = month(sample_date))%>%
  filter(month > 4 & month < 10)%>%
  left_join(sum_precip, by = "year4")
  

ggplot(filter(summer_zoops_daphnia, year(sample_date) > 2007))+
  geom_boxplot(aes(x = as.factor(year(sample_date)), y = mg_m3))
ggplot(filter(summer_zoops_daphnia, year(sample_date) > 2007))+
  geom_boxplot(aes(x = as.factor(year(sample_date)), y = chl_use))

ggplot(summer_zoops_daphnia)+
  geom_point(aes(x = chl_use, y = mg_m3))


ggplot(filter(summer_zoops_daphnia, mg_m3 < 3000 & chl_use < 100))+
  geom_point(aes(x = chl_use, y = mg_m3))#+
 # geom_smooth(aes(x = chl_use, y = mg_m3), method = "loess")

ggplot(filter(summer_zoops_daphnia, year4 > 2007))+
  geom_point(aes(x = yday(sample_date), y = mg_m3, color = year4))+
  scale_color_viridis_c()
# geom_smooth(aes(x = chl_use, y = mg_m3), method = "loess")
