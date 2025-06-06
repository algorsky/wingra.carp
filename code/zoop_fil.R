summer_zoop_sampling<- read_csv('data/zooplankton.biomass_WI.csv')|>
  mutate(mg_m3 = ug_m3/1000)%>%
  mutate(month = month(sample_date))%>%
  filter(month > 5 & month < 9)%>%
  mutate(year = year(sample_date))
zoop_summer_mean<- summer_zoop_sampling%>%
  group_by(sample_date)%>%
  summarize(sum = sum(mg_m3))
zoop_summer_sum<- zoop_summer_mean%>%
  mutate(year = year(sample_date))%>%
  group_by(year)%>%
  summarize(mean_sum = mean(sum),
            n = n())%>%
  mutate(removal = ifelse(year < 2008, "<2008", ">=2008"))

fil_algae_timeseries<- read_csv("data/ntl_macrophyte.csv")|>
  filter(lakeid == "WI")%>%
  group_by(year4)%>%
  summarize(fil_algae_sum = sum(fil_algae_wt))#%>%
# mutate(removal = ifelse(year4 < 2008, '< 2008', '≥ 2008'))


zoop_fil<- zoop_summer_sum%>%
  left_join(fil_algae_timeseries, by = c("year" = "year4"))


ggplot(filter(zoop_fil, year > 2007))+
  geom_smooth(aes(x = fil_algae_sum, y = mean_sum), method = "lm", color = "black")+
  geom_point(aes(x = fil_algae_sum, y = mean_sum), size = 3)+
  ylab(expression(paste("Mean summer zooplankton biomass", " (", µ,"g ", L^-1,")")))+
  xlab("Fil. algae \n (wet mass per rake throw)")+
  theme_bw(base_size = 12)
ggsave("figures/fil_zoop.png", width = 6, height = 4, units = 'in')

zoop_fil_lm <- lm(mean_sum ~ fil_algae_sum, data = filter(zoop_fil, year> 2007 ))
summary(zoop_fil_lm)
