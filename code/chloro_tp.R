tp<- nuts|> 
  filter(month(sampledate) %in% c(6,7,8)) |> 
  filter(depth == 0) |> 
  select(sampledate, year4, lakeid, totpuf_WSLH, totpuf) |> 
  mutate(totpuf_WSLH = totpuf_WSLH * 1000) |> 
  pivot_longer(cols = c(totpuf_WSLH, totpuf)) |> 
  filter(value < 400)%>%
  group_by(sampledate) |>
  summarise(totpuf = mean(value, na.rm = T)) |> 
  mutate(year4 = year(sampledate))|>
  mutate(group = if_else(year4 < 2008, 'pre', 'post'))

chloro_all<- read_csv("data/wingra_chl_data_update.csv")|>
  mutate(removal = ifelse(year(sampledate) < 2008, "<2008", ">=2008"))%>%
  filter(month(sampledate) > 5 & month(sampledate) < 9)

fil_algae_timeseries<- read_csv("data/ntl_macrophyte.csv")|>
  filter(lakeid == "WI")%>%
  group_by(year4)%>%
  summarize(fil_algae_sum = sum(fil_algae_wt))#%>%
 # mutate(removal = ifelse(year4 < 2008, '< 2008', '≥ 2008'))

tp_mean<- tp%>%
  group_by(year4)%>%
  summarize(mean_tp = mean(totpuf, na.rm = T))

chloro_mean <- chloro_all%>%
  group_by(year4)%>%
  summarize(mean_chloro = mean(chl_use, na.rm = T))

chloro_tp<- tp_mean%>%
  left_join(chloro_mean, by = "year4")%>%
  left_join(fil_algae_timeseries, by = "year4")%>%
  mutate(removal = ifelse(year4 < 2008, "<2008", "≥2008"))
  

ggplot(chloro_tp)+
  geom_point(aes(y = mean_tp, x = mean_chloro, color = removal))+
  geom_smooth(aes(y = mean_tp, x = mean_chloro, color = removal), method = "lm")


ggplot(chloro_tp) +
  geom_smooth(aes(y = mean_chloro, x = mean_tp, group = removal),method = "lm", color = "black")+
  geom_point(aes(y = mean_chloro, x = mean_tp, fill = removal),shape = 21, size = 3)+
  scale_fill_manual(values = c( "white", "black"))+
  ylab(expression(paste("Chlorophyll a", " (µg ", L^-1,")")))+
  xlab(expression(paste("Total Phosphorus", " (µg ", L^-1,")")))+
  theme_bw()

ggplot(filter(chloro_tp, year4 > 2007)) +
  geom_smooth(aes(x = mean_chloro, y = fil_algae_sum, group = removal),method = "lm", color = "black")+
  geom_point(aes(x = mean_chloro, y = fil_algae_sum, fill = removal),shape = 21, size = 3)+
  scale_fill_manual(values = c( "white", "black"))+
  xlab(expression(paste("Chlorophyll a", " (µg ", L^-1,")")))+
  ylab(expression(paste("Filamentous algae", " (µg ", L^-1,")")))+
  theme_bw()

ggsave("figures/chloro_tp.png", width = 6, height = 4, units = 'in')

chloro_tp_post <- lm(mean_tp ~ mean_chloro, data = filter(chloro_tp, year4> 2007 ))
summary(chloro_tp_post)
chloro_tp_pre <- lm(mean_tp ~ mean_chloro, data = filter(chloro_tp, year4 < 2008 ))
summary(chloro_tp_pre)


fil_algae<- summary_medians%>%
  

