nuts = loadLTERnutrients() |> filter(lakeid == 'WI')

secchi_all<- read_csv("data/secchi_all.csv")%>%
  filter(month(sampledate) > 5 & month(sampledate) < 9)%>%
  mutate(removal = ifelse(year(sampledate) < 2008, "<2008", ">=2008"))

secchi_summary<- secchi_all%>%
  group_by(removal)%>%
  summarize(median = median(secnview, na.rm = T))
tn<- nuts|> 
  filter(month(sampledate) %in% c(6,7,8)) |> 
  filter(depth == 0) |> 
  select(sampledate, year4, lakeid, totnuf_WSLH, totnuf) |> 
  mutate(totnuf_WSLH = totnuf_WSLH * 1000) |> 
  pivot_longer(cols = c(totnuf_WSLH, totnuf)) |> 
  group_by(sampledate) |>
  summarise(totnuf = mean(value, na.rm = T)) |> 
  na.omit()|>
  filter(totnuf > 12)|>
  mutate(year4 = year(sampledate))|>
  mutate(group = if_else(year4 < 2008, 'pre', 'post'))

tn_summary<- tn%>%
  group_by(group)%>%
  summarize(median = median(totnuf, na.rm = T))

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

tp_summary<- tp%>%
  group_by(group)%>%
  summarize(median = median(totpuf, na.rm = T))


chloro_all<- read_csv("data/wingra_chl_data_update.csv")|>
  mutate(removal = ifelse(year(sampledate) < 2008, "<2008", ">=2008"))%>%
  filter(month(sampledate) > 5 & month(sampledate) < 9)

chloro_summary<- chloro_all%>%
  group_by(removal)%>%
  summarize(median = median(chl_use, na.rm = T))


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

macro<- read_csv("data/dnr_macrophyte_sum.csv")