library(tidyverse)
library(patchwork)
#read in datasets
sum_precip<- read_csv("data/sum_precip.csv")
sum_precip<- read_csv("data/summer_precipitation.csv")
secchi_summer<- read_csv("data/secchi_summer.csv")|>
  mutate(year = ifelse(year4 == 2008.2, 2008, year4))%>%
  mutate(removal = ifelse(year4 < 2008, "<2008", "≥ 2008"))

chloro_all<- read_csv("data/wingra_chl_data_update.csv")
nutrients<- read_csv("data/ntl_nut_WI.csv")
macrophyte_transect<- read_csv("data/ntl_macrophyte.csv")
fil_algae<- read_csv("data/macrophyte/fil_algae.csv")
macrophyte <- macrophyte_transect%>%
  filter(transect == 2 | transect == 5 | transect == 7 | transect == 9|transect == 11)%>%
  filter(lakeid == "WI")%>%
 # rbind(fil_algae)%>%
  mutate(removal = ifelse(year4 < 2008, "<2008", "≥2008"))
  #mutate(removal = ifelse(year4 < 2008, "pre", "post"))
secchi_summer<- read_csv("data/secchi_summer.csv")|>
  mutate(year = ifelse(year4 == 2008.2, 2008, year4))%>%
  mutate(removal = ifelse(year4 < 2008, "<2008", "≥ 2008"))%>%
  left_join(sum_precip, by = c("year4"))%>%
  mutate(removal = as.factor(removal))

secchi_median<- secchi_summer%>%
  select(secchi_median, year4)

chloro_summer_sum <- chloro_all%>%
  mutate(month = month(sampledate))%>%
  mutate(removal = ifelse(sampledate < as.Date("2008-03-15"), "pre", "post"))%>%
  mutate(season = ifelse(month > 5 & month < 9, "summer",
                         ifelse(month > 10 & month < 12, "fall",
                                ifelse(month > 2 &  month < 5, "spring", "winter"))))%>%
  filter(season == "summer")%>%
  filter(sampledate != as.Date("2010-07-02"))%>%
  group_by(year4)%>%
  summarize(chl_median = median(chl_use, na.rm = TRUE),
            chl_mean = mean(chl_use, na.rm = TRUE))%>%
  mutate(removal = ifelse(year4 < 2008, '< 2008', '≥ 2008'))%>%
  left_join(sum_precip, by = c("year4"))%>%
  mutate(removal = as.factor(removal))

summer_variables<- chloro_summer_sum%>%
  left_join(secchi_median, by = "year4")%>%
  left_join(tp_summer_use, by = "year4")%>%
  left_join(tn_summer_use, by = "year4")%>%
  left_join(filamentous_algae_timeseries, by = "year4")%>%
  rename(algae_sum = sum)%>%
  left_join(zoop_summer_sum, by = c("year4"= "year"))

#write_csv(summer_variables, "data/summer_variables.csv")

summer<- read_csv("data/summer_variables.csv")|>
  left_join(fil_algae_timeseries, by = c("year4", "removal"))

secchi_precip_plot<-ggplot()+
  geom_smooth(data = filter(summer, removal == '≥ 2008'), aes(x = sum_precip, y = secchi_median), method = "lm", color = "black")+
  geom_jitter(data = summer, aes(x = sum_precip, y = secchi_median, fill = removal), shape = 21, size = 4)+
  scale_fill_manual(values = c("white","black"), name = "Carp removal:")+
  xlab("Precipitation (mm)")+
  ylab("Secchi (m) ")+
  # annotate(geom = 'text', x = 810, y= 1.7, fontface = "bold", label = paste("R^2 == ", 0), parse = TRUE)+
  # geom_text(aes(x = 400, y = 30), label = bquote(bold(Adj.~~R^2 == 0.43)), size = 5)+
  #geom_text(aes(x = 400, y = 30), label = expression(bold(Adj.~~R^2 == 0.43)), size = 4)+
  theme_bw(base_size = 14)+
  theme(
    plot.caption = element_text(hjust = 0))

chl_precip_plot<-ggplot()+
  geom_smooth(data = filter(summer, removal == '≥ 2008'), aes(x = sum_precip, y = chl_median), method = "lm", color = "black")+
  geom_jitter(data = summer, aes(x = sum_precip, y = chl_median, fill = removal), shape = 21, size = 4)+
  scale_fill_manual(values = c("white","black"), name = "Carp removal:")+
  xlab("Precipitation (mm)")+
  ylab("Chlorophyll a (μg/L) ")+
  # annotate(geom = 'text', x = 810, y= 1.7, fontface = "bold", label = paste("R^2 == ", 0), parse = TRUE)+
  # geom_text(aes(x = 400, y = 30), label = bquote(bold(Adj.~~R^2 == 0.43)), size = 5)+
  #geom_text(aes(x = 400, y = 30), label = expression(bold(Adj.~~R^2 == 0.43)), size = 4)+
  theme_bw(base_size = 14)+
  theme(
    plot.caption = element_text(hjust = 0))

tn_precip_plot<-ggplot()+
  geom_smooth(data = filter(summer, removal == '≥ 2008'), aes(x = sum_precip, y = totnuf_median/1000), method = "lm", color = "black")+
  geom_jitter(data = summer, aes(x = sum_precip, y = totnuf_median/1000, fill = removal), shape = 21, size = 4)+
  scale_fill_manual(values = c("white","black"), name = "Carp removal:")+
  xlab("Precipitation (mm)")+
  ylab("Total nitrogen (mg/L) ")+
  annotate(geom = 'text', x = 810, y=1.59, fontface = "bold", label = paste("R^2 == ", 0.22), parse = TRUE)+
  # annotate(geom = 'text', x = 810, y= 1.7, fontface = "bold", label = paste("R^2 == ", 0), parse = TRUE)+
  # geom_text(aes(x = 400, y = 30), label = bquote(bold(Adj.~~R^2 == 0.43)), size = 5)+
  #geom_text(aes(x = 400, y = 30), label = expression(bold(Adj.~~R^2 == 0.43)), size = 4)+
  theme_bw(base_size = 14)+
  theme(
    plot.caption = element_text(hjust = 0))

tp_precip_plot<-ggplot()+
  geom_smooth(data = filter(summer, removal == '≥ 2008'), aes(x = sum_precip, y = tp_median), method = "lm", color = "black")+
  geom_jitter(data = summer, aes(x = sum_precip, y = tp_median, fill = removal), shape = 21, size = 4)+
  scale_fill_manual(values = c("white","black"), name = "Carp removal:")+
  xlab("Precipitation (mm)")+
  ylab("Total phosphorus (mg/L) ")+
  annotate(geom = 'text', x = 810, y=0.065, fontface = "bold", label = paste("R^2 == ", 0.31), parse = TRUE)+
  # annotate(geom = 'text', x = 810, y= 1.7, fontface = "bold", label = paste("R^2 == ", 0), parse = TRUE)+
  # geom_text(aes(x = 400, y = 30), label = bquote(bold(Adj.~~R^2 == 0.43)), size = 5)+
  #geom_text(aes(x = 400, y = 30), label = expression(bold(Adj.~~R^2 == 0.43)), size = 4)+
  theme_bw(base_size = 14)+
  theme(
    plot.caption = element_text(hjust = 0))


  

secchi_plot<-ggplot()+
  geom_smooth(data = filter(secchi_summer, removal == '≥ 2008'), aes(x = sum_precip, y = secchi_median), method = "lm", color = "black")+
  geom_jitter(data = secchi_summer, aes(x = sum_precip, y = secchi_median, fill = removal), shape = 21, size = 4)+
  scale_fill_manual(values = c("white","black"), name = "Carp removal:")+
  xlab("Precipitation (mm)")+
  ylab("Secchi (m) ")+
 # annotate(geom = 'text', x = 810, y= 1.7, fontface = "bold", label = paste("R^2 == ", 0), parse = TRUE)+
  # geom_text(aes(x = 400, y = 30), label = bquote(bold(Adj.~~R^2 == 0.43)), size = 5)+
  #geom_text(aes(x = 400, y = 30), label = expression(bold(Adj.~~R^2 == 0.43)), size = 4)+
  theme_bw(base_size = 14)+
  theme(
    plot.caption = element_text(hjust = 0))

secchi_model <- lm(secchi_median ~ sum_precip, data = filter(secchi_summer, removal == '≥ 2008'))
chl_model <- lm(chl_median ~ sum_precip, data = filter(summer, removal == '≥ 2008'))
tn_model <- lm(totnuf_median ~ sum_precip, data = filter(summer, removal == '≥ 2008'))
tp_model <- lm(tp_median ~ sum_precip, data = filter(summer, removal == '≥ 2008'))
summary(secchi_model)
summary(chl_model)
summary(tn_model)
summary(tp_model)


secchi_model <- lm(secchi_median ~ sum_precip, data = filter(secchi_summer, removal == '<2008'))


summary(secchi_model)

color_plot<- ggplot()+
 geom_smooth(data = filter(color_summer, removal == '≥ 2008'), aes(x = sum_precip, y = dWL_median), method = "lm", color = "black")+
  geom_jitter(data = filter(color_summer), aes(x = sum_precip, y = dWL_median, fill = removal), shape = 21, size = 4)+
  scale_fill_manual(values = c("white","black"), name = "Carp removal:")+
  xlab("Precipitation (mm)")+
  ylab("Wavelength (nm) ")+
  annotate(geom = 'text', x = 810, y= 560, fontface = "bold", label = paste("R^2 == ", 0.13), parse = TRUE)+
  # geom_text(aes(x = 400, y = 30), label = bquote(bold(Adj.~~R^2 == 0.43)), size = 5)+
  #geom_text(aes(x = 400, y = 30), label = expression(bold(Adj.~~R^2 == 0.13)), size = 4)+
  theme_bw(base_size = 14)+
  theme(
    plot.caption = element_text(hjust = 0))
color_model <- lm(dWL_median ~ sum_precip, data = filter(color_summer, removal == '≥ 2008'))
color_model <- lm(dWL_median ~ sum_precip, data = filter(color_summer, removal != '≥ 2008'))

ggplot(color_summer)+
  geom_point(aes(x = sum_precip, y = dWL_mean))+
  facet_wrap(~removal)

summary(color_model)

chloro_plot<-ggplot()+
  geom_smooth(data = filter(chloro_summer_sum, removal == '≥ 2008'), aes(x = sum_precip, y = chl_median), method = "lm", color = "black")+
  geom_jitter(data = filter(chloro_summer_sum), aes(x = sum_precip, y = chl_median, fill = removal), shape = 21, size = 4)+
  scale_fill_manual(values = c("white","black"), name = "Carp removal:")+
  xlab("Precipitation (mm)")+
  ylab("Chlorophyll a (μg/L) ")+
  annotate(geom = 'text', x = 810, y= 32, fontface = "bold", label = paste("R^2 == ", 0.5), parse = TRUE)+
  #annotate("text", x = 810, y=32, label = "R = 0.43*", fontface = "bold")+
 # geom_text(aes(x = 400, y = 30), label = bquote(bold(Adj.~~R^2 == 0.43)), size = 5)+
  #geom_text(aes(x = 400, y = 30), label = expression(bold(Adj.~~R^2 == 0.43)), size = 4)+
  theme_bw(base_size = 14)+
  theme(
        plot.caption = element_text(hjust = 0))

chlorophyll_model <- lm(chl_median ~ sum_precip, data = filter(chloro_summer_sum, removal == '≥ 2008'))
chlorophyll_model <- lm(chl_median ~ sum_precip, data = chloro_summer_sum_na)
ggplot(filter(chloro_summer_sum_na, removal != '≥ 2008'))+
  geom_point(aes(x= sum_precip, y = chl_median))
chloro_summer_sum_na<- chloro_summer_sum%>%
  filter(chl_median != "NA")%>%
  filter(removal != '≥ 2008')
summary(chlorophyll_model)

summer_tp<- nutrients%>%
  mutate(month = month(sampledate))%>%
  filter(month > 5 & month < 9)%>%
  dplyr::select(year4, sampledate, totpuf, totpuf_sloh)%>%
  mutate(totpuf = totpuf/1000)%>%
  mutate(tp = ifelse(totpuf %in% NA, totpuf_sloh,
                     ifelse(totpuf < 0.02 & !is.na(totpuf_sloh) , totpuf_sloh, totpuf)))%>%
  mutate(tp_diff = totpuf-totpuf_sloh)%>%
  mutate(tp_use = ifelse(sampledate == as.Date("2017-07-12"), totpuf_sloh, tp))

summer_tp_median <- summer_tp%>%
  dplyr::select(year4, sampledate, tp_use)%>%
  na.omit()%>%
  group_by(year4)%>%
  summarize(tp_mean = mean(tp_use))%>%
  merge(sum_precip, by = "year4")%>%
  mutate(removal = ifelse(year4 < 2008, '< 2008', '≥ 2008'))%>%
  mutate(removal = as.factor(removal))


tp_plot<-ggplot()+
  geom_smooth(data = filter(summer_tp_median, (year4 != 2020 & removal == '≥ 2008')), aes(x = sum_precip, y = tp_mean), method = "lm", color = "black")+
  geom_jitter(data = filter(summer_tp_median, (year4 != 2020)), aes(x = sum_precip, y = tp_mean, fill = removal), shape = 21, size = 4)+
  scale_fill_manual(values = c("white","black"), name = "Carp removal:")+
  xlab("Precipitation (mm)")+
  ylab("Total phosphorus (mg/L) ")+
  #annotate("text", x = 810, y=0.06, label = "R = 0.31*", fontface = "bold")+
  annotate(geom = 'text', x = 810, y=0.065, fontface = "bold", label = paste("R^2 == ", 0.31), parse = TRUE)+
  theme_bw(base_size = 14)+
  theme(
        plot.caption = element_text(hjust = 0))

tp_model <- lm(tp_mean ~ sum_precip, data = filter(summer_tp_median, removal == '≥ 2008' & year4!= 2020))
tp_model <- lm(tp_mean ~ sum_precip, data = filter(summer_tp_median, removal != '≥ 2008' & year4!= 2020))

summary(tp_model)

summer_tn<- nutrients%>%
  mutate(month = month(sampledate))%>%
  filter(month > 5 & month < 9)%>%
  dplyr:: select(year4, sampledate, totnuf)%>%
  mutate(totnuf_mgL = totnuf)

summer_tn_median <- summer_tn%>%
  dplyr::select(year4, sampledate, totnuf_mgL)%>%
  na.omit()%>%
  group_by(year4)%>%
  summarize(tn = mean(totnuf_mgL))%>%
  merge(sum_precip, by = "year4")%>%
  mutate(removal = ifelse(year4 < 2008, '< 2008', '≥ 2008'))%>%
  mutate(removal = as.factor(removal))

tn_plot<- ggplot()+
  geom_smooth(data = filter(summer_tn_median, (removal == "≥ 2008")), aes(x = sum_precip, y = tn/1000), method = "lm", color = "black")+
  geom_jitter(data = filter(summer_tn_median, year4 != 2008), aes(x = sum_precip, y = tn/1000, fill = removal), shape = 21, size = 4)+
  scale_fill_manual(values = c("white","black"), name = "Carp removal:")+
  xlab("Precipitation (mm)")+
  ylab("Total nitrogen (mg/L) ")+
  annotate(geom = 'text', x = 810, y=1.59, fontface = "bold", label = paste("R^2 == ", 0.22), parse = TRUE)+
#  annotate("text", x = 810, y=1.59, label = "R = 0.22", fontface = "bold")+
  theme_bw(base_size = 14)+
  theme(
        plot.caption = element_text(hjust = 0))

tn_model <- lm(tn ~ sum_precip, data = filter(summer_tn_median, removal == "≥ 2008"))
tn_model <- lm(tn ~ sum_precip, data = filter(summer_tn_median, removal != "≥ 2008"))

summary(tn_model)

use_crosswalk<- tibble(transect = c(11, 2, 5, 7, 9), direction = c("west", "north", "east", "southeast", "southwest"))
macrophyte_algae<- macrophyte%>%
  merge(use_crosswalk, by = "transect")%>%
  group_by(transect, year4, sampledate, depth, direction)%>%
  summarize(algae = mean(fil_algae_wt))
filamentous_algae_timeseries<- macrophyte_algae%>%
  group_by(year4)%>%
  summarize(sum = sum(algae))

fil_algae_precip <- filamentous_algae_timeseries%>%
  left_join(sum_precip, by = "year4")%>%
  mutate(removal = ifelse(year4 < 2008, '< 2008', '≥ 2008'))%>%
  mutate(removal = as.factor(removal))%>%
  filter(year4 < 2019)

fil_algae_precip_after<- fil_algae_precip%>%
  filter(removal == "≥ 2008")


fil_plot<-ggplot()+
  geom_smooth(data = filter(fil_algae_precip, removal == "≥ 2008"), aes(x = sum_precip, y = sum), method = "lm", color = "black")+
  geom_jitter(data = fil_algae_precip, aes(x = sum_precip, y = sum, fill = removal), shape = 21, size = 4)+
  scale_fill_manual(values = c("white","black"), name = "Carp removal:")+
  xlab("Precipitation (mm)")+
  ylab("Fil. algae wet weight (g)")+
  annotate(geom = 'text', x = 810, y=2990, fontface = "bold", label = paste("R^2 == ", 0.27), parse = TRUE)+
  theme_bw(base_size = 14)+
  theme(
        plot.caption = element_text(hjust = 0))
fil_model <- lm(sum ~ sum_precip, data = filter(fil_algae_precip, removal == "≥ 2008" ))
fil_model <- lm(sum ~ sum_precip, data = filter(fil_algae_precip, removal != "≥ 2008" ))

summary(fil_model)
(tn_plot + tp_plot) / (chloro_plot + fil_plot) + plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ")") + plot_layout(guides='collect') &theme(legend.position='bottom') 
ggsave("figures/manuscript/precip_plots.png", width = 9, height = 8, units = 'in')

secchi_plot + color_plot + plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ")") + plot_layout(guides='collect') &theme(legend.position='bottom') 
ggsave("figures/manuscript/secchi_color_precip.png", width = 7, height = 4, units = 'in')

tn_model <- lm(tn ~ sum_precip, summer_tn_after)
summary(tn_model)

summer_tn_after<- summer_tn_median%>%
  filter(year4 != 2020)%>%
  filter(removal == "after")

summary(tp_model)
summary(tn_model)
summary(chlorophyll_model)
