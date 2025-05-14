library(tidyverse)
library(patchwork)
library(lubridate)
#Read in data
secchi_summer<- read_csv("data/secchi_summer.csv")|>
  mutate(year = ifelse(year4 == 2008.2, 2008, year4))%>%
  mutate(removal = ifelse(year4 < 2008, "<2008", ">=2008"))
tn_summer<- read_csv("data/tn_summer.csv")|>
  filter(month(sampledate) > 5 & month(sampledate) < 9)%>%
  group_by(year4)%>%
  summarize(totnuf_median = (mean(totnuf,na.rm = TRUE)),
            totnuf_min = (min(totnuf,na.rm = TRUE)),
            totnuf_max = (max(totnuf,na.rm = TRUE)))%>%
  mutate(removal = ifelse(year4 < 2008, "<2008", ">=2008"))

tp_summer<- read_csv("data/summer_tp.csv")|>
  filter(month(sampledate) > 5 & month(sampledate) < 9)%>%
  filter(year4 != 2020)%>%
  group_by(year4)%>%
  summarize(tp_median = (mean(tp_use,na.rm = TRUE)),
            tp_min = (min(tp_use,na.rm = TRUE)),
            tp_max = (max(tp_use,na.rm = TRUE)))%>%
  mutate(removal = ifelse(year4 < 2008, "<2008", ">=2008"))
tp_summer_use<- tp_summer%>%
  select(tp_median, year4)

chloro_summer<- read_csv("data/wingra_chl_data_update.csv")|>
  filter(month(sampledate) > 5 & month(sampledate) < 9)%>%
  group_by(year4)%>%
  summarize(chloro_median = (mean(chl_use,na.rm = TRUE)),
            chloro_min = (min(chl_use,na.rm = TRUE)),
            chloro_max = (max(chl_use,na.rm = TRUE)))%>%
  mutate(removal = ifelse(year4 < 2008, "<2008", ">=2008"))

chloro_summer_acf<- chloro_summer%>%
  filter(year4 > 2005)
acf(chloro_summer_acf$chloro_median)

ggplot(secchi_summer)+
  geom_vline(xintercept = 2008, linetype = "dashed")+
  geom_path(aes(x = year, y = secchi_median))+
  #geom_errorbar(aes(ymin =secchi_min, ymax =  secchi_max), width = 0)+
  geom_point(aes(x = year, y = secchi_median, fill = removal), size = 2.5, shape = 21)+
  scale_x_continuous(limits = c(1995,2023), breaks = seq(1995, 2023, 5))+
  ylab("Secchi (m)")+
  xlab("")+
  scale_fill_manual(values = c( "white","black"))+
  theme_bw(base_size = 14)+
  theme(legend.position = "none", 
        plot.caption = element_text(hjust = 0))

ggplot(data = tn_summer)+
  # geom_vline(xintercept = 2008, linetype = "dashed")+
  geom_path(aes(x = year4, y = totnuf_median/1000))+
  # geom_errorbar(aes(ymin =totnuf_min, ymax =  totnuf_max), width = 0)+
  geom_point(aes(x = year4, y = totnuf_median/1000, fill = removal),size = 2.5, shape = 21)+
  scale_x_continuous(limits = c(1995,2023), breaks = seq(1995, 2023, 5))+
  ylab("TN (mg/L)")+
  xlab("")+
  scale_fill_manual(values = c("white", "black"))+
  theme_bw(base_size = 14)+
  theme(legend.position = "none", 
        plot.caption = element_text(hjust = 0))

acf(tp_summer_use$tp_median)

ggplot()+
  #geom_vline(xintercept = 2008, linetype = "dashed")+
  geom_line(data = filter(tp_summer, year4< 2020), aes(x = year4, y = tp_median))+
  geom_point(data = filter(tp_summer, year4< 2020),aes(x = year4, y = tp_median, fill = removal),size = 2.5,  shape = 21)+
  geom_line(data = filter(tp_summer, year4 > 2020), aes(x = year4, y = tp_median))+
  geom_point(data = filter(tp_summer, year4 > 2020),aes(x = year4, y = tp_median, fill = removal), size = 2.5, shape = 21)+
  # geom_errorbar(aes(ymin =tp_min, ymax =  tp_max), width = 0)+
  scale_x_continuous(limits = c(1995,2024), breaks = seq(1995, 2023, 5))+
  ylab("TP (mg/L)")+
  xlab("")+
  scale_fill_manual(values = c("white", "black"))+
  theme_bw(base_size = 14)+
  theme(legend.position = "none", 
        plot.caption = element_text(hjust = 0))

ggplot()+
  # geom_vline(xintercept = 2008, linetype = "dashed")+
  geom_path(data = filter(chloro_summer, year4< 2002), aes(x = year4, y = chloro_median))+
  geom_path(data = filter(chloro_summer, year4 > 2004), aes(x = year4, y = chloro_median))+
  # geom_errorbar(data = chloro_plot, aes(x = year4, ymin = chloro_min, ymax =  chloro_max), width = 0)+
  geom_point(data = chloro_summer, aes(x = year4, y = chloro_median, fill = removal), size = 2.5, shape = 21)+
  ylab(expression(paste("Chlorophyll a", " (", µ,"g ", L^-1,")")))+
  xlab("")+
  scale_x_continuous(limits = c(1995,2023), breaks = seq(1995, 2023, 5))+
  scale_fill_manual(values = c("white", "black"))+
  theme_bw(base_size = 14)+
  theme(legend.position = "none", plot.caption = element_text(hjust = 0))

fil_algae_timeseries<- read_csv("data/ntl_macrophyte.csv")|>
  filter(lakeid == "WI")%>%
  group_by(year4)%>%
  summarize(fil_algae_sum = sum(fil_algae_wt))%>%
  mutate(removal = ifelse(year4 < 2008, '< 2008', '≥ 2008'))

macrophyte<- read_csv("data/ntl_macrophyte.csv")|>
  filter(lakeid == "WI")%>%
  mutate(removal = ifelse(year4 < 2008, "<2008", ">=2008"))

fil_time<-ggplot(filter(fil_algae_timeseries, year4 < 2019), aes(x = year4, y = fil_algae_sum, fill = removal))+
  geom_bar(stat = "identity", color = "black")+
  geom_vline(xintercept = 2008, linetype = "dashed")+
  xlab("")+
  ylab("Fil. algae wet weight (g)")+
  scale_fill_manual(values = c( "white", "black"))+
  theme_bw(base_size = 14)+
  theme(legend.position = "none", plot.caption = element_text(hjust = 0))
acf(fil_algae_timeseries$fil_algae_sum)

fil_box<-ggplot(filter(macrophyte, year4 < 2019), aes(x = removal, y = fil_algae_wt))+
  geom_boxplot(outlier.color = NA)+
  geom_jitter(aes(fill = removal), shape = 21, alpha = 0.5)+
  scale_fill_manual(values = c( "white", "black"))+
  annotate("text", x=1, y=600, label= "p < 2.2e-16")+
  xlab("")+
  ylab("")+
  scale_x_discrete(labels = c(expression("< 2008"), expression(phantom(x)>= 2008)))+
  theme_bw(base_size = 14)+
  theme(legend.position = "none",panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        plot.caption = element_text(hjust = 0))
t.test(fil_algae_wt ~removal, data = macrophyte, var.equal = TRUE)

#fil_plots<- fil_time +fil_box + fil_chl_plot + plot_layout(widths = c(2,1))+plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ")") 
#ggsave("figures/manuscript/manuscript/figure3.png", width = 8, height = 5, units = 'in')

secchi_mean<- secchi_all%>%
  mutate(year4 = year(sampledate))%>%
  group_by(year4)%>%
  summarize(secchi_mean = mean(secnview),
            secchi_median = median(secnview))%>%
  mutate(removal = ifelse(year4 < 2008, '< 2008', '≥ 2008'))%>%
  filter(year4 < 2019)


secchi_fil<- fil_algae_timeseries%>%
  left_join(secchi_mean, by = c("year4", "removal"))

ggplot(filter(secchi_fil))+
  geom_point(aes(y = fil_algae_sum, x = secchi_median, fill= removal), size = 4 , shape = 21)+
  scale_fill_manual(values = c( "white", "black"), name = "Carp removal:")+
  ylab("Fil. algae wet weight (g)")+
  xlab("Secchi (m) ")+
  scale_y_log10()+
  theme_bw(base_size = 14)+
  theme(legend.position = "bottom",plot.caption = element_text(hjust = 0))

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
ggplot(zoop_summer_sum)+
  geom_point(aes(x = year, y = mean_sum, fill = removal), size = 2.5, shape = 21)+
  xlab("")+
  ylab("Zooplankton biomass (mg/m3)")+
  scale_fill_manual(values = c( "white", "black"))+
  theme_bw(base_size = 14)+
  theme(legend.position = "none", plot.caption = element_text(hjust = 0))

acf(zoop_summer_sum$mean_sum)

zoop_fil<- zoop_summer_sum%>%
  left_join(fil_algae_timeseries, by = c("year" = "year4"))

fil_zoop_plot<-ggplot(filter(zoop_fil, year > 2007))+
  geom_smooth(aes(x = fil_algae_sum, y = mean_sum), method = "lm", color = "black")+
  geom_point(aes(x = fil_algae_sum, y = mean_sum), size = 2)+
  xlab("Fil. algae wet weight (g)")+
  ylab(expression(paste("Mean summer zooplankton biomass", " (", µ,"g ", L^-1,")")))+
  theme_bw(base_size = 14)

fil_chl_plot<-ggplot(filter(summer_post_fil))+
  geom_smooth(aes(x = fil_algae_sum, y = chl_mean), method = "lm", color = "black")+
  geom_point(aes(x = fil_algae_sum, y = chl_mean), size = 3)+
  ylab(expression(paste("Chlorophyll a", " (", µ,"g ", L^-1,")")))+
  xlab("Fil. algae wet weight (g)")+
  theme_bw(base_size = 16)
fil_chl_plot+fil_zoop_plot+plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ")") 
ggsave("figures/manuscript/manuscript/sf1.png", width = 8, height = 4, units = 'in')
ggsave("figures/defense/chla_fil.png", width = 5, height = 4, units = 'in')

fil_chl_lm<- lm(chl_mean~fil_algae_sum, data = filter(summer_post_fil,  year4 > 2007))
summary(fil_chl_lm)

zoop_fil_lm<- lm(fil_algae_sum ~ mean_sum, data = filter(zoop_fil, year > 2007))
summary(zoop_fil_lm)
