library(tidyverse)
library(patchwork)
library(lubridate)
#Read in data
secchi_summer<- read_csv("data/secchi_summer.csv")|>
  mutate(year = ifelse(year4 == 2008.2, 2008, year4))%>%
  mutate(removal = ifelse(year4 < 2008, "<2008", ">=2008"))
secchi_all<- read_csv("data/secchi_all.csv")%>%
  filter(month(sampledate) > 5 & month(sampledate) < 9)%>%
  mutate(removal = ifelse(year(sampledate) < 2008, "<2008", ">=2008"))

tn_summer<- read_csv("data/tn_summer.csv")|>
  mutate(removal = ifelse(year(sampledate) < 2008, "<2008", ">=2008"))
tn_plot<- tn_summer%>%
  mutate(year4 = year(sampledate))%>%
  filter(month(sampledate) > 5 & month(sampledate) < 9)%>%
  group_by(year4)%>%
  summarize(totnuf_median = (mean(totnuf,na.rm = TRUE)),
            totnuf_min = (min(totnuf,na.rm = TRUE)),
            totnuf_max = (max(totnuf,na.rm = TRUE)))%>%
  mutate(year4 = ifelse(year4 == 2008, 2008.2, year4))%>%
  mutate(removal = ifelse(year4 < 2008, "before", "after"))

tp_summer<- read_csv("data/summer_tp.csv")|>
  mutate(removal = ifelse(year(sampledate) < 2008, "<2008", ">=2008"))%>%
  filter(year4 !=2020)
tp_plot<- tp_summer%>%
  filter(month(sampledate) > 5 & month(sampledate) < 9)%>%
#  filter(year4 != 2020)%>%
  group_by(year4)%>%
  summarize(tp_median = (mean(tp_use,na.rm = TRUE)),
            tp_min = (min(tp_use,na.rm = TRUE)),
            tp_max = (max(tp_use,na.rm = TRUE)))%>%
  mutate(year4 = ifelse(year4 == 2008, 2008.2, year4))%>%
  mutate(removal = ifelse(year4 < 2008, '< 2008', '≥ 2008'))

tp_plot<- tp_summer%>%
  filter(month(sampledate) > 5 & month(sampledate) < 9)%>%
 # filter(year4 != 2020)%>%
  group_by(year4)%>%
  summarize(tp_median = (mean(tp_use,na.rm = TRUE)),
            tp_min = (min(tp_use,na.rm = TRUE)),
            tp_max = (max(tp_use,na.rm = TRUE)))%>%
  mutate(year4 = ifelse(year4 == 2008, 2008.2, year4))%>%
  mutate(removal = ifelse(year4 < 2008, '< 2008', '≥ 2008'))

chloro_all<- read_csv("data/wingra_chl_data_update.csv")|>
  mutate(removal = ifelse(year(sampledate) < 2008, "<2008", ">=2008"))
chloro_plot<-chloro_all%>%
  filter(month(sampledate) > 5 & month(sampledate) < 9)%>%
  group_by(year4)%>%
  summarize(chloro_median = (mean(chl_use,na.rm = TRUE)),
            chloro_min = (min(chl_use,na.rm = TRUE)),
            chloro_max = (max(chl_use,na.rm = TRUE)))%>%
  mutate(year4 = ifelse(year4 == 2008, 2008.2, year4))%>%
  mutate(removal = ifelse(year4 < 2008, '< 2008', '≥ 2008'))
#filter(year4 < 2002 | year4> 2004)

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

secchi_timeseries<- ggplot()+
  geom_vline(xintercept = 2008, linetype = "dashed")+
  geom_point(data = secchi_all, aes(x = sampledate, y = secnview, fill = removal), size = 2.5, shape = 21, alpha = 0.5)+
  geom_line(data = secchi_summer, aes(x = as.Date(paste0(year, "-07-01")), y = secchi), size = 1.5)+
   #geom_errorbar(aes(ymin =secchi_min, ymax =  secchi_max), width = 0)+
 # scale_x_continuous(limits = c(1995,2023), breaks = seq(1995, 2023, 5))+
  ylab("Secchi (m)")+
  xlab("")+
  scale_fill_manual(values = c( "white","black"))+
  theme_bw(base_size = 14)+
  theme(legend.position = "none", 
        plot.caption = element_text(hjust = 0))
secchi_boxplot<-ggplot(secchi_summer, aes(x = removal, y = secchi))+
  geom_boxplot(outlier.color = NA)+
  geom_jitter(aes(fill = removal), shape = 21, alpha = 0.5)+
  scale_fill_manual(values = c( "white", "black"))+
  xlab("")+
  ylab("")+
  annotate("text", x=1, y=2.2, label= "p < 0.01")+
  scale_x_discrete(labels = c(expression("< 2008"), expression(phantom(x)>= 2008)))+
  theme_bw(base_size = 14)+
  theme(legend.position = "none",panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        plot.caption = element_text(hjust = 0))

t.test(secchi ~removal, data = secchi_summer, var.equal = TRUE)
acf(secchi_summer$secchi)

secchi_plots<-secchi_timeseries + secchi_boxplot+ plot_layout(widths = c(2,1))

tn_timeseries<-ggplot()+
  # geom_vline(xintercept = 2008, linetype = "dashed")+
  # geom_errorbar(aes(ymin =totnuf_min, ymax =  totnuf_max), width = 0)+
  geom_point(data = tn_summer, aes(x = sampledate, y = totnuf/1000, fill = removal),size = 2.5, shape = 21, alpha = 0.5)+
  geom_line(data = tn_plot, aes(x = as.Date(paste0(year4, "-07-01")), y = totnuf_median/1000), size = 1.5)+
  ylab(expression(paste("Total nitrogen", " (mg ", L^-1,")")))+
  xlab("")+
  scale_fill_manual(values = c("white", "black"))+
  theme_bw(base_size = 14)+
  theme(legend.position = "none", 
        plot.caption = element_text(hjust = 0))

tn_box<- ggplot(tn_plot, aes(x = removal, y = totnuf_median/1000))+
  geom_boxplot(outlier.color = NA)+
  geom_jitter(aes(fill = removal), shape = 21, alpha = 0.5)+
  scale_fill_manual(values = c( "white", "black"))+
  xlab("")+
  ylab("")+
  annotate("text", x=1, y=2.00, label= "p < 0.01")+
  scale_x_discrete(labels = c(expression("< 2008"), expression(phantom(x)>= 2008)))+
  theme_bw(base_size = 14)+
  theme(legend.position = "none",panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        plot.caption = element_text(hjust = 0))
t.test(totnuf_median ~removal, data = tn_plot, var.equal = TRUE)
acf(tn_plot$totnuf_median)
tn_plots<-tn_timeseries + tn_box + plot_layout(widths = c(2,1))

secchi_plots / chloro_plots / tn_plots / tp_plots

tp_timeseries<-ggplot()+
  #geom_vline(xintercept = 2008, linetype = "dashed")+
  geom_point(data = tp_summer,aes(x = sampledate, y = tp_use, fill = removal),size = 2.5,  shape = 21, alpha = 0.5)+
  geom_line(data = tp_plot, aes(x = as.Date(paste0(year4, "-07-01")), y = tp_median), size = 1.5)+
  # geom_errorbar(aes(ymin =tp_min, ymax =  tp_max), width = 0)+
  # scale_x_continuous(limits = c(1995,2024), breaks = seq(1995, 2023, 5))+
  ylab(expression(paste("Total phosphorus", " (mg ", L^-1,")")))+
  xlab("")+
  scale_fill_manual(values = c("white", "black"))+
  theme_bw(base_size = 14)+
  theme(legend.position = "none", 
        plot.caption = element_text(hjust = 0))

tp_box<- ggplot(tp_plot, aes(x = removal, y = tp_median))+
  geom_boxplot(outlier.color = NA)+
  geom_jitter(aes(fill = removal), shape = 21, alpha = 0.5)+
  scale_fill_manual(values = c( "white", "black"))+
  xlab("")+
  ylab("")+
  annotate("text", x=1, y=0.1, label= "p < 0.01")+
  scale_y_continuous(limits = c(0,0.1), breaks = seq(0, 0.1, 0.05))+
  scale_x_discrete(labels = c(expression("< 2008"), expression(phantom(x)>= 2008)))+
  theme_bw(base_size = 14)+
  theme(legend.position = "none",panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        plot.caption = element_text(hjust = 0))
t.test(tp_median ~removal, data = tp_plot, var.equal = TRUE)
tp_plots<-tp_timeseries + tp_box + plot_layout(widths = c(2,1))
acf(tp_plot$tp_median)

chloro_time<- ggplot()+
  geom_point(data = chloro_all, aes(x = sampledate, y = chl_use, fill = removal), size = 2.5, shape = 21, alpha = 0.5)+
  # geom_vline(xintercept = 2008, linetype = "dashed")+
  geom_line(data = filter(chloro_plot, year4< 2002), aes(x = as.Date(paste0(year4, "-07-01")), y = chloro_median), size = 1.5)+
  geom_line(data = filter(chloro_plot, year4 > 2004), aes(x = as.Date(paste0(year4, "-07-01")), y = chloro_median), size = 1.5)+
  # geom_errorbar(data = chloro_plot, aes(x = year4, ymin = chloro_min, ymax =  chloro_max), width = 0)+
  ylab(expression(paste("Chlorophyll a", " (", µ,"g ", L^-1,")")))+
  xlab("")+
  scale_fill_manual(values = c("white", "black"))+
  theme_bw(base_size = 14)+
  theme(legend.position = "none", plot.caption = element_text(hjust = 0))
chloro_box<- ggplot(chloro_plot, aes(x = removal, y = chloro_median))+
  geom_boxplot(outlier.color = NA)+
  geom_jitter(aes(fill = removal), shape = 21, alpha = 0.5)+
  scale_fill_manual(values = c( "white", "black"))+
  xlab("")+
  ylab("")+
  annotate("text", x=1, y=65, label= "p > 0.05")+
  scale_x_discrete(labels = c(expression("< 2008"), expression(phantom(x)>= 2008)))+
  theme_bw(base_size = 14)+
  theme(legend.position = "none",panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        plot.caption = element_text(hjust = 0))
t.test(chloro_median ~removal, data = chloro_plot, var.equal = TRUE)
chloro_plots<- chloro_time + chloro_box + plot_layout(widths = c(2,1))
chloro_acf<-chloro_plot%>%
  filter(year4 > 2004)
acf(chloro_acf$chloro_median)

macrophyte<- read_csv("data/ntl_macrophyte.csv")
macrophyte_wi<- macrophyte%>%
  filter(lakeid == "WI")%>%
  mutate(removal = ifelse(year4 < 2008, "<2008", ">=2008"))
fil_algae_sum<- macrophyte%>%
  filter(lakeid == "WI")%>%
  group_by(year4)%>%
  summarize(sum = sum(fil_algae_wt))%>%
  mutate(removal = ifelse(year4 < 2008, "<2008", ">=2008"))

fil_time<-ggplot(fil_algae_sum, aes(x = year4, y = sum, fill = removal))+
  geom_bar(stat = "identity", color = "black")+
  geom_vline(xintercept = 2008, linetype = "dashed")+
  xlab("")+
  scale_x_continuous(limits = c(1995,2023), breaks = seq(1995, 2023, 5))+
  ylab("Fil. algae (wet mass per rake throw)")+
  scale_fill_manual(values = c( "white", "black"))+
  theme_bw(base_size = 16)+
  theme(legend.position = "none", plot.caption = element_text(hjust = 0))

fil_box<-ggplot(fil_algae_sum, aes(x = removal, y = sum))+
  geom_boxplot(outlier.color = NA)+
  geom_jitter(aes(fill = removal), shape = 21, alpha = 0.5)+
  scale_fill_manual(values = c( "white", "black"))+
  annotate("text", x=1, y=600, label= "p < 0.01")+
  xlab("")+
  ylab("")+
  scale_x_discrete(labels = c(expression("< 2008"), expression(phantom(x)>= 2008)))+
  theme_bw(base_size = 16)+
  theme(legend.position = "none",panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        plot.caption = element_text(hjust = 0))
t.test(sum ~removal, data = fil_algae_sum, var.equal = TRUE)
fil_plots<- fil_time +fil_box + plot_layout(widths = c(2,1))
acf(fil_algae_sum$sum)



secchi_plots/chloro_plots/tn_plots/tp_plots+ plot_layout(widths = c(2,1)) + 
  plot_annotation(tag_levels = paste("a"), tag_suffix = ")",  theme(element_text(size = 9)))
ggsave("figures/manuscript/summer_timeseries_nutrients.png", width = 11, height = 9, units = 'in')

layout <- (secchi_time/chloro_time/tn_time/tp_time) | (secchi_box/chloro_box/tn_box/tp_box)

# Adjust the relative widths: first column (time series) twice as wide as second column (box plots)
layout + 
  plot_layout(widths = c(2, 1)) +  # First column twice as wide
  plot_annotation(tag_levels = 'a', tag_prefix = "(",tag_suffix = ")")

ggsave("figures/manuscript/summer_timeseries_nutrients.png", width = 11, height = 8.5, units = 'in')


(secchi_time|chloro_time|tn_time|tp_time) / (secchi_box/chloro_box/tn_box/tp_box) + 
  plot_annotation(tag_levels = paste("a"), tag_prefix = "(",tag_suffix = ")",  theme(element_text(size = 9)))
ggsave("figures/summer_box.png", width = 7, height = 7.5, units = 'in')

