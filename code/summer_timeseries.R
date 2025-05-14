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
  filter(year4 != 2020)%>%
  group_by(year4)%>%
  summarize(tp_median = (mean(tp_use,na.rm = TRUE)),
            tp_min = (min(tp_use,na.rm = TRUE)),
            tp_max = (max(tp_use,na.rm = TRUE)))%>%
  mutate(year4 = ifelse(year4 == 2008, 2008.2, year4))%>%
  mutate(removal = ifelse(year4 < 2008, '< 2008', '≥ 2008'))

tp_plot<- tp_summer%>%
  filter(month(sampledate) > 5 & month(sampledate) < 9)%>%
  filter(year4 != 2020)%>%
  group_by(year4)%>%
  summarize(tp_median = (mean(tp_use,na.rm = TRUE)),
            tp_min = (min(tp_use,na.rm = TRUE)),
            tp_max = (max(tp_use,na.rm = TRUE)))%>%
  mutate(year4 = ifelse(year4 == 2008, 2008.2, year4))%>%
  mutate(removal = ifelse(year4 < 2008, '< 2008', '≥ 2008'))

min(tp_summer$tp_use, na.rm = TRUE)* 1000

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

tp_chloro<- tp_plot%>%
  left_join(chloro_plot, by = c("year4", "removal"))%>%
  left_join(tn_plot, by = c("year4", "removal"))

ggplot(tp_chloro)+
  geom_point(aes(x = tp_median, y = chloro_median, fill = removal), size = 2.5, shape = 21)+
  scale_fill_manual(values = c( "white","black"), name = "Carp removal:")+
  ylab(expression(paste("Chlorophyll a", " (", µ,"g ", L^-1,")")))+
  xlab(expression(paste("Total phosphorus", " (mg ", L^-1,")")))+
  theme_bw(base_size = 14)+
  theme(legend.position = "bottom", 
        plot.caption = element_text(hjust = 0))

#ggsave("figures/tp_chloro.png", width = 6, height = 4, units = 'in')

ggpot(tp)

  

tp_chloro_mod <- lm(tp_median ~ chloro_median, data = filter(tp_chloro, removal == "after"))
tp_chloro_mod <- lm(tp_median ~ chloro_median, data = filter(tp_chloro, removal == "before"))

summary(tp_chloro_mod)

ggplot(tp_chloro)+
  geom_point(aes(x = tp_median, y = chloro_median, color = removal))+
  geom_smooth(aes(x = tp_median, y = chloro_median), method = "lm")+
  facet_wrap(~removal, scales = "free")
  geom_smooth(data = filter(tp_chloro, removal == "after"), aes(x = tp_use, y = chl_use), method = "lm")



secchi_time<-ggplot(secchi_summer)+
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

secchi_box<-ggplot(secchi_all, aes(x = removal, y = secnview))+
  geom_boxplot(outlier.color = NA)+
  geom_jitter(aes(fill = removal), shape = 21, alpha = 0.5)+
  scale_fill_manual(values = c( "white", "black"))+
  xlab("")+
  ylab("")+
  annotate("text", x=1, y=2.2, label= "p < 2.2e-16")+
  scale_x_discrete(labels = c(expression("< 2008"), expression(phantom(x)>= 2008)))+
  theme_bw(base_size = 14)+
  theme(legend.position = "none",panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        plot.caption = element_text(hjust = 0))
t.test(secnview ~removal, data = secchi_all, var.equal = TRUE)

ggplot(tn_summer)+
  geom_histogram(aes(totnuf/1000))+
  facet_wrap(~removal)

secchi_plots<-secchi_time + secchi_box+ plot_layout(widths = c(2,1))

tn_time<-ggplot(data = tn_plot)+
 # geom_vline(xintercept = 2008, linetype = "dashed")+
  geom_path(aes(x = year4, y = totnuf_median/1000))+
 # geom_errorbar(aes(ymin =totnuf_min, ymax =  totnuf_max), width = 0)+
  geom_point(aes(x = year4, y = totnuf_median/1000, fill = removal),size = 2.5, shape = 21)+
  scale_x_continuous(limits = c(1995,2023), breaks = seq(1995, 2023, 5))+
  ylab(expression(paste("Total nitrogen", " (mg ", L^-1,")")))+
  xlab("")+
  scale_fill_manual(values = c("black", "white"))+
  theme_bw(base_size = 14)+
  theme(legend.position = "none", 
        plot.caption = element_text(hjust = 0))

tn_box<- ggplot(tn_summer, aes(x = removal, y = totnuf/1000))+
  geom_boxplot(outlier.color = NA)+
  geom_jitter(aes(fill = removal), shape = 21, alpha = 0.5)+
  scale_fill_manual(values = c( "white", "black"))+
  xlab("")+
  ylab("")+
  annotate("text", x=1, y=2.00, label= "p = 6.9e-14")+
  scale_x_discrete(labels = c(expression("< 2008"), expression(phantom(x)>= 2008)))+
  theme_bw(base_size = 14)+
  theme(legend.position = "none",panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        plot.caption = element_text(hjust = 0))
t.test(totnuf ~removal, data = tn_summer, var.equal = TRUE)
tn_plots<-tn_time + tn_box + plot_layout(widths = c(2,1))

secchi_plots / chloro_plots / tn_plots / tp_plots

tp_time<-ggplot()+
  #geom_vline(xintercept = 2008, linetype = "dashed")+
  geom_line(data = filter(tp_plot, year4< 2020), aes(x = year4, y = tp_median))+
  geom_point(data = filter(tp_plot, year4< 2020),aes(x = year4, y = tp_median, fill = removal),size = 2.5,  shape = 21)+
  geom_line(data = filter(tp_plot, year4 > 2020), aes(x = year4, y = tp_median))+
  geom_point(data = filter(tp_plot, year4 > 2020),aes(x = year4, y = tp_median, fill = removal), size = 2.5, shape = 21)+
  # geom_errorbar(aes(ymin =tp_min, ymax =  tp_max), width = 0)+
  scale_x_continuous(limits = c(1995,2023), breaks = seq(1995, 2023, 5))+
 # scale_x_continuous(limits = c(1995,2024), breaks = seq(1995, 2023, 5))+
  ylab(expression(paste("Total phosphorus", " (mg ", L^-1,")")))+
  xlab("")+
  scale_fill_manual(values = c("white", "black"))+
  theme_bw(base_size = 14)+
  theme(legend.position = "none", 
        plot.caption = element_text(hjust = 0))

tp_box<- ggplot(tp_summer, aes(x = removal, y = tp_use))+
  geom_boxplot(outlier.color = NA)+
  geom_jitter(aes(fill = removal), shape = 21, alpha = 0.5)+
  scale_fill_manual(values = c( "white", "black"))+
  xlab("")+
  ylab("")+
  annotate("text", x=1, y=0.1, label= "p = 1.5e-10")+
  scale_y_continuous(limits = c(0,0.1), breaks = seq(0, 0.1, 0.05))+
  scale_x_discrete(labels = c(expression("< 2008"), expression(phantom(x)>= 2008)))+
  theme_bw(base_size = 14)+
  theme(legend.position = "none",panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        plot.caption = element_text(hjust = 0))
t.test(tp_use ~removal, data = tp_summer, var.equal = TRUE)
tp_plots<-tp_time + tp_box + plot_layout(widths = c(2,1))

chloro_time<- ggplot()+
 # geom_vline(xintercept = 2008, linetype = "dashed")+
  geom_path(data = filter(chloro_plot, year4< 2002), aes(x = year4, y = chloro_median))+
  geom_path(data = filter(chloro_plot, year4 > 2004), aes(x = year4, y = chloro_median))+
 # geom_errorbar(data = chloro_plot, aes(x = year4, ymin = chloro_min, ymax =  chloro_max), width = 0)+
  geom_point(data = chloro_plot, aes(x = year4, y = chloro_median, fill = removal), size = 2.5, shape = 21)+
  ylab(expression(paste("Chlorophyll a", " (", µ,"g ", L^-1,")")))+
  xlab("")+
  scale_x_continuous(limits = c(1995,2023), breaks = seq(1995, 2023, 5))+
  scale_fill_manual(values = c("white", "black"))+
  theme_bw(base_size = 14)+
  theme(legend.position = "none", plot.caption = element_text(hjust = 0))
chloro_box<- ggplot(chloro_all, aes(x = removal, y = chl_use))+
  geom_boxplot(outlier.color = NA)+
  geom_jitter(aes(fill = removal), shape = 21, alpha = 0.5)+
  scale_fill_manual(values = c( "white", "black"))+
  xlab("")+
  ylab("")+
  annotate("text", x=1, y=65, label= "p = 0.02")+
  scale_x_discrete(labels = c(expression("< 2008"), expression(phantom(x)>= 2008)))+
  theme_bw(base_size = 14)+
  theme(legend.position = "none",panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        plot.caption = element_text(hjust = 0))
t.test(chl_use ~removal, data = chloro_all, var.equal = TRUE)
chloro_plots<- chloro_time + chloro_box + plot_layout(widths = c(2,1))


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

ggsave("figures/fil_timeseries_tag.png", width = 6, height = 3, units = 'in')

fil_box<-ggplot(macrophyte_wi, aes(x = removal, y = fil_algae_wt))+
  geom_boxplot(outlier.color = NA)+
  geom_jitter(aes(fill = removal), shape = 21, alpha = 0.5)+
  scale_fill_manual(values = c( "white", "black"))+
  annotate("text", x=1, y=600, label= "p < 2.2e-16")+
  xlab("")+
  ylab("")+
  scale_x_discrete(labels = c(expression("< 2008"), expression(phantom(x)>= 2008)))+
  theme_bw(base_size = 16)+
  theme(legend.position = "none",panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        plot.caption = element_text(hjust = 0))
t.test(fil_algae_wt ~removal, data = macrophyte, var.equal = TRUE)
fil_plots<- fil_time +fil_box + plot_layout(widths = c(2,1))

ggsave("figures/defense/fil_algae_timeseries.png", width = 10, height = 5, units = 'in')


secchi_plots/chloro_plots/tn_plots/tp_plots+ plot_layout(widths = c(2,1)) + 
  plot_annotation(tag_levels = paste("a"), tag_suffix = ")",  theme(element_text(size = 9)))
ggsave("figures/manuscript/summer_timeseries_nutrients.png", width = 11, height = 9, units = 'in')

secchi_plots/chloro_plots/tn_plots/tp_plots+ plot_layout(widths = c(2,1)) + 
  plot_annotation(tag_levels = paste("a"),tag_prefix = "(", tag_suffix = ")",  theme(element_text(size = 9)))

secchi_time/chloro_time/tn_time/tp_time+ 
  plot_annotation(tag_levels = paste("a"), tag_prefix = "(",tag_suffix = ")",  theme(element_text(size = 9)))
ggsave("figures/summer_time.png", width = 7, height = 7.5, units = 'in')
secchi_box/chloro_box/tn_box/tp_box+ 
  plot_annotation(tag_levels = paste("a"), tag_prefix = "(",tag_suffix = ")",  theme(element_text(size = 9)))
ggsave("figures/summer_box.png", width = 7, height = 7.5, units = 'in')

layout <- (secchi_time/chloro_time/tn_time/tp_time) | (secchi_box/chloro_box/tn_box/tp_box)

# Adjust the relative widths: first column (time series) twice as wide as second column (box plots)
layout + 
  plot_layout(widths = c(2, 1)) +  # First column twice as wide
  plot_annotation(tag_levels = 'a', tag_prefix = "(",tag_suffix = ")")

ggsave("figures/manuscript/summer_timeseries_nutrients.png", width = 11, height = 8.5, units = 'in')


(secchi_time|chloro_time|tn_time|tp_time) / (secchi_box/chloro_box/tn_box/tp_box) + 
  plot_annotation(tag_levels = paste("a"), tag_prefix = "(",tag_suffix = ")",  theme(element_text(size = 9)))
ggsave("figures/summer_box.png", width = 7, height = 7.5, units = 'in')


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
fil_plots<- (fil_time +fil_box) + plot_layout(widths = c(2,1)) +
  plot_annotation(tag_levels = paste("a"), tag_prefix = "(",tag_suffix = ")",  theme(element_text(size = 9)))
ggsave("figures/manuscript/fil_box.png", width = 7, height = 3, units = 'in')
(fil_time + fil_box + plot_layout(widths = c(2, 1))) / 
  (plot_spacer() + secchi_fil_plot + plot_spacer() + plot_layout(widths = c(0.2, 3, 1))) +
  plot_layout(heights = c(1, 1.2))+
  plot_annotation(tag_levels = paste("a"), tag_prefix = "(",tag_suffix = ")",  theme(element_text(size = 9)))
ggsave("figures/manuscript/fil_box_secchi.png", width = 7, height = 6, units = 'in')

secchi_mean<- secchi_all%>%
  mutate(year4 = year(sampledate))%>%
  group_by(year4)%>%
  summarize(secchi_mean = mean(secnview),
            secchi_median = median(secnview))%>%
  mutate(removal = ifelse(year4 < 2008, '< 2008', '≥ 2008'))%>%
  filter(year4 < 2019)


secchi_fil<- fil_algae_timeseries%>%
  left_join(secchi_mean, by = c("year4", "removal"))

secchi_fil_plot<-ggplot(filter(secchi_fil))+
  geom_point(aes(y = fil_algae_sum, x = secchi_median, fill= removal), size = 4 , shape = 21)+
  scale_fill_manual(values = c( "white", "black"), name = "Carp removal:")+
  ylab("Fil. algae wet weight (g)")+
  xlab("Secchi (m) ")+
  scale_y_log10()+
  theme_bw(base_size = 14)+
  theme(legend.position = "bottom",plot.caption = element_text(hjust = 0))
