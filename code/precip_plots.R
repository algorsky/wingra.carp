library(tidyverse)
library(patchwork)
#read in datasets
summer<- read_csv("data/summer_variables.csv")
summer_2020<- summer%>%
  mutate(tp_median = ifelse(year4 == 2020, 0.011, tp_median))

secchi_precip_plot<-ggplot()+
  geom_smooth(data = filter(summer, removal == '≥ 2008'), aes(x = sum_precip, y = secchi_median), method = "lm", color = "black")+
  geom_jitter(data = summer, aes(x = sum_precip, y = secchi_median, fill = removal), shape = 21, size = 4)+
  scale_fill_manual(values = c("white","black"), name = "Carp removal:")+
  xlab("Precipitation (mm)")+
  ylab("Secchi (m) ")+
  annotate(geom = 'text', x = 810, y=1.7, fontface = "bold", label = paste("R^2 == ", 0.06), parse = TRUE)+
  # annotate(geom = 'text', x = 810, y= 1.7, fontface = "bold", label = paste("R^2 == ", 0), parse = TRUE)+
  theme_bw(base_size = 14)+
  theme(
    plot.caption = element_text(hjust = 0))

chl_precip_plot<-ggplot()+
  geom_smooth(data = filter(summer, removal == '≥ 2008'), aes(x = sum_precip, y = chl_median), method = "lm", color = "black")+
  geom_jitter(data = summer, aes(x = sum_precip, y = chl_median, fill = removal), shape = 21, size = 4)+
  scale_fill_manual(values = c("white","black"), name = "Carp removal:")+
  xlab("Precipitation (mm)")+
  ylab(expression(paste("Chlorophyll a", " (", µ,"g ", L^-1,")")))+
  annotate(geom = 'text', x = 810, y = 32, fontface = "bold", label = "R^2 == 0.5 ~ '*' ", parse = TRUE)+
  theme_bw(base_size = 14)+
  theme(
    plot.caption = element_text(hjust = 0))

tn_precip_plot<-ggplot()+
  geom_smooth(data = filter(summer, removal == '≥ 2008'), aes(x = sum_precip, y = totnuf_median/1000), method = "lm", color = "black")+
  geom_jitter(data = summer, aes(x = sum_precip, y = totnuf_median/1000, fill = removal), shape = 21, size = 4)+
  scale_fill_manual(values = c("white","black"), name = "Carp removal:")+
  xlab("Precipitation (mm)")+
  ylab(expression(paste("Total nitrogen", " (mg ", L^-1,")")))+
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
  ylab(expression(paste("Total phosphorus", " (mg ", L^-1,")")))+
#  annotate(geom = 'text', x = 810, y = 0.065, fontface = "bold", label = "R^2 == 0.31 ~ '*' ", parse = TRUE)+
  # annotate(geom = 'text', x = 810, y= 1.7, fontface = "bold", label = paste("R^2 == ", 0), parse = TRUE)+
  # geom_text(aes(x = 400, y = 30), label = bquote(bold(Adj.~~R^2 == 0.43)), size = 5)+
  #geom_text(aes(x = 400, y = 30), label = expression(bold(Adj.~~R^2 == 0.43)), size = 4)+
  theme_bw(base_size = 14)+
  theme(
    plot.caption = element_text(hjust = 0))

(secchi_precip_plot + chl_precip_plot) / (tn_precip_plot + tp_precip_plot) + plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ")") + plot_layout(guides='collect') &theme(legend.position='bottom') 
ggsave("figures/manuscript/precip_plots_all.png", width = 9, height = 8, units = 'in')

secchi_model <- lm(secchi_median ~ sum_precip, data = filter(secchi_summer, removal == '≥ 2008'))
chl_model <- lm(chl_median ~ sum_precip, data = filter(summer, removal == '≥ 2008'))
tn_model <- lm(totnuf_median ~ sum_precip, data = filter(summer, removal == '≥ 2008'))
tp_model <- lm(tp_median ~ sum_precip, data = filter(summer_2020, removal == '≥ 2008'))
summary(secchi_model)
summary(chl_model)
summary(tn_model)
summary(tp_model)
