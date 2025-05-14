library(tidyverse)

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/20/36/3c7ddd692d3ac8e90bf2954a16b39e89" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
weather <-read_csv(infile1)

precip<- read_csv("data/StnData.csv")|>
  mutate(precip_in = as.numeric(precip_in))%>%
  mutate(precip_mm = precip_in * 25.4)%>%
  filter(year(sampledate) > 1994)

summer_precipitation<- precip%>%
  mutate(year4 = year(sampledate))%>%
  filter(month(sampledate) > 5 & month(sampledate) < 9)%>%
  replace(is.na(.), 0)%>%
  group_by(year4)%>%
  summarize(sum_precip = sum(precip_mm))

ggplot(summer_precipitation)+
  geom_col(aes(x = year4, y = sum_precip))


ggplot(sum_precip)+
  geom_col(aes(x = year4, y = sum_precip))

sum_precip<- summer_precip%>%
  group_by(year4)%>%
  summarize(sum_precip = sum(precip_raw_mm),
            mean_precip = mean(precip_raw_mm))


ggplot(precip)+
  geom_point(aes(x = ))

weather_years<- weather%>%
  filter(year4 > 1995)%>%
  filter(year4 < 2019)

summer_precip<- weather%>%
  filter(year4 > 1995)%>%
  mutate(month = month(sampledate))%>%
  filter(month > 4 & month < 10)

#Filter to 1996 to present
#Get a sum of the summer precipation (May to September) per year
summer_precip<- weather%>%
  filter(year4 > 1995)%>%
  mutate(month = month(sampledate))%>%
  filter(month > 4 & month < 10)

annual_precip <- weather%>%
  group_by(year4)%>%
  summarize(total_precip = sum(precip_raw_mm))

ggplot(filter(summer_precip, year4 == 2018))+
  geom_point(aes(x = sampledate, y = precip_raw_mm))

max(summer_precip$precip_raw_mm)
mean(summer_precip$precip_raw_mm)

sum_precip<- summer_precip%>%
  group_by(year4)%>%
  summarize(sum_precip = sum(precip_raw_mm),
            mean_precip = mean(precip_raw_mm))

ggplot(filter(sum_precip, year4 > 1996))+
  geom_point(aes(x = year4, y = sum_precip))+
  geom_line(aes(x = year4, y = sum_precip))

ggplot(sum_precip)+
  geom_col(aes(x = year4, y = sum_precip))
ggplot(filter(annual_precip, year4 > 1996))+
  geom_point(aes(x = year4, y = total_precip))+
  geom_line(aes(x = year4, y = total_precip))

nutrients<- read_csv("data/ntl_nut_WI.csv")

summer_tp<- nutrients%>%
  mutate(month = month(sampledate))%>%
  filter(month > 4 & month < 10)%>%
  dplyr::select(year4, sampledate, totpuf, totpuf_sloh)%>%
  mutate(totpuf = totpuf/1000)%>%
  mutate(tp = ifelse(totpuf %in% NA, totpuf_sloh,
                     ifelse(totpuf < 0.02 & !is.na(totpuf_sloh) , totpuf_sloh, totpuf)))%>%
  mutate(tp_diff = totpuf-totpuf_sloh)%>%
  mutate(tp_use = ifelse(sampledate == as.Date("2017-07-12"), totpuf_sloh, tp))


tp_ntl<- nutrients%>%
  mutate(month = month(sampledate))%>%
  dplyr::select(year4, month, sampledate, totpuf, totpuf_sloh)%>%
  mutate(totpuf = totpuf/1000)%>%
  mutate(tp = ifelse(totpuf %in% NA, totpuf_sloh,
                     ifelse(totpuf < 0.02 & !is.na(totpuf_sloh) , totpuf_sloh, totpuf)))%>%
  mutate(tp_diff = totpuf-totpuf_sloh)%>%
  filter(tp < 0.13)%>%
  filter(tp > 0)%>%
  filter((year4 != 1999 & tp != 0.002) | (year4 != 1999 & tp != 0.004))%>%
  mutate(season = ifelse(month > 4 & month < 10, "summer",
                         ifelse(month > 10 & month < 12, "fall",
                                ifelse(month > 2 &  month < 5, "spring", "winter"))))

tp_ntl_month<- tp_ntl%>%
  group_by(month, year4)%>%
  summarize(median_tp = median(tp))%>%
  mutate(date = ymd(paste(year4, month, "01", sep = "-")))%>%
  mutate(season = ifelse(month > 4 & month < 10, "summer",
                         ifelse(month > 10 & month < 12, "fall",
                                ifelse(month > 2 &  month < 5, "spring", "winter"))))%>%
  mutate(removal = ifelse(date < as.Date("2008-03-15"), "pre", "post"))%>%
  filter(median_tp > 0.007)
tn_ntl_month<- nutrients%>%
  mutate(month = month(sampledate))%>%
  group_by(month, year4)%>%
  summarize(median_tn = median(totnuf))%>%
  mutate(date = ymd(paste(year4, month, "01", sep = "-")))%>%
  mutate(season = ifelse(month > 4 & month < 10, "summer",
                         ifelse(month > 10 & month < 12, "fall",
                                ifelse(month > 2 &  month < 5, "spring", "winter"))))%>%
  mutate(removal = ifelse(date < as.Date("2008-03-15"), "pre", "post"))
tp_time<-ggplot(tp_ntl_month)+
  geom_segment(aes(x=as.Date("1996-02-01"),xend=as.Date("2008-03-01"),y=0.04512613,yend=0.04512613))+
  geom_segment(aes(xend=as.Date("2022-11-01"),x=as.Date("2008-03-01"),y=0.03638843,yend=0.03638843))+
  #geom_segment(aes(x=15,xend=20,y=0.035,yend=0.035))+
  geom_point(aes(x = date, y = median_tp), size = 2)+
  ylim(c(0, 0.1))+
  geom_vline(xintercept = as.Date("2008-03-01"), linetype = "dashed")+
  ylab("Total phosphorus (mg/L) ")+
  guides(color = FALSE, size = FALSE)+
  scale_x_date(breaks = "4 years", date_labels = "%Y")+
  xlab("")+
  theme_bw(base_size = 16)

tp_time<- ggplot()+
# geom_segment(aes(x=as.Date("1996-02-01"),xend=as.Date("2008-03-01"),y=0.0468,yend=0.0468))+
#geom_segment(aes(xend=as.Date("2022-11-01"),x=as.Date("2008-03-01"),y=0.0367 ,yend=0.0367 ))+
  geom_point(data = tp_ntl, aes(x = sampledate, y = tp, color = season, size = season, shape = season, alpha = season))+
  scale_color_manual(values = c("black", "black", "red", "black"))+
  scale_size_manual(values = c(2, 2, 3, 2))+
  scale_shape_manual(values = c(16, 16, 17, 16))+
  scale_alpha_manual(values = c(0.5, 0.5, 1, 0.5))+
 # geom_point(data = (tp_ntl), aes(x = sampledate, y = tp), size = 2)+
 # geom_point(data = (tp_ntl), aes(x = sampledate, y = tp), size = 2, alpha = 0.5)+
#  geom_point(data = filter(tp_ntl, month > 4 & month < 10), aes(x = sampledate, y = tp), size = 3, shape = 17, color = "red")+
  ylim(c(0, 0.085))+
  geom_vline(xintercept = as.Date("2008-03-01"), linetype = "dashed")+
  ylab("Total phosphorus (mg/L) ")+
  guides(color = FALSE, size = FALSE, shape = FALSE, alpha = FALSE)+
  scale_x_date(breaks = "4 years", date_labels = "%Y")+
  xlab("")+
  theme_bw(base_size = 16)

#mean before and after
tp_ntl_month_mean <- tp_ntl%>%
  mutate(removal = ifelse(year4 < 2008, "before", "after"))%>%
  mutate(tp_use = ifelse(sampledate == as.Date("2017-07-12"), totpuf_sloh, tp))%>%
  group_by(removal)%>%
  summarize(mean_tp = mean(tp_use),
            median_tp = median(tp_use))

tn_ntl_month_mean <- tn_ntl%>%
  group_by(removal)%>%
  summarize(mean_tn = mean(tn, na.rm = TRUE),
            median_tn = median(tn, na.rm = TRUE))
  
tn_ntl<- nutrients%>%
  mutate(month = month(sampledate))%>%
  mutate(season = ifelse(month > 4 & month < 10, "summer",
                         ifelse(month > 10 & month < 12, "fall",
                                ifelse(month > 2 &  month < 5, "spring", "winter"))))%>%
  mutate(removal = ifelse(sampledate < as.Date("2008-03-15"), "pre", "post"))%>%
  filter(totnuf > 12)%>%
  filter(totnuf < 3000)%>%
  mutate(tn = totnuf/1000)

tn_time<-ggplot(filter(tn_ntl_month, median_tn < 3000))+
 geom_segment(aes(x=as.Date("1996-02-01"),xend=as.Date("2008-03-01"),y=1094.2864/1000,yend=1094.2864/1000))+
geom_segment(aes(xend=as.Date("2022-11-01"),x=as.Date("2008-03-01"),y=819.7071/1000,yend=819.7071/1000))+
  geom_point(aes(x = date, y = median_tn/1000), size = 2)+
  geom_vline(xintercept = as.Date("2008-03-01"), linetype = "dashed")+
  ylab("Total nitrogen (mg/L) ")+
  guides(color = FALSE, size = FALSE)+
  scale_x_date(breaks = "4 years", date_labels = "%Y")+
  xlab("")+
  theme_bw(base_size = 16)
tn_time<-ggplot()+
 #geom_segment(aes(x=as.Date("1996-02-01"),xend=as.Date("2008-03-01"),y=1.0291611,yend=1.0291611))+
 #geom_segment(aes(xend=as.Date("2022-11-01"),x=as.Date("2008-03-01"),y=0.7931852,yend=0.7931852))+
  geom_point(data = tn_ntl, aes(x = sampledate, y = tn, color = season, size = season, shape = season, alpha = season))+
  scale_color_manual(values = c("black", "black", "red", "black"))+
  scale_size_manual(values = c(2, 2, 3, 2))+
  scale_shape_manual(values = c(16, 16, 17, 16))+
  scale_alpha_manual(values = c(0.5, 0.5, 1, 0.5))+
 #geom_point(data = tn_ntl, aes(x = sampledate, y = tn), size = 2)+
 #geom_point(data = tn_ntl, aes(x = sampledate, y = tn), size = 2, alpha = 0.5)+
 # geom_point(filter(tn_ntl_month, median_tn < 3000 & (month > 4 & month < 10)), aes(x = date, y = median_tn/1000), size = 3, shape = 17, color = "red")+
  geom_vline(xintercept = as.Date("2008-03-01"), linetype = "dashed")+
  ylab("Total nitrogen (mg/L) ")+
  guides(color = FALSE, size = FALSE, shape = FALSE, alpha = FALSE)+
  ylim(c(0, 2.3))+
  scale_x_date(breaks = "4 years", date_labels = "%Y")+
  xlab("")+
  theme_bw(base_size = 16)


ggsave("figures/tn_tp_summer.png", width = 12, height = 5, units = 'in')
library(cowplot)
prow <- plot_grid( tn_time + theme(legend.position="none"),
                   tp_time + theme(legend.position="none"),
                   align = 'vh',
                   labels = c("a)", "b)"),
                   hjust = -1,
                   nrow = 1
)
  
tp_green <- ggplot()+
 # geom_point(data = filter(tp_ntl_month, removal =="before"), aes(x = date, y = median_tp), size = 2)+
  #geom_point(data = filter(tp_ntl_month, removal =="after"), aes(x = date, y = median_tp, color = season, size = season, shape = season))+
  geom_point(data = tp_ntl, aes(x = sampledate, y = tp, color = season, size = season, shape = season))+
   geom_vline(xintercept = as.Date("2008-03-01"), linetype = "dashed")+
  scale_color_manual(values = c("black", "black", "red", "black"))+
  ylab("Total phosphorus (mg/L) ")+
  scale_size_manual(values = c(2, 2, 3, 2))+
  scale_shape_manual(values = c(16, 16, 17, 16))+
  scale_x_date(breaks = "4 years", date_labels = "%Y")+
  xlab("")+
  guides(color = FALSE, size = FALSE)+
  theme_bw(base_size = 16)+
  theme(legend.position = "none")

tn_green<- ggplot()+
 # geom_point(data = filter(tn_ntl_month, removal =="before" & median_tn < 3000), aes(x = date, y = median_tn/1000), size = 2)+
  #geom_point(data = filter(tn_ntl_month, removal =="after" & median_tn < 3000), aes(x = date, y = median_tn/1000, color = season, size = season, shape = season))+
  geom_point(data = filter(tn_ntl, month < 4 & month > 10), aes(x = date, y = tn/1000, color = season, size = season, shape = season))+
  geom_point(data = filter(tn_ntl, month > 4 & month < 10), aes(x = sampledate, y = tn), size = 3, shape = 17, color = "red")+
  geom_vline(xintercept = as.Date("2008-03-01"), linetype = "dashed")+
  scale_color_manual(values = c("black", "black", "red", "black"))+
  ylab("Total nitrogen (mg/L) ")+
  scale_size_manual(values = c(2, 2, 3, 2))+
  scale_shape_manual(values = c(16, 16, 17, 16))+
  scale_x_date(breaks = "4 years", date_labels = "%Y")+
  xlab("")+
  guides(color = FALSE, size = FALSE)+
  theme_bw(base_size = 16)+
  theme(legend.position = "none")

  
ggplot(filter(nutrients, month(sampledate) < 10 & month(sampledate) > 4))+
  geom_point(aes(x = sampledate, y = totnuf))+
  geom_vline(xintercept = as.Date("2008-03-01"), linetype = "dashed")+
  theme_bw()
summer_tn<- nutrients%>%
  mutate(month = month(sampledate))%>%
  filter(month > 4 & month < 10)%>%
  dplyr:: select(year4, sampledate, totnuf)%>%
  mutate(totnuf_mgL = totnuf/1000)

summer_tp_median <- summer_tp%>%
  dplyr::select(year4, sampledate, tp_use)%>%
  na.omit()%>%
  group_by(year4)%>%
  summarize(tp = median(tp_use))%>%
  merge(sum_precip, by = "year4")%>%
  mutate(removal = ifelse(year4 <= 2008, "pre", "post"))


ggplot(summer_tp_median)+
  geom_point(aes(x = sum_precip, y = tp, color = year4))+
  facet_wrap(~removal, scales = "free")+
  scale_color_viridis_c()+
  theme_bw()


ggplot(summer_tp_median)+
  geom_jitter(aes(x = sum_precip, y = tp, fill = removal), shape = 21, size = 4)+
  scale_fill_manual(values = c("white", "black"))+
  xlab("Total precipitation (mm)")+
  ylab("Total phosphorus (mg/L) ")+
  theme_bw(base_size = 16)+
  theme(legend.title = element_blank())
tp<-ggplot()+
  geom_smooth(data = filter(summer_tp_median, (year4 != 2020 & removal == "after")), aes(x = sum_precip, y = tp), method = "lm", color = "black")+
  geom_jitter(data = filter(summer_tp_median, (year4 != 2020)), aes(x = sum_precip, y = tp, fill = removal), shape = 21, size = 4)+
  scale_fill_manual(values = c("black","white"))+
  xlab("Summer total precipitation (mm)")+
  ylab("Median summer total phosphorus (mg/L) ")+
  theme_bw(base_size = 16)+
  theme(legend.title = element_blank())
ggsave("figures/tp_precip_trend.png", width = 8, height = 6, units = 'in')

weather_tp<- tp_ntl%>%
  left_join(weather, by = c("sampledate", "year4"))%>%
  select(year4, sampledate, totpuf, totpuf_sloh, tp, precip_raw_mm)%>%
  filter(month(sampledate) > 4 & month(sampledate) < 10)

tp_model <- lm(tp ~ sum_precip, summer_after)
summary(tp_model)

summer_after<- summer_tp_median%>%
  filter(removal == "post")%>%
  filter(year4 != 2017)
ggplot(summer_after)+
  geom_point(aes(x = sum_precip, y = tp))

ggplot()+
  geom_point(data = summer_precip, aes(x = sampledate, y = precip_raw_mm))+
  geom_point(data = summer_tp, aes(x = sampledate, y = tp *1000), color = "red")

#chlorophyll
chloro <- read_csv('data/wingra_chl_data.csv')
  

ggplot(filter(chloro, month(sampledate) > 4 & month(sampledate) < 10))+
  geom_point(aes(x = sampledate, y = chl_use))

#Nitrogen

ggplot(filter(nutrients, month(sampledate) < 10 & month(sampledate) > 4))+
  geom_point(aes(x = sampledate, y = totnuf))+
  geom_vline(xintercept = as.Date("2008-03-01"), linetype = "dashed")+
  theme_bw()
summer_tn<- nutrients%>%
  mutate(month = month(sampledate))%>%
  filter(month > 4 & month < 10)%>%
 dplyr:: select(year4, sampledate, totnuf)%>%
  mutate(totnuf_mgL = totnuf/1000)

summer_tn_median <- summer_tn%>%
  dplyr::select(year4, sampledate, totnuf_mgL)%>%
  na.omit()%>%
  group_by(year4)%>%
  summarize(tn = median(totnuf_mgL))%>%
  merge(sum_precip, by = "year4")%>%
  mutate(removal = ifelse(year4 <= 2008, "before", "after"))

ggplot(summer_tn_median)+
  geom_jitter(aes(x = sum_precip, y = tn, fill = removal), shape = 21, size = 4)+
  scale_fill_manual(values = c("white", "black"))+
  xlab("Total precipitation (mm)")+
  ylab("Total nitrogen (mg/L) ")+
  theme_bw(base_size = 16)+
  theme(legend.title = element_blank())

ggplot()+
 # geom_boxplot(data = summer_tn_median, aes(x = removal, y = tn/1000, group = removal))+ 
  geom_boxplot(data = summer_tp_median, aes(x = removal, y = tp*1000, group = removal))+ 
  ylab("Total nitrogen (mg/L) ")+
  theme_bw()

var.test(tn ~removal, data = summer_tn_median)
var.test(tp ~removal, data = summer_tp_median)
var.test(chl_median ~removal, data = chloro_summer_sum)

ggplot((chloro_summer_sum))+
  geom_histogram(aes(x = chl_median))+
  facet_wrap(~removal)
plot(tn ~removal, data = summer_tn_median)
t.test(tn ~removal, data = summer_tn_median, var.equal = TRUE)

plot(tp ~removal, data = summer_tp_median)
t.test(tp ~removal, data = summer_tp_median, var.equal = TRUE)

plot((chl_median) ~removal, data = chloro_summer_sum)
t.test((chl_median) ~removal, data = chloro_summer_sum, var.equal = TRUE)


silica <- nutrients%>%
  dplyr::select(year4, sampledate, drsif)%>%
  mutate(month = month(sampledate) )


#ggplot(filter(silica, month > 3 & month < 12))+
ggplot(silica)+
  geom_point(aes(x = sampledate, y = drsif, color = month))+
  geom_vline(xintercept = as.Date("2008-03-01"), linetype = "dashed")+
  ylab("Dissolved reactive silica (µg/L)")+
  xlab("")+
  scale_color_viridis_c()+
  theme_bw(base_size = 16)
ggsave(filename = 'figures/silica.png',width = 8,height = 6,units = 'in')

ggplot(filter(silica, month > 7 & month < 10))+
  geom_point(aes(x = sampledate, y = drsif, color = as.factor(month)))+
  geom_vline(xintercept = as.Date("2008-03-01"), linetype = "dashed")+
  ylab("Dissolved reactive silica (µg/L)")+
  xlab("")+
  theme_bw(base_size = 16)


summer_tn_median <- nutrients%>%
  mutate(month = month(sampledate))%>%
  filter(month > 4 & month < 10)%>%
  dplyr::select(year4, sampledate, totnuf)%>%
  na.omit()%>%
  group_by(year4)%>%
  summarize(tn = median(totnuf))%>%
  merge(sum_precip, by = "year4")%>%
  filter(year4 != 2008)%>%
  mutate(removal = ifelse(year4 <= 2008, "pre", "post"))

tn<-ggplot()+
  geom_smooth(data = filter(summer_tn_median, (removal == "after")), aes(x = sum_precip, y = tn), method = "lm", color = "black")+
  geom_jitter(data = (summer_tn_median), aes(x = sum_precip, y = tn, fill = removal), shape = 21, size = 4)+
  scale_fill_manual(values = c("black","white"))+
  xlab("Summer total precipitation (mm)")+
  ylab("Median summer total nitrogen (mg/L) ")+
  theme_bw(base_size = 16)+
  theme(legend.title = element_blank())
ggsave("figures/tn_precip_trend.png", width = 8, height = 6, units = 'in')

tn_model <- lm(tn ~ sum_precip, summer_tn_after)
summary(tn_model)

summer_tn_after<- summer_tn_median%>%
  filter(year4 != 2020)%>%
  filter(removal == "after")

library(patchwork)
tn + tp + plot_layout(guides = "collect")
ggsave("figures/tn_tp_precip_trend_wide.png", width = 12, height = 5, units = 'in')

prow <- plot_grid( tn + theme(legend.position="none"),
                   tp + theme(legend.position="none"),
                   align = 'vh',
                   labels = c("a)", "b)"),
                   hjust = -1,
                   nrow = 1
)
