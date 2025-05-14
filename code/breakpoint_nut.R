library(tidyverse)
nutrients<- read_csv("data/ntl_nut_WI.csv")

summer_tp<- nutrients%>%
  mutate(month = month(sampledate))%>%
 # filter(month > 4 & month < 10)%>%
  dplyr::select(year4, sampledate, totpuf, totpuf_sloh)%>%
  mutate(totpuf = totpuf/1000)%>%
  mutate(tp = ifelse(totpuf %in% NA, totpuf_sloh,
                     ifelse(totpuf < 0.02 & !is.na(totpuf_sloh) , totpuf_sloh, totpuf)))%>%
  filter(tp < 0.12)%>%
  mutate(tp_diff = totpuf-totpuf_sloh)

summer_tn<- nutrients %>%
  dplyr::select(year4, sampledate, totnuf)%>%
  na.omit()%>%
  filter(totnuf < 2000)
  


summer_tp_median <- summer_tp%>%
  dplyr::select(year4, sampledate, tp)%>%
  na.omit()%>%
  group_by(year4)%>%
  summarize(tp = median(tp))%>%
  merge(sum_precip, by = "year4")%>%
  mutate(removal = ifelse(year4 <= 2008, "before", "after"))

library(BreakPoints)
ggplot(tp_ntl_month)+
  geom_point(aes(x = date, y = median_tp), size = 2)+
  geom_vline(xintercept = as.Date("2008-03-01"), linetype = "dashed")+
  ylab("Total phosphorus (mg/L) ")+
  guides(color = FALSE, size = FALSE)+
  scale_x_date(breaks = "4 years", date_labels = "%Y")+
  xlab("")+
  theme_bw(base_size = 16)

ggplot(summer_tp)+
  geom_point(aes(x = sampledate, y = tp), size = 2)+
  geom_vline(xintercept = as.Date("2008-03-01"), linetype = "dashed")+
  ylab("Total phosphorus (mg/L) ")+
  guides(color = FALSE, size = FALSE)+
 # scale_x_date(breaks = "4 years", date_labels = "%Y")+
  xlab("")+
  theme_bw(base_size = 16)

#Breakpoint all
bubreak_tp<- Buishand_R(summer_tp$tp, n_period = 5, dstr = 'self', simulations = 1000)
ptbreak_tp<-pettit(summer_tp$tp,n_period=5)
bubreak_tp
ptbreak_tp
summer_tp[174,] # 2007-05-15 2007-05-03

bubreak_tp<- Buishand_R(summer_tp$tp, n_period = 5, dstr = 'self', simulations = 1000)
ptbreak_tp<-pettit(summer_tp$tp,n_period=5)
bubreak_tp
ptbreak_tp
summer_tp[174,] # 2007-05-15 2007-05-03

plot(summer_tp$tp)
abline(v = bubreak_tp$breaks,lwd=6,col="blue")
abline(v = ptbreak_tp$breaks, lty=2,col="red",lwd=4)
bubreak_tp$p.value

#Breakpoint all
bubreak_tp<- Buishand_R(summer_tp$tp, n_period = 5, dstr = 'self', simulations = 1000)
ptbreak_tp<-pettit(summer_tp$tp,n_period=5)
bubreak_tp
ptbreak_tp
summer_tp[174,] # 2007-05-15 2007-05-03

plot(summer_tn$totnuf)
abline(v = bubreak_tp$breaks,lwd=6,col="blue")
abline(v = ptbreak_tp$breaks, lty=2,col="red",lwd=4)
bubreak_tp$p.value

#Breakpoint all
bubreak_tp<- Buishand_R(summer_tn$totnuf, n_period = 5, dstr = 'self', simulations = 1000)
ptbreak_tp<-pettit(summer_tn$totnuf,n_period=5)
bubreak_tp
ptbreak_tp
summer_tp[166,] # 2007-05-15 2007-05-03


bubreak_tp_all<- Buishand_R(tp_ntl$tp, n_period = 5, dstr = 'self', simulations = 1000)
ptbreak_tp_all<-pettit(tp_ntl$tp,n_period=5)

plot(tp_ntl$tp)
abline(v = bubreak_tp_all$breaks,lwd=6,col="blue")
abline(v = ptbreak_tp_all$breaks, lty=2,col="red",lwd=4)
bubreak_tp$p.value

tp_ntl[156,]

summer_tp<- tp_ntl%>%
  filter(month > 4 & month < 10)
