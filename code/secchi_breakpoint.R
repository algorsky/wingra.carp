library(tidyverse)
library(patchwork)
library(lubridate)
# Data set title: North Temperate Lakes LTER: Secchi Disk Depth; Other Auxiliary Base Crew Sample Data 1981 - current.
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/31/32/d01c782e0601d2217b94dd614444bd33" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
secchi = read_csv(infile1)
# Filter for Wingra 
secchi = secchi %>% filter(lakeid == 'WI')%>%
  mutate(month = month(sampledate))%>%
  mutate(season = ifelse(month > 11, "winter",
                         ifelse(month < 3, "winter",
                                ifelse(month <5 & month > 2, "spring",
                                       ifelse(month < 12 & month > 9, "fall", "summer")))))%>%
  mutate(removal = ifelse(sampledate < as.Date("2008-03-01"), "before", "after"))


ggplot()+
  geom_smooth(data = filter(secchi, removal == "after"), aes(x = month, y = secnview), method = "loess", color = "blue", linetype = "dashed")+
  geom_smooth(data = filter(secchi, removal == "before"), aes(x = month, y = secnview), method = "loess", color = "red")+
  ylab("Secchi depth (m)")+
  xlab("")+
  scale_x_continuous(
    breaks = seq_along(month.name), 
    labels = month.name
  ) +
  scale_color_viridis_c()+
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 55, hjust = 1)
  )

ggplot(secchi)+
  geom_point(aes(x = sampledate, y = secnview))

secchi_year<- secchi%>%
  group_by(year4)%>%
  summarize(n = n())

secchi_all<- secchi%>%
  left_join()



secchi_new<- read_csv("data/secchi_2022.csv")|>
  filter(Site ==1)
secchi_22<- read_csv("data/secchi_2022.csv")|>
  filter(Site == 1)%>%
  mutate(year4 = year(Date))%>%
  group_by(year4)%>%
  summarize(secchi_median = (median(secchi_m,na.rm = TRUE)),
            secchi_min = (min(secchi_m,na.rm = TRUE)),
            secchi_max = (max(secchi_m,na.rm = TRUE)))%>%
  mutate(year4 = ifelse(year4 == 2008, 2008.2, year4))%>%
  mutate(removal = ifelse(year4 < 2008, "before", "after"))

secchi_summer<- secchi%>%
  mutate(month = month(sampledate))%>%
  filter(month > 5 & month < 9)%>%
  group_by(year4)%>%
  summarize(secchi_median = (mean(secnview,na.rm = TRUE)),
            secchi_min = (min(secnview,na.rm = TRUE)),
            secchi_max = (max(secnview,na.rm = TRUE)))%>%
  mutate(year4 = ifelse(year4 == 2008, 2008.2, year4))%>%
  mutate(removal = ifelse(year4 < 2008, "before", "after"))%>%
  bind_rows(secchi_22)%>%
  mutate(feet_median = secchi_median * 3.281)%>%
  mutate(feet_min = secchi_min * 3.281)%>%
  mutate(feet_max = secchi_max * 3.281)


ggplot(secchi_summer)+
  geom_point(aes(x = year4, y = secchi_median))+
  scale_y_reverse()

#Breakpoint
library(BreakPoints)

bubreak_secchi<- Buishand_R(summer_data$secchi_depth, n_period = 10, dstr = 'self', simulations = 1000)
ptbreak_secchi<-pettit(summer_data$secchi_depth,n_period=10)
bubreak_secchi
ptbreak_secchi
summer_data[80,]

bubreak_secchi<- Buishand_R(secchi_summer_all$secchi_median, n_period = 10, dstr = 'self', simulations = 1000)
ptbreak_secchi<-pettit(secchi_summer$secchi_median,n_period=10)
bubreak_secchi
ptbreak_secchi
secchi_summer[13,]

plot(summer_data$secchi_depth)
abline(v = bubreak_secchi$breaks,lwd=6,col="blue")
abline(v = ptbreak_secchi$breaks, lty=2,col="red",lwd=4)

#Breakpoint all
bubreak_secchi<- Buishand_R(secchi$secnview, n_period = 1, dstr = 'self', simulations = 1000)
ptbreak_secchi<-pettit(secchi$secnview,n_period=1)
bubreak_secchi
ptbreak_secchi
secchi[187,] # 2008-02-18

plot(secchi$secnview)
abline(v = bubreak_secchi$breaks,lwd=6,col="blue")
abline(v = ptbreak_secchi$breaks, lty=2,col="red",lwd=4)

model_before<- lm(secnview ~ sampledate, data = subset(secchi, sampledate < as.Date("2008-02-18")))
model_after<- lm(secnview ~ sampledate, data = subset(secchi, sampledate > as.Date("2008-02-18")))

breakpoint <- as.Date("2008-02-18")

ggplot(secchi)+
  geom_vline(xintercept = as.Date("2008-02-18"), color = "blue", size = 1.5)+
  geom_vline(xintercept = as.Date("2008-02-18"), color = "red", linetype = "dashed", size = 1.5)+
  geom_abline(intercept = coef(model_before)[1], slope = coef(model_before)[2], xmin = -Inf, xmax = breakpoint) +  # Add line for before breakpoint
  geom_abline(intercept = coef(model_after)[1], slope = coef(model_after)[2]) +  # Add line for after breakpoint
  geom_point(aes(x = sampledate, y = secnview), size = 2, shape = 21)+
  xlab("")+
  scale_x_date(breaks = "4 year", date_labels = "%Y") +
  ylab("Secchi (m)")+
  theme_bw(base_size = 16)
ggsave("figures/secchi_breakpoint1.png", width = 8, height = 6, units = 'in')


library(segmented)
# create a linear model of x vs y
secchi.lm<-lm(secnview ~ sampledate, data = secchi)
summary(secchi.lm)

secchi.na.break<- segmented(secchi.lm, seg.Z = ~sampledate,
                            psi = NA)

secchi.break<-segmented(secchi.lm, #linear model from above
                     seg.Z = ~ sampledate, #the variable you want to be segmented (will be time for regime shifts)
                     psi = list(sampledate = c(as.Date("2008-03-10")))) #starting value for the breakpoint to be estimated (year of carp removal)
summary(secchi.break)
secchi.break$psi #get the break point and estimate of error
slope(secchi.break) #slope of the two pieces of the regression
intercept(secchi.break) #intercept of the two pieces of the regression

#Plot the data with the breakpoint
ggplot(data=secchi, aes(y=secnview, x=sampledate))+
  geom_vline(xintercept=11883.3, color = "red", size = 1.5)+
  #geom_rect(aes(xmin=(74.8-8.2), xmax=(74.8+8.2), ymin=-Inf, ymax=Inf), color=NA, fill="grey80")+
  geom_point(shape = 21)+
  theme_linedraw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  geom_abline(intercept=2.09200, slope=-0.00011159, color="black")+
  geom_abline(intercept=-0.62577, slope=  0.00011711, color="black")+
  geom_vline(xintercept = as.Date("2008-03-10"), linetype = "dashed")
  
ggsave("figures/secchi_breakpoint2.png", width = 8, height = 6, units = 'in')
