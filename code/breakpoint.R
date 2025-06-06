library(BreakPoints)
library(trend)

secchi_all<- secchi_all%>%
  na.omit()
pettitt_result <- pettitt.test(secchi_all$secnview)

secchi_all[79,]

plot(secchi_all$sampledate, secchi_all$secnview, type = "p",
     xlab = "Date", ylab = "Secchi Depth (m)",
     main = "Pettitt's Test for Change Point in Secchi Depth")
abline(v = break_date, col = "red", lty = 2, lwd = 2)

chl_ordered <- chloro_all %>%
 # arrange(sampledate) %>%
 filter(!is.na(chl_use))%>%
filter(year(sampledate)>2003)
pettitt_result_chl <- pettitt.test(chl_ordered$chl_use)
chl_ordered[98,]

break_date_chl <- chl_ordered$sampledate[pettitt_result_chl$estimate]

plot(chl_ordered$sampledate, chl_ordered$chl_use, type = "b",
     xlab = "Date", ylab = "Chlorophyll-a (µg/L)",
     main = "Pettitt's Test for Change Point in Chlorophyll-a")
abline(v = break_date_chl, col = "red", lty = 2, lwd = 2)

tp_ordered <- tp %>%
  arrange(sampledate) %>%
  filter(!is.na(totpuf))%>%
  filter(year(sampledate) < 2020)
pettitt_result_tp <- pettitt.test(tp_ordered$totpuf)
break_date_tp <- tp_ordered$sampledate[pettitt_result_tp$estimate]
tp_ordered[79,]

plot(tp_ordered$sampledate, tp_ordered$totpuf, type = "b",
     xlab = "Date", ylab = "Chlorophyll-a (µg/L)",
     main = "Pettitt's Test for Change Point in Chlorophyll-a")
abline(v = break_date_tp, col = "red", lty = 2, lwd = 2)

tn_ordered <- tn %>%
  arrange(sampledate) %>%
  filter(year(sampledate) < 2020)
pettitt_result_tn <- pettitt.test(tn_ordered$totnuf)
break_date_tn <- tn_ordered$sampledate[pettitt_result_tn$estimate]
tn_ordered[70,]
plot(tn$sampledate, tn$totnuf, type = "b",
     xlab = "Date", ylab = "TN (µg/L)",
     main = "Pettitt's Test for Change Point in TN")
abline(v = break_date_tn, col = "red", lty = 2, lwd = 2)

pettitt_result_tn <- pettitt.test(tn$totnuf)
tn[70,]

#Breakpoint all
ptbreak_secchi<-pettit(secchi_all$secnview,n_period=1)
ptbreak_secchi #80 and p-value <0.05
secchi_all[80,] # 2008-06-11

plot(secchi_all$secnview)
abline(v = ptbreak_secchi$breaks, lty=2,col="red",lwd=4)


ptbreak_tp<-pettit(secchi_all$secnview,n_period=1)
ptbreak_tp #80 and p-value <0.05
secchi_all[80,] # 2008-06-11

plot(secchi_all$secnview)
abline(v = ptbreak_secchi$breaks, lty=2,col="red",lwd=4)

chloro_break<- chloro_all%>%
  filter(year(sampledate) > 2004)
ptbreak_chloro<-pettit(chloro_break$chl_use,n_period=1)
ptbreak_chloro #99 and p-value <0.05
chloro_break[99,] # 2020-06-08   

plot(chloro_break$chl_use)
abline(v = ptbreak_chloro$breaks, lty=2,col="red",lwd=4)
