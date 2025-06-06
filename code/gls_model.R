library(tidyverse)
library(nlme)

ag.data<- read_csv("data/summary_medians.csv")|> 
  mutate(group = if_else(year4 < 2008, 'pre', 'post'))
acf(chl_gls$chloro_median)
chl_gls<- summary_medians%>%
  select(chloro_median, group)%>%
  na.omit()
tn_gls<- summary_medians%>%
  select(totnuf_median, group, year4)%>%
  na.omit()
acf(tn_gls$totnuf_median)
tp_gls<- summary_medians%>%
  select(tp_median, group, year4)%>%
  na.omit()
acf(tp_gls$tp_median)
fil_gls<- summary_medians%>%
  select(sum_fil_algae, group, year4)%>%
  na.omit()
pettitt_result_fil <- pettitt.test(fil_gls$sum_fil_algae)
fil_gls[12,]

max(fil_gls$sum_fil_algae)
break_date_macro <- macro_gls$Year[pettitt_result_macro$estimate]

plot(macro_gls$Year, macro_gls$colonization, type = "b",
     xlab = "Date", ylab = "Colonization depth (m)",
     main = "Pettitt's Test for Change Point in Colonization depth")
abline(v = break_date_macro, col = "red", lty = 2, lwd = 2)
acf(fil_gls$sum_fil_algae)|> 
  mutate(group = if_else(Year < 2008, 'pre', 'post'))
acf(zoop_summer_sum$mean_sum)

macro_gls<- read_csv("data/dnr_macrophyte_sum.csv")|> 
  mutate(group = if_else(Year < 2008, 'pre', 'post'))%>%
  rename(colonization = `Maximum depth of plants (ft)`)%>%
  mutate(colonization = colonization/3.281)%>%
  select(colonization, group, Year)

pettitt_result_macro <- pettitt.test(macro_gls$colonization)
macro_gls[5,]

break_date_macro <- macro_gls$Year[pettitt_result_macro$estimate]

plot(macro_gls$Year, macro_gls$colonization, type = "b",
     xlab = "Date", ylab = "Colonization depth (m)",
     main = "Pettitt's Test for Change Point in Colonization depth")
abline(v = break_date_macro, col = "red", lty = 2, lwd = 2)

gls_secchi <- gls(secchi_median ~ group, data = summary_medians, correlation = corAR1(form = ~ year4))
summary(gls_secchi)
residuals_gls <- resid(gls_tn) # extract model residuals 
acf(residuals_gls, main = "ACF of GLS Residuals")  # If the AR(1) structure has effectively accounted for autocorrelation, 
# the ACF plot should show little to no significant autocorrelation

gls_tp <-  gls(tp_median ~ group, data = tp_gls, correlation = corAR1(form = ~ year4))
summary(gls_tp)

gls_tn <-  gls(totnuf_median ~ group, data = tn_gls, correlation = corAR1(form = ~ year4))
summary(gls_tn)


gls_fil <- gls(sum_fil_algae ~ group, data = fil_gls)
summary(gls_fil)

gls_chloro <- gls(chloro_median ~ group, data = chl_gls)
summary(gls_chloro)

gls_macro <- gls(colonization ~ group, data = macro_gls)
summary(gls_macro)

gls_zoop <- gls(sum_fil_algae ~ group, data = fil_gls)
summary(gls_zoop)

acf(fil_gls$sum_fil_algae)
acf(zoop_summer_sum$mean_sum)