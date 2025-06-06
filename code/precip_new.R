# Grab .csv from Lizzie's repo 
arb.precip = read_csv('https://raw.githubusercontent.com/hdugan/Wingra_SaltTrajectory/refs/heads/main/data_input/Climate/3944435.csv') |> 
  mutate(year4 = year(DATE))

arb.spring<- arb.precip|>
  filter(NAME == 'UW ARBORETUM MADISON, WI US') |> 
  filter(year4 >= 1995) |> 
  filter(month(DATE) %in% c(3,4,5,6,7,8)) |> 
  group_by(year4, NAME) |>
  summarise(arb.precip = sum(PRCP, na.rm = T), num_na = sum(is.na(PRCP)))

summary_medians<- read_csv("data/summary_medians.csv")|>
  left_join(arb.spring, by = "year4")%>%
  mutate(removal = ifelse(removal == ">=2008", "≥2008", removal))


ggplot(summary_medians) +
  geom_smooth(aes(x = arb.precip, y = tp_median, col = removal), method = 'lm') +
  geom_point(aes(x = arb.precip, y = tp_median, fill = removal), shape = 21, size = 2.5)+
  scale_fill_manual(values = c( "white", "black"))+
  scale_color_manual(values = c( "black", "black"), guide = "none")+
  ylab(expression(paste("Total phosphorus", " (µg ", L^-1,")")))+
  xlab("Precipitation (mm)")+
  theme_bw(base_size = 14)

precip_tp<-ggplot(summary_medians) +
  geom_smooth(aes(x = sum_precip, y = tp_median, col = removal), method = 'lm') +
  geom_point(aes(x = sum_precip, y = tp_median, fill = removal), shape = 21, size = 2.5)+
  scale_fill_manual(values = c( "white", "black"))+
  scale_color_manual(values = c( "black", "black"), guide = "none")+
  ylab(expression(paste("Total phosphorus", " (µg ", L^-1,")")))+
  xlab("Precipitation (mm)")+
  theme_bw(base_size = 14)

precip_secchi<-ggplot(summary_medians) +
  geom_smooth(aes(x = sum_precip, y = secchi_median, col = removal), method = 'lm') +
  geom_point(aes(x = sum_precip, y = secchi_median, fill = removal), shape = 21, size = 2.5)+
  scale_fill_manual(values = c( "white", "black"))+
  scale_color_manual(values = c( "black", "black"), guide = "none")+
  ylab(expression(paste("Secchi (m)")))+
  xlab("Precipitation (mm)")+
  theme_bw(base_size = 14)

precip_chloro<-ggplot(summary_medians) +
  geom_smooth(aes(x = sum_precip, y = chloro_median, col = removal), method = 'lm') +
  geom_point(aes(x = sum_precip, y = chloro_median, fill = removal), shape = 21, size = 2.5)+
  scale_fill_manual(values = c( "white", "black"))+
  scale_color_manual(values = c( "black", "black"), guide = "none")+
  ylab(expression(paste("Chlorophyll a", " (µg ", L^-1,")")))+
  xlab("Precipitation (mm)")+
  theme_bw(base_size = 14)

precip_tn<-ggplot(summary_medians) +
  geom_smooth(aes(x = sum_precip, y = totnuf_median, col = removal), method = 'lm') +
  geom_point(aes(x = sum_precip, y = totnuf_median, fill = removal), shape = 21, size = 2.5)+
  scale_fill_manual(values = c( "white", "black"))+
  scale_color_manual(values = c( "black", "black"), guide = "none")+
  ylab(expression(paste("Total nitrogen", " (µg ", L^-1,")")))+
  xlab("Precipitation (mm)")+
  theme_bw(base_size = 14)

(precip_secchi + precip_tp) / (precip_tn + precip_chloro) + plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ")") + plot_layout(guides='collect') &theme(legend.position='bottom') 
ggsave("figures/precip_plots.png", width = 8, height = 7, units = 'in')


secchi_lm_pre <- lm(secchi_median ~ sum_precip, data = filter(summary_medians, group == "pre" ))
secchi_lm_post <- lm(secchi_median ~ sum_precip, data = filter(summary_medians, group == "post" ))
summary(secchi_lm_pre)
summary(secchi_lm_post)

tp_lm_pre <- lm(tp_median ~ sum_precip, data = filter(summary_medians, group == "pre" ))
tp_lm_post <- lm(tp_median ~ sum_precip, data = filter(summary_medians, group == "post" ))
summary(tp_lm_pre)
summary(tp_lm_post)

tn_lm_pre <- lm(totnuf_median ~ sum_precip, data = filter(summary_medians, group == "pre" ))
tn_lm_post <- lm(totnuf_median ~ sum_precip, data = filter(summary_medians, group == "post" ))
summary(tn_lm_pre)
summary(tn_lm_post)

chloro_lm_pre <- lm(chloro_median ~ sum_precip, data = filter(summary_medians, group == "pre" ))
chloro_lm_post <- lm(chloro_median ~ sum_precip, data = filter(summary_medians, group == "post" ))
summary(chloro_lm_pre)
summary(chloro_lm_post)


ggplot(summary_medians) +
  geom_smooth(aes(x = sum_precip, y = sum_fil_algae, col = removal), method = 'lm') +
  geom_point(aes(x = sum_precip, y = sum_fil_algae, fill = removal), shape = 21, size = 2)+
  scale_fill_manual(values = c( "white", "black"))+
  scale_color_manual(values = c( "black", "black"), guide = "none")+
  ylab(expression(paste("Total nitrogen", " (µg ", L^-1,")")))+
  xlab("Precipitation (mm)")+
  theme_bw()

ggplot(fil_algae_sum, aes(x = year4, y = sum, fill = removal))+
  geom_bar(stat = "identity", color = "black")+
  geom_vline(xintercept = 2008, linetype = "dashed")+
  xlab("")+
  scale_x_continuous(limits = c(1995,2023), breaks = seq(1995, 2023, 5))+
  ylab("Fil. algae \n (wet mass per rake throw)")+
  scale_fill_manual(values = c( "white", "black"))+
  theme_bw(base_size = 14)+
  theme(legend.position = "none", plot.caption = element_text(hjust = 0))

ggplot(summary_medians) +
  geom_point(aes(y = chloro_median, x = tp_median, fill = removal),shape = 21, size = 3)+
  scale_fill_manual(values = c( "white", "black"))+
  ylab(expression(paste("Chlorophyll a", " (µg ", L^-1,")")))+
  xlab(expression(paste("Phosphorus", " (µg ", L^-1,")")))+
  theme_bw()
  

chloro_tp_post <- lm(tp_median ~ chloro_median, data = filter(summary_medians, group == "post" ))
summary(chloro_tp_post)
ggplot(summary_medians) +
  geom_smooth(aes(y = chloro_median, x = tp_median, col = group), method = 'lm') +
  geom_point(aes(y = chloro_median, x = tp_median, col = group))
