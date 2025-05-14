library(tidyverse)
library(ggridges)

zoops_biomass<- read_csv('data/zooplankton.biomass_WI.csv')|>
  mutate(mg_m3 = ug_m3/1000)%>%
  mutate(length = ifelse(is.na(individuals_measured), 0, 1))

#Ice
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/33/39/933b6eb9b31cc1e41c6a02ee40a91877" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
ice<- read_csv(infile1)|>
  filter(lakeid == "WI")%>%
  dplyr::select(year, ice_off)

zoops_length<- zoops_biomass%>%
  filter(length == 1)%>%
  mutate(year = year(sample_date))%>%
  mutate(month = month(sample_date))%>%
  left_join(ice, by = "year")

zoops_CLADOCERA <- zoops_biomass%>%
  filter(larger_group == "CLADOCERA")

cladocera_length<- zoops_CLADOCERA%>%
  filter(length == 1)%>%
  mutate(year = year(sample_date))%>%
  mutate(month = month(sample_date))

ggplot(cladocera_length)+
  geom_point(aes(x = sample_date, y = avg_length))

cladocera_sum_summer<- zoops_CLADOCERA%>%
  filter(month(sample_date) > 6 & month(sample_date) < 10)%>%
  group_by(sample_date)%>%
  summarize(biomass = sum(mg_m3))

zoops_sum<- zoops_biomass%>%
  group_by(sample_date)%>%
  summarize(biomass = sum(mg_m3))


ggplot(cladocera_sum_summer)+
  geom_point(aes(x = sample_date, y = biomass, color = month(sample_date)))+
  scale_color_viridis_c()

cladocera_spring_month<- cladocera_length%>%
  filter(month > 3 & month < 7)

cladocera_spring<- cladocera_length%>%
  left_join(ice, by = "year")%>%
  filter(ice_off < sample_date)%>%
  filter(month < 7)#%>%
  #mutate(removal = ifelse(year < 200))

cladocera_spring_summary<- cladocera_length%>%
  filter(month > 3 & month < 7)%>%
  group_by(year)%>%
  summarize(length = mean(avg_length))

#April-June
zoop_density_plot<-ggplot(data = filter(cladocera_length, month > 3 & month < 7), aes(x = avg_length, y = as.factor(year)))+ 
  stat_density_ridges(quantile_lines = TRUE, quantiles = 2)+
  xlab("Average length (mm)")+
  ylab("")+
  theme_bw()

ggplot(data = filter(cladocera_length, (month > 3 & month < 7) & species_name == ""), aes(x = avg_length, y = as.factor(year)))+ 
  stat_density_ridges(quantile_lines = TRUE, quantiles = 2)+
  xlab("Average length (mm)")+
  ylab("")+
  theme_bw()

ggplot(data = cladocera_spring, aes(x = avg_length, y = as.factor(year)))+ 
  stat_density_ridges(quantile_lines = TRUE, quantiles = 2)+
  xlab("Average length (mm)")+
  ylab("")+
  ggtitle("Cladocera: April-June")+
  theme_bw()

ggplot(cladocera_spring, aes(x = avg_length, fill = as.factor(year))) + 
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Density Plot of Zooplankton Lengths")

# Normality Test for each year
years <- unique(cladocera_spring$year)
for (year in years) {
  cat("Shapiro-Wilk test for year", year, "\n")
  print(shapiro.test(cladocera_spring$avg_length[cladocera_spring$year == year]))
}

# Levene's test for homogeneity of variances
#install.packages("car")
library(car)
leveneTest(avg_length ~ as.factor(year), data = cladocera_spring)

anova_result <- aov(avg_length ~ as.factor(year), data = cladocera_spring)
summary(anova_result)

ggplot(data = filter(cladocera_length, month > 3 & month < 7), aes(x = factor(year), y = avg_length))+
  geom_violin()+
  geom_point(aes(color = month(sample_date)), alpha = 0.5)+
  theme_bw()

ggsave("figures/manuscript/zoops_size.png", width = 6, height = 6, units = 'in')

ggplot(data = filter(cladocera_length, month > 6 & month < 10), aes(x = avg_length, y = as.factor(year)))+ 
  stat_density_ridges(quantile_lines = TRUE, quantiles = 2)+
  xlab("Average length (mm)")+
  ylab("")+
  theme_bw()


ggplot(data = daphnia_open, aes(x = avg_length, y = as.factor(year)))+ 
  stat_density_ridges(quantile_lines = TRUE, quantiles = 2)

ggplot(data = daphnia, aes(x = avg_length, y = as.factor(year)))+ 
  geom_density_ridges()

daphnia<- zoops_length%>%
  filter(grepl("daphnia", species_name))%>%
  mutate(doy = yday(sample_date))

daphnia_ice<- daphnia%>%
  left_join(ice, by = "year")

daphnia_open<- daphnia_ice%>%
  group_by(year)%>%
  filter(ice_off < sample_date)%>%
  filter(month < 7)%>%
  group_by(sample_date)%>%
  summarize(biomass = sum(mg_m3))%>%
  mutate(year = year(sample_date))

ggplot(data = filter(daphnia_open), aes(x = avg_length, y = as.factor(year)))+ 
  stat_density_ridges(quantile_lines = TRUE, quantiles = 2)

ggplot(daphnia_open)+
  geom_point(aes(x = yday(sample_date), y = mg_m3))+
  facet_wrap(~year)


daphnia_average_summer<- daphnia%>%
  filter(month > 3 | month < 10)%>%
  group_by(species_name, year)%>%
  summarize(avg_length = mean(avg_length),
            biomass = sum(mg_m3))

daphnia_average_season<- daphnia%>%
  mutate(season = ifelse(month < 4, "winter",
                ifelse(month > 9, "fall",
                       ifelse((month >3 & month < 7), "spring", "summer"))))%>%
  filter(season == "spring"| season == "summer")%>%
  group_by(season, species_name, year)%>%
  summarize(avg_length = mean(avg_length))
  
ggplot(daphnia_average_season, aes(x = year, y = avg_length, fill = season))+
  geom_bar(stat = 'identity',position = position_dodge())+
  facet_wrap(~species_name)+
  theme_bw()

ggplot(filter(daphnia, doy> 90 & doy < 215), aes(x= doy, y = avg_length, fill = species_name))+
  geom_bar(stat = 'identity', position = position_dodge())+
  facet_wrap(~year)
  
macrophyte_transect<- read_csv("data/ntl_macrophyte.csv")
fil_algae<- read_csv("data/macrophyte/fil_algae.csv")
macrophyte<- macrophyte_transect%>%
  filter(transect == 2 | transect == 5 | transect == 7 | transect == 9|transect == 11)%>%
  filter(lakeid == "WI")%>%
  rbind(fil_algae)
filamentous_algae_timeseries<- macrophyte_algae%>%
  group_by(year4)%>%
  summarize(sum = sum(algae))
use_crosswalk<- tibble(transect = c(11, 2, 5, 7, 9), direction = c("west", "north", "east", "southeast", "southwest"))
macrophyte_algae<- macrophyte%>%
  merge(use_crosswalk, by = "transect")%>%
  group_by(transect, year4, sampledate, depth, direction)%>%
  summarize(algae = mean(fil_algae_wt))
filamentous_algae_timeseries<- macrophyte_algae%>%
  group_by(year4)%>%
  summarize(sum = sum(algae))%>%
  rename(year = year4)

daphnia_biomass<- zoops_biomass%>%
  filter(grepl("daphnia", species_name))%>%
  mutate(month = month(sample_date))%>%
  mutate(year = year(sample_date))%>%
  filter(month > 6 & month < 10)%>%
  group_by(year)%>%
  summarize(avg_biomass = mean(mg_m3),
            sum_biomass = sum(mg_m3))%>%
  left_join(filamentous_algae_timeseries, by = "year")

zoops_biomass_algae<- zoops_biomass%>%
  mutate(month = month(sample_date))%>%
  mutate(year = year(sample_date))%>%
  filter(month > 6 & month < 10)%>%
  group_by(year)%>%
  summarize(avg_biomass = mean(mg_m3),
            sum_biomass = sum(mg_m3))%>%
  left_join(filamentous_algae_timeseries, by = "year")

ggplot(filter(zoops_biomass_algae, year > 2007))+
  geom_point(aes(x = sum, y = sum_biomass))

zoops_density_biomass<- zoops_biomass%>%
  mutate(species_name = ifelse(species_name %in% "aglaodiaptomus clavipes", "diaptomus spp", species_name))%>%
  mutate(species = if_else(str_detect(species_name, "daphnia"),"daphnia",species_name))%>%  
  group_by(sample_date, species)%>%
  summarize(biomass = sum(mg_m3), .groups = 'drop')
  
zoops_biomass_avg<- zoops_density_biomass%>%
  group_by(year(sample_date), species)%>%
  summarize(biomass = mean(biomass))

zoops_density_biomass_large<- zoops_biomass%>%
  mutate(species_name = ifelse(species_name %in% "aglaodiaptomus clavipes", "diaptomus spp", species_name))%>%
  mutate(species = if_else(str_detect(species_name, "daphnia"),"daphnia",species_name))%>%  
  group_by(sample_date, larger_group)%>%
  summarize(biomass = mean(mg_m3), .groups = 'drop')

ggplot(zoops_biomass_avg, aes(x = `year(sample_date)`, y = biomass, fill = species)) +
  geom_area(position = 'stack') +
  labs(title = 'Zooplankton Biomass Over Time',
       x = 'Date',
       y = 'Biomass') +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") 

zoops_biomass_group<- zoops_biomass%>%
  mutate(species_name = ifelse(species_name %in% "aglaodiaptomus clavipes", "diaptomus spp", species_name))%>%
  mutate(species = ifelse(species_name %in% c("acanthocyclops", "mesocyclops edax", "tropocyclops prasinus mexicanus", "chydorus"),
         "Cyclopoid",
         ifelse(species_name %in% c("diaptomus spp", "aglaodiaptomus clavipes", "diacyclops thomasi", "diaptomid"),
                "Calenoid",
                ifelse(species_name %in% c("daphnia pulicaria", "daphnia retrocurva", "daphnia mendotae", "daphnia parvula", "diaphanosoma birgei", "daphnia"),
                       "Daphnia",
                       ifelse(species_name %in% c("sinobosmina fryei", "copepod nauplii", "copepodites"),
                              "Small Cladocera",
                              "Other")))))

zoops_density_group<- zoops_biomass_group%>%
  group_by(sample_date, species_name)%>%
  summarize(biomass = mean(mg_m3), .groups = 'drop')%>%
  ungroup()%>%
  mutate(species = ifelse(species_name %in% c("acanthocyclops", "mesocyclops edax", "tropocyclops prasinus mexicanus",  "diacyclops thomasi","tropocyclops"),
                          "Cyclopoid",
                          ifelse(species_name %in% "copepod nauplii", 
                                 "Nauplii",
                                 ifelse(species_name %in% c("copepodites"), 
                                        "Copepoda",
                                 ifelse(species_name %in% c("diaptomus spp", "aglaodiaptomus clavipes",  "diaptomid"),
                                 "Calanoid",
                                 ifelse(species_name %in% c("daphnia pulicaria",  "daphnia retrocurva", "daphnia mendotae", "daphnia parvula",  "daphnia"),
                                        "Daphnia",
                                        ifelse(species_name %in% c("sinobosmina fryei",  "chydorus", "diaphanosoma birgei", "ceriodaphnia dubia"),
                                               "Small Cladocera",
                                               "Other")))))))%>%
  group_by(sample_date, species)%>%
  summarize(biomass = sum(biomass))

zoops_density_group$species <- factor(zoops_density_group$species, levels = c("Calanoid",  "Cyclopoid",  "Nauplii", "Copepoda", "Daphnia", "Small Cladocera"))

zoop_timeseries_plot<-ggplot(zoops_density_group, aes(x = sample_date, y = biomass, fill = species)) +
  geom_area(position = 'stack') +
  ylab(expression(paste("Zooplankton biomass", " (", µ,"g ", L^-1,")")))+
  xlab("")+
  scale_fill_manual(values = c( "#762a83","#af8dc3",  "#e7d4e8", "#f7f7f4", "#008837", "#a6dba0"))+
  guides(fill = guide_legend(
    label = c("Calanoid", "Cyclopoid", "Nauplii", "Copepoda", 
              expression(paste("Large Cladocera (", italic("Daphnia"), ")")), 
              "Small Cladocera")))+
  theme_bw(base_size = 14)+
  theme(legend.title= element_blank())
ggsave("figures/manuscript/fig4_daphnia.png", width = 6, height = 3, units = 'in')

zoops_timeseries_biomass<- zoops_density_group%>%
  group_by(sample_date)%>%
  summarize(biomass = sum(biomass))%>%
  left_join(chloro_all, by = c("sample_date" = "sampledate"))
ggplot(zoops_timeseries_biomass)+
  geom_point(aes(x = biomass, y = chl_use))+
  facet_wrap(~removal)
library(patchwork)
zoop_timeseries_plot +fil_zoop_plot+ plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ")") + 
  plot_layout(widths = c(2, 1))

ggsave("figures/manuscript/manuscript/fig4_fil_zoop.png", width = 10, height = 5, units = 'in')

chloro_all<- read_csv("data/wingra_chl_data_update.csv")|>
  mutate(removal = ifelse(year(sampledate) < 2008, "<2008", ">=2008"))
chloro_summer_sum <- chloro_all%>%
  mutate(month = month(sampledate))%>%
  mutate(removal = ifelse(sampledate < as.Date("2008-03-15"), "pre", "post"))%>%
  mutate(season = ifelse(month > 4 & month < 10, "summer",
                         ifelse(month > 10 & month < 12, "fall",
                                ifelse(month > 2 &  month < 5, "spring", "winter"))))%>%
  filter(season == "summer")%>%
  filter(sampledate != as.Date("2010-07-02"))%>%
  group_by(year4)%>%
  summarize(chl_median = median(chl_use, na.rm = TRUE),
            chl_mean = mean(chl_use, na.rm = TRUE))%>%
  mutate(removal = ifelse(year4 < 2008, "pre", "post"))%>%
  left_join(sum_precip, by = c("year4"))%>%
  left_join(daphnia_open, by = c("year4" = "year"))

cladocera_mean <- cladocera_length%>%
  filter(month > 3 & month < 7)%>%
  group_by(year)%>%
  summarize(length = mean(avg_length),
            biomass = sum(mg_m3))

ggplot(filter(chloro_summer_sum, year4 > 2008))+
  geom_point(aes(x = biomass, y = chl_mean))

ggplot(chloro_summer_sum)+
  geom_point(aes(x = year4, y = chl_mean))
