library(tidyverse)
library(lubridate)
library(segmented)
library(sf)
library(raster)
library(patchwork)

macrophyte<- read_csv("data/ntl_macrophyte.csv")
fil_algae<- read_csv("data/macrophyte/fil_algae.csv")
macrophyte_lter<- macrophyte%>%
  filter(transect == 2 | transect == 5 | transect == 7 | transect == 9|transect == 11)%>%
  filter(lakeid == "WI")%>%
  rbind(fil_algae)

macrophyte<- macrophyte%>%
  mutate(removal = ifelse(year4 < 2008, "pre", "post"))
macrophyte_abund<- read_csv("data/ntl_macrophyte_abund.csv")
macrophyte_algae_abund<- macrophyte_abund%>%
  filter(transect == 2 | transect == 5 | transect == 7 | transect == 9|transect == 11)%>%
  filter(spname == "FILAMENTOUS ALGEA" | spname == "FILAMENTOUS ALGAE")
  mutate(removal = ifelse(year4 < 2008, "pre", "post"))

ggplot(macrophyte_a)+
  geom_point(aes(x = sampledate, y = rake_rating))+
  facet_wrap(~spname, scales = "free")
density_rank<- macrophyte%>%
  group_by(sampledate, year4, depth, transect)%>%
  summarize(dr = mean(rake_rating),
            biomass = mean(plant_wt_hand))%>%
  mutate(removal = ifelse(year4 < 2008, "pre", "post"))%>%
  mutate(category = ifelse(depth < 1.6, "1-1.5 m", 
                           ifelse((depth > 1.5 & depth < 3), "2-2.5 m","3-4 m")))

ggplot(density_rank)+
  geom_point(aes(x = year4, y = (biomass), color = depth))+
  geom_smooth(aes(x = year4, y = (biomass)), method = 'gam')+
  geom_vline(xintercept = 2008, linetype = "dashed")+
  ylab("Macrophyte wet biomass (g)")+
  xlab("")+
  ylim(0, 4500)+
  scale_color_viridis_c()+
  facet_wrap(~category)+
  theme_bw(base_size = 16) + 
  theme(panel.border = element_rect(colour = "black", fill=NA), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
#ggsave("figures/macrophyte_biomass.png", width = 10, height = 6, units = 'in')
indicators<- read_csv("data/dnr_indicators.csv")
indicators<- indicators%>%
  mutate(macrophyte = as.factor(Macrophyte))

ggplot()+
  geom_point(data = dplyr::filter(indicators, Macrophyte == "MYRIOPHYLLUM SPICATUM"), aes(x = Year, y = rel_frequency), color = "darkgreen")+
  geom_line(data = dplyr::filter(indicators, Macrophyte == "MYRIOPHYLLUM SPICATUM"),aes(x = Year, y = rel_frequency), color = "darkgreen")+
  geom_point(data = dplyr::filter(indicators, Macrophyte == "CERATOPHYLLUM DEMERSUM"), aes(x = Year, y = rel_frequency))+
  geom_line(data = dplyr::filter(indicators, Macrophyte == "CERATOPHYLLUM DEMERSUM"),aes(x = Year, y = rel_frequency))+
  geom_vline(xintercept = 2007.8, linetype = "dashed")+
  ylim(0, 60)+
  theme_bw()

 low_macro<- indicators%>%
  filter(Macrophyte != "MYRIOPHYLLUM SPICATUM" )%>%
  filter(Macrophyte != "CERATOPHYLLUM DEMERSUM")%>%
  filter(Macrophyte != "FILAMENTOUS ALGAE")

ggplot(low_macro)+
  geom_point(aes(x = Year, y = freq_vegetated_areas))+
  facet_wrap(~Macrophyte)

ggplot(dplyr::filter(macrophyte, plant_wt_hand < 6000))+
  geom_point(aes(x = sampledate, y = rake_rating, color = plant_wt_hand))+
  geom_vline(xintercept = as.Date("2008-03-01"))+
  facet_wrap(~transect + depth, scales = "free_y")+
  scale_color_viridis_b()+
  theme_bw()
chara<- macrophyte_a%>%
  filter(spname == "CHARA SP")
chara<- macrophyte_a%>%
  filter(spname == "CERATOPHYLLUM DEMERSUM" |spname == "MYRIOPHYLLUM SPICATUM" | spname == "STUCKERIA PECTINATA"|
         spname == "POTAMOGETON ZOSTERIFORMIS" | spname == "CHARA SP")

POTAMOGETON<- macrophyte_a%>%
  filter(grepl("POTAMOGETON", spname))
  
ggplot(density_rank)+
  geom_point(aes(x = depth, y = dr, color = removal))+  
  facet_wrap(~spname)+
  theme_bw()


ggplot(POTAMOGETON)+
  geom_point(aes(x = year4, y = rake_rating, color = depth))+
  geom_line(aes(x = year4, y = rake_rating, group = transect, color = depth))+
  geom_vline(xintercept = 2008, linetype = "dashed")+
  scale_color_viridis_b()+
  theme_bw()

macrophyte_sum<- macrophyte_a%>%
  group_by(year4, transect, depth)%>%
  summarize(n = length(unique(spname)),
            rake_rating = mean(rake_rating))


use_crosswalk<- tibble(transect = c(11, 2, 5, 7, 9), direction = c("west", "north", "east", "southeast", "southwest"))
macrophyte_algae<- macrophyte%>%
  merge(use_crosswalk, by = "transect")%>%
  group_by(transect, year4, sampledate, depth, direction)%>%
  summarize(algae = mean(fil_algae_wt))

filamentous_algae_timeseries<- macrophyte%>%
  group_by(year4)%>%
  summarize(sum = sum(algae))

fil_algae_precip <- filamentous_algae_timeseries%>%
  left_join(sum_precip, by = "year4")%>%
  filter(year4> 1995 & year4 < 2023)%>%
  

ggplot(fil_algae_precip)+
  geom_point(aes(x = sum_precip, y = sum))

zoops<- read_csv('data/zooplankton.biomass_WI.csv')|>
  mutate(mg_m3 = ug_m3/1000)

zoops_biomass_summer<- zoops%>%
  mutate(month = month(sample_date))%>%
  mutate(year4 = year(sample_date))%>%
  filter(month > 5 & month < 9)%>%
  group_by(year4)%>%
  summarize(ug_L_biomass = sum(ug_L))

zoops_biomass_year<- zoops%>%
  mutate(month = month(sample_date))%>%
  mutate(year4 = year(sample_date))%>%
  group_by(sample_date)%>%
  summarize(ug_L_biomass = sum(ug_L))%>%
  mutate(year4 = year(sample_date))

zoops_year_algae_summer<- zoops_biomass_year %>%
  left_join(filamentous_algae_timeseries, by = "year4")%>%
  left_join(sum_precip, by = "year4")%>%
  mutate(month = month(sample_date))%>%
  filter(month > 5 & month < 9)%>%
  mutate(year4 = year(sample_date))

ggplot(filter(zoops_year_algae_summer, year4 > 2007), aes(y = sum, x = sum_precip))+
  geom_boxplot(aes(group = year4))

precip_fil_mod <- lm(sum_precip~sum, data = zoops_year_algae_summer)
summary(precip_fil_mod)
ggplot(filter(zoops_year_algae_summer, year4 > 2007), aes(y = sum, x = sum_precip))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_bw()
  

daphnia_biomass<- zoops%>%
  filter(grepl("daphnia", species_name))%>%
  mutate(month = month(sample_date))%>%
  mutate(year4 = year(sample_date))%>%
  filter(month > 4 & month < 10)%>%
  group_by(year4)%>%
  summarize(ug_L_biomass = sum(ug_L))

zoops_biomass_algae <- zoops_biomass_summer%>%
  left_join(filamentous_algae_timeseries, by = "year4")%>%
  left_join(sum_precip, by = "year4")
daphnia_biomass_algae <- daphnia_biomass%>%
  left_join(filamentous_algae_timeseries, by = "year4")
zoop_algae_lm<- lm(ug_L_biomass ~ sum, data = zoops_biomass_algae)
summary(zoop_algae_lm)
ggplot(filter(zoops_biomass_algae, year4 > 2007))+
  geom_point(aes(x = sum, y = ug_L_biomass), size = 3)+
  geom_smooth(aes(x = sum, y = ug_L_biomass), method = "lm", color = "black", se = FALSE)+
  xlab("Filamentous algae wet weight (g)")+
  ylab(expression(paste("Summer zooplankton biomass", " (", µ,"g ", L^-1,")")))+
  scale_x_continuous(breaks = seq(0, 2600, by = 500))+
  theme_bw(base_size = 16)
ggsave("figures/algae_zoop.png", width = 8, height = 6, units = 'in')

ggplot(zoops_biomass_algae)+
  geom_point(aes(x =ug_L_biomass , y = sum_precip))

ggplot(filter(filamentous_algae_timeseries), aes(x = year4, y = sum))+
  geom_col()+
  geom_vline(xintercept = 2007.6, linetype = "dashed")+
  ylab("Filamentous algae wet weight (g)")+
  xlab("")+
  theme_bw(base_size = 16)
  
ggplot(macrophyte_algae)+
    geom_jitter(aes(x = sampledate, y = fil_algae_wt))+
    geom_vline(xintercept = as.Date("2008-03-15"), linetype = "dashed")+
    ylab("Filamentous algae wet weight (g)")+
    xlab("")+
    scale_x_date(breaks = "4 years", date_labels = "%Y")+
    theme_bw(base_size = 16)
  ggsave("figures/filalgae_bar.png", width = 8, height = 6, units = 'in')
  

ggplot(macrophyte_algae)+
  geom_point(aes(x = sampledate, y = algae, color = depth), size = 3)+
  geom_vline(xintercept = as.Date("2008-03-01"), linetype = "dashed")+
  ylab("Filamentous algae wet weight (g)")+facet_wrap(~direction)+
  scale_color_viridis_b(name = "Depth (m)")+
  theme_bw(base_size = 16) + 
  theme(panel.border = element_rect(colour = "black", fill=NA), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
ggsave("figures/algae.png", width = 10, height = 6, units = 'in')

algae_sum<- macrophyte_algae%>%
  group_by(year4)%>%
  summarize(algae_sum = sum(algae))

ggplot(algae_sum)+
  geom_point(aes(x = year4, y = algae_sum))


#Maps
lakes = st_read('data/maps/yld_study_lakes.shp')
wingra <- lakes%>%
  filter(LAKEID == "WI")
wingra_bath = st_read('data/maps/wingra-contours-all.shp')

## transects
sites = read_csv('data/ntl_macrophyte_coord.csv') 
sites.sf = st_as_sf(sites, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")

ggplot() +
  geom_sf(data = wingra, color = 'grey90')+
  geom_sf(data = sites.sf, aes(color = as.factor(transect)))+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


