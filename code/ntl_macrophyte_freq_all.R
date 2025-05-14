library(tidyverse)

littoral_ntl<- read_csv("data/macrophyte/ntl_littoral_macrophyte.csv") |>
  mutate(across(where(is.numeric), ~ replace_na(.x, 0) * 100))

lake_all<- read_csv("data/macrophyte/littoral_wider_all.csv")%>%
  pivot_longer(cols = -species, names_to = "Year", values_to = "Relative_Abundance")%>%
  filter(species != "Total")
  
 
  # filter(species == "Ceratophyllum demersum" | species == "Myriophyllum spicatum")%>%
  #  mutate(Year = as.numeric(Year))%>%
  #rename(year = Year)

long_historic<- read_csv("data/macrophyte/historic_long.csv")|>
  rename(species = Species)

lake_all<- lake_all%>%
  rbind(long_historic)

# Transform the data
data_long <- littoral_ntl %>%
  pivot_longer(cols = -year, names_to = "Year", values_to = "Relative_Abundance")%>%
  rename(species = year)%>%
  filter(species != "Total")

data_category<- lake_all%>%
  mutate(Year = as.numeric(Year))%>%
  mutate(year = ifelse(Year < 1998, "1995-1997",
                       ifelse(Year > 1997 & Year < 2001, "1998-2000",
                              ifelse(Year > 2000 & Year < 2004, "2001-2003",
                                     ifelse(Year > 2003 & Year < 2008, "2004-2007",
                                            ifelse(Year > 2007 & Year < 2011, "2008-2010",
                                                   ifelse(Year > 2010 & Year < 2014, "2011-2013",
                                                          ifelse(Year > 2013 & Year < 2017, "2014-2016", "2017-2018"))))))))

data_category_summary<- data_category%>%
  group_by(year, species)%>%
  summarize(frequency = mean(Relative_Abundance))

category_filter<- data_category_summary%>%
  group_by(species) %>%
  mutate(Contains_Greater_Than_1 = any(frequency > 2.5),
         All_Other_Less_Than_1 = all(frequency <= 2.5)) %>%
  ungroup() %>%
  mutate(species = ifelse(Contains_Greater_Than_1 & All_Other_Less_Than_1, "Other", species))

category_filter_species<- category_filter%>%
  mutate(Species = ifelse(All_Other_Less_Than_1 == TRUE, "Other", species))%>%
  mutate(Species = ifelse(Species == "Potamogeton praelongus", "Potamogeton richardsonii", Species))%>%
  group_by(year, Species)%>%
  summarize(frequency = sum(frequency))
data_frequency<- data_long%>%
  mutate(year = as.numeric(Year))
ggplot()+
  geom_point(data = lake_all, aes(x = year, y = Relative_Abundance, shape = species), size = 3)+
  geom_line(data = lake_all, aes(x = year, y = Relative_Abundance, group = species, linetype = species))+
  ylab("Frequency of occurence (%)")+
  geom_vline(xintercept = 2007.9, linetype = "dashed")+
  scale_shape_manual(values = c(17, 19))+
  xlab("")+
  theme_bw(base_size = 16)+
  theme(legend.position="bottom", legend.title=element_blank())
  

littoral_wider_filter <- category_filter_species %>% 
  pivot_wider(names_from = year, values_from = frequency)

write_csv(littoral_wider_filter, "data/macrophyte/littoral_wider_all_filter2.5.csv")
