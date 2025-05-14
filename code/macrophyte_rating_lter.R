library(tidyverse)


inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/23/30/aa5720aec0e577431faeee352b91a937" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))

d1<- read_csv(infile1)|>
  filter(lakeid == "WI")

crosswalk<- read_csv("data/macrophyte/crosswork_macrophyte.csv")

df<- d1%>%
  filter(spname != "UNKNOWN"  & spname != "UNKNOWN 1")%>%
  filter(spname != "FILAMENTOUS ALGAE")%>%
  filter(spname != "FILAMENTOUS ALGEA")%>%
  filter(spname != "LEMNA MINOR")%>%
  mutate(spname = ifelse(spname == "POTAMOGETON NATANS:", "POTAMOGETON NATANS", spname))%>%
  mutate(spname = ifelse(spname == "NYMPHAEA ODORATA SSP. TUBEROSA", "NYMPHAEA ODORATA",
                         ifelse(spname == "NUPHAR VARIEGATA", "NYMPHAEA ODORATA", spname)))%>%
  select(-flag)

df_crosswalk<- df%>%
  left_join(crosswalk, by = "spname")%>%
  mutate(spname = ifelse(spname == "STRUCKENIA PECTINATA", "STUCKENIA PECTINATA", spname))%>%
  mutate(name = ifelse(spname == "STUCKENIA PECTINATA", "Potamogeton pectinatus L.", name))%>%
  mutate(label = ifelse(name %in% NA, "other", name))

# Group by year and species, then count occurrences
species_counts_by_year <- df_crosswalk %>%
  group_by(year4, spname) %>%
  summarize(Count = n(), .groups = 'drop')

# Calculate the total number of occurrences per year
total_occurrences_by_year <- species_counts_by_year %>%
  group_by(year4) %>%
  summarize(Total_Count = sum(Count), .groups = 'drop')

# Join the total occurrences back to the species counts
species_counts_with_totals <- species_counts_by_year %>%
  left_join(total_occurrences_by_year, by = "year4")

# Calculate relative frequencies
species_counts_with_frequencies <- species_counts_with_totals %>%
  mutate(Relative_Frequency = Count / Total_Count)

#decending order
species_counts_with_frequencies <- species_counts_with_frequencies %>%
  arrange(year4, desc(Relative_Frequency))%>%
  mutate(Relative_Frequency_perc =Relative_Frequency*100 )

eurasian <- species_counts_with_frequencies%>%
  filter(spname == "MYRIOPHYLLUM SPICATUM")

ggplot(filter(species_counts_with_frequencies, spname == "MYRIOPHYLLUM SPICATUM"))+
  geom_point(aes(x = year4, y = Relative_Frequency*100))+
  geom_line(aes(x = year4, y = Relative_Frequency*100))+
  geom_vline(xintercept = 2008, linetype = "dashed")+
  xlab("")+
  ylab("Relative Frequency (%)")+
  theme_bw()

#Depth below 2
# Group by year and species, then count occurrences
species_counts_by_year <- df_crosswalk %>%
  filter(depth < 2.5)%>%
  group_by(year4, label) %>%
  summarize(Count = n(), .groups = 'drop')

# Calculate the total number of occurrences per year
total_occurrences_by_year <- species_counts_by_year %>%
  group_by(year4) %>%
  summarize(Total_Count = sum(Count), .groups = 'drop')

# Join the total occurrences back to the species counts
species_counts_with_totals <- species_counts_by_year %>%
  left_join(total_occurrences_by_year, by = "year4")

# Calculate relative frequencies
species_counts_with_frequencies <- species_counts_with_totals %>%
  mutate(Relative_Frequency = Count / Total_Count)

#decending order
species_counts_with_frequencies <- species_counts_with_frequencies %>%
  arrange(year4, desc(Relative_Frequency))%>%
  mutate(Relative_Frequency_perc = Relative_Frequency*100)

ggplot(filter(species_counts_with_frequencies, label == 'Myriophfllum spicatum L.'))+
  geom_point(aes(x = year4, y = Relative_Frequency * 100))+
  geom_line(aes(x = year4, y = Relative_Frequency * 100))+
  geom_vline(xintercept = 2008, linetype = "dashed")+
  xlab("")+
  ylab("Relative Frequency (%)")+
  theme_bw()

ggplot(filter(species_counts_with_frequencies, label == 'Myriophfllum spicatum L.'))+
  geom_point(aes(x = year4, y = Relative_Frequency * 100))+
  geom_line(aes(x = year4, y = Relative_Frequency * 100))+
  geom_vline(xintercept = 2008, linetype = "dashed")+
  xlab("")+
  ylab("Relative Frequency (%)")+
  theme_bw()

ggplot(filter(species_counts_with_frequencies, label == 'Elodea canadensis Michx.'))+
  geom_point(aes(x = year4, y = Relative_Frequency * 100))+
  geom_line(aes(x = year4, y = Relative_Frequency * 100))+
  geom_vline(xintercept = 2008, linetype = "dashed")+
  xlab("")+
  ylab("Relative Frequency (%)")+
  theme_bw()

table_frequ<- species_counts_with_frequencies %>% 
  select(year4, label, Relative_Frequency_perc)%>%
  pivot_wider(names_from = year4, values_from = Relative_Frequency_perc)



#combine Potamogeton praelongus and richardsonii
#inclusive of 2m and below


