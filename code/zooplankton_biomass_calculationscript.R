# LTER Zooplankton Length -> Biomass # 

# Libraries 
library(tidyverse)
library(lubridate)
library(here)
remotes::install_github("bmcafee/EDIutilsAddons", force = TRUE)
library(EDIutilsAddons)


# Load in datasets #=======================
zoops = get_data('knb-lter-ntl.90.33')
zoops

conversion = read_csv('data/conversion_alpha-beta.csv')
conversion


zoops_WI<- zoops%>%
  filter(lakeid == "WI")

zoops_WI_cladocera <- zoops %>%
  filter(species_name == "ceriodaphnia dubia")%>%
  filter(avg_length > 0.000001)

ggplot(data = filter(zoops_WI_cladocera, month(sample_date) > 3 & month(sample_date) < 7), aes(x = avg_length, y = as.factor(year(sample_date))))+ 
  stat_density_ridges(quantile_lines = TRUE, quantiles = 2)+
  xlab("Average length (mm)")+
  ylab("")+
  theme_bw()

# Join datasets #========================
zoops$species_name = tolower(zoops$species_name)
zoops

conversion$species = tolower(conversion$species)
conversion

conversion_join = conversion %>% 
dplyr::	select(larger_group, species,  length,  mass_formula, mass_formula_reference,alpha, alpha2, beta, modifier, exp, mod, base, tricho) %>% 
	rename(species_name = species, 
				 length_mm_ref = length)
conversion_join

# Separate by lake to make a little more feasible and trim down info # =====================

## Mendota ##============================
zoops_WI = zoops %>%
	filter(lakeid == 'WI') %>% 
	dplyr::select(!c(lakeid, year4, station, species_code)) 
	# Remove lakeid - R object is coded to specific lake 
	# Remove year4 - can use lubridate to extract that out from sample_date 
	# remove station as these are deep hole sites + it's reflected in tow depth 
	# remove species code as we have species name 
zoops_WI

zoops_WI_conv = left_join(zoops_WI, conversion_join, by = 'species_name')
zoops_WI_conv

### Lots of NAs for species that don't have an alpha or beta value ##================================= 
findmissing_WI = zoops_WI_conv %>% 
	filter(is.na(alpha))
findmissing_WI
missing.spp_WI = unique(findmissing_WI$species_name) # 3 missing species # 
missing.spp_WI #"leptodora kindti", "bythotrephes longimanus", "ceriodaphnia dubia" 

# Remove spinies, chaobs, and leptodora as there's better ways to sample for those (which exist in LTER data) 
zoops_WI_conv = filter(zoops_WI_conv, species_name != "bythotrephes longimanus " & species_name != 'chaoborus' & 
											 	species_name != 'leptodora kindti')
zoops_WI_conv
bythotrephes<- zoops_WI_conv%>%
  filter(species_name == "bythotrephes longimanus ")
### find species that don't have an average length listed from the conversions data frame ##======================= 
findmissing_WI.length = zoops_WI_conv %>% 
	filter(is.na(avg_length))
findmissing_WI.length	

missing.length_WI = unique(findmissing_WI.length$species_name) 
missing.length_WI # 5 observation # 

# copepod nauplii; diacyclops thomasi; mesocyclops edax; daphnia pulicaria; daphnia; diaptomid;
	# acanthocyclops; tropocyclops prasinus mexicanus; bythotrephes longimanus; diaphanosoma birgei; 
	# tropocyclops; copepodites

### Find the annual average for that species in the dataset ##===========================

	# daphnia mendotae
daphnia.mendotae_range = zoops_WI_conv %>% 
	filter(species_name == 'daphnia mendotae') %>%
	mutate(year4 = year(sample_date)) %>%
	group_by(year4) %>% 
	summarize(avg_length = mean(avg_length, na.rm = T)) %>% 
	ungroup()
daphnia.mendotae_range 
daphnia.mendotae_annum = daphnia.mendotae_range %>%
	summarize(avg_length = mean(avg_length, na.rm = T))
daphnia.mendotae_annum

  # daphnia parvula
daphnia.parvula_range = zoops_WI_conv %>% 
	filter(species_name == 'daphnia parvula') %>% 
		mutate(year4 = year(sample_date)) %>%
	group_by(year4) %>% 
	summarize(avg_length = mean(avg_length, na.rm = T)) %>% 
	ungroup()
daphnia.parvula_range
daphnia.parvula_annum = daphnia.parvula_range %>%
	summarize(avg_length = mean(avg_length, na.rm = T))
daphnia.parvula_annum

	# daphnia
daphnia_range = zoops_WI_conv %>% 
	filter(species_name == 'daphnia') %>%
	mutate(year4 = year(sample_date)) %>%
	group_by(year4) %>% 
	summarize(avg_length = mean(avg_length, na.rm = T)) %>% 
	ungroup()
daphnia_range
daphnia_annum = daphnia_range %>%
	summarize(avg_length = mean(avg_length, na.rm = T))
daphnia_annum

	# daphnia retrocurva
daphnia.retrocurva_range = zoops_WI_conv %>%
	filter(species_name == 'daphnia retrocurva') %>% 
	mutate(year4 = year(sample_date)) %>%
	group_by(year4) %>% 
	summarize(avg_length = mean(avg_length, na.rm = T)) %>% 
	ungroup()
daphnia.retrocurva_range
daphnia.retrocurva_annum = daphnia.retrocurva_range %>%
	summarize(avg_length = mean(avg_length, na.rm = T))
daphnia.retrocurva_annum

# daphnia pulicaria
daphnia.pulicaria_range = zoops_WI_conv %>% 
  filter(species_name == 'daphnia pulicaria') %>%
  mutate(year4 = year(sample_date)) %>%
  group_by(year4) %>% 
  summarize(avg_length = mean(avg_length, na.rm = T)) %>% 
  ungroup()
daphnia.pulicaria_range 
daphnia.pulicaria_annum = daphnia.pulicaria_range %>%
  summarize(avg_length = mean(avg_length, na.rm = T))
daphnia.pulicaria_annum

# ceriodaphnia dubia
daphnia.ceriodaphnia_range = zoops_WI_conv %>% 
  filter(species_name == 'ceriodaphnia dubia') %>%
  mutate(year4 = year(sample_date)) %>%
  group_by(year4) %>% 
  summarize(avg_length = mean(avg_length, na.rm = T)) %>% 
  ungroup()
daphnia.ceriodaphnia_range 
daphnia.ceriodaphnia_annum = daphnia.ceriodaphnia_range %>%
  summarize(avg_length = mean(avg_length, na.rm = T))
daphnia.ceriodaphnia_range

# "bythotrephes longimanus " 
bythotrephes.longimanus_range = zoops_WI_conv %>%
  filter(species_name == "bythotrephes longimanus" ) %>%
  mutate(year4 = year(sample_date)) %>%
  group_by(year4) %>% 
  summarize(avg_length = mean(avg_length, na.rm = T)) %>% 
  ungroup()
daphnia.ceriodaphnia_range 
daphnia.ceriodaphnia_annum = daphnia.ceriodaphnia_range %>%
  summarize(avg_length = mean(avg_length, na.rm = T))
daphnia.ceriodaphnia_range


### Fill in NA length with the global average mean for that species within the dataset ##=============================== 
unique(findmissing_WI.length$species_name)

# this replaces all NA forthe species specified in species_name # 
zoops_WI_conv$avg_length[is.na(zoops_WI_conv$avg_length & 
                                 zoops_WI_conv$species_name == 'daphnia pulicaria')] <- daphnia.pulicaria_annum$avg_length
zoops_WI_conv$avg_length[is.na(zoops_WI_conv$avg_length & 
                                 zoops_WI_conv$species_name == 'daphnia')] <- daphnia_annum$avg_length
zoops_WI_conv

### Now can calculate biomass in ug #=================================
# Equations follow a few different forms denoted by four grouping columns
		# exp = ln(alpha) + beta*ln(length) 
		# base = alpha*(length)^beta 
		# trich = alpha*(length)*((alpha2*length))^2
		# mod = (alpha*(length)^beta)*100

# take the avg_length (in mm) and quantify average biomass of that species, 
	# then multiply by density to get ug/m2, 
  # then divide by tow depth to get ug/m3 (LTER has some reservations about this step - they like hypsometrically pooled data better, I think it's fine)
  # then divide by 1000 to get ug/L 

zoops_WI_biomass = zoops_WI_conv %>% 
	mutate(ug = case_when(base == 'Y' ~ (avg_length^beta)*alpha, 
												exp == 'Y' ~ exp(alpha + beta*log(avg_length)),
												mod == 'Y' ~ ((avg_length^beta)*alpha)*modifier, 
												tricho == 'Y' ~ (alpha*avg_length*((alpha2*avg_length))^beta)*modifier
												))
zoops_WI_biomass

# Only missing species is bythotrephes which for the life of me I can't filter out # 
zoops_WI_biomass = filter(zoops_WI_biomass, species_name != "bythotrephes longimanus ")
findmiss.ug = zoops_WI_biomass %>%
	filter(is.na(ug))
unique(findmiss.ug$species_name)	# ITS STILL THERE (whatever)

### Create Mendota dataset with biomass density #=========================
zoops.WI_biomassdens = zoops_WI_biomass %>%
	mutate(ug_m2 = ug*density) %>%
	mutate(ug_m3 = ug_m2/towdepth) %>% # m2 to m3 
	mutate(ug_L = ug_m3/1000) %>% # m3 to L
	select(sample_date, species_name, larger_group, density, avg_length, individuals_measured, ug_m2, ug_m3, ug_L) %>%
	arrange(sample_date)
zoops.WI_biomassdens	

# Need to combine aglodiaptomus with diaptomus (decided to join later in the time series - eye roll, eye roll, eye roll) # 
diaptomus.select = zoops.ME_biomassdens %>% 
	filter(species_name == 'diaptomid' | species_name == 'aglaodiaptomus clavipes') %>% 
	group_by(sample_date, .add = T) %>% 
	summarise(density = sum(density), 
						avg_length = mean(avg_length, na.rm = T), 
						individuals_measured = sum(individuals_measured),
		        ug_m2 = sum(ug_m2), 
						ug_m3 = sum(ug_m3), 
						ug_L = sum(ug_L))
diaptomus.select # now prep to add to the larger dataset 

diaptomus.select$species_name <- 'diaptomus spp'
diaptomus.select$larger_group <- 'COPEPODA'
diaptomus.select = select(diaptomus.select, sample_date, species_name, larger_group, density, avg_length, individuals_measured, 
													ug_m2, ug_m3, ug_L)
diaptomus.select

zoop.ME_biomassdens_final = rbind(zoops.ME_biomassdens, diaptomus.select)
zoop.ME_biomassdens_final

setwd(here('derived data'))
# write_csv(zoops_ME_long, 'zooplankton.biomass_ME.csv')
# write_csv(zoops_ME_biomass, 'zooplankton.comm_ME.csv')

