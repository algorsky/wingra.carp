library(tidyverse)

macrophyte<- read_csv("data/dnr_macrophyte_sum.csv")|>
  mutate(presence_perc = (`Total number of sites with vegetation`/`Total number of  points sampled`)*100)
occurence<- read_csv("data/frequency_occurrence.csv")

richness<- macrophyte%>%
  dplyr::select(`Species Richness`, Year)%>%
  mutate(removal = ifelse(Year < 2008, "pre", "post"))

avg_richness<- richness%>%
  group_by(removal)%>%
  summarize(mean = mean(`Species Richness`))
colon_plot<-ggplot(macrophyte)+
  geom_point(aes(x = Year, y = `Maximum depth of plants (ft)`/3.281), size = 4)+
  geom_line(aes(x = Year, y = `Maximum depth of plants (ft)`/3.281))+
  geom_vline(xintercept = 2007.9, linetype = "dashed")+
  ylab("Max colonization depth (m)")+
  xlab("")+
  theme_bw(base_size = 14)

colon_plot<-ggplot(macrophyte)+
  geom_vline(xintercept = 2007.9, linetype = "dashed")+
  geom_line(aes(x = Year, y = `Maximum depth of plants (ft)`/3.281))+
  geom_point(aes(x = Year, y = `Maximum depth of plants (ft)`/3.281), size = 4, shape =21, fill = "white", stroke = 1.5)+
  ylab("Max colonization depth (m)")+
  xlab("")+
  theme_bw(base_size = 16)
occurence_plot<-ggplot()+
  geom_point(data = occurence, aes(x = Year, y = Percent, shape = Species), size = 4)+
  geom_line(data = occurence, aes(x = Year, y = Percent, group = Species, linetype = Species))+
  ylab("Frequency of occurence (%)")+
  geom_vline(xintercept = 2007.9, linetype = "dashed")+
  scale_shape_manual(values = c(17, 19))+
  xlab("")+
  ylim(c(0, 100))+
  theme_bw(base_size = 16)+
  theme(legend.position="bottom", legend.title=element_blank())
colon_plot/occurence_plot
ggsave("figures/defense/colonization.png", width = 6, height = 7, units = 'in')


colon_plot + eurasian_plot+ plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ")") 
ggsave("figures/macrophyte/coontail_eurasian.png", width = 6, height = 4, units = 'in')

ggsave("figures/defense/colonization.png", width = 6, height = 4, units = 'in')


ggplot(macrophyte)+
  geom_point(aes(x = Year, y = `Simpson Diversity Index`), size = 3)+
  geom_line(aes(x = Year, y = `Simpson Diversity Index`))+
  geom_vline(xintercept = 2007.9, linetype = "dashed")+
  xlab("")+
  theme_bw(base_size = 16)
ggsave("figures/macrophyte/SDI.png", width = 8, height = 6.5, units = 'in')

ggplot()+
  geom_point(data = macrophyte, aes(x = Year, y = `Species Richness`), size = 3)+
  geom_line(data = macrophyte, aes(x = Year, y = `Species Richness`))+
  geom_point(data = occurence, aes(x = Year, y = Percent/2), size = 3, color = "red3", shape = 17)+
  geom_line(data = occurence, aes(x = Year, y = Percent/2), color = "red3")+
  geom_vline(xintercept = 2007.9, linetype = "dashed")+
  scale_y_continuous(
    
    # Features of the first axis
    name = "# of species",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis( trans=~.*2, name="% Littoral points with\n eurasian water-milfoil"),
    limits = c(15, 37.75)
  ) +
  xlab("")+
  theme_bw(base_size = 16)+
  theme(axis.title.y.right = element_text(color = "red3"),
        axis.text.y.right = element_text(color = "red3"))
ggplot()+
  geom_point(data = macrophyte, aes(x = Year, y = `Species Richness`), size = 3)+
  geom_line(data = macrophyte, aes(x = Year, y = `Species Richness`))+
 # geom_point(data = occurence, aes(x = Year, y = Percent/2), size = 3, color = "red3", shape = 17)+
 # geom_line(data = occurence, aes(x = Year, y = Percent/2), color = "red3")+
  geom_vline(xintercept = 2007.9, linetype = "dashed")+
  scale_y_continuous(
    
    # Features of the first axis
    name = "# of species",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis( trans=~.*2, name="% Littoral points with\n eurasian water-milfoil"),
    limits = c(15, 37.75)
  ) +
  xlab("")+
  theme_bw(base_size = 16)+
  theme(axis.title.y.right = element_text(color = "white"),
        axis.text.y.right = element_text(color = "white"))

ggplot()+
  geom_point(data = macrophyte, aes(x = Year, y = `Species Richness`), size = 3)+
  geom_line(data = macrophyte, aes(x = Year, y = `Species Richness`))+
  geom_vline(xintercept = 2007.9, linetype = "dashed")+
  scale_y_continuous(limits = c(15, 37.75)) +
  ylab("# of species")+
  xlab("")+
  theme_bw(base_size = 16)

ggsave("figures/macrophyte/sameaxisspeciesnumber.png", width = 6, height = 4, units = 'in')

  
  ggplot(occurence)+
    geom_point(aes(x = Year, y = Percent), size = 3)+
    geom_line(aes(x = Year, y = Percent))+
    geom_vline(xintercept = 2007.9, linetype = "dashed")+
    ylab("% Littoral Points with Eurasian water-milfoil")+
    theme_bw(base_size = 16)
  ggsave("figures/macrophyte/eurasian.png", width = 8, height = 6, units = 'in')

ggplot(macrophyte)+
  geom_point(aes(x = Year, y = `Average number of native species per site`), size = 3)+
  geom_vline(xintercept = 2007.9, linetype = "dashed")+
  xlab("")+
  theme_bw(base_size = 16)

ggplot(macrophyte)+
  geom_point(aes(x = Year, y = `Average number of native species per site (veg. sites only)`), size = 3)+
  geom_line(aes(x = Year, y = `Average number of native species per site (veg. sites only)`))+
  geom_vline(xintercept = 2007.9, linetype = "dashed")+
  ylab("Average number of native species per site\n (veg. sites only)")+
  xlab("")+
  theme_bw(base_size = 16)
ggsave("figures/macrophyte/native.png", width = 8, height = 6, units = 'in')
viewer<- read_csv('data/aquatic_viewer_dnr.csv')|>
  mutate(Date = as.Date(`Survey Date`, format = "%m/%d/%y"))%>%
  mutate(total = `Total Number of Species`)

ggplot(viewer)+
  geom_point(aes(x = Date, y = total), size = 3)+
  geom_line(aes(x = Date, y = total))+
  geom_vline(xintercept = as.Date("2008-03-01"), linetype = "dashed")+
  ylab("Total Number of Species")+
  xlab("")+
  theme_bw(base_size = 16)
ggsave("figures/macrophyte/total.png", width = 8, height = 6, units = 'in')
ggplot(viewer)+
  geom_point(aes(x = Date, y = `Average Number of Native Species per Site`), size = 3)+
  geom_vline(xintercept = as.Date("2008-03-01"), linetype = "dashed")+
  xlab("")+
  theme_bw(base_size = 16)
ggplot(viewer)+
  geom_point(aes(x = Date, y = `Percent of Sites Shallow Enough for Plant Growth`), size = 3)+
  geom_vline(xintercept = as.Date("2008-03-01"), linetype = "dashed")+
  xlab("")+
  theme_bw(base_size = 16)



ggplot(occurence)+
  geom_point(aes(x = Year, y = Percent), size = 3)+
  geom_line(aes(x = Year, y = Percent))+
  geom_vline(xintercept = 2007.9, linetype = "dashed")+
  ylab("% Littoral Points with Eurasian water-milfoil")+
  theme_bw(base_size = 16)
ggsave("figures/macrophyte/eurasian.png", width = 8, height = 6, units = 'in')

