setwd("/Users/adriannagorsky/Documents/wingra_map/")
setwd("/Users/adriannagorsky/Documents/wingra_carp/")

library(sf)
library(tidyverse)
library(raster)
library(patchwork)
library(lubridate)
library(cowplot)

lakes = st_read('yld_study_lakes.shp')
wingra <- lakes%>%
  filter(LAKEID == "WI")
wingra_bath = st_read('wingra-contours-all.shp')

## Sites
sites = read_csv('sites.csv') 
sites.sf = st_as_sf(sites, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")
deep.sf <- sites.sf%>%
  filter(Site == 1)

#Rake Years
pre_05<- read_csv('data/macrophyte_05_rake.csv')
pre_07<- read_csv('data/macrophyte_07.csv')
post_13<- read_csv('data/macrophyte_13.csv')
post_23<- read_csv('data/macrophyte_23.csv')

#Averages
post_13_avg<- post_13%>%
  group_by(rake2)%>%
  summarize(n = n())
#Averages
post_23_avg<- post_23%>%
  group_by(rake2)%>%
  summarize(n = n())

#Sf
pre_05.sf<- st_as_sf(pre_05, coords = c("longitude", "latitude"), crs = 4326)
pre_07.sf<- st_as_sf(pre_07, coords = c("longitude", "latitude"), crs = 4326)
post_13.sf<- st_as_sf(post_13, coords = c("longitude", "latitude"), crs = 4326)
post_23.sf<- st_as_sf(post_23, coords = c("longitude", "latitude"), crs = 4326)

pre_05$rake

#upload data
pre_05_plot<-ggplot() +
  geom_sf(data = wingra, color = 'grey99') +
  geom_sf(data = filter(pre_05.sf), aes(color = as.factor(rake), shape = as.factor(rake), fill = as.factor(rake), size = as.factor(rake)), stroke = 1.1) +
  geom_sf(data = deep.sf, color = 'black', size = 6, shape = 18)+
  scale_color_manual(values = c("lightgray",'#c2e699',"#78c679",  "#006837", "gray77"),labels = c("0", "1", "2", "3", "Not sampled"),  name = "", guide = guide_legend(title.position = "top")) +
  scale_fill_manual(values = c("lightgray",'#c2e699',"#78c679",  "#006837", "gray77"),labels = c("0", "1", "2", "3", "Not sampled"), name = "", guide = guide_legend(title.position = "top")) +
  scale_shape_manual(values = c(19, 19, 19, 19, 4), labels = c("0", "1", "2", "3", "Not sampled"), name = "", guide = guide_legend(title.position = "top")) +
  scale_size_manual(values = c(2,2,2,2, 2), labels = c("0", "1", "2", "3", "Not sampled"), name = "", guide = guide_legend(title.position = "top"))+
  scale_y_continuous(breaks = c(43.05, 43.06)) +
  scale_x_continuous(breaks = c(-89.43, -89.42, -89.41))+
  ggtitle(expression(italic('Total Rake Fullness 2005')))+
  guides(color = guide_legend(override.aes = list(size = 5), title.position = "top", nrow = 1, byrow = TRUE))+
  theme_bw(base_size = 11)+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = 'bottom',
    legend.title = element_text(size=12),
    legend.text = element_text(size=12),
    legend.direction="horizontal"
  )+
  ggspatial::annotation_scale( bar_cols = c("grey", "white"),  location = "br")

pre_07_plot<-ggplot() +
  geom_sf(data = wingra, color = 'grey99') +
  geom_sf(data = filter(pre_07.sf), aes(color = as.factor(rake), shape = as.factor(rake), fill = as.factor(rake), size = as.factor(rake)), stroke = 1.1) +
  geom_sf(data = deep.sf, color = 'black', size = 6, shape = 18)+
  scale_color_manual(values = c("lightgray",'#c2e699',"#78c679",  "#006837", "gray77"),labels = c("0", "1", "2", "3", "Not sampled"),  name = "", guide = guide_legend(title.position = "top")) +
  scale_fill_manual(values = c("lightgray",'#c2e699',"#78c679",  "#006837", "gray77"),labels = c("0", "1", "2", "3", "Not sampled"), name = "", guide = guide_legend(title.position = "top")) +
  scale_shape_manual(values = c(19, 19, 19, 19, 4), labels = c("0", "1", "2", "3", "Not sampled"), name = "", guide = guide_legend(title.position = "top")) +
  scale_size_manual(values = c(2,2,2,2, 2), labels = c("0", "1", "2", "3", "Not sampled"), name = "", guide = guide_legend(title.position = "top"))+
  scale_y_continuous(breaks = c(43.05, 43.06)) +
  scale_x_continuous(breaks = c(-89.43, -89.42, -89.41))+
  ggtitle(expression(italic('Total Rake Fullness 2007')))+
  guides(color = guide_legend(override.aes = list(size = 5), title.position = "top", nrow = 1, byrow = TRUE))+
  theme_bw(base_size = 11)+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = 'bottom',
    legend.title = element_text(size=12),
    legend.text = element_text(size=12),
    legend.direction="horizontal"
  )+
  ggspatial::annotation_scale( bar_cols = c("grey", "white"),  location = "br")

post_13_plot<-ggplot() +
  geom_sf(data = wingra, color = 'grey99') +
  geom_sf(data = filter(post_13.sf), aes(color = as.factor(rake), shape = as.factor(rake), fill = as.factor(rake), size = as.factor(rake)), stroke = 1.1) +
  geom_sf(data = deep.sf, color = 'black', size = 6, shape = 18)+
  scale_color_manual(values = c("lightgray",'#c2e699',"#78c679",  "#006837", "gray77"),labels = c("0", "1", "2", "3", "Not sampled"),  name = "", guide = guide_legend(title.position = "top")) +
  scale_fill_manual(values = c("lightgray",'#c2e699',"#78c679",  "#006837", "gray77"),labels = c("0", "1", "2", "3", "Not sampled"), name = "", guide = guide_legend(title.position = "top")) +
  scale_shape_manual(values = c(19, 19, 19, 19, 4), labels = c("0", "1", "2", "3", "Not sampled"), name = "", guide = guide_legend(title.position = "top")) +
  scale_size_manual(values = c(2,2,2,2, 2), labels = c("0", "1", "2", "3", "Not sampled"), name = "", guide = guide_legend(title.position = "top"))+
  scale_y_continuous(breaks = c(43.05, 43.06)) +
  scale_x_continuous(breaks = c(-89.43, -89.42, -89.41))+
  ggtitle(expression(italic('Total Rake Fullness 2013')))+
  guides(color = guide_legend(override.aes = list(size = 5), title.position = "top", nrow = 1, byrow = TRUE))+
  theme_bw(base_size = 11)+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = 'bottom',
    legend.title = element_text(size=12),
    legend.text = element_text(size=12),
    legend.direction="horizontal"
  )+
  ggspatial::annotation_scale( bar_cols = c("grey", "white"),  location = "br")

post_23_plot<-ggplot() +
  geom_sf(data = wingra, color = 'grey99') +
  geom_sf(data = filter(post_23.sf), aes(color = as.factor(rake), shape = as.factor(rake), fill = as.factor(rake), size = as.factor(rake)), stroke = 1.1) +
  geom_sf(data = deep.sf, color = 'black', size = 6, shape = 18)+
  scale_color_manual(values = c("lightgray",'#c2e699',"#78c679",  "#006837", "gray77"),labels = c("0", "1", "2", "3", "Not sampled"),  name = "", guide = guide_legend(title.position = "top")) +
  scale_fill_manual(values = c("lightgray",'#c2e699',"#78c679",  "#006837", "gray77"),labels = c("0", "1", "2", "3", "Not sampled"), name = "", guide = guide_legend(title.position = "top")) +
  scale_shape_manual(values = c(19, 19, 19, 19, 4), labels = c("0", "1", "2", "3", "Not sampled"), name = "", guide = guide_legend(title.position = "top")) +
  scale_size_manual(values = c(2,2,2,2, 2), labels = c("0", "1", "2", "3", "Not sampled"), name = "", guide = guide_legend(title.position = "top"))+
  scale_y_continuous(breaks = c(43.05, 43.06)) +
  scale_x_continuous(breaks = c(-89.43, -89.42, -89.41))+
  ggtitle(expression(italic('Total Rake Fullness 2023')))+
  guides(color = guide_legend(override.aes = list(size = 5), title.position = "top", nrow = 1, byrow = TRUE))+
  theme_bw(base_size = 11)+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = 'bottom',
    legend.title = element_text(size=12),
    legend.text = element_text(size=12),
    legend.direction="horizontal"
  )+
  ggspatial::annotation_scale( bar_cols = c("grey", "white"),  location = "br")

colon_plot +(pre_05_plot + pre_07_plot)/(post_13_plot + post_23_plot) + plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ")") + plot_layout(guides='collect') &
  theme(legend.position='bottom') 

colon_plot / (pre_05_plot + pre_07_plot)/(post_13_plot + post_23_plot) + plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ")") + plot_layout(guides='collect') &
  theme(legend.position='bottom') 

ggsave(filename = 'manuscript/macrophyte_colon_post_deepsite_2.png',width = 10,height = 12,units = 'in')


prow_pre <- plot_grid( pre_05_plot + theme(legend.position="none"),
                   pre_07_plot + theme(legend.position="none"),
                   align = 'vh',
                   labels = c("a)", "b)"),
                #   hjust = -1,
                   nrow = 1
)
prow_post <- plot_grid( 
                       post_13_plot + theme(legend.position="none"),
                       post_23_plot + theme(legend.position="none"),
                       align = 'vh',
                       labels = c("c)", "d)"),
                      # hjust = -1,
                       nrow = 1
)
p <- plot_grid(prow, mylegend, ncol = 1, rel_heights = c(1, .2))

ggsave(prow_post, filename = 'jake/macrophyte_map_post_deep.png',width = 10,height = 4,units = 'in')

