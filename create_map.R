#install.packages("usmap")
library(usmap) #import the package
library(ggplot2) #use ggplot2 to add layer for visualization
library(readxl)
library(ggpattern)
library(tidyverse)
library(magick)

#Load data from excel
#Map_1
map_data_1 <- read_excel("data/Map_Data.xlsx", 
                         sheet = "Map 1", skip = 3)
names(map_data_1)<-c("abbr","net_metering_policy")

map_data <- usmapdata::us_map(regions = "states")%>%
  dplyr::left_join(map_data_1,by="abbr")

names(map_data_1)<-c("state","net_metering_policy")

#Map_2
map_data_2 <- read_excel("data/Map_Data.xlsx", 
                       sheet = "Map 2", skip = 3)
names(map_data_2)<-c("abbr","res_pv_capacity_MW")
map_data_2b <- usmapdata::us_map(regions = "states")%>%
  dplyr::left_join(map_data_2,by="abbr")
names(map_data_2)<-c("state","res_pv_capacity_MW")

#Map_3
map_data_3 <- read_excel("data/Map_Data.xlsx", 
                       sheet = "Map 3", col_types = c("text", 
                                                      "skip", "skip", "skip", "skip", "skip", 
                                                      "skip", "skip", "skip", "skip", "skip", 
                                                      "skip", "skip", "numeric"), skip = 3)
names(map_data_3)<-c("state","res_pv_generation_MWh")

#Map_4
map_data_4 <- read_excel("data/Map_Data.xlsx", 
              sheet = "Map 4", col_types = c("text", 
                                             "skip", "skip", "numeric"), skip = 3)
names(map_data_4)<-c("state","res_pv_generation_MWhppl")


# Generate plot 1
plot_usmap(data = map_data_1, values = "net_metering_policy", color = "black", regions = "states") +
  geom_polygon_pattern(
    data = map_data,
    aes(x,y, group=group, fill=net_metering_policy,
        pattern_fill=net_metering_policy,
        pattern_colour=net_metering_policy, 
        pattern=net_metering_policy,
        pattern_density=net_metering_policy,
        pattern_linetype=net_metering_policy),
    colour="black",
    pattern_spacing=0.02,
    pattern_size=0.05)+
  scale_pattern_manual(values=c("none","none","stripe","none","stripe"))+
  scale_pattern_colour_manual(values=c("grey10","grey40","grey60","grey80","white"))+
  scale_pattern_fill_manual(values=c("black","black","black","black","black"))+
  scale_fill_manual(values=c("grey10","grey40","grey60","grey80","white"))+
  scale_pattern_linetype_manual(values=c(0,0,0.1,0,0.1))+
  scale_pattern_density_manual(values=c(0,0,0.2,0,0.2))+
  theme(legend.position = "bottom", legend.key.size = unit(0.1, 'in'), #change legend key size
        legend.direction = "vertical",
        legend.key.height = unit(0.1, 'in'), #change legend key height
        legend.key.width = unit(0.1, 'in'), #change legend key width
        legend.title=element_blank(),
        legend.text = element_text(size=8))
# Save plot 1
ggsave("figure/us_map_policy.png", height=1200, width=2000, units="px",
       bg = "white")

# Generate plot 2
plot_usmap(data = map_data_2, values = "res_pv_capacity_MW", color = "black", regions = "states") + 
  scale_fill_continuous(low ="grey90", high = "black", name = "Capacity (MW)     ", label = scales::comma, na.value="white") +
  geom_polygon_pattern(
    data = map_data_2b %>%
      dplyr::filter(is.na(res_pv_capacity_MW)),
    aes(x, y, group = group),
    pattern="stripe",
    color = "black",
    linewidth = 0.05, 
    fill=NA,
    pattern_spacing = 0.02, 
    pattern_density = 0.05)+
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(size=8), #change legend title font size
        legend.text = element_text(size=6))
# Save plot 2
ggsave("figure/us_map_res_pv_cap_dec_2022.png", height=800, width=1600, units="px",
       bg = "white")

# Generate plot 3
plot_usmap(data = map_data_3, values = "res_pv_generation_MWh", color = "black", regions = "states") + 
  scale_fill_continuous(low = "grey90", high = "black", name = "Generation (MWh)  ", label = scales::comma, na.value="white") +
  geom_polygon_pattern(
    data = map_data_2b %>%
      dplyr::filter(is.na(res_pv_capacity_MW)),
    aes(x, y, group = group),
    pattern="stripe",
    color = "black",
    linewidth = 0.05, 
    fill=NA,
    pattern_spacing = 0.02, 
    pattern_density = 0.05)+
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(size=8), #change legend title font size
        legend.text = element_text(size=6))

# Save plot 3
ggsave("figure/us_map_res_pv_gen_dec_2022.png", height=800, width=1600, units="px",
       bg = "white")

# Generate plot 4
plot_usmap(data = map_data_4, values = "res_pv_generation_MWhppl", color = "black", regions = "states") + 
  scale_fill_continuous(low = "grey90", high = "black", name = "Generation (MWh) \nper 100,000 people", label = scales::comma, na.value="white") +
  geom_polygon_pattern(
    data = map_data_2b %>%
      dplyr::filter(is.na(res_pv_capacity_MW)),
    aes(x, y, group = group),
    pattern="stripe",
    color = "black",
    linewidth = 0.05, 
    fill=NA,
    pattern_spacing = 0.02, 
    pattern_density = 0.05)+             
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(size=8), #change legend title font size
        legend.text = element_text(size=6))

# Save plot 4
ggsave("figure/us_map_res_pv_gen_dens_dec_2022.png", height=800, width=1600, units="px",
       bg = "white")
