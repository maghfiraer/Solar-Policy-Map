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


map_data_1$net_metering_policy[
  map_data_1$net_metering_policy=='No net metering or compensation']<-
  'No net metering or other compensation policy'
map_data_1$net_metering_policy[
  map_data_1$net_metering_policy=='No state-wide rules, but some utilities do offer net metering']<-
  'No state policy, but some utilities offer net metering'
map_data_1$net_metering_policy[
  map_data_1$net_metering_policy=='State-mandated compensation other that net metering']<-
  'Alternative compensation policy'
map_data_1$net_metering_policy[
  map_data_1$net_metering_policy=='State-mandated rules for certain utilities']<-
  'Net metering required for all (or certain) utilities'
map_data_1$net_metering_policy[
  map_data_1$net_metering_policy=='Transitioning to compensation other than net metering']<-
  'Transitioning from net metering to alternative compensation policy'
map_data_1$net_metering_policy[map_data_1$abbr=='CT']<-'Alternative compensation policy'

map_data_1$net_metering_policy <- factor(map_data_1$net_metering_policy,                 # Relevel group factor
                                         levels = c('Net metering required for all (or certain) utilities', 
                                                    'No state policy, but some utilities offer net metering', 
                                                    'Transitioning from net metering to alternative compensation policy', 
                                                    'Alternative compensation policy', 
                                                    'No net metering or other compensation policy'))

map_data <- usmapdata::us_map(regions = "states")%>%
  dplyr::left_join(map_data_1,by="abbr")

names(map_data_1)<-c("state","net_metering_policy")

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
  # scale_pattern_manual(values=c("none","none","stripe","none","stripe"))+
  # scale_pattern_colour_manual(values=c("grey10","grey40","grey60","grey80","white"))+
  # scale_pattern_fill_manual(values=c("black","black","black","black","black"))+
  # scale_fill_manual(values=c("grey10","grey40","grey60","grey80","white"))+
  # scale_pattern_linetype_manual(values=c(0,0,0.1,0,0.1))+
  # scale_pattern_density_manual(values=c(0,0,0.2,0,0.2))+
  scale_pattern_manual(values=c("none","none","stripe","stripe","none"))+
  scale_pattern_colour_manual(values=c("grey80","grey40","white","grey60","grey10"))+
  scale_pattern_fill_manual(values=c("black","black","black","black","black"))+
  scale_fill_manual(values=c("grey80","grey40","white","grey60","grey10"))+
  scale_pattern_linetype_manual(values=c(0,0,0.1,0.1,0))+
  scale_pattern_density_manual(values=c(0,0,0.2,0.2,0))+
  theme(legend.position = "bottom", legend.key.size = unit(0.1, 'in'), #change legend key size
        legend.direction = "vertical",
        legend.key.height = unit(0.1, 'in'), #change legend key height
        legend.key.width = unit(0.1, 'in'), #change legend key width
        legend.title=element_blank(),
        legend.text = element_text(size=8))
# Save plot 1
ggsave("figure/us_map_policy_rev_1.png", height=1200, width=2000, units="px",
       bg = "white")
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
  scale_pattern_colour_manual(values=c("grey30","grey50","grey60","grey80","white"))+
  scale_pattern_fill_manual(values=c("black","black","black","black","black"))+
  scale_fill_manual(values=c("grey30","grey50","grey60","grey80","white"))+
  scale_pattern_linetype_manual(values=c(0,0,0.1,0,0.1))+
  scale_pattern_density_manual(values=c(0,0,0.2,0,0.2))+
  # scale_pattern_manual(values=c("none","none","stripe","stripe","none"))+
  # scale_pattern_colour_manual(values=c("grey80","grey40","white","grey60","grey10"))+
  # scale_pattern_fill_manual(values=c("black","black","black","black","black"))+
  # scale_fill_manual(values=c("grey80","grey40","white","grey60","grey10"))+
  # scale_pattern_linetype_manual(values=c(0,0,0.1,0.1,0))+
  # scale_pattern_density_manual(values=c(0,0,0.2,0.2,0))+
  theme(legend.position = "bottom", legend.key.size = unit(0.1, 'in'), #change legend key size
        legend.direction = "vertical",
        legend.key.height = unit(0.1, 'in'), #change legend key height
        legend.key.width = unit(0.1, 'in'), #change legend key width
        legend.title=element_blank(),
        legend.text = element_text(size=8))
# Save plot 1
ggsave("figure/us_map_policy_rev_2.png", height=1200, width=2000, units="px",
       bg = "white")