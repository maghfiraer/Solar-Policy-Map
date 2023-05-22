#install.packages("usmap")
library(usmap) #import the package
library(ggplot2) #use ggplot2 to add layer for visualization
library(readxl)
library(ggpattern)

#Load data from excel
#Map_1
map_data_1 <- read_excel("data/Map_Data.xlsx", 
                         sheet = "Map 1", skip = 3)
names(map_data_1)<-c("state","net_metering_policy")
#View(map_data_1)

#Map_2
map_data_2 <- read_excel("data/Map_Data.xlsx", 
                       sheet = "Map 2", skip = 3)
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
  scale_fill_grey(name="", breaks=c('Transitioning to compensation other than net metering',
                               'State-mandated rules for certain utilities',
                               'State-mandated compensation other that net metering',
                               'No state-wide rules, but some utilities do offer net metering',
                               'No net metering or compensation'))+
  labs(title = "Net Metering Policy by State as of April 2022") + 
  theme(legend.position = "bottom", legend.key.size = unit(0.1, 'in'), #change legend key size
      legend.direction = "vertical",
      legend.key.height = unit(0.1, 'in'), #change legend key height
      legend.key.width = unit(0.1, 'in'), #change legend key width
      legend.title = element_text(size=8), #change legend title font size
      legend.text = element_text(size=8)) #change legend text font size

# Save plot 1
ggsave("figure/us_map_1.png", width = 10, units = "in",
       bg = "white")

# Generate plot 2
plot_usmap(data = map_data_2, values = "res_pv_capacity_MW", color = "black", regions = "states") + 
  scale_fill_continuous(low ="grey90", high = "grey39", name = "Capacity (MW)", label = scales::comma, na.value="white") +
  labs(title = "Estimated Aggregate Residential Solar PV Capacity (MW) as of Dec. 2022") + 
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(size=8), #change legend title font size
        legend.text = element_text(size=8)) #change legend text font size

# Save plot 2
ggsave("figure/us_map_2.png",height = 6, units = "in",
       bg = "white")

# Generate plot 3
plot_usmap(data = map_data_3, values = "res_pv_generation_MWh", color = "black", regions = "states") + 
  scale_fill_continuous(low = "grey90", high = "grey39", name = "Generation (MWh)", label = scales::comma, na.value="white") +
  labs(title = "Estimated Aggregate Residential Solar PV Generation (MWh) as of Dec. 2022") + 
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(size=8), #change legend title font size
        legend.text = element_text(size=8)) #change legend text font size

# Save plot 3
ggsave("figure/us_map_3.png", height = 6, units = "in",
       bg = "white")

# Generate plot 4
plot_usmap(data = map_data_4, values = "res_pv_generation_MWhppl", color = "black", regions = "states") + 
  scale_fill_continuous(low = "grey90", high = "grey39", name = "Generation (MWh) per 100,000 people", label = scales::comma, na.value="white") +
  labs(title = "Estimated Aggregate Residential Solar PV Generation (MWh) per 100,000 people as of Dec. 2022") + 
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(size=8), #change legend title font size
        legend.text = element_text(size=8)) #change legend text font size

# Save plot 4
ggsave("figure/us_map_4.png", height = 6, units = "in",
       bg = "white")
