library(tidyverse)

nei <- readRDS("exdata_data_NEI_data/summarySCC_PM25.rds")

baltimoreCity <- subset(nei, fips == "24510")
dim(baltimoreCity)

pmType <- baltimoreCity %>% 
    group_by(type, year) %>% 
    summarize(sum_by_type = sum(Emissions))

barGraph <- ggplot(pmType, aes(x = as.factor(year), y = sum_by_type, fill = type)) +
    geom_col() +
    facet_grid(. ~ type) +
    labs(title = "Total Annual PM"[2.5] ~ " Emissions in Baltimore City, Maryland",
         x = "Year", y = expression("PM"[2.5] ~ " Emissions in Tons")) +
    theme(axis.text.x = element_text(angle = 90),
          legend.position = "none") 
barGraph

dev.copy(png, file = "plot.3.png", width = 480, height = 480)
dev.off()
