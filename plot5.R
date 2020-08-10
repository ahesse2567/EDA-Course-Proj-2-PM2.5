library(tidyverse)

nei <- readRDS("exdata_data_NEI_data/summarySCC_PM25.rds")
scc <- readRDS("exdata_data_NEI_data/Source_Classification_Code.rds")

baltimoreCity <- subset(nei, fips == "24510")

grepResults <- function(x, pattern) {
    for(i in 1:ncol(x)) {
        if(i == 1) {
            resultsIdx <- list()
        }
        hits <- grep(pattern, tolower(scc[,i]))
        if(length(hits) > 0) {
            resultsIdx[[names(scc[i])]] <- hits
        }
    }
    resultsIdx
}

motor <- grepResults(scc, "motor")
vehicle <- grepResults(scc, "vehicle")
veh <- grepResults(scc, "veh")
onRoad <- grepResults(scc, "on-road")

summary(motor)
summary(vehicle)
summary(veh)
summary(onRoad)

View(scc[motor$Short.Name,]) # this includes solvents, probably not the best
View(scc[motor$SCC.Level.Three,]) # this includes solvents, probably not the best
View(scc[motor$SCC.Level.Four,]) # includes fuel combustion and off road equipment

View(scc[vehicle$Short.Name,]) # solvents, mining, and more
View(scc[vehicle$EI.Sector,]) # nothing that seems related to non-vehicle emissions
View(scc[vehicle$SCC.Level.Two,]) # many off-highway fuel sources, e.g. logging
View(scc[vehicle$SCC.Level.Three,]) # solvents
View(scc[vehicle$SCC.Level.Four,]) # nothing to do with actually driving a vehicle

View(scc[veh$Short.Name,]) # solvents, mining, and more
View(scc[veh$EI.Sector,]) # nothing that seems related to non-vehicle emissions
View(scc[veh$SCC.Level.Two,]) # many off-highway fuel sources, e.g. logging
View(scc[veh$SCC.Level.Three,]) # solvents
View(scc[veh$SCC.Level.Four,]) # nothing to do with actually driving a vehicle

View(scc[onRoad$EI.Sector,]) # on-road also included "non-road"

nonRoad <- grepResults(scc[onRoad$EI.Sector,], "non-road")

length(nonRoad$EI.Sector)

length(onRoad$EI.Sector)

length(veh$EI.Sector)

length(onRoad$EI.Sector)-length(nonRoad$EI.Sector)

# we want the EI sector for veh or vehicles

View(scc[onRoad$EI.Sector,])


vehicleSCC <- vehicle$EI.Sector

scc$SCC[vehicleSCC]


baltimoreCity$SCC <- as.factor(baltimoreCity$SCC)

vehicleEmissionsIdx <- which(baltimoreCity$SCC %in% scc$SCC[vehicleSCC])

vehicleEmissions <- baltimoreCity[vehicleEmissionsIdx,]

vehEmitSmry <- vehicleEmissions %>% 
    group_by(year) %>% 
    summarize(sum_emissions = sum(Emissions))

vehicleBarplot <- ggplot(vehEmitSmry, aes(x = as.factor(year), y = sum_emissions,
                                          fill = as.factor(year))) +
    geom_col() +
    labs(title = "Motor Vehicle Emissions in Baltymore City, Maryland",
         x = "Year", y = expression("Annual PM"[2.5] ~ " Emissions in Tons")) +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5))
vehicleBarplot

