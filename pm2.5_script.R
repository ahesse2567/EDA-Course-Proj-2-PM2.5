library(tidyverse)

downloadUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"

nei <- readRDS("exdata_data_NEI_data/summarySCC_PM25.rds")
scc <- readRDS("exdata_data_NEI_data/Source_Classification_Code.rds")

str(nei)
str(scc)

any(is.na(nei)) # no NA values at all

#############################################################################
# Plot 1

annualEmissions <- tapply(nei$Emissions, as.factor(nei$year), FUN = sum)

class(annualEmissions)

barplot(annualEmissions, col = names(annualEmissions),
        main = expression("Total Annual PM"[2.5] ~ " Emissions in the United States"),
        xlab  = "Year", ylab = expression("PM"[2.5] ~ " Emissions in Tons"))

dev.copy(png, file = "plot.1.png", width = 480, height = 480)
dev.off()


#############################################################################
# Plot 2

baltimoreCity <- subset(nei, fips == "24510")
dim(baltimoreCity)

baltAnnualEmissions <- tapply(baltimoreCity$Emissions,
                              as.factor(baltimoreCity$year), FUN = sum)

barplot(baltAnnualEmissions, col = names(baltAnnualEmissions),
        main = expression("Total Annual PM"[2.5] ~ " Emissions in Baltimore City, Maryland"),
        xlab  = "Year", ylab = expression("PM"[2.5] ~ " Emissions in Tons"),
        ylim = c(0,3500))

balt <- data.frame(baltAnnualEmissions, names(baltAnnualEmissions))
plot(balt$names.baltAnnualEmissions., balt$baltAnnualEmissions)

abline(lm(baltAnnualEmissions ~ as.numeric(names.baltAnnualEmissions.), data  = balt))

dev.copy(png, file = "plot.2.png", width = 480, height = 480)
dev.off()


#############################################################################
# Plot 3

pmType <- baltimoreCity %>% 
    group_by(type, year) %>% 
    summarize(sum_by_type = sum(Emissions))

lineGraph <- ggplot(pmType, aes(x = year, y = sum_by_type, color = type)) +
    geom_point(size = 2.5) +
    geom_smooth() +
    labs(title = "Total Annual PM"[2.5] ~ " Emissions in Baltimore City, Maryland",
         x = "Year", y = expression("PM"[2.5] ~ " Emissions in Tons"))
lineGraph

barGraph <- ggplot(pmType, aes(x = as.factor(year), y = sum_by_type, fill = type)) +
    geom_col() +
    facet_grid(. ~ type) +
    labs(title = "Total Annual PM"[2.5] ~ " Emissions in Baltimore City, Maryland",
         x = "Year", y = expression("Annual PM"[2.5] ~ " Emissions in Tons")) +
    theme(axis.text.x = element_text(angle = 90),
          legend.position = "none",
          plot.title = element_text(hjust = 0.5)) 
barGraph

dev.copy(png, file = "plot.3.png", width = 480, height = 480)
dev.off()


#############################################################################
# Plot 4

scc <- readRDS("exdata_data_NEI_data/Source_Classification_Code.rds")

str(scc)

# we need to find all results that have something to do with coal

for(i in 1:ncol(scc)) {
    if(i == 1) {
        results <- list()
    }
    hits <- length(grep("coal", tolower(scc[,i])))
    results <- c(results, hits)
    if(i == ncol(scc)) {
        names(results) <- names(scc)
    }
}
results

results[results > 0]


# get a list output

for(i in 1:ncol(scc)) {
    if(i == 1) {
        resultsIdx <- list()
    }
    hits <- grep("coal", tolower(scc[,i]))
    resultsIdx[[names(scc[i])]] <- hits
}
resultsIdx

scc$Short.Name[resultsIdx$Short.Name]

coal <- sapply(resultsIdx, length)
coal <- coal[coal > 0]
coal

scc$Short.Name[resultsIdx$Short.Name]
scc$EI.Sector[resultsIdx$EI.Sector] # none of the other items mention combustion specifically
scc$SCC.Level.Three[resultsIdx$SCC.Level.Three]
scc$SCC.Level.Four[resultsIdx$SCC.Level.Four]

coal <- scc[resultsIdx$EI.Sector,]
coal$SCC
class(coal$SCC)
nei$SCC <- as.factor(nei$SCC)

coalComb <- which(nei$SCC %in% coal$SCC)

neiCoal <- nei[coalComb,]

coalSummary <- neiCoal %>% 
    group_by(year) %>% 
    summarize(sum_emissions = sum(Emissions))


ggplot(coalSummary, aes(x = as.factor(year), y = sum_emissions,
                        fill = as.factor(year))) +
    geom_col() +
    labs(title = "Coal Combustion Emissions in the Unitd States",
         x = "Year", y = "Annual Emission in Tons") +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5))

dev.copy(png, file = "plot.4.png", width = 480, height = 480)
dev.off()


#############################################################################
# Plot 5

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

dev.copy(png, file = "plot.5.png", width = 480, height = 480)
dev.off()
