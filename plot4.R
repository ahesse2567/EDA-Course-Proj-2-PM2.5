library(tidyverse)

nei <- readRDS("exdata_data_NEI_data/summarySCC_PM25.rds")
scc <- readRDS("exdata_data_NEI_data/Source_Classification_Code.rds")


# we need to find all results that have something to do with coal

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