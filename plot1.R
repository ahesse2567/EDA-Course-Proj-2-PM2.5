nei <- readRDS("exdata_data_NEI_data/summarySCC_PM25.rds")
ssc <- readRDS("exdata_data_NEI_data/Source_Classification_Code.rds")

str(nei)

any(is.na(nei)) # no NA values at all


annualEmissions <- tapply(nei$Emissions, as.factor(nei$year), FUN = sum)

class(annualEmissions)

barplot(annualEmissions, col = names(annualEmissions),
        main = expression("Total Annual PM"[2.5] ~ " Emissions in the United States"),
        xlab  = "Year", ylab = expression("PM"[2.5] ~ " Emissions in Tons"))

dev.copy(png, file = "plot.1", width = 480, height = 480)
dev.off()

