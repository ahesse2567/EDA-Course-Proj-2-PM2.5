nei <- readRDS("exdata_data_NEI_data/summarySCC_PM25.rds")

baltimoreCity <- subset(nei, fips == "24510")
dim(baltimoreCity)

baltAnnualEmissions <- tapply(baltimoreCity$Emissions,
                              as.factor(baltimoreCity$year), FUN = sum)

barplot(baltAnnualEmissions, col = names(baltAnnualEmissions),
        main = expression("Total Annual PM"[2.5] ~ " Emissions in Baltimore City, Maryland"),
        xlab  = "Year", ylab = expression("PM"[2.5] ~ " Emissions in Tons"),
        ylim = c(0,3500))

balt <- data.frame(baltAnnualEmissions, names(baltAnnualEmissions))

abline(lm(baltAnnualEmissions ~ as.numeric(names.baltAnnualEmissions.), data  = balt))

dev.copy(png, file = "plot.2.png", width = 480, height = 480)
dev.off()
