library(tidyverse)

downloadUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"

nei <- readRDS("exdata_data_NEI_data/summarySCC_PM25.rds")
ssc <- readRDS("exdata_data_NEI_data/Source_Classification_Code.rds")

