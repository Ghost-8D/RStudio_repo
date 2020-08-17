# Load libraries
library(data.table)
library(tidyr)

# Download the dataset from the url if it does not exist
NEI_file <- "summarySCC_PM25.rds"
SCC_file <- "Source_Classification_Code.rds"
if (!file.exists(NEI_file)){
    url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
    dest_file <- "NEI_data.zip"
    download.file(url, destfile = dest_file, method = "curl")
    unzip(dest_file)
    file.remove(dest_file)
    rm(url)
    rm(dest_file)
}

# Read data into R memory
NEI <- readRDS(NEI_file)

# Convert to tibble objects
NEI <- as_tibble(NEI)

# Transform year into factor
NEI <- transform(NEI, year = factor(year))

# Create plot as png file
png(filename = "plot1.png", width = 480, height = 480)
par(mfrow=c(1,1), bg="white")
#boxplot(log10(Emissions) ~ year, NEI, xlab = "Year", 
#        ylab = expression("PM"[2.5]*" Emissions in tons (in log scale)"))
#title(main=expression("Total PM"[2.5]*" Emissions per year"))
barplot(tapply(NEI$Emissions, NEI$year, sum)/1000, xlab="Years", 
        ylab=expression("PM"[2.5]*" Emissions (Kilo-Tons)"), 
        main=expression("Total PM"[2.5]*" Emissions per year"))
dev.off()

# Cleanup
file.remove(NEI_file)
file.remove(SCC_file)
rm(NEI_file)
rm(SCC_file)
rm(NEI)
detach("package:data.table", unload=TRUE)
detach("package:tidyr", unload=TRUE)
