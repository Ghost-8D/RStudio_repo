# Load libraries
library(data.table)
library(tidyr)
library(ggplot2)

# Download the dataset from the url if it does not exist
NEI_file <- "summarySCC_PM25.rds"
SCC_file <- "Source_Classification_Code.rds"
if (!file.exists(NEI_file) | !file.exists(SCC_file)){
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
SCC <- readRDS(SCC_file)

# Convert to tibble objects
NEI <- as_tibble(NEI)
SCC <- as_tibble(SCC)

# Use SCC file to get the scc codes for sectors that are coal related
use_scc <- subset(SCC, grepl("Coal", EI.Sector, fixed = TRUE))$SCC

# Filter emissions data using the scc codes that are coal related
coal_data <- subset(NEI, SCC %in% use_scc)

# Clean some memory
rm(NEI)
rm(SCC)

# Transform year and type into factors
coal_data <- transform(coal_data, year = factor(year))

# Convert to tibble 
coal_data <- as_tibble(coal_data)

# Create violin plot
png(filename = "plot4.png", width = 480, height = 480)
#qplot(year, log10(Emissions), data=coal_data, geom="boxplot", 
#      ylab="PM2.5 Emissions in tons (in logarithmic scale)", xlab="Year", 
#      main="Total PM2.5 Emissions from coal combustion-related sources")
g <- ggplot(coal_data, aes(x=year, y=log10(Emissions), fill=year)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white")+
  labs(title="Total PM2.5 Emissions from coal combustion-related sources",
       x="Year", y = "PM2.5 Emissions in tons (in logarithmic scale)")
g + theme_classic()
dev.off()

# Cleanup
rm(coal_data)
rm(use_scc)
file.remove(NEI_file)
file.remove(SCC_file)
rm(NEI_file)
rm(SCC_file)
detach("package:data.table", unload=TRUE)
detach("package:tidyr", unload=TRUE)
detach("package:ggplot2", unload=TRUE)
