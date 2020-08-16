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

# Use SCC file to get the scc codes for motor vehicles sources
use_scc <- subset(SCC, grepl("Vehicle", EI.Sector, fixed = TRUE))$SCC

# Filter emissions using the scc codes from motor vehicles sources in Baltimore and Los Angeles
bc_data <- subset(NEI, SCC %in% use_scc & (fips == "24510" | fips == "06037"))
la_data <- subset(NEI, fips == "06037" & SCC %in% use_scc)

# Clean some memory
rm(NEI)
rm(SCC)

# Transform year and type into factors
bc_data <- transform(bc_data, year = factor(year))
la_data <- transform(la_data, year = factor(year))

# Convert to tibble 
bc_data <- as_tibble(bc_data)
la_data <- as_tibble(la_data)

bc_data$Location <- sapply(bc_data$fips, FUN=function(x){
  if (x=="24510"){"Baltimore City"}
  else{"Los Angeles"}}) 

# Create plot and save to png file
png(filename = "plot6.png", width = 480, height = 480)
g <- ggplot(bc_data, aes(x=year, y=log10(Emissions), fill=year)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white")+
  labs(title="PM2.5 Emissions from motor vehicle sources",
       x="Year", y = "PM2.5 Emissions in tons (in logarithmic scale)")
g + scale_fill_brewer(palette="Blues") + theme_minimal() + facet_grid(.~Location)
dev.off()

# Cleanup
rm(bc_data)
rm(la_data)
rm(use_scc)
file.remove(NEI_file)
file.remove(SCC_file)
rm(NEI_file)
rm(SCC_file)
detach("package:data.table", unload=TRUE)
detach("package:tidyr", unload=TRUE)
detach("package:ggplot2", unload=TRUE)
