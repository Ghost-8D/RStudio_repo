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

# Filter emissions using the scc codes from motor vehicles sources in Baltimore
motor_data <- subset(NEI, fips == "24510" & SCC %in% use_scc)

# Clean some memory
rm(NEI)
rm(SCC)

# Transform year and type into factors
motor_data <- transform(motor_data, year = factor(year))

# Convert to tibble 
motor_data <- as_tibble(motor_data)

# Create plot and save to png file
png(filename = "plot5.png", width = 480, height = 480)
#qplot(year, log10(Emissions), data=motor_data, geom="boxplot", 
#      ylab="PM2.5 Emissions in tons (in logarithmic scale)", xlab="Year", 
#      main="PM2.5 Emissions from motor vehicle sources in Baltimore")
g <- ggplot(motor_data, aes(x=year, y=log10(Emissions), fill=year)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white")+
  labs(title=expression("PM"[2.5]*" Emissions from motor vehicle sources in Baltimore"),
       x="Year", y=expression("PM"[2.5]*" Emissions in tons (in log scale)"))
g + scale_fill_brewer(palette="Dark2") + theme_minimal()
dev.off()

# Cleanup
rm(motor_data)
rm(use_scc)
file.remove(NEI_file)
file.remove(SCC_file)
rm(NEI_file)
rm(SCC_file)
detach("package:data.table", unload=TRUE)
detach("package:tidyr", unload=TRUE)
detach("package:ggplot2", unload=TRUE)
