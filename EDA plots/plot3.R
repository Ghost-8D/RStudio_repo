# Load libraries
library(data.table)
library(tidyr)
library(ggplot2)

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

# Get data only for Baltimore City, Maryland (fips == "24510")
NEI_BC <- subset(NEI, fips == "24510")

# Clean some memory
rm(NEI)

# Transform year and type into factors
NEI_BC <- transform(NEI_BC, year = factor(year))
NEI_BC <- transform(NEI_BC, type = factor(type))

# Convert to tibble 
NEI_BC <- as_tibble(NEI_BC)

# Create plot and save to png file
png(filename = "plot3.png", width = 480, height = 480)

#qplot(year, log10(Emissions), data=NEI_BC, geom="boxplot", facets=type~., 
#      ylab="PM2.5 Emissions in tons (in logarithmic scale)", xlab="Year", 
#      main="Total PM2.5 Emissions in Baltimore City per source type")

#g <- ggplot(NEI_BC, aes(x=year, y=log10(Emissions), fill=year)) + 
#  geom_violin(trim=FALSE)+
#  geom_boxplot(width=0.1, fill="white")+
#  labs(title="Total PM2.5 Emissions in Baltimore City per source type",
#       x="Year", y = "PM2.5 Emissions in tons (in logarithmic scale)")
#g + scale_fill_brewer(palette = "Set2") + theme_minimal() + facet_grid(type~.)

g <- ggplot(NEI_BC, aes(x=year, y=Emissions, fill=type)) + 
  geom_violin(trim=FALSE)+
  geom_bar(stat="identity")+
  labs(title=expression("Total PM"[2.5]*" Emissions in Baltimore City per source type"),
       x="Year", y = expression("PM"[2.5]*" Emissions (Tons)"))
g + scale_fill_brewer(palette = "Set2") + facet_grid(.~type) +guides(fill=F)
dev.off()

# Cleanup
rm(NEI_BC)
rm(g)
file.remove(NEI_file)
file.remove(SCC_file)
rm(NEI_file)
rm(SCC_file)
detach("package:data.table", unload=TRUE)
detach("package:tidyr", unload=TRUE)
detach("package:ggplot2", unload=TRUE)
