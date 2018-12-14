library(dplyr)
library(data.table)
library(ggplot2)
#evita la notacion cientifica en los resultados scipen = 999 es apagado y prendido es 0
options(scipen = 999)
NEI <- readRDS("summarySCC_PM25.rds")
NEI[,"year_date"]=as.Date(as.character(NEI[,"year"]),"%Y")
Baltimore=subset(NEI,fips=="24510")
SCC <- readRDS("Source_Classification_Code.rds")
Vehicles<-grepl("vehicle",SCC$SCC.Level.Two,ignore.case = TRUE)
VEH = SCC[Vehicles,]
BALT_VEHICLE = Baltimore[Baltimore$SCC %in% VEH$SCC,]
plot5<-ggplot(BALT_VEHICLE,aes(factor(year),Emissions))
plot5+geom_bar(stat = "identity")+theme_bw()+labs(x="years",y=expression("Emissions of PM"[2.5]*" (tons)"))+labs(title = expression("Baltimore City Year Vehicles emissions PM"[2.5]))
dev.copy(png,file="plot5.png")
dev.off()