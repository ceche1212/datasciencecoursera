library(dplyr)
library(data.table)
library(ggplot2)
#evita la notacion cientifica en los resultados scipen = 999 es apagado y prendido es 0
options(scipen = 999)
NEI <- readRDS("summarySCC_PM25.rds")
NEI[,"year_date"]=as.Date(as.character(NEI[,"year"]),"%Y")
#Subdivision of Baltimore City
Baltimore=subset(NEI,fips=="24510")
#subdivision of Los Angeles Country
LA = subset(NEI,fips=="06037")
SCC <- readRDS("Source_Classification_Code.rds")
Vehicles<-grepl("vehicle",SCC$SCC.Level.Two,ignore.case = TRUE)
VEH = SCC[Vehicles,]
BALT_VEHICLE = Baltimore[Baltimore$SCC %in% VEH$SCC,]
LA_VEHICLE = LA[LA$SCC %in% VEH$SCC,]
LA_VEHICLE[,"City"]= c("Los Angeles")
BALT_VEHICLE["City"]=c("Baltimore City")
both = rbind(LA_VEHICLE,BALT_VEHICLE)
plot6<-ggplot(both,aes(factor(year),Emissions))
plot6+geom_bar(stat = "identity")+theme_bw()+ facet_wrap(.~City)+labs(x="years",y=expression("Emissions of PM"[2.5]*" (tons)"))+labs(title = expression("Baltimore City and LA Year Vehicles emissions PM"[2.5]))
dev.copy(png,file="plot6.png")
dev.off()