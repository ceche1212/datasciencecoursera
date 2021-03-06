library(dplyr)
library(data.table)
#evita la notacion cientifica en los resultados scipen = 999 es apagado y prendido es 0
options(scipen = 999)
NEI <- readRDS("summarySCC_PM25.rds")
NEI[,"year_date"]=as.Date(as.character(NEI[,"year"]),"%Y")
Baltimore=subset(NEI,fips=="24510")
suma_Baltimore=with(Baltimore,tapply(Baltimore$Emissions,Baltimore$year,sum,na.rm=TRUE))
barplot(suma_Baltimore/1000,xlab = "Years",ylab = "Emissions (Thousands of tons of P M2.5)",main = "Total Emissions over the years in Baltimore City",col="red")
dev.copy(png,file="plot2.png")
dev.off()