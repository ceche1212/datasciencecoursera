library(dplyr)
library(data.table)
library(ggplot2)
#evita la notacion cientifica en los resultados scipen = 999 es apagado y prendido es 0
options(scipen = 999)
NEI <- readRDS("summarySCC_PM25.rds")
NEI[,"year_date"]=as.Date(as.character(NEI[,"year"]),"%Y")
SCC <- readRDS("Source_Classification_Code.rds")
combustion<-grepl("comb",SCC$SCC.Level.One,ignore.case = TRUE)
carbon<-grepl("coal",SCC$SCC.Level.Four,ignore.case = TRUE)
COMB_CARBON=SCC[combustion & carbon,]
NEI_COMB_CAR=NEI[NEI$SCC %in% COMB_CARBON$SCC,]
plot4<-ggplot(NEI_COMB_CAR,aes(factor(year),Emissions/10^5))
plot4+geom_bar(stat = "identity")+theme_bw()+labs(x="years",y=expression("Emissions of PM"[2.5]*" (100k tons)"))+labs(title = expression("US Year Coal-Combustion emissions PM"[2.5]))
dev.copy(png,file="plot4.png")
dev.off()