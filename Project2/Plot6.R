if(!(file.exists(a <-"summarySCC_PM25.rds") & file.exists(b <-"Source_Classification_Code.rds"))){
      URL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
      temp <- tempfile()
      download.file(URL,temp)
      NEI <- readRDS(unzip(temp,a))
      SCC <- readRDS(unzip(temp,b))
}else{
      NEI <- readRDS(a)
      SCC <- readRDS(b)
}
library(ggplot2);library(reshape2);library(gridExtra);library(dplyr)

NEICom <- merge(NEI,SCC,by.x="SCC")
MotVech <- NEICom[grepl("vehicle|motor",NEICom$EI.Sector,ignore.case = T) & (NEICom$fips=="24510"|NEICom$fips=="06037"),]

ToT_Sum <- melt(tapply(MotVech$Emissions,list(MotVech$year,MotVech$fips,as.character(MotVech$EI.Sector)),sum))
names(ToT_Sum) <- c("year","Flips","EI.Sector","Emissions")
ToT_Sum <- transform(ToT_Sum,Flips=as.factor(Flips))
levels(ToT_Sum$Flips) <- c("California","Baltimore")
TotSUM <- dcast(ToT_Sum,year~Flips,sum)
TotSUM <- mutate(TotSUM,California= California/sum(California),Baltimore= Baltimore/sum(Baltimore))

TotSUM <- melt(TotSUM,id.vars = "year")
names(TotSUM) <- c("year","Flips","Emissions")

my.colors = colorRampPalette(c("red", "blue"))
png(filename = "P2_Plot6.png",width = 1200, height = 480)

a <- ggplot(TotSUM,aes(year,Emissions))+
      geom_bar(stat="identity",aes(fill=factor(year)))+
      scale_x_continuous(breaks=TotSUM$year)+
      scale_fill_brewer(palette="Set1")+
      theme_bw()+
      xlab("year")+
      ggtitle(expression('Total PM'[2.5]*" Emission by City"))+
      geom_smooth(method="lm",se=F)+
      facet_grid(~Flips)+
      theme(legend.position="none",strip.text.x = element_text(size=9))

b <- ggplot(ToT_Sum,aes(x=year,y=Emissions))+
      geom_bar(stat="identity",aes(fill=as.factor(year)))+
      scale_x_continuous(breaks=ToT_Sum$year)+
      scale_fill_brewer(palette="Set1")+
      theme_bw()+
      xlab("year")+
      ggtitle(expression('Total PM'[2.5]*" Emission by Vehicle Type"))+
      facet_wrap(EI.Sector~Flips,scales = "free",ncol = 2)+
      geom_smooth(method="lm",se=F)+
      theme(legend.position="none",strip.text.x = element_text(size=9))

grid.arrange(a,b,ncol=2)
dev.off()
