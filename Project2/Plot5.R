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
library(ggplot2);library(reshape2);library(gridExtra)

NEICom <- merge(NEI,SCC,by.x="SCC")

MotVech <- NEICom[grepl("vehicle|motor",NEICom$EI.Sector,ignore.case = T) & NEICom$fips=="24510",]

VechSummary <- tapply(MotVech$Emissions,list(MotVech$year,as.character(MotVech$EI.Sector)),sum,na.rm=TRUE)

VechSummary <- melt(VechSummary)
names(VechSummary) <- c("year","EI.Source","Emissions")

png(filename = "P2_Plot5.png",width = 960, height = 480)
a <- ggplot(VechSummary,aes(year,Emissions))+
      geom_bar(stat="identity",aes(fill=factor(year)))+
      scale_x_continuous(breaks=VechSummary$year)+
      scale_fill_brewer(palette="Set1")+
      theme_bw()+
      theme(legend.position="none")+
      xlab("year")+
      ggtitle(expression('Total PM'[2.5]*" Emission by Motor Vehicle"))

b <- ggplot(VechSummary,aes(x=year,y=Emissions))+
      geom_bar(stat="identity",aes(fill=as.factor(year)))+
      scale_x_continuous(breaks=VechSummary$year)+
      scale_fill_brewer(palette="Set1")+
      theme_bw()+
      xlab("year")+
      ggtitle(expression('Total PM'[2.5]*" Emission by Vehicle Type"))+
      facet_wrap(~EI.Source,scales = "free")+
      geom_smooth(method="lm",se=F)+
      theme(legend.position="none",strip.text.x = element_text(size=9))

grid.arrange(a,b,ncol=2)
dev.off()