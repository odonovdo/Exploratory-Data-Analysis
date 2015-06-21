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
NEICoal <- merge(NEI,SCC,by.x="SCC")
NEICoal <- NEICoal[grepl("Coal",NEICoal$Short.Name,ignore.case = TRUE),]

NEICoalSum <- tapply(NEICoal$Emissions,list(NEICoal$year,as.character(NEICoal$EI.Sector)),sum,na.rm=TRUE)
NEICoalSum <- melt(NEICoalSum)
names(NEICoalSum) <- c("year","Source","Emissions")

png(filename = "P2_Plot4.png",width = 1340, height = 480)
a <- ggplot(NEICoalSum,aes(year,Emissions))+
      geom_bar(stat="identity",aes(fill=as.factor(year)))+
      scale_fill_brewer(palette="Set1")+
      scale_x_continuous(breaks=NEICoalSum$year)+
      theme_bw()+
      theme(legend.position="none")+
      ggtitle(expression('Total PM'[2.5]*" Emission by Coal Across US"))

b <- ggplot(NEICoalSum,aes(x=year,y=Emissions))+
      geom_bar(stat="identity",aes(fill=as.factor(year)))+
      scale_x_continuous(breaks=NEICoalSum$year)+
      scale_fill_brewer(palette="Set1")+
      theme_bw()+
      xlab("year")+
      ggtitle(expression('Total PM'[2.5]*" Emission by Coal Source"))+
      facet_wrap(~Source,scales = "free")+
      geom_smooth(method="lm",se=F)+
      theme(legend.position="none",strip.text.x = element_text(size=9))

grid.arrange(a,b,ncol=2)
dev.off()
