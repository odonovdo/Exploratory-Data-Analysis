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
library(ggplot2);library(reshape2)
NEICoal <- merge(NEI,SCC,by.x="SCC")
NEICoal <- NEICoal[grepl("Coal",NEICoal$Short.Name,ignore.case = TRUE),]

NEICoalSum <- tapply(NEICoal$Emissions,NEICoal$year,sum,na.rm=TRUE)
NEICoalSum <- melt(NEICoalSum)
names(NEICoalSum) <- c("year","Emissions")

png(filename = "P2_Plot4.png")
ggplot(NEICoalSum,aes(year,Emissions))+
      geom_bar(stat="identity",aes(fill=as.factor(year)))+
      scale_fill_brewer(palette="Set1")+
      geom_smooth(method="lm",se=FALSE,color="black",linetype=2)+
      scale_x_continuous(breaks=NEICoalSum$year)+
      theme_bw()+
      theme(legend.position="none")+
      ggtitle(expression('Total PM'[2.5]*" Emission by Coal Across US"))
dev.off()
