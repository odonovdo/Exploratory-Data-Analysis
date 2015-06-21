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

NEIbalt <- subset(NEI,fips=="24510")

library(reshape2);library(ggplot2)

TotEmbalY_T <- tapply(NEIbalt$Emissions,list(NEIbalt$year,NEIbalt$type),sum,na.rm=TRUE)

ToT_Sum <- melt(TotEmbalY_T)
names(ToT_Sum) <- c("year","type","Emissions")

png(filename = "P2_Plot3.png")

ggplot(ToT_Sum,aes(x=year,y=Emissions,fill=type))+
      geom_bar(stat="identity")+
      scale_x_continuous(breaks=ToT_Sum$year)+
      scale_fill_brewer(palette="Set1")+
      theme_bw()+
      theme(legend.position="none")+
      xlab("year")+
      ggtitle(expression('Total PM'[2.5]*" Emission by Type"))+
      geom_smooth(method="lm",se=FALSE,color="black",linetype=2)+
      facet_wrap(~type,scales = "free")

dev.off()
