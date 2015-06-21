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
TotEmmsionsbal <- tapply(NEIbalt$Emissions,NEIbalt$year,sum,na.rm=T)
fitEmisbal <- lm(TotEmmsionsbal~as.numeric(names(TotEmmsionsbal)))

my.colors = colorRampPalette(c("red", "blue"))
png(filename = "P2_Plot2.png")
par(mfrow =c(1,2))

barplot(TotEmmsionsbal,col =my.colors(length(TotEmmsionsbal)),main="Total Emissions for Baltimore by Year",cex.main=0.8,xlab="Year",ylab=expression('Total PM'[2.5]*" Emission"))

plot(TotEmmsionsbal~as.numeric(names(TotEmmsionsbal)),type="b",col =my.colors(length(TotEmmsions)),main="Total Emissions for Baltimore by Year",cex.main=0.8,xlab="Year",ylab=expression('Total PM'[2.5]*" Emission"),ylim = c(0,max(TotEmmsionsbal)))

abline(fitEmisbal,col="blue",lty=2)
dev.off()