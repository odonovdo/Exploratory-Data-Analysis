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

TotEmmsions <- tapply(NEI$Emissions,NEI$year,sum,na.rm=T)
fitEmis <- lm(TotEmmsions~as.numeric(names(TotEmmsions)))
my.colors = colorRampPalette(c("red", "blue"))

png(filename = "P2_Plot1.png")
par(mfrow =c(1,2))
barplot(TotEmmsions,col =my.colors(length(TotEmmsions)),main="Total Emissions by Year",cex.main=0.8,xlab="Year",ylab=expression('Total PM'[2.5]*" Emission"))

plot(TotEmmsions~as.numeric(names(TotEmmsions)),type="b",col =my.colors(length(TotEmmsions)),main="Total Emissions by Year",cex.main=0.8,xlab="Year",ylab=expression('Total PM'[2.5]*" Emission"),ylim = c(0,max(TotEmmsions)))

abline(fitEmis,col="blue",lty=2)

dev.off()