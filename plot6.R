plot6 <- function(NEI, SCC) {
    ## NEI is the dataset provided in the assignment
    ## SCC is the source classification dataset provided
    ## 
    ## Question 5:  Compare emissions from motor vehicle sources in Baltimore City with emissions 
    ## from motor vehicle sources in Los Angeles County, California (fips=="06037"). Which city has 
    ## seen greater changes over time in motor vehicle emissions?
    ## 
    ## Reference the following document for more information on SCC:
    ## https://ofmpub.epa.gov/sccwebservices/sccsearch/docs/SCC-IntroToSCCs.pdf
    ## 
    ## Based on this SCC codes starting with "220"  According to this reference:
    ## "This set of SCCs is for emissions from activities related to transportation
    ## equipment that is typically used on roads and highways for transportation purposes. 
    ## For example, processes related to trucks, motor homes, motorcycles, passenger cars, 
    ## buses, etc."
    
    library(dplyr)
    library(ggplot2)
    
    BaltCityFIPS <- "24510"
    LA_FIPS <- "06037"
    ref <- data.frame(fips = c("24510", "06037"), City = c("Baltimore City", "Los Angeles"))
    
    ## Compare.NEI <- subset(NEI, fips == BaltCityFIPS | fips == LA_FIPS)
    Compare.NEI <- subset(NEI, fips %in% ref[,1])
    mv <- grepl("^220", Compare.NEI$SCC)
    Compare.MV.NEI <- Compare.NEI[mv,]
    City.NEI <- group_by(merge(Compare.MV.NEI, ref), year, City)
    
    HL.City.NEI <- summarise(City.NEI, Total.Emissions = sum(Emissions))
    
    png("plot6.png")
    plot6 <- qplot(x = year, y = Total.Emissions, data = HL.City.NEI, col = City, geom = c("point", "line"))
    print(plot6)
    dev.off()
    
    HL.City.NEI
    
}