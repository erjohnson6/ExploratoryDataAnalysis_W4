plot5 <- function(NEI, SCC) {
    ## NEI is the dataset provided in the assignment
    ## SCC is the source classification dataset provided
    ## 
    ## Question 5:  How have emissions from motor vehicle sources changed from 1999–2008 in 
    ## Baltimore City?
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
    
    BC.NEI <- subset(NEI, fips == BaltCityFIPS)
    mv <- grepl("^220", BC.NEI$SCC)
    BC.MV.NEI <- group_by(BC.NEI[mv,], year)
    HL.BC.MV.NEI <- summarise(BC.MV.NEI, Total.Emissions = sum(Emissions))
    
    png("plot5.png")
    plot5 <- qplot(x = year, y = Total.Emissions, data = HL.BC.MV.NEI, geom = c("point", "line"))
    print(plot5)
    dev.off()
    
    HL.BC.MV.NEI
    
}
