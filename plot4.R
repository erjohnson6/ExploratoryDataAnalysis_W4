plot4 <- function(NEI, SCC) {
    ## NEI is the dataset provided in the assignment
    ## SCC is the source classification dataset provided
    ## 
    ## Question 4:  Across the United States, how have emissions from coal combustion-related 
    ## sources changed from 1999–2008?
    
    library(dplyr)
    library(ggplot2)
    
    ## Selecting combustion-related sources
    L1.l <- grepl("Combust", SCC$SCC.Level.One)
    
    ## Selecting Coal related sources
    coal.l <- grepl("Coal", SCC$EI.Sector)
    
    SCC.Coal <- SCC[L1.l & coal.l,]$SCC
    NEI.Coal <- NEI[NEI$SCC %in% SCC.Coal,]
    NEI.Coal <- group_by(NEI.Coal, year)
    
    HL.NEI.Coal <- summarise(NEI.Coal, Total.Emissions = sum(Emissions))
    
    png("plot4.png")
    plot4 <- qplot(x = year, y = Total.Emissions, data = HL.NEI.Coal, geom = c("point","line"))
    print(plot4)
    dev.off()
    
    HL.NEI.Coal
}