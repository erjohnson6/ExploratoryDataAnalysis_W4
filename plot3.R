plot3 <- function(NEI) {
    ## NEI is the dataset provided in the assignment
    ## 
    ## Question 3: Of the four types of sources indicated by the type (point, nonpoint, onroad, 
    ## nonroad) variable, which of these four sources have seen decreases in emissions from 
    ## 1999–2008 for Baltimore City? Which have seen increases in emissions from 1999–2008? 
    ## 
    ## Use the ggplot2 plotting system to make a plot answer this question.
    
    library(dplyr)
    library(ggplot2)
    
    MarylandFIPS <- "24510"
    ML.NEI <- subset(NEI, fips == MarylandFIPS)
    
    ML.NEI <- group_by(ML.NEI, year, type)
    HL.ML.NEI <- summarise(ML.NEI, Total.Emissions = sum(Emissions))
    
    png("plot3.png")
    plot3 <- qplot(x = year, y = Total.Emissions, data = HL.ML.NEI, col = type, geom = "line")
    print(plot3)
    dev.off()
    
    HL.ML.NEI
    
}