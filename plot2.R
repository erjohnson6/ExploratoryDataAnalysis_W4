plot2 <- function(NEI) {
    ## NEI is the dataset provided in the assignment
    ## 
    ## Question 2: Have total emissions from PM2.5 decreased in the Baltimore City, 
    ## Maryland (fips=="24510") from 1999 to 2008? 
    ## Use the base plotting system to make a plot answering this question.
    ## 

    library(dplyr)
    
    BaltCityFIPS <- "24510"
    ML.NEI <- subset(NEI, fips == BaltCityFIPS)
    
    ML.NEI <- group_by(ML.NEI, year)
    HL.ML.NEI <- summarise(ML.NEI, sum(Emissions))
    
    ## Open PNG Graphics Device
    png("plot2.png")
    plot(x = HL.ML.NEI[[1]], y = HL.ML.NEI[[2]], xlab = "Year", ylab = "Total Emissions", type = "b")
    dev.off()
    
    HLNEI
    
    
}