plot1 <- function(NEI) {
    ## NEI is the dataset provided in the assignment
    ## 
    ## Question 1: Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
    ## Using the base plotting system, make a plot showing the total PM2.5 emission from all sources 
    ## for each of the years 1999, 2002, 2005, and 2008.
    
    library(dplyr)
    
    NEI <- group_by(NEI, year)
    ## Create high-level NEI data set
    HLNEI <- summarise(NEI, sum(Emissions))
    
    ## Open PNG Graphics Device
    png("plot1.png")
    plot(x = HLNEI[[1]], y = HLNEI[[2]], xlab = "Year", ylab = "Total Emissions", type = "b")
    dev.off()
    
    HLNEI
}