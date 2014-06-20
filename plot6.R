library("ggplot2")

draw.plot6 <- function() {

  # Read data
  data <- readRDS("summarySCC_PM25.rds")
  cls <- readRDS("Source_Classification_Code.rds")
  data <- data[data$fips == "24510" | data$fips == "06037",]
  data$type <- factor(data$type)
  data$year <- factor(data$year)
  
  # Prepare data
  data[data$fips == "24510", "Location"] <- "Baltimore"
  data[data$fips == "06037", "Location"] <- "LA County"
  
  cls.veh <- subset(cls, grepl("veh", cls[["EI.Sector"]], 
                                ignore.case=TRUE), select=c(SCC, EI.Sector, SCC.Level.One, SCC.Level.Two))
  data <- merge(data, cls.veh, by=c("SCC"))
  by.year.type <- aggregate(data$Emissions, list(Year=data$year, Type=data$SCC.Level.Two, 
                                                 Location=data$Location), sum)
  colnames(by.year.type)[4] <- "Total"
  
  # Build the plot
  p <- ggplot(by.year.type) + aes(factor(Year), Total) + 
    geom_bar(aes(fill=Location), stat="identity", position="dodge") + 
    stat_smooth(data=by.year.type[by.year.type$Location == "LA County",], 
                method="lm", se=FALSE, size=1, alpha=0.8, color="darkgreen", 
                mapping=aes(factor(Year), Total, group = 1)) + 
    stat_smooth(data=by.year.type[by.year.type$Location == "Baltimore",], 
                method="lm", se=FALSE, size=1, alpha=0.8, color="darkred", 
                mapping=aes(factor(Year), Total, group = 1)) + 
    facet_wrap(~Type, ncol=2) + 
    xlab("Year") + 
    ylab("") + 
    ggtitle(expression("Emissions of" ~ PM[2.5] ~ " From Vehicle Sources (tons)")) + 
    geom_text(data=by.year.type[by.year.type$Location == "Baltimore",], 
              aes(label=format(Total, digits=2)), position=position_dodge(width=0.9), 
              vjust=-0.35, hjust=1.4, size=4, colour=c("Black")) + 
    geom_text(data=by.year.type[by.year.type$Location == "LA County",], 
              aes(label=format(Total, digits=2)), position=position_dodge(width=0.9), 
              vjust=-0.35, hjust=0, size=4, colour=c("darkgrey"))

  ggsave(file="plot6.png")
}
