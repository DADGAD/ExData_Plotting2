library("ggplot2")

draw.plot4 <- function() {

  # Read data
  data <- readRDS("summarySCC_PM25.rds")
  cls <- readRDS("Source_Classification_Code.rds")
  data$type <- factor(data$type)
  data$year <- factor(data$year)
  
  # Prepare data
  cls.coal <- subset(cls, grepl("coal", cls[["Short.Name"]], 
                                ignore.case=TRUE), select=c(SCC, EI.Sector, SCC.Level.One, SCC.Level.Two))
  data <- merge(data, cls.coal, by=c("SCC"))
  by.year.type <- aggregate(data$Emissions, list(Year=data$year, Type=data$SCC.Level.One), sum)
  colnames(by.year.type)[3] <- "Total"
  by.year.type$Total = by.year.type$Total / 1000 
  
  # Squash all misc categories and re-aggregate
  by.year.type$Type <- as.character(by.year.type$Type)  
  by.year.type[by.year.type$Total < 1, "Type"] <- "Other"
  
  by.year.type$Type <- factor(by.year.type$Type)  
  by.year.type <- aggregate(by.year.type$Total, list(Year=by.year.type$Year, 
                                                     Type=by.year.type$Type), sum)
  colnames(by.year.type)[3] <- "Total"  
  
  # Build the plot
  p <- ggplot(by.year.type) + aes(factor(Year), Total) + 
  geom_bar(stat="identity") + 
  stat_smooth(method="lm", se=FALSE, size=1.5, alpha=0.8, color="darkgreen", 
              mapping=aes(factor(Year), Total, group = 1)) + 
  facet_wrap(~Type, ncol=2) + 
  xlab("Year") + 
  ylab("") + 
  ggtitle(expression("Emissions of" ~ PM[2.5] ~ "in Baltimore, MA From Coal Sources (1000 of tons)")) + 
  geom_text(data=by.year.type, aes(label=format(Total, digits=2)), 
            position=position_dodge(width=0.9), vjust=-0.35, size=4)  

  ggsave(file="plot4.png")
}
