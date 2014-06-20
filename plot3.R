library("ggplot2")

draw.plot3 <- function() {

  # Read data
  data <- readRDS("summarySCC_PM25.rds")
  data$type <- factor(data$type)
  data$year <- factor(data$year)
  
  # Prepare data
  data <- data[data$fips == "24510",]
  
  by.year.type <- aggregate(data$Emissions, list(Year=data$year, Type=data$type), sum)
  colnames(by.year.type)[3] <- "Total"
  
  # Build the plot
  p <- ggplot(by.year.type) + aes(factor(Year), Total) + 
  geom_bar(stat="identity") + 
  stat_smooth(method="lm", se=FALSE, size=1.5, alpha=0.8, color="darkgreen", 
              mapping=aes(factor(Year), Total, group = 1)) + 
  facet_wrap(~Type, nrow=2, ncol=2) + 
  xlab("Year") + 
  ylab("") + 
  ggtitle(expression("Emissions of" ~ PM[2.5] ~ "in Baltimore, MA by Source Type (tons)")) + 
  geom_text(data=by.year.type, aes(label=format(Total, digits=3)), 
            position=position_dodge(width=0.9), vjust=-0.35, size=4)  

  ggsave(file="plot3.png")
}
