draw.plot2 <- function() {

  # Set the graphics parameters
  png(filename = "plot2.png",
      width = 800, height = 600, units = "px", pointsize = 12,
      bg = "white", res = NA, family = "", restoreConsole = TRUE,
      type = c("windows", "cairo", "cairo-png"))
  
  par(mfrow = c(1,1))
  
  # Read data
  data <- readRDS("summarySCC_PM25.rds")
  data$type <- factor(data$type)
  data$year <- factor(data$year)
  
  # Prepare data
  data <- data[data$fips == "24510",]
  
  by.year <- cbind(by(data, data$year, function(y) {sum(y$Emissions)}))
  colnames(by.year) <- c("Total") 
  by.year <- data.frame(Year=rownames(by.year), by.year)
  
  # Build the plot
  scale <- 1
  ramp <- colorRamp(c("white","#FF2500"))
  max <- max(by.year$Total)
  cols <- ramp(by.year$Total / max)
  cols <- rgb(cols[,1], cols[,2], cols[,3], maxColorValue=256)
  
  p <- barplot(height=by.year$Total/scale, names.arg=by.year$Year, col=cols, 
               ylim=c(0, max*1.1/scale))

  abline(lm(by.year$Total ~ p), col="darkgreen", lwd=3)
  
  text(p, 1000, format(by.year$Total, digits = 3), xpd=TRUE, col="black")  
  title(expression("Total Emissions of" ~ PM[2.5] ~ "in Baltimore, MA (tons)"))  
  
  # Finish up.
  dev.off()
  
}
