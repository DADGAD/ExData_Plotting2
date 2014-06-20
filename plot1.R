draw.plot1 <- function() {

  # Set the graphics parameters
  png(filename = "plot1.png",
      width = 800, height = 600, units = "px", pointsize = 12,
      bg = "white", res = NA, family = "", restoreConsole = TRUE,
      type = c("windows", "cairo", "cairo-png"))
  
  par(mfrow = c(1,1))
  
  # Read data
  data <- readRDS("summarySCC_PM25.rds")
  data$type <- factor(data$type)
  data$year <- factor(data$year)
  
  
  # Prepare data
  by.year <- cbind(by(data, data$year, function(y) {sum(y$Emissions)}))
  colnames(by.year) <- c("Total") 
  by.year <- data.frame(Year=rownames(by.year), by.year)
  
  # Build the plot
  ramp <- colorRamp(c("white","#FF2500"))
  max <- max(by.year$Total)
  cols <- ramp(by.year$Total / max)
  cols <- rgb(cols[,1], cols[,2], cols[,3], maxColorValue=256)
  
  p <- barplot(height=by.year$Total/1000000, names.arg=by.year$Year, col=cols, 
               ylim=c(0, max*1.1/1000000))

  text(p, seq_along(p), format(by.year$Total/1000000, digits = 3), xpd=TRUE, col="black")  
  title(expression("Total Emissions of" ~ PM[2.5] ~ "(millions of tons)"))  
  
  # Finish up.
  dev.off()
  
}
