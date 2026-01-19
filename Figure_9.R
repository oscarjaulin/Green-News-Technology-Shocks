# Upload the data

DATA      = read_excel('Quarter_data_Long.xlsx')
year_ini  = substr(DATA$Date,1,4)[1]
quarter_ini = as.numeric(substr(DATA$Date,6,7)[1])/3 + 2/3
#Dates     = as.matrix(as.character(DATA$Date,1))
Dates = as.matrix(as.character(DATA$Date))

par(mfrow=c(1,3))
# Plot 1: Levels
#jpeg(paste0('index_plot', ".jpeg"), width = 350, height = 350)
  # Plot GPBII (left axis, green)
  plot(DATA$Date, exp(DATA$GFGPBII), type = "l", col = "chartreuse4", ylab = "", xlab = "Period", lwd = 2)
  axis(side = 2, col.axis = "chartreuse4")  # Left axis in green
  
  # Add NGPBII (right axis, red)
  par(new = TRUE)
  plot(DATA$Date, exp(DATA$NGFGPBII), type = "l", col = "brown", axes = FALSE, xlab = "", ylab = "", lwd = 2)
  axis(side = 4, col.axis = "brown")  # Right axis in red
  # Add legend
  legend("topleft", legend = c("GFGPBII", "NGFGPBII"), col = c("chartreuse4", "brown"), lwd = 2, bty = "n")

#dev.off()

# Plot 2: Ratio
# Calculate ratio
ratio <- exp(DATA$GFGPBII) / exp(DATA$NGFGPBII)
#jpeg(paste0('ratio_plot', ".jpeg"), width = 350, height = 350)

# Plot
plot(DATA$Date, ratio, type = "l", col = "chartreuse4", ylab = "Value", xlab = "Period", lwd = 2)

#dev.off()

# Plot 3: Growth Rates
# Calculate log growth rates
GPBII_growth <- c(NA, diff((DATA$GFGPBII)))
NGPBII_growth <- c(NA, diff((DATA$NGFGPBII)))

#jpeg(paste0('index_growth_plot', ".jpeg"), width = 350, height = 350)

# Plot
plot(DATA$Date, GPBII_growth, type = "l", col = "chartreuse4", ylab = "percent", xlab = "Period", lwd = 2)
lines(DATA$Date, NGPBII_growth, col = "brown", lwd = 2)

# Add legend

#dev.off()

