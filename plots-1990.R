haiti <- read.csv("~/RStudio/Math300-Haiti-Final/haiti.csv", row.names=1)

# Scatter Plot
plot(haiti$year, haiti$depressive, 
     type = "b", ylim = c(3.5, 5.0), xlab = "Year", ylab = "Share of Population", col = "blue", pch = 16, 
     main = "Depressive vs Anxiety Disorder Share Over Years")
lines(haiti$year, haiti$anxiety, 
      type = "b", col = "red", pch = 17)

## Add legend
legend("topright", legend = c("Depressive Share", "Anxiety Share"), col = c("blue", "red"), 
       pch = c(16, 17), bty = "n")

correlation <- cor.test(haiti$depressive, haiti$anxiety)
mtext(paste("Correlation:", round(correlation$estimate, 2), "\n", "p-value:", 
            signif(correlation$p.value, digits = 3)), side = 1, line = 3, col = "darkgreen", adj = 0)
abline(v=2008, col="maroon", lwd=2, lty=3)



# Box plots
par(mfrow=c(1,2))
## depressive
boxplot(haiti$depressive, names = c("Depressive Disorder"), 
        main = "Depressive Disorder Population Share", xlab = "Disorder", ylab = "Share of Population")
## anxiety
boxplot(haiti$anxiety, names = c("Anxiety Disorder"), 
        main = "Anxiety Disorder Population Share", xlab = "Disorder", ylab = "Share of Population")
par(mfrow=c(1,1))



# Histograms
## depressive disorder population share
hist(haiti$depressive, main = "Distribution of Depressive Disorder Population Share", xlab = "Percentage", ylab = "Frequency")

## anxiety disorder population share
hist(haiti$anxiety, main = "Distribution of Anxiety Disorder Population Share", xlab = "Percentage", ylab = "Frequency")



# Unused Line Plots
# depressive disorder population share over time
plot(haiti$year, haiti$depressive, type = "l", main = "Trends in Depressive Disorder Population Share", xlab = "Year", ylab = "Percentage")
# anxiety disorder population share over time
plot(haiti$year, haiti$anxiety, type = "l", main = "Trends in Anxiety Disorder Population Share", xlab = "Year", ylab = "Percentage")



# Plot Correlation
correlation_test <- cor.test(haiti$depressive, haiti$anxiety)

## Scatter plot
plot(haiti$depressive, haiti$anxiety, 
     xlab = "Depressive Share", ylab = "Anxiety Share", 
     main = "Correlation between Depressive and Anxiety Share", 
     pch = 16, col = "blue")

## Add trend line
abline(lm(haiti$anxiety ~ haiti$depressive), col = "red")

## Add confidence intervals
segments(min(haiti$depressive), 
         min(haiti$anxiety), 
         min(haiti$depressive), 
         min(haiti$anxiety) + confint(lm(haiti$anxiety ~ haiti$depressive))[2, ], 
         col = "maroon", lty = 2)
segments(max(haiti$depressive), 
         min(haiti$anxiety), 
         max(haiti$depressive), 
         min(haiti$anxiety) + confint(lm(haiti$anxiety ~ haiti$depressive))[2, ], 
         col = "maroon", lty = 2)

## Highlight points outside confidence interval
points(haiti$depressive[correlation_test$outlier], 
       haiti$anxiety[correlation_test$outlier], 
       pch = 16, col = "red")

## Add legend
legend("topright", 
       legend = c("Data Points", "Trendline", "Confidence Interval"), 
       col = c("blue", "red", "maroon"), 
       pch = c(16, NA, NA, 16), 
       lty = c(NA, 1, 2, NA))

## Add correlation coefficient and p-value to the plot
text(x = min(haiti$depressive), 
     y = max(haiti$anxiety), 
     labels = paste("Correlation:", round(correlation_test$estimate, 2), "\n", 
                    "p-value:", signif(correlation_test$p.value, digits = 3)), 
     pos = 4, col = "darkgreen")
