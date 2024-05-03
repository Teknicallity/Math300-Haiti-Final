haiti <- read.csv("~/RStudio/Math300-Haiti-Final/haiti.csv", row.names=1)

# Perform Pearson correlation test
correlation_test <- cor.test(haiti$depressive, haiti$anxiety, method = "pearson")
# Print the result
print(correlation_test)

# Linear Model
haiti.lm <- lm(haiti$anxiety~haiti$depressive)
summary(haiti.lm)
