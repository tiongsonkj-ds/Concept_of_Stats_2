# read in excel spreadsheet
library(readxl)
HBAT_week2 <- read_excel("HBAT.xls")
View(HBAT_week2)                                                              
data <- HBAT_week2

# grabbing x6 column
x6 <- data[ , c("x6")]
## storing column data as an actual array variable
x <- x6$x6

# plot histogram with labels
hist(x, xlab="x6 Product Quality", ylab="Count", main="")

# plot normal density curve
## take the mean and standard deviation of the data
m <- mean(x)
std <- sqrt(var(x6))
## use dnorm function to return the values of the normal probablity density
curve(dnorm(x, mean=m, sd=std) * 50, lwd=2, add=TRUE, yaxt="n")
