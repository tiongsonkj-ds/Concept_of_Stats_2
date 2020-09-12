# grabbing x6, x7, x1 columns and setting variables for them
columns_we_want <- data[, c("x6", "x7", "x8", "x12", "x13")]
x6 <- columns_we_want$x6
x7 <- columns_we_want$x7
x1 <- columns_we_want$x1

# Box plot package
library(car)

# plotting first figure
Boxplot(x6~x1, 
        id.method="y", 
        ylim=c(4, 11), 
        names=c("Less than 1 year", "1 to 5 years", "More than 5 years"), 
        ylab = "x6 Product Quality", 
        xlab="x1 Customer Type"
)


# plotting second figure
Boxplot(x7~x1, 
        id.method="y", 
        ylim=c(2, 6), 
        names=c("Less than 1 year", "1 to 5 years", "More than 5 years"), 
        ylab = "x7 E-Commerce Activities", 
        xlab="x1 Customer Type"
)