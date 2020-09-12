# read in the excel spreadsheet using the code from figure 2-1.r
# setting variables for the columns we want
columns_we_want <- data[, c("x6", "x7", "x8", "x12", "x13")]
x6 <- columns_we_want$x6
x7 <- columns_we_want$x7
x8 <- columns_we_want$x8
x12 <- columns_we_want$x12
x13 <- columns_we_want$x13

# this created the scatter plot matrix of the data
pairs(columns_we_want)

# this created the bivariate correlation that we wanted
cor(x6, x7, method="pearson")

# this created the scatterplot matrix we were looking for
pairs.panels(columns_we_want,
             method = "pearson",
             hist.col = "grey",
             density = FALSE,
             ellipses = FALSE,
             smooth = FALSE,
             digits = 3,
             rug = FALSE,
             breaks=14,
)