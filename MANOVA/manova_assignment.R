### reading SAS file ###
# installing the right package
install.packages(c("haven", "sas7bdat"))
# read SAS file and store into variable
data <- read_sav("HBAT_200.sav")
cols_we_want <- data[, c("x5", "x19", "x20", "x21")]

### 7-5 ###
library(psych)
describeBy(data$x19, data$x5)
describeBy(data$x20, data$x5)
describeBy(data$x21, data$x5)

### 7-6 ###
# convert values to be a specific text in column
cols_we_want$x5 <- as.character(cols_we_want$x5)
cols_we_want$x5[cols_we_want$x5 %in% "1"] <- "Direct"
cols_we_want$x5[cols_we_want$x5 %in% "0"] <- "Indirect"
# Box's M test
boxM(cbind(x19, x20, x21) ~ x5, data=cols_we_want)
# Levene's test
leveneTest(x19 ~ x5, data=cols_we_want)
leveneTest(x20 ~ x5, data=cols_we_want)
leveneTest(x21 ~ x5, data=cols_we_want)
# bartlett's test for sphericity
# had to convert values back to 1s and 0s
data <- read_sav("HBAT_200.sav")
cols_we_want <- data[, c("x5", "x19", "x20", "x21")]
bartlett_test <- cortest.bartlett(cols_we_want, n=nrow(cols_we_want))
bartlett_test$p.value
# box plots for outliers
boxplot(cols_we_want$x19 ~ cols_we_want$x5, xlab = "x5 Distribution System", ylab = "x19 Satisfaction")
boxplot(cols_we_want$x20 ~ cols_we_want$x5, xlab = "x5 Distribution System", ylab = "x20 Likely to Recommend")
boxplot(cols_we_want$x21 ~ cols_we_want$x5, xlab = "x5 Distribution System", ylab = "x21 Likely to Purchase")

### 7-7 (if we can) ###
# had to convert values back to 1s and 0s
# multivariate tests - MANOVA
manova_test <- manova(cbind(x19, x20, x21) ~ x5, data=cols_we_want)
summary(manova_test, test="Wilks")
summary(manova_test, test = "Pillai")
summary(manova_test, test = "Hotelling-Lawley")
summary(manova_test, test = "Roy")
# univariate tests- one way ANOVA
summary(aov(formula = cols_we_want$x19 ~ cols_we_want$x5))
summary(aov(formula = cols_we_want$x20 ~ cols_we_want$x5))
summary(aov(formula = cols_we_want$x21 ~ cols_we_want$x5))