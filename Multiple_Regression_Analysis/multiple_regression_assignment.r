# import dataset
library(readxl)
week4_dataset <- read_excel("dataset.xls")
View(week4_dataset)  

## evaluate highest bi-variate correlation with x19 ## 
# run correlation matrix with dependent variable x19
# grabbing columns and setting variable for the list of columns
columns_we_want <- week4_dataset[, c("x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15", "x16", "x17", "x18", "19")]
# get the correlations (Pearson correlation) among variables table 4-7
correlation_matrix = cor(columns_we_want)

# n, number of observations
n <- nrow(columns_we_want)

## select highest bi-variate correlation, variable x9 ##
## first step, build a regression equation using just x9 ##
multiple_r_x9 <- correlation_matrix['x19', 'x9']
coeff_of_det_x9 <- multiple_r_x9^2

# function to calculate adjusted R^2
calc_adj_rsq <- function(coeff_of_det, n_obs, num_ind_vars) {
  1 - (1 - coeff_of_det) * ((n_obs - 1)/(n_obs - num_ind_vars - 1))
}

adj_r_sq_x9 <- calc_adj_rsq(coeff_of_det_x9, n, 1)

#####################################
### Table 4-8 ###
model.v1 <- lm(x19 ~ x9, data=columns_we_want)
summary(model.v1)

### Table 4-9 ###
model.v2 <- lm(x19 ~ x9 + x6, data=columns_we_want)
summary(model.v2)
# collinearity statistics of model.v2
library(mctest)
collinearity.table.4_9 <- imcdiag(model.v2)

### table 4-10 ###
model.v3 <- lm(x19 ~ x9 + x6 + x12, data=columns_we_want)
summary(model.v3)
collinearity.table.4_10 <- imcdiag(model.v3)


###################################################
### table 4-11 ###
final_lr_model <- lm(x19 ~ x9 + x6 + x12 + x11 + x7, data=columns_we_want)
summary(final_lr_model)
collinearity.final_model <- imcdiag(final_lr_model)

# grab studentized residual values
library(MASS)
stud_res_vals <- studres(model.v4)
# create predicted values from data with model and standardize them
predicted_values <- predict(model.v4, newdata = columns_we_want)
stand_predict_values <- scale(predicted_values)
# plot chart
plot(stand_predict_values, stud_res_vals, xlab = "Standardized Predicted Value", ylab = "Studentized Residual")
# plotting with predicted values rather than standardized predicted values
plot(predicted_values, stud_res_vals, xlab = "Predicted Value", ylab = "Studentized Residual")

### figure 4-11 ###
# residuals of model
model.resid = resid(final_lr_model)
# plotting each variable in regression model with residuals of model
plot(scale(columns_we_want$x6), model.resid, xlab = "x6 Product Quality", ylab = "Residuals")
plot(scale(columns_we_want$x7), model.resid, xlab = "x7 E-Commerce Activities", ylab = "Residuals")
plot(scale(columns_we_want$x9), model.resid, xlab = "x9 Complaint Resolution", ylab = "Residuals")
plot(scale(columns_we_want$x11), model.resid, xlab = "x11 Product Line", ylab = "Residuals")
plot(scale(columns_we_want$x12), model.resid, xlab = "x12 Salesforce Image", ylab = "Residuals")

### figure 4-12 ###
# normal probability plot: standardized residuals
qqnorm(scale(model.resid))
qqline(scale(model.resid))

### figure 4-13 ###
plot(stud_res_vals, ylab = "Studentized Residual", xlab = "ID")
abline(h=c(-2, -1, 0, 1))


