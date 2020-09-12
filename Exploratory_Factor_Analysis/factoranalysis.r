# read in excel spreadsheet
library(readxl)
week3_dataset <- read_excel("week3_dataset.xls")
View(week3_dataset)                                                              
data <- week3_dataset

########## part 1: correlation matrix ###############
# grabbing columns and setting variable for the list of columns
columns_we_want <- data[, c("x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15", "x16", "x17", "x18")]

# get the correlations (Pearson correlation) among variables table 3-4
correlation_matrix = cor(columns_we_want)

# measure of  sampling adequacy (KAISER's) 
library(psych)
msa <- KMO(columns_we_want)
# overall MSA
overall_msa <- msa$MSA
msa_per_variable <- msa$MSAi
# and partial correlations
partial_correlations <- msa$Image


######### part 2: eigenvalues & scree plot ##############
# http://www.sthda.com/english/wiki/eigenvalues-quick-data-visualization-with-factoextra-r-software-and-data-mining
eigvaluecols <- data[, c("x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x16", "x18")]

# see first 5 rows, helped me see if i had the right columns and right data
head(eigvalcols)

# installed the right packages
install.packages("devtools")
library(devtools)
# !!! used dev tools to install a package from GitHub !!!
install_github("kassambara/factoextra")
library("factoextra")

# gives me the data needed to extract the eigenvalues
# Performs a principal components analysis on the given data matrix and returns the results as an object of class 
res.pca <- prcomp(eigvalcols, scale = TRUE)
# get the eigenvalues and their data (% of values and cumulative variance %)
get_eig(res.pca)
# plot the eigenvalues
# this specific line makes it look exactly how it looks in the book
fviz_eig(res.pca, choice="eigenvalue", geom="line")


######### table 3-7 unloaded component analysis factor matrix ########
library(FactoMineR)
facto.pca <- PCA(eigvalcols, ncp = 4)
unrotated_factor_loadings <- facto.pca$var$coord
# converting loading matrix into a dataframe to add rows and columns
unloadedframe <- data.frame(round(unrotated_factor_loadings, digits=3))
# adding communality column
communality <- vector()
for(i in 1:nrow(unloadedframe)) {
  row <- unloadedframe[i,]
  comm <- 0
  for(i in 1:4) {
    comm <- (row[,i] * row[,i]) + comm
  }
  comm <- round(comm, digits = 3)
  communality <- c(communality, comm)
}
unloadedframe$Communality <- communality
# adding sum of squares and percentage trace

####### table 3-8 VARIMAX rotated factor loading #######
varimax_rot_fact_loading <- varimax(unrotated_factor_loadings)
####### table 3-8 VARIMAX rotated factor loading NO X11 #####
eigvalcols_no_11 <-data[, c("x6", "x7", "x8", "x9", "x10", "x12", "x13", "x14", "x16", "x18")]
facto_no_11.pca <- PCA(eigvalcols_no_11, ncp = 4)
unrotated_factor_loadings_no_11 <- facto_no_11.pca$var$coord
varimax_rot_fact_loading_no_11 <- varimax(unrotated_factor_loadings_no_11)


###### table 3-10 VARIMAX rotated factor loading for two split samples ###########
# split data into two equal sets
split_data = sort(sample(nrow(eigvalcols_no_11), nrow(eigvalcols_no_11)*0.5))
first_half <- eigvalcols_no_11[-split_data,]
second_half <- eigvalcols_no_11[split_data,]
# grab the varimax-rotated loadings for each split sample
# split-sample loading 1
first_half.pca <- PCA(first_half, ncp=4)
rotated_split_sample_1 <- varimax(first_half.pca$var$coord)
# split-sample loading 2
second_half.pca <- PCA(second_half, ncp=4)
rotated_split_sample_2 <- varimax(second_half.pca$var$coord)