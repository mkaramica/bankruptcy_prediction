
# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
# 1- Preparation

# 1-1- Install & Load Required Libraries

packages <- c("dplyr", "tidyverse", "caret", "plotly", "reshape", "gridExtra",
              "rpart", "rpart.plot", "ROCR", "MLmetrics", 
              "randomForest",
              "kernlab", "e1071", "kableExtra")



# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

# Setting the Timeout:
options(timeout = 120)
#-------------------------------------------------------------------------------

# 1-2- Loading & Preparation of Dataset:

# Database Source on the web: 
db_url <- "https://github.com/mkaramica/bankruptcy_prediction/blob/c030c3c2ef6529e7b27f954c7b7d2170c551ede8/archive.zip?raw=true"

# database:
db <- "archive.zip"

# Download the zipped file if it does not exist in the working directory:
if(!file.exists(db))
  download.file(db_url,db)

# Unzip the csv file if it does not exist in the corresponding folder:----------
database_file <- "data.csv"
if(!file.exists(database_file))
  unzip(db, database_file)



# Read csv file:
bankruptcyDB <- read.csv(database_file,header=TRUE)

# NOTE: If error happened, please manually download the zip file from the mentioned links and put it in the root of current directory.
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
# 2- Data Inspection

# 2-1- Overview
nrows <- nrow(bankruptcyDB)
ncols <- ncol(bankruptcyDB)

# Number of rows and columns:
print(c(nrows, ncols))

#------------------------------
# Remove useless column(s): 

# The 95th columns is constant for all rows (all=1):
#  'Net Income Flag'

print(colnames(bankruptcyDB)[95])
print(min(bankruptcyDB[,95]))
print(max(bankruptcyDB[,95]))

#Hence, it does not have any effect and can be removed:
bankruptcyDB <- bankruptcyDB[,-95]
#------------------------------


# Replace column names with numbers for better presentation & understanding:
colnames(bankruptcyDB) <- paste0("[",seq(0,ncol(bankruptcyDB)-1),"]" )

# Change the name of class label to 'label'
colnames(bankruptcyDB)[1] <- "label"

# Number & Portion of Class Labels:
dataLabelTable <- bankruptcyDB %>% group_by(label) %>% 
  summarize( Number=n() ) %>%
  mutate("Portion(%)" = format(100*Number/nrow(bankruptcyDB),2) )

# Print the table of overview of data labels:
knitr::kable(dataLabelTable, caption = "Overview of Data Labels")


# Showing Histogram Plots for the First 18 Features (column #2 to #19):

# Feature #1 to #9 

# Make a tile plot:
par(mfrow = c(3, 3))
par(mar = rep(4, 4))

plots <- list()
for (i in 1:9) {
  plots[[i]] <- hist(bankruptcyDB[,i+1],col = 'skyblue', breaks = 20,
                     main = "",
                     xlab=colnames(bankruptcyDB)[i+1], ylab="")
}

mtext("Histograms of Bankruptcy Database: Features #1 to #9", 
      outer = TRUE, cex = 1.5, line = -2.5)

# Feature #10 to #18

# Make a tile plot:
par(mfrow = c(3, 3))
par(mar = rep(4, 4))

for (i in 1:9) {
  hist(bankruptcyDB[,i+10],col = 'skyblue', breaks = 20,
       main = "",
       xlab=colnames(bankruptcyDB)[i+10], ylab="")
}
mtext("Histograms of Bankruptcy Database: Features #10 to #18", 
      outer = TRUE, cex = 1.5, line = -2.5)

# Normalize all the columns to the range 0 to 1:

calcNormalize <- function(vecIn) {
  return ( (vecIn-min(vecIn))/(max(vecIn)-min(vecIn))  )
}

for (i in 2:ncol(bankruptcyDB))  {
  bankruptcyDB[,i] <- calcNormalize(bankruptcyDB[,i])
}


# Show the Features Variation for Normalized Dataset:

# Feature #1 to #46
ggplot(melt(bankruptcyDB[,2:47]), aes(x=variable, y=value)) + 
  geom_boxplot() +
  ggtitle("Box Plots of Features of Bankruptcy Database: Features #1 to #46") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Show the Features Variation for Normalized Dataset:

# Feature #47 to #94
ggplot(melt(bankruptcyDB[,48:ncol(bankruptcyDB)]), aes(x=variable, y=value)) + 
  geom_boxplot() +
  ggtitle("Box Plots of Features of Bankruptcy Database: Features #47 to #94")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Calculate Standard Deviation of All Columns:
sd_Columns <- bankruptcyDB[,2:ncol(bankruptcyDB)] %>% apply( 2, sd) 



# Plot Default Setting:
par(mfrow = c(1, 1))

barplot(sort(sd_Columns, decreasing = TRUE)[1:40], xlab = "Standard Deviation",
        ylab = "Feature ID", col="#69b3a2", horiz=T , las=1,
        main = "Top 40 Features with the highest Standard Deviations")

# Show 2D Plots of the Data Labels:

# Four columns with the Largest Standard Deviations:
names(sort(sd_Columns, decreasing = TRUE))[1:4]

# Feature IDs: #72, #48, #11, #74 
selectedCols <- c(73,49,12,75)


getPlot <- function(db, indx_X, indx_Y) {
  # Returns a plot of data labels in a 2D graph.
  # the components of x & Y axes are given by indices.
  
  colX <- names(db)[indx_X]
  colY <- names(db)[indx_Y]
  
  # Store the plot in a variable:
  p <- db %>%  ggplot(aes(x = !!as.name(colX), 
                          y = !!as.name(colY),
                          color = label, size = label)) +
    geom_point() +
    scale_color_gradient(low = "blue", high = "red") +
    scale_size(range = c(2, 4)) +
    theme(legend.position = "none") +
    xlab(colX) +
    ylab(colY)
  
  return(p)
}


get6_2DPlots <- function(db, givenCols, givenTitle) {
  # Create 6 plots:
  p1 <- getPlot(db,givenCols[1], givenCols[2])
  p2 <- getPlot(db,givenCols[3], givenCols[2])
  p3 <- getPlot(db,givenCols[1], givenCols[3])
  p4 <- getPlot(db,givenCols[4], givenCols[3])
  p5 <- getPlot(db,givenCols[1], givenCols[4])
  p6 <- getPlot(db,givenCols[2], givenCols[4])
  
  # Arrange 6 plots in one graph:
  plots <- list(p1,p2,p3,p4,p5,p6)
  return (grid.arrange(grobs = plots,ncol=2, top = givenTitle) )
}



# 6 plots in one figure:
get6_2DPlots(db = bankruptcyDB, 
             givenCols=selectedCols,
             givenTitle = "Original Database: Features with the Highest Standatd Deviation")

# Calculate Correlation--------------------------------------------------start--
calcCorr <- function(vec1,vec2) {
  # Calculates the correlation of two input vectors.
  # The output is a single number in range (-1,1).
  
  return (sum((vec1-mean(vec1))*(vec2-mean(vec2)))/
            sqrt(sum((vec1-mean(vec1))^2)*sum((vec2-mean(vec2))^2)) )
}

corrMat <- matrix(0, nrow = ncol(bankruptcyDB), ncol = ncol(bankruptcyDB))

for (i in 1:ncol(bankruptcyDB)) {
  for (j in i:ncol(bankruptcyDB)) {
    corrMat[i,j] <- calcCorr(bankruptcyDB[,i],bankruptcyDB[,j])
    corrMat[j,i] <- corrMat[i,j]
  }
  
}

colnames(corrMat) <- names(bankruptcyDB)
rownames(corrMat) <- names(bankruptcyDB)

corr_df <- melt(corrMat)
colnames(corr_df) <- c("x", "y", "Correlation")

ggplot(corr_df, aes(x = x, y = y, fill = Correlation)) +
  geom_tile(color = "black") +
  scale_fill_gradient2(limits = c(-1, 1),
                       low = "blue",
                       mid = "white",
                       high = "red") +
  coord_fixed() +
  labs(x = "Feature ID", y = "Feature Name & ID") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

corr_df %>% 
  mutate(bin = cut(Correlation, 
                   breaks = c(-1, seq(-0.8, 0.8, by = 0.2), 1))) %>% 
  count(bin) %>%
  ggplot(aes(bin, n)) + 
  geom_col(color = "white", fill = "blue") +
  geom_text(aes(label = n), vjust = -0.5, hjust = 0.5, col = "red") +
  scale_y_log10() + 
  ggtitle("Distribtution of Cross-Correlations of the Features") +
  xlab("Correlation") +
  ylab("Frequency")

# Proper Orthogonal Decomposition (POD):---------------------------start--------
POD_results <- svd(as.matrix(bankruptcyDB[,2:ncol(bankruptcyDB)]))


# Normalizing the POD weights
normSigma <- POD_results$d/sum(POD_results$d)

print(cumsum(normSigma)[25])

# Make POD data frame
POD_df <- data.frame(matrix(cbind(bankruptcyDB[,1], 
                                  POD_results$u), 
                            ncol = ncol(bankruptcyDB)) )


colnames(POD_df) <- paste0("POD_", seq(0,ncol(bankruptcyDB)-1))

colnames(POD_df)[1] <- "label"

selectedCols <- c(2,3,4,5)

# Plot POD contribution
barplot(normSigma, names.arg = seq(1,length(normSigma)), 
        xlab = "Mode Number", ylab = "Contribution", las = 1, 
        col="green", main = "Contribution of Individual POD Modes in Total Variance")

# Plot POD accumulated contribution
barplot(cumsum(normSigma), names.arg = seq(1,length(normSigma)), 
        xlab = "Mode Number", ylab = "Accumulated Contribution", las = 1, 
        col="skyblue", main = "Accumulated Contribution of POD Modes in Total Variance")

# 6 plots of POD modes:
get6_2DPlots(db = POD_df, 
             givenCols=selectedCols,
             givenTitle = "Original Database: Features with the Highest Standatd Deviation")
# Proper Orthogonal Decomposition (POD):---------------------------End----------

# Print confusion matrix
confMat <- matrix(c("#TP", "#FP", "#FN", "#TN"),  nrow=2, byrow = TRUE)

colnames(confMat) <- c("Actual Positive","Actual Negative")
rownames(confMat) <- c("Predicted Positive","Predicted Negative")

knitr::kable(confMat,  
             caption = "Schematic of a Confusion Matrix", valign = 't') 

# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
# 3- ??Data Classification


# Keep required variables in memory and delete the rest of them:
keptVariables <- c("bankruptcyDB", "POD_df", "POD_results", 
                   "getPlot", "get6_2DPlots")
rm(list = ls()[!(ls() %in% keptVariables)])


performPredictingModel <- function(trainSet, testSet, model) {
  # Returns the trained model as well as the confusion matrix.
  # The model can be either "DT" for Decision Tree, 
  # or "SVM" for Support Vector Machine.
  
  # Convert label class into factor:
  trainSet$label <- as.factor(trainSet$label)
  testSet$label <- as.factor(testSet$label)
  
  # Train the model with training set:
  if (model == "DT") {
    predictingModel <- rpart(label ~ ., data = trainSet, model = TRUE,
                             minbucket = 5, cp = 0.01)
  } else if (model == "SVM") {
    predictingModel <- svm(label ~ ., data = trainSet, 
                           kernel = "sigmoid", type = 'C-classification',
                           cost = 0.5)
  }
  
  
  # Calculate prediction for test set:
  predictions <- predict(predictingModel, newdata = testSet, type = "class")
  
  # Build the confusion matrix
  confMat <- confusionMatrix(data = predictions, reference = testSet$label,
                             mode = "everything")
  
  
  # Build the outputs in a list:
  results <- list( predictingModel = predictingModel , confMat = confMat  )
  
  return( results )
  
}


# Divide datasets into train and test sets:
set.seed(1, sample.kind="Rounding") 

# original:
test_index <- createDataPartition(y = bankruptcyDB$label, 
                                  times = 1, p = 0.2, list = FALSE)

train_set_original <- bankruptcyDB[-test_index,]
test_set_original <- bankruptcyDB[test_index,]

# POD result:
test_index <- createDataPartition(y = POD_df$label, 
                                  times = 1, p = 0.2, list = FALSE)

train_set_POD <- POD_df[-test_index,]
test_set_POD <- POD_df[test_index,]



# Classification of Original Database with DT:
result_DT_Original <- performPredictingModel(train_set_original, 
                                             test_set_original,
                                             model = "DT")
# Classification of POD Database with DT:
result_DT_POD <- performPredictingModel(train_set_POD, 
                                        test_set_POD,
                                        model = "DT")

# Classification of Original Database with SVM:
result_SVM_original <- performPredictingModel(train_set_original, 
                                              test_set_original,
                                              model = "SVM")
# Classification of POD Database with SVM:
result_SVM_POD <- performPredictingModel(train_set_POD, 
                                         test_set_POD,
                                         model = "SVM")

# Prepare confusion matrix for models to show them as a table:

confMat_DT_Origin <- result_DT_Original$confMat$table
confMat_DT_POD <- result_DT_POD$confMat$table
confMat_SVM_Origin <- result_SVM_original$confMat$table
confMat_SVM_POD <- result_SVM_POD$confMat$table

colnamesNew <- c("Actual 0","Actual 1")
rownamesNew <- c("Predicted 0","Predicted 1")

colnames(confMat_DT_Origin) <- colnamesNew
rownames(confMat_DT_Origin) <- rownamesNew

colnames(confMat_DT_POD) <- colnamesNew
rownames(confMat_DT_POD) <- rownamesNew 

colnames(confMat_SVM_Origin) <- colnamesNew
rownames(confMat_SVM_Origin) <- rownamesNew 

colnames(confMat_SVM_POD) <- colnamesNew
rownames(confMat_SVM_POD) <- rownamesNew 


knitr::kable(confMat_DT_Origin, 
             caption = "Confusion Matrix:DT-Original", valign = 't') 

knitr::kable(confMat_DT_POD, 
             caption = "Confusion Matrix:DT-POD", valign = 't') 

knitr::kable(confMat_SVM_Origin, 
             caption = "Confusion Matrix:SVM-Original", valign = 't') 


knitr::kable(confMat_SVM_POD,  
             caption = "Confusion Matrix:SVM-POD", valign = 't') 

# Returns the metrics of the predicting model resulted from confusion matrix
getModelInfo <- function(confMat) {
  # Returns the metrics of the predicting model resulted from confusion matrix
  Accuracy <- confMat$overall[[1]]
  Specificity <- confMat$byClass[[2]]
  Precision <- confMat$byClass[[5]]
  Recall <- confMat$byClass[[6]]
  F1 <- confMat$byClass[[7]]
  
  return(list(Accuracy = Accuracy,
              Specificity = Specificity,
              Precision = Precision,
              Recall= Recall,
              F1 = F1))
}


# Create a matrix of all metrics
metricsMat <- matrix(c(unlist(getModelInfo(result_DT_Original$confMat)),
                       unlist(getModelInfo(result_DT_POD$confMat)),
                       unlist(getModelInfo(result_SVM_original$confMat)),
                       unlist(getModelInfo(result_SVM_POD$confMat))
), nrow = 4, byrow = TRUE )

colnames(metricsMat) <- names(unlist(getModelInfo(result_DT_Original$confMat)))
rownames(metricsMat) <- c("DT-Original", "DT-POD", "SVM-Original", "SVM-POD")

# Show table of metrics
knitr::kable(metricsMat, 
             caption = "Metrics of Predicting Models", valign = 't') 

# Show schematic of DT model for original
rpart.plot(result_DT_Original$predictingModel,
           type=2,extra="auto",under=TRUE,fallen.leaves=FALSE,cex=0.7,
           main="Schematic of Decision Tree for Original Database")

# Show schematic of DT model for original
rpart.plot(result_DT_POD$predictingModel,
           type=2,extra="auto",under=TRUE,fallen.leaves=FALSE,cex=0.7,
           main="Schematic of Decision Tree for POD Results")

# Give the most important features:
importantFeatureOrigin <- names(result_DT_Original$predictingModel$variable.importance[1:6])
importantFeaturePOD <- names(result_DT_POD$predictingModel$variable.importance[1:6])
importantFeatures <- matrix( c(importantFeatureOrigin,importantFeaturePOD)
                             , nrow = 2, byrow = TRUE )

colnames(importantFeatures) <- c("1st", "2nd", "3rd", "4th", "5th", "6th")
rownames(importantFeatures) <- c("Original", "POD")

knitr::kable(importantFeatures, 
             caption = "Most Important Features for Decision Tree Model", valign = 't') 


featuresCols_Origin <- match(importantFeatureOrigin, names(bankruptcyDB))[1:4]
featuresCols_POD <- match(importantFeaturePOD, names(POD_df))[1:4]

get6_2DPlots(db = bankruptcyDB, 
             givenCols=featuresCols_Origin,
             givenTitle =  "Most Important Features of DT Model: Original Database")


get6_2DPlots(db = POD_df, 
             givenCols=featuresCols_POD,
             givenTitle =  "Most Important Features of DT Model: POD Database")


