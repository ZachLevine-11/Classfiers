#' Calculate the Euclidean Distance between two points
#'
#' Used in the K-nearest neighbour classification algorithm to quantify the difference between measurements. The algorithm calculates the euclidean distance between all points and classifies unlabelled data according to the label of data it is closest to in the Euclidean plane.
#'
#' @param a One of two points to calculate the Euclidean distance between.
#' @param b One of two points to calculate the Euclidean distance between.

#' @examples euclidean_dist(a, b)
euclidean_dist <- function(a, b){
  euclideanDistance <- sqrt(sum((a-b)^2))
  euclideanDistance
}

#' Calculate KNN accuracy.
#'
#' Used to benchmark the accuracy of the K-nearest neighbour classification algorithm.
#'
#' @param testData A tibble or dataframe object with the classes of each observation labelled "varInterest", and the predictions labelled as "predictionColumn". The \code{knn_predict} function returns a test dataset of this form.

#' @examples knn_accuracy(knn_predict(tbl, varInterest, k_value))
knn_accuracy <- function(testData){
  correct = 0
  for(i in c(1:nrow(testData))){
    if ( is.na(testData[i,"varInterest"]) == FALSE && is.na(testData[i,"predictionColumn"]) == FALSE){
      if(testData[i,"varInterest"] == testData[i,"predictionColumn"]){
        correct = correct+1
      }
    }
  }
  accuracy = correct/nrow(testData) * 100
  return(accuracy)
}

#' Classify and predict categorical variables using machine learning.
#'
#' Using the K-nearest neighbours supervised machine learning algorithm, predict categorical variables of interest. Each unlabelled data point is labbeled according to the label of its k-closest neighbours in the Euclidean Plane.
#'
#' @param tbl The tibble containing the dataset.
#' @param varInterest The variable to predict from the dataset. This should be the column index that attribute is stored at, formatted as an integer.
#' @param k_value Set to 2the square root of the number of measurements by default.
#' @examples knn_predict(tbl = surveyData, varInterest = 74)
#' @return \code{testTbl} A tibble containing only the numerical attributes of the input tibble, as well as two new columns. \code{"varInterest"} holds the true classification of each observation, and \code{"prediction"} contains the predicted values. The output, \code{testTbl}, can be passed as an input to the \code{accuracy} function for validation of accuracy.
knn_predict <- function(tbl = surveyData, varInterest, k_value = sqrt(length(tbl))){
  set.seed(25) #Allows us to generate random numbers with reproducible results.

  #Prepare the dataset for machine learning
  extractNumeric <- dplyr::select_if(tbl, is.numeric) #We will only be training from columns with quantifiable and useful numerical information. The first step is to eliminate any columns that don't have numerical information.
  extractedPredict <- tbl[varInterest] #Keep the column of interest though. We still need to predict this.
  tbl <- extractNumeric

  #Drop rows that confuse the Euclidean Distance function.
  tbl <- tbl[ c(3, 8, 9, 10, 11, 16, 17, 18, 19, 20, 21, 22, 23, 24) ] #Keep columns that contain data we can use.

  tbl["varInterest"] <- extractedPredict #Restore the column containing the variable of interest.
  prediction <- c()

  #Prepare the indices to split the dataset randomly between training and testing data.
  trainIndex <- sample(1:nrow(tbl), 0.8 * nrow(tbl))
  testIndex <- setdiff(1:nrow(tbl), trainIndex)

  #Split the data as 80% training data and 20% test data.
  trainTbl <- tbl[trainIndex,]
  testTbl <- tbl[testIndex,]

  #Loop 1: looping over the testing data.
  for(i in c(1:nrow(testTbl))){ #Loop over the test data. For each training point, append the Euclidean distance and classification of that point to two separate atomic vectors.
    euclideanDist =c()          #These should be empty at the begining of each loop.
    trainingClass = c()

    #Loop 2 looping over training data .
    for(j in c(1:nrow(trainTbl))){
      euclideanDist <- c(euclideanDist, euclidean_dist(testTbl[i,], trainTbl[j,]))   #Append Euclidean distance between the test data point and training data to the euclideanDist vector.
      trainingClass <- c(trainingClass, as.character(trainTbl[j,][["varInterest"]]))   #Adding the classification of each training data point in the trainingClass vector.
    }

    classDistance <- data.frame(euclideanDist, trainingClass)   #classDistance dataframe created with columns euclideanClass and  euclideanDist.
    classDistance <- classDistance[order(classDistance$euclideanDist),]       #Sort the classDistance dataframe to find the closest K neighbors.
    classDistance <- classDistance[1:k_value,]    #Reduce the classDistancedataframe to only the closest K neighbors.

    #Count the freqency of the classes of the nearest neighbhors.
    classFrequencies <- plyr::count(df = classDistance, vars = "trainingClass")

    mostNeighbours <- max(classFrequencies$freq) #Find the class that occurs most frequently among the closest K neighbours
    singlePrediction <- toString(classFrequencies[classFrequencies$freq == mostNeighbours, "trainingClass"])
    prediction <- c(prediction, singlePrediction) #Add the prediction to the prediction vector for accuracy testing.

  }
  testTbl$predictionColumn <- prediction
  testTbl
}
