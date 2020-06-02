#' Split a dataset into k - folds, or sections.
#'
#' Used to cross-validate the Random Forest machine learning algorithm, this function splits an input tibble into k randomly generated folds, or partitions.
#'
#' @param tbl The tibblecontaining the dataset.
#' @param kValue The number of folds to split the dataset into.
#' @examples k_fold_split(tbl = surveyData, kValue = 5)
#' @return folds An atomic vector containing k unique and randomly generated folds, or sections of the input dataset, \code{tbl}.
k_fold_split <- function(tbl = surveyData, kValue){
  folds <- rep(dplyr::tibble(0), kValue) #Instantiate the list of folds we will be returning.
  foldSize <- as.integer(nrow(tbl) / kValue) #The size of each fold
  chooseFromRows <- sample(nrow(tbl)) #Generate a random sample of indices to chose rows based on
  foldsCompleted <- 1
  #Loop once for each fold.
  while (foldsCompleted < kValue +1){#Put foldSize observations from the tbl dataset into each fold.
    chooseIndices <- sample(chooseFromRows, foldSize, replace = FALSE) #Generate a random list of indices to chose rows from
    fold <- tbl[chooseIndices,] #Create a fold with those random indices
    folds[[foldsCompleted]] <- fold #Add that fold to the atomic vector
    foldsCompleted <- foldsCompleted + 1 #Update the counter at the end of each loop.
  }
  return(folds)
}

#' Calculate random forest accuracy.
#'
#' Used to benchmark the accuracy of the random forest classification algorithm.
#'
#' @param predictions An array holding the predictions from the algorithm.
#' @param actualValues An array holding the variable's actual values.
#' @examples forest_accuracy()
forest_accuracy <- function(predictions, actualValues){
  compareCounter <- 1
  amountCorrect <- 0
  missingPredictions <- 0
  while (compareCounter < length(predictions) + 1){
    if (is.na(actualValues$varInterest[compareCounter]) == TRUE){
      compareCounter <- compareCounter + 1
      missingPredictions <- missingPredictions + 1
    }
    else{
      if (predictions[compareCounter] == actualValues$varInterest[compareCounter]){
        amountCorrect <- amountCorrect + 1
      }
      compareCounter <- compareCounter + 1
    }
  }
  accuracy <- (amountCorrect/ (length(actualValues)- missingPredictions))
  return(accuracy)
}

#' Cross-validate algorithms within the random forest.
#'
#' Used to benchmark the accuracy of different algorithms within the random forest classification algorithm.
#'
#' @param tbl The tibble containing the dataset.
#' @param algorithm The algorithm from the random forest to score.
#' @param Nfolds The number of folds to cross validate the algorithm with.
#' @examples evaluate_cross_val_split(tbl = surveyData, varInterest = 10, algorithm, Nfolds = 5)
#' @return \code{accuracies}, The accuracies of the predictions for each fold.
evaluate_cross_val_split <- function(tbl = surveyData, algorithm, Nfolds, maxDepth, minSize, sampleSize, Ntrees, Nfeatures){
  folds <- k_fold_split(tbl, Nfolds)
  foldSize <- as.integer(nrow(tbl) / Nfolds)
  accuracies <- c()
  foldCounter <- 1
  predictionsMatrix <- matrix(vector(), nrow = Nfolds, ncol = foldSize)
  #Loop over each fold
  while (foldCounter <= length(folds)){ #Iterate over each fold. For each fold, create a test tibble containing the fold, and a training tibble containing all other folds.
    allButFold <- folds[-foldCounter] #Select only folds that are not the current one.
    trainSet <- dplyr::bind_rows(allButFold) #Create a training set for each fold: a tibble with all folds other than the current one.
    testSet <- folds[foldCounter] #The test set is the fold itself.

    predictions <- algorithm(trainSet, testSet, maxDepth, minSize, sampleSize, Ntrees, Nfeatures) #Run the algorithm and save the result.

    predictionsColumnCounter <- 1
    while (predictionsColumnCounter < length(predictions) + 1){
      predictionsMatrix[foldCounter,predictionsColumnCounter] <- predictions[[predictionsColumnCounter]]
      predictionsColumnCounter <- predictionsColumnCounter + 1
    }

    trueValues <- testSet[[1]]
    accuracy <- forest_accuracy(predictions, trueValues) #Obtain algorithm accuracy.
    accuracies <- c(accuracies, accuracy) #Add the accuracy to the accuracies atomic vector.

    foldCounter <- foldCounter + 1
  }

  evaluation <- data.frame("Fold" = 1:Nfolds, "Prediction" = predictionsMatrix, "PredictionAccuracy" = accuracies)
  return(evaluation) #Return both the predictions and the accuracies for each fold
}

#' Split a dataset based on the values of an attribute.
#'
#' Used heavily in the random forest algorithm, this function splits a dataframe into two other dataframes based on their values of particular attributes, and returns both dataframes as an atomic vector.
#'
#' @param tbl The tibble to containing the dataset.
#' @param value The value to sort the column by.
#' @examples test_split(columnIndex = 10, value = 2, tbl = surveyData)
#' @return \code{leftAndRight}, a list vector containing two tibbles, left (which holds the rows that had a columnIndex less than the specified value), and right (which holds the rows that had a columnIndex greater than, or equal to the specified value).
#'
test_split <- function(columnIndex, value, tbl = surveyData){
  rowCounter <- 1
  leftIndices <- c()
  rightIndices <- c()
  while (rowCounter <= nrow(tbl)){
    if (is.na(tbl[rowCounter,columnIndex]) == FALSE){
      if (as.integer(tbl[rowCounter, columnIndex]) < as.integer(value)){
        leftIndices <- c(leftIndices, rowCounter)
      }
      if (as.integer(tbl[rowCounter, columnIndex]) >= as.integer(value)){
        rightIndices <- c(leftIndices, rowCounter)
      }
    }
    rowCounter <- rowCounter + 1
  }
  left <- tbl[leftIndices,] #The left tibble contains all rows where the attribute is less than value.
  right <- tbl[rightIndices,] #The right tibble contains all rows where the attribute greater than or equal to the value.
  leftAndRight <- list(left, right) #Create the leftAndRight vector.
  return(leftAndRight)
}


#' Calculate the Gini coefficient for a particular split point.
#'
#' The Gini coefficient for split points (columnIndex, value) is the scoring function of this random forest algorithm. This function is most useful when combined with \code{test_split}. The coefficient was created by Italian statistician Corrado Gini and published in 1912.
#'
#' @param splitTibbles A list vector containing two tibbles. Each tibble should be a partition of another tibble based on a columnIndex and value. The output from \code{test_split}, is an atomic vector of this form.
#' @examples gini_index(splitTibbles = someSplitVector)
#' @return \code{gini}, the weighted Gini coefficient for a particular split.
gini_index <- function(splitTbls){
  #Aggregate the split tibbles to find the unique values, or classes of the variable of interest.
  wholeTbl <- dplyr::bind_rows(splitTbls)
  classesAsOne <- unique(as.list(wholeTbl["varInterest"])) #Grab the unique values of the variable of interest. The format will need to be corrected though, as the unique values are stored in one element of a list.

  #Loop once to obtain a list of only unique values of the variable of interest.
  classes <- c()
  for (class in classesAsOne[[1]]){
    classes <- c(classes, class)
  }
  classes <- classes[!is.na(classes)]
  Nclasses <- length(classes)
  Nrows <- nrow(wholeTbl)

  #If either dataframe is empty (the left dataframe can be for values of 0, for instance), set the Gini index to be high enough that this split will never be chosen.
  if  (dim(splitTbls[[1]])[[1]] == 0 || dim(splitTbls[[2]])[[1]] ==  0){
    gini <- 999
    return(gini)
  }

  #For each split of numerical value and attribute, calculate the Gini index for the variable of interest.

  #Starting with the Gini coefficient for the left tibble.
  left <- splitTbls[[1]]$varInterest
  left <- left[!is.na(left)]
  left <- as.list(left)
  giniLeft <- 0
  scoreLeft <- 0
  sizeLeft <- length(left)
  loopCounter <- 1
  while (loopCounter < length(classes)){#Loop over each class for the left tibble.
    classCount <- sum(toString(left) == classes[[loopCounter]]) #Count the number of times that class occurs.
    classSizeCount <- classCount / sizeLeft
    scoreLeft <- scoreLeft + classSizeCount ^ 2
    loopCounter <- loopCounter + 1
    #Weight the Gini coefficient based on the size of the left tibble. We want to favour splits with higher row counts in their groups, and so the smaller the row count, the larger the Gini.
  }

  giniLeft <- (1.0 - scoreLeft) / (sizeLeft / Nrows) #Weight the Gini coefficient based on the size of the left tibble.

  #Now to calculate the Gini coefficient for the right tibble.
  right <- splitTbls[[2]]$varInterest
  right <- right[!is.na(right)]
  right <- as.list(right)
  giniRight <- 0
  scoreRight <- 0
  sizeRight <- length(right)
  loopCounter <- 1
  while (loopCounter < length(classes)){#Loop over each class for the right tibble.
    classCount <- sum(toString(right) == classes[[loopCounter]]) #Count the number of times that class occurs.
    classSizeCount <- classCount / sizeRight
    scoreRight <- scoreRight + classSizeCount ^ 2
    loopCounter <- loopCounter + 1
    #Weight the Gini coefficient based on the size of the right tibble.
  }
  giniRight <- (1.0 - scoreLeft) / (sizeLeft / Nrows) #Weight the Gini coefficient based on the size of the right tibble. We want to favour splits with higher row counts

  #Return the overall Gini coefficient for the split.
  gini <- giniLeft + giniRight
  return(gini)
}


# 'Find the best split point for a dataset.
#'
#' Brute force all combinations of index and value to find the split that results in the most equal split of the variable of interest. The Gini coefficient is used as the scoring function.
#'
#' @param tbl The tibble containing the dataset.
#' @param Nfeatures The number of features to evaluate, formatted as an integer.
#' @examples get_split(tbl = surveyData).
get_split <- function(tbl, Nfeatures){
  classes <- as.list(unique(tbl["varInterest"]))
  Nclasses <- length(classes)
  Nfeatures <- length(tbl)

  #Instantiate these to arbitrarily high numbers that won't cause integer overflow.
  optimalIndex <- 999
  optimalValue <- 999
  optimalScore <- 999
  optimalGroups <- "None"
  splitVar <- FALSE

  randomIndices <- sample(Nfeatures) #Select from the tibble randomly by selecting indices randomly.
  #Try every combination of feature and value for that feature to find the one with the lowest Gini coefficient, representative of the most homogenous split of the variable of interest.
  for (index in randomIndices){
    for (row in tbl){
      if (is.na(row[index]) == FALSE && index != 15 ){ #Do not try spliting by missing values or the variable of interest, which sits at column 15.
        split <- test_split(index, value = row[index], tbl)
        gini <- gini_index(split)
        if (gini != 0){
          if (gini < optimalScore){
            #Find the value and column index that holds that attribute in the dataset that produces the best Gini coefficient.
            optimalIndex <- index
            optimalValue <- row[index]
            optimalScore <- gini
            optimalGroups <- split
            splitVar <- TRUE
          }
        }
      }
    }
  }
  result <- c("index" = optimalIndex, "value" = optimalValue, "score" = optimalScore, "groups" = optimalGroups, "split" = splitVar, "left" = "none" , "right" = "none" )
  return(result)
}

#' Create a terminal node value
#'
#' Returns the most common class of the variable of interest Used in the random forest machine learning algorithm.
#'
#' @param group A list of of split dataframes.
#' @examples to_terminal(someGroup)
#' @return The most common class of the variable of interest.
to_terminal <- function(group){
  outcomes <- group$varInterest
  #Return a table of the most frequent classes
  outcomesFreq <- sort(table(outcomes),decreasing=TRUE)
  #That table is an atomic vector, sorted with the most frequent class as a key to the frequency of that class. Return that class as the prediction.
  prediction <- names(outcomesFreq)[[1]]

  #Make sure that integers stay integers and strings stay strings.

  if (is.na(as.double(prediction)) == TRUE){ #If we're dealing with a string variable, leave the prediction as it is
  }
  else{ #If the prediction should be a number, convert it to one.
    prediction <- as.double(prediction)
  }
  return(prediction)
}

#' Recursively create child splits for a node or make it terminal.
#'
#' Either create child splits for a node or make it a terminal node(leaf).
#'
#' @param node An atomic vector node.
#' @param maxDepth The maximum depth of recursion to reach.
#' @param minSize The minimum size of a binary tree
#' @param depth The depth recursion that a node is situated at.
#' @examples split(thisNode, maxDepth, minimumSize, NfeaturesTree, thisDepth)
#' @return The root node of the decision tree.
split <- function(node, maxDepth, minSize, Nfeatures, depth){

  #We do not want any missing values to propagate into the forest.
  if (is.na(node[["left"]]) == TRUE || is.na(node[["right"]]) == TRUE){
    return("Missing Value")
  }

  #Indicating that there is no further split available. These cases indicate that the child node should be a terminal one.
  if ((nrow(node[["groups1"]]) == 1 && nrow(node[["groups2"]]) == 1 )){
    node[["left"]] <- to_terminal(dplyr::bind_rows(node[["groups1"]], node[["groups1"]]))
    node[["right"]] <- to_terminal(dplyr::bind_rows(node[["groups1"]], node[["groups1"]]))
    newRootNode <- node
    return(newRootNode)
  }
  right <- node[["groups2"]]
  left <- node[["groups1"]]
  if (depth > maxDepth){ # Indicating that the max depth has been reached
    node[["left"]] <- to_terminal(left)
    node[["right"]] <- to_terminal(right)
    newRootNode <- node
    return(newRootNode)
  }

  # Process the left child
  if (dim(node[["groups1"]])[[1]] != 0){#Dont split the left dataframe if it is empty
    if (nrow(left) <= minSize){
      node[["left"]] <- to_terminal(left)
    }
    else{
      leftSplit <- get_split(left, Nfeatures)
      if (leftSplit[["split"]] == "FALSE"){ #If there isn't a split available
        node[["left"]] <- to_terminal(dplyr::bind_rows(node[["groups1"]], node[["groups1"]])) #Make the child node terminal
      }
      else{
        node[["left"]] <- split(node = leftSplit, maxDepth, minSize, Nfeatures, depth+1)
      }
    }
  }

  # Process the right child
  if (dim(node[["groups2"]])[[1]] != 0){#Dont split right dataframe if it is empty
    if (nrow(right) <= minSize){
      node[["right"]] <- to_terminal(right)
    }
    else{
      rightSplit <- get_split(right, Nfeatures)
      if (rightSplit[["split"]] == "FALSE"){ #If there isn't a split available
        node[["right"]] <- to_terminal(dplyr::bind_rows(node[["groups1"]], node[["groups1"]])) #Make the child node terminal
      }
      else{
        node[["right"]] <- split(node = rightSplit, maxDepth, minSize, Nfeatures, depth+1)
      }
    }
  }
  newRootNode <- node
  return(newRootNode)
}

#' Build a decision tree, randomly.
#'
#' A description
#'
#' @param train The training dataset
#' @param minSize The minimum size of a tree
#' @param Nfeatures The number of splits per tree.
#' @examples tree <- build_tree(trainSet, maxDepthNumber, MinumumSize, NumberFeatures)
#' @return The root node of the decision tree.
build_tree <- function(train, maxDepth, minSize, Nfeatures){
  rootNode <- get_split(train, Nfeatures)
  #Only split if the rootnode has no empty tibbles in its group
  newRootNode <- split(node = rootNode, maxDepth = maxDepth, minSize = minSize, Nfeatures = Nfeatures, depth = 1)
  if (newRootNode == "Missing Value"){
    return(rootNode)
  }
  else{
    return(newRootNode)
  }
}

#' Predict the class (value) of the variable of interest for a single row.
#'
#' Make a prediction row a row based on a single decision tree
#'
#' @param node An atomic vector node returned from get_split.
#' @param row A row of the testing dataset.
#' @examples predict (thisNode, thisRow)
#' @return The predicted class of the variable of interest for that row of the dataset.
predict <- function(node, row){
  if (is.na(row[node[["index"]]]) == TRUE){
    return("Missing")
  }
  #Catch missing values in the row we're predicting. If there is a missing value, that row will be predicted to have a missing value.

  #The prediction is the left child
  else if (row[node[["index"]]] < node[["value"]]){
    if (class(node[["left"]]) !=  "character"){ #If there is a child node
      return(predict(node[["left"]], row)) #Predict that for child node
    }
    else{
      return(node[["left"]])
    }
  }

  #The prediction is the right child
  else{
    if (class(node[["right"]]) != "character"){
      return(predict(node[["right"]], row))
    }
    else{
      return(node[["right"]])
    }
  }
}

#' Sample a tibble randomly.
#'
#' Randomly sample from a tibble. Used in the random forest algorithm.
#'
#' @param tbl The tibble containing the dataset.
#' @param ratio The ratio of the tibble the sample, formatted as an integer or floating point number.
#' @examples subsample(tbl, 3/4)
#' @return a tibble containing the random sample of the dataset
subsample <- function(tbl, ratio){
  Ntotal <- nrow(tbl)
  Nsample <- as.integer(nrow(tbl) * ratio)
  sampleIndices <- sample(Ntotal, Nsample)
  sample <- tbl[sampleIndices,]
  return (sample)
}

#' Predict the value of a variable using tree bootstrap aggregation (bagging).
#'
#' Prediction algorithm used in the random forest algorithm.
#'
#' @param trees A list of trees.
#' @param row A row of in the dataset.
#' @examples bagging_predict(treesList, thisRow)
#' @return A prediction.
bagging_predict <- function(trees, row){
  predictions <- c()
  for (tree in trees){
    if (tree == "Missing Value"){#Catch missing trees
      predictions <- c(predictions, "Missing")
    }
    else{
      predictions <- c(predictions, predict(tree, row))
    }
  }
  prediction <- (sort(table(predictions),decreasing=TRUE)[1])
  return(prediction)
}

#' Random forest machine learning algorithm
#'
#' Main algorithm for machine learning. Pulls all functions together to predict the classes "ofunseen" data.
#'
#' @param train The training dataset.
#' @param test The testing dataset
#' @param MaxDepth The maximum depth each decision tree is allowed to reach, an integer.
#' @param minSize The minimum size of each decision tree, an integer.
#' @param sampleSize The ratio to sample for each decision tree, a fraction.
#' @param Ntrees The size of the forest to generate, specified in the number of trees, an integer.
#' @param Nfeatues The number of features to evaluate for each tree, an integer.
#' @return A result set, containing predictions and the accuracy of those predictions.
#' @examples bagging_predict(treesList, thisRow).
#' @return An atomic vector containing predictions.
random_forest <- function(train, test, maxDepth, minSize, sampleSize, Ntrees, Nfeatures){
  test <- test[[1]]
  treesCounter <- 1
  trees <- rep(dplyr::tibble(0), Ntrees) #Generate the forest (list) of trees we will be using.

  #Build the forest
  while (treesCounter < Ntrees + 1){
    sample <- subsample(train, sampleSize)
    #Build a single decision tree
    tree <- build_tree(sample, maxDepth, minSize, Nfeatures)

    #Catch trees with missing values
    if (tree == "Missing Value"){
      trees[[treesCounter]] <- "Missing Value"
      treesCounter <- treesCounter + 1
    }
    else{ #Add that tree to the forest
      trees[[treesCounter]] <- tree
      treesCounter <- treesCounter + 1
    }
  }
  #Predict
  predictions <- rep(0, nrow(test))
  TestrowCounter <- 1
  while (TestrowCounter <= nrow(test)){
    prediction <- bagging_predict(trees, row = test[TestrowCounter,])
    prediction <- names(prediction)
    predictions[[TestrowCounter]] <- prediction
    TestrowCounter <- TestrowCounter + 1
  }
  return(predictions)
}

#' Prepare a dataset for machine learning.
#'
#' Drop all non-numeric rows, and drop all rows that do not contain real number data.
#'
#' @param tbl The tibble containing the dataset.
#' @param varInterest The column index of the attribute of interest in the surveyData tibble.
#' @return The tibble of interest, ready for machine learning.
#' @examples prepare_dataset(tbl = surveyData)
prepare_dataset <- function(tbl = surveyData, varInterest){
  #Prepare the dataset for machine learning
  extractNumeric <- dplyr::select_if(tbl, is.numeric) #We will only be training from columns with quantifiable and useful numerical information. The first step is to eliminate any columns that don't have numerical information.
  extractedPredict <- tbl[varInterest] #Keep the column of interest though. We still need to predict this.
  tbl <- extractNumeric
  #Drop rows that confuse the ML algorithm.
  tbl <- tbl[ c(3, 8, 9, 10, 11, 16, 17, 18, 19, 20, 21, 22, 23, 24) ] #Keep columns that contain data we can use.

  tbl["varInterest"] <- extractedPredict #Restore the column containing the variable of interest.
  return(tbl)
}

#' A automated pipeline for the random forest Machine Learning model
#'
#' Drop all non-numeric rows, and drop all rows that do not contain real number data. Then run the random forest algorithm
#'
#' @param tbl The tibble containing the dataset.
#' @param varInterest The column index of the attribute of interest in the surveyData tibble.
#' @param setSeed A boolean (TRUE/FALSE) indicating whether one would like a seed to be set (for reprodicible random results)
#' @param Nfolds The number of folds to split the dataset into for validation.
#' @param MaxDepth The maximum depth each decision tree is allowed to reach, an integer.
#' @param minSize The minimum size of each decision tree, an integer.
#' @param sampleSize The ratio to sample for each decision tree, a fraction.
#' @param Nfeatues The number of features to evaluate for each tree, an integer.
#' @param Ntrees The size of the forest to generate, specified in the number of trees, an integer.
#' @param sampleSize The ratio to sample for each decision tree.
#' @return \code{finalScore}, The accuracies of the predictions for each fold.
#' @examples random_forest_pipeline(tbl = surveyData, varInterest = 10)
random_forest_pipeline <- function(tbl = surveyData, varInterest, setSeed = FALSE, Nfolds = 5, maxDepth = 10, minSize = 1, sampleSize = 1, Nfeatures = sqrt(length(tbl[[1]])), Ntrees = 10){
  #Prepare data
  newData <- prepare_dataset(tbl, varInterest)
  if (setSeed == TRUE){
    set.seed(20)
  }
  #Run the algorithm
  finalScore <- evaluate_cross_val_split(tbl = newData, random_forest, Nfolds, maxDepth, minSize, sampleSize, Ntrees, Nfeatures)
  return(finalScore)
}
