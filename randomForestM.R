
randomForestM <- function(fullDataFrame, numberOfTrees, methodCode) {
	# initialize a list of numberOfTrees matrices
	ForestList <- vector("list", numberOfTrees)
	# make a whole bunch of trees and put them in the list
	for (i in 1:numberOfTrees) {
		#cat("start building tree number ",i,"\n")
		ForestList[[i]] <- buildATree(fullDataFrame, methodCode)
	}
	#return the list
	return(ForestList)
}


buildATree <- function(data, methodCode) {
	#cat("inside the recursive wrapper","\n")
	#initialize the treeMatrix
	treeMatrix <- matrix(nrow=1,ncol=6)
	isRoot <- TRUE
	iterationCount <- 0

	RecursiveBuildTree <- function(dataToSlice,parentRow,leftRight) {	
		iterationCount <<- iterationCount+1
		#cat("inside the actual recursive function ",iterationCount,"\n")
		
		#initialize a 1 row matrix to hold new data for this node
		newRow <- matrix(nrow=1,ncol=6)
		
		#cat("data to slice",dataToSlice,"\n")		
		#cat("parent row", parentRow,"\n")
		#cat("left right", leftRight,"\n")
		
		currentRowNumber <- nrow(treeMatrix)+1	
	
		#determine if this is the root or a leaf
		if (parentRow==0) {
			isRoot <<- TRUE
			currentRowNumber <- 1
		}
		else { 
			isRoot <<- FALSE
			currentRowNumber <- nrow(treeMatrix)+1	
			#################################
			#if this node is a leaf, return
			#assign left and right children the value of 0 or 1
			#################################
			if (length(which(dataToSlice[,ncol(dataToSlice)] == 0))==0) {
				#1 leaf - populate everything and return
				newRow[3] <- FALSE
				newRow[4] <- parentRow
				newRow[5] <- 1
				newRow[6] <- 1
				treeMatrix <<- rbind(treeMatrix, newRow)
				currentRowNumber <- nrow(treeMatrix)
				if (leftRight==1) { 				#this was a left side call
					treeMatrix[parentRow, 5] <<- currentRowNumber
				} else if (leftRight==2) {			#this was a right side call
					treeMatrix[parentRow, 6] <<- currentRowNumber
				}
				return(1)
			}
			else if (length(which(dataToSlice[,ncol(dataToSlice)] == 1)) == 0) {
				#0 leaf - populate everything and return
				newRow[3] <- TRUE
				newRow[4] <- parentRow				
				newRow[5] <- 0
				newRow[6] <- 0				
				treeMatrix <<- rbind(treeMatrix, newRow)
				currentRowNumber <- nrow(treeMatrix)
				#cat("current row number inside a leaf: ",currentRowNumber,"\n")
				
				if (leftRight==1) { 				#this was a left side call
					treeMatrix[parentRow, 5] <<- currentRowNumber	#should be current row number
				} else if (leftRight==2) {			#this was a right side call
					treeMatrix[parentRow, 6] <<- currentRowNumber	 #should be current row number
				}
				return(1)
			}
		}
		
		#calculate an M
		MforNode <- DetermineM(ncol(dataToSlice)-1, methodCode)
		#cat("M = ",MforNode,"\n")
		
		#create new data for the function
		applyMlist <- ApplyM(dataToSlice, MforNode)
		newDataToSlice <- applyMlist[[1]]
		vectorOfColumnsUsed <- applyMlist[[2]]
		#print(newDataToSlice)
		
		#cat("we have the new data \n")
		
		#find the split
		bestSplitData <- FindBestSplit(newDataToSlice)
		#print(bestSplitData)
		
		variable <- bestSplitData[[1]] #this is which column the splitting function thinks
		variable <- vectorOfColumnsUsed[variable] #this is which column in the original data
		#print(variable)
		splitValue <- bestSplitData[[2]]
		#print(splitValue)
		isLeftMoreZero <- bestSplitData[[3]]
		#print(isLeftMoreZero)
		
		#assign these results and add the next row
		newRow[1] <- variable
		newRow[2] <- splitValue
		newRow[3] <- isLeftMoreZero
		newRow[4] <- parentRow
		#newRow[5] <-  #left child populated later leave as NA
		#newRow[6] <-  #right child populated later leave as NA
		#add this row to the treeMatrix
		treeMatrix <<- rbind(treeMatrix, newRow)
		#if we are making the first split, delete the first row of the tree
		#which was all NAs
		
		#cat("tree matrix at %^&1 \n")
		#print(treeMatrix)
		
		currentRowNumber <- nrow(treeMatrix)
		
		if (isRoot==TRUE) {
			treeMatrix <<- treeMatrix[-1,]
			currentRowNumber <- 1
			#cat("FIRST ROW IS ROOT CURRENT ROW NUMBER IS: ",currentRowNumber,"\n")
		} else if (leftRight==1) { 			#this was a left side call
			#cat("lr1: parent row number: ",parentRow,"\n")
			#cat("lr1: current row number: ",currentRowNumber,"\n")
			treeMatrix[parentRow, 5] <<- currentRowNumber
		} else if (leftRight==2) {			#this was a right side call
			#cat("lr2: parent row number: ",parentRow,"\n")
			#cat("lr2: current row number: ",currentRowNumber,"\n")
			treeMatrix[parentRow, 6] <<- currentRowNumber
		}
		
		#cat("tree matrix at %^&2 \n")
		#print(treeMatrix)

		#split the data frame into two
		dataSplit <- SplitTheData(dataToSlice, variable, splitValue)
		leftSideData <- dataSplit[[1]]
		rightSideData <- dataSplit[[2]]
		
		#call the recursive function again with right and left sides
		left <- RecursiveBuildTree(leftSideData,currentRowNumber,1)
		right <- RecursiveBuildTree(rightSideData,currentRowNumber,2)
		
		return(1)
	}
	
	#call the root
	flag <- RecursiveBuildTree(data, 0, 0)
	#cat("outside of the recursive function \n")
	return(treeMatrix) 
}

######################################################################
######################################################################

DetermineM <- function(numberOfColumns, methodCode) {
	###############################################
	#methodCode: 	-X) if negative, make it positive and return that number for m
	#				1) uniform over 1 to total number of columns
	#				2) random normal with mean at the center and sd = 1/5
	#				3) uniform over the middle half (1/4 to 3/4)
	#				4) poisson with mean 1/3  <rpois(1, lambda=3)>
	#				5) poisson with mean 1/2
	#				6) rainbow - randomly choose one of the others
	###############################################
	
	if (methodCode < 0) { #useful for baseline comparison
		return(methodCode * -1)
	}
	
	if (methodCode == 6) {
		methodCode <- sample(1:5,1)
	}
	
	if (methodCode == 1) {
		mtemp <- sample(1:numberOfColumns,1)
	} else if (methodCode == 2) {
		mtemp <- as.integer(0.5 + rnorm(1,numberOfColumns/2,numberOfColumns/5))
	} else if (methodCode == 3) {
		mtemp <- sample(as.integer(0.5 + numberOfColumns/4):as.integer(0.5+ numberOfColumns*3/4),1)
	} else if (methodCode == 4) {
		mtemp <- as.integer(0.5 + rpois(1, lambda=(numberOfColumns/3)))	
	} else if (methodCode == 5) {
		mtemp <- as.integer(0.5 + rpois(1, lambda=(numberOfColumns/2)))	
	}
	
	#some of these distributions can go under 0 or over the max, so cover for that
	if (mtemp > numberOfColumns) { 
		return(numberOfColumns)
	} else if (mtemp < 2) { 
		return(2) 
	} else { 
		return(as.integer(mtemp)) 
	}
}

######################################################################
######################################################################

ApplyM <- function(data, M) {
	# pass along a vector with the column numbers
	# and when i'm ready to populate the tree matrix, take that entry of this vector
	
	#return a dataframe with only M columns chosen randomly
	#cat("inside applyM \n")
	columns <- sample(1:(ncol(data)-1),M,replace=FALSE)
	columns <- sort(columns, decreasing=FALSE)
	#print(columns)
		
	newData <- cbind(data[,columns],data[,ncol(data)])
	#print(newData)
	return(list(newData,columns)) #return m variables and the response
}


SplitTheData <- function(dataToSlice, variable, splitValue) { 
	#sort the data on this variable
	sortedData <- dataToSlice[order(dataToSlice[,variable]),]
	
	divide <- 1
	for (i in 1:nrow(sortedData)) {
		if (sortedData[i,variable] < splitValue) {
			divide <- i
		} else break
	}
	
	leftData <- sortedData[1:divide,]
	rightData <- na.omit(sortedData[divide+1:nrow(dataToSlice),])
	return(list(leftData,rightData))
}


FindBestSplit <- function(data) { 
	
	giniMatrix <- matrix(nrow=(nrow(data)-1),ncol=(ncol(data)-1))
	for (i in 1:(ncol(data)-1)) { # loop through each variable not the response
		#sort by the Ith variable
		sortedData <- data[order(data[,i]),]
		for (j in 1:(nrow(data)-1)) { #loop through each inter-row split
			#cut between row j and row j+1
			if (sortedData[j,i] != sortedData[j+1,i]) { #handles if they are the same
				splitValue <- mean(sortedData[j,i], sortedData[j+1,i])
				giniMatrix[j,i] <- CalculateGini(sortedData,i,splitValue)
			} else {
				giniMatrix[j,i] <- 1 #if its the same data, can't split here
			}
		}
		#cat("GINI MATRIX ",i,"\n")
		#print(giniMatrix)
	}
	#print(giniMatrix)
	
	
	#locate the minimum value in the gini matrix
	#if there are multiple, pick one at random
	minGini <- which(giniMatrix==min(giniMatrix),arr.ind=TRUE)
	minGiniIndex <- sample(1:nrow(minGini),1) 
	minRow <- minGini[minGiniIndex,1]
	#cat("min row: ",minRow,"\n")
	variable <- minGini[minGiniIndex,2]
	#cat("variable: ",variable,"\n")
	#sort on that variable
	sortedData <- data[order(data[,variable]),]
	splitValue <- mean(sortedData[minRow,variable], sortedData[minRow+1,variable])
	
	#count the number of 0s left of the split
	numZerosLeft <- length(which(sortedData[1:minRow,ncol(sortedData)] == 0))
	
	if (numZerosLeft > minRow/2) {
		isLeftMoreZero <- TRUE
	}else if (numZerosLeft <= minRow/2) {
		isLeftMoreZero <- FALSE
	} else {  
		#perfectly even, break ties arbitrarily
		randomFlag <- sample(1:2,1)
		if (randomFlag == 1) { isLeftMoreZero <- TRUE }
		else { isLeftMoreZero <- FALSE }
	}  
	
	#cat("variable: ",variable,"\n")
	#cat("splitValue: ",splitValue,"\n")
	#cat("isLeftMoreZero: ",isLeftMoreZero,"\n")

	return(list(variable, splitValue, isLeftMoreZero))
}


CalculateGini <- function(sortedData,variable,splitValue) {
	
	giniR <- as.double(0)
	giniL <- as.double(0)
	gini <- as.double(0)
	
	splitUpData <- SplitTheData(sortedData, variable, splitValue)
	leftData <- splitUpData[[1]]
	rightData <- splitUpData[[2]]
	
	numZerosLeft <- length(which(leftData[,ncol(leftData)] == 0))
	percentZerosLeft <- as.double(numZerosLeft/nrow(leftData))
	percentOnesLeft <- as.double(1-percentZerosLeft)
	numZerosRight <- length(which(rightData[,ncol(rightData)] == 0))
	percentZerosRight <- as.double(numZerosRight/nrow(rightData))
	percentOnesRight <- as.double(1-percentZerosRight)
	
	giniR <- as.double(1- percentZerosRight^2 - percentOnesRight^2)
	giniL <- as.double(1- percentZerosLeft^2 - percentOnesLeft^2)
	gini <- as.double(giniL * nrow(leftData)/nrow(sortedData))
	gini <- gini + as.double(giniR * nrow(rightData)/nrow(sortedData))

	return(gini)
}


######################################################################
######################################################################
######################################################################


##############
#EVALUATE
##############

rf_predict <- function(rf, testset) {
	numberOfTrees <- length(rf)
	numberOfItems <- dim(testset)[1]
	percentageGuesses <- rep(0,numberOfItems)
	
	currentNumberOfOnes <- 0
	for (i in 1:numberOfItems) {
		#print(percentageGuesses)
		for (j in 1:numberOfTrees) {
			if (TraverseATree(rf[[j]],testset[i,])==1) { 
				#pass one tree and one row of test data
				currentNumberOfOnes <- currentNumberOfOnes + 1 #increment the count
			} # else {  cat("not a 1 \n") }
		}
		percentageGuesses[i] <- as.double(currentNumberOfOnes / numberOfTrees)
		#print(percentageGuesses[i])
		currentNumberOfOnes <- 0
	}
	
	# just make sure nothing is 0 or 1 exactly so the ROC curves look right
	for (i in 1:length(percentageGuesses)) {
		if (percentageGuesses[i]==0) { percentageGuesses[i] <- percentageGuesses[i]+0.01 }
		else if (percentageGuesses[i]==1) { percentageGuesses[i] <- percentageGuesses[i]-0.01 }
	}
	
	return(percentageGuesses)
}


TraverseATree <- function(myTree, myData) {
	#columns are: 	1. variable is being split, 
	#				2. the split value, 
	#				3. 0 or 1 for which is less than the split, 
	#				4. parent row,
	#				5. row of child split to the left, 
	#				6. row of child split to the right
	#########################################################
	
	
	thisTreesVote <- -1
	currentRow <- 1
	repeat { #traversing the tree
		splitVariable <- myTree[currentRow,1]
		splitValue <- myTree[currentRow,2]
		whichIsLess <- myTree[currentRow,3]
		
		#update the current row
		if (myData[splitVariable] < splitValue) {
			currentRow <- myTree[currentRow,5]
		} else if (myData[splitVariable] > splitValue) {
			currentRow <- myTree[currentRow,6]
		} else { #### BREAK TIES ARBITRARILY ####
			if (sample(1:2,1)==1) { currentRow <- myTree[currentRow,5] }
			else { currentRow <- myTree[currentRow,6] }
		}
		
		# exit the loop if this is a leaf
		if (myTree[currentRow,5] == 0) { 
			thisTreesVote <- 0
			break;
		} else if (myTree[currentRow,5] == 1) {
			thisTreesVote <- 1
			break;
		}
	}
	return(thisTreesVote)
}
