# This is a function for randomly assigning items to raters

# required library: tidyverse

# required input: 
# items: a vector of items that need to be rated
# raters: a vector of raters' names or IDs
# pair_size: the number of raters assigned for a given item

# output: 
# The output is a list of 3 data frame: 
# goal_record shows the names of raters assigned to each goal 
# rater_record shows all the goals assigned to each rater
# sum_record shows the total number of goals being assigned, and the total number of goals assigned to each rater

ratingAssignment <- function(items, raters, pair_size){
  
  
  # extract the total number of raters
  raNum <- length(raNames)
  
  # randomize the order of rater names
  raNames <- sample(raNames)
  
  # generate all pairs of combination
  raPairs <- combn(raNames, pair_size)
  
  # transpose the matrix
  raPairs <- t(raPairs)
  
  # extract the number of pairs
  pairNum <- nrow(raPairs)
  
  # randomize the order of the ra pairs
  raPairs <- raPairs[sample(pairNum),]
  
  # get rid of empty items
  items <- items[items!="" & is.na(items) == F]
  
  # randomize items
  items <- as.vector(sample(items))
  
  # total number of non empty items
  total_items <- length(items)
  
  # modify the length of the items so that it can be divided by the number of pairs. The additional items will be NAs.
  length(items) <- prod(dim(matrix(items, nrow = pairNum)))
  
  # assign items across all pairs and transform the result into a dataframe 
  itemMatrix <- matrix(items, nrow = pairNum)
  itemMatrix <- as.data.frame(itemMatrix)
  
  # combine the output with the ra pairs
  assignDf <- cbind(raPairs, itemMatrix)
  
  # transform the dataframe into a long format
  assignDf <- assignDf %>% 
    gather(key = order, value = items, starts_with("V")) %>%
    select(-order) %>%
    filter(!is.na(items))
  
  # calculate the maximum number of items assigned for each RA
  rowMax <- length(items) * (pair_size/raNum)
  
  # initialize an empty dataframe where each rater is represented by a column
  raDf <- data.frame(matrix(nrow = rowMax, ncol = raNum))
  colnames(raDf) <- raNames
  
  # loop through each rater and extract the goals assigned to the rater pairs that this rater belongs 
  for (raIdx in 1:length(raNames)){
    ra <- raNames[raIdx]
    pairIdx <- which(raPairs == ra, arr.ind = T)[,1] # see which pair includes this rater
    item <- itemMatrix[pairIdx,] # extract the goals from these pairs
    raItems <- as.vector(as.matrix(item)) # vectorize the goals 
    raDf[,raIdx] <- sample(raItems) # randomize the order
  }
  
  # get a summary of the number of items assigned to each rater
  assign_sum <- colSums(!is.na(raDf))
  
  # generate a dataframe for logging the summary
  log <- data.frame(date = as.character(Sys.Date()),
                    total_items = total_items)
  log <- cbind(log, bind_rows(assign_sum))
  
  # generate a list to store all output dataframes 
  out <- list()
  
  out$goal_record <- assignDf
  out$rater_record <- raDf
  out$sum_record <- log
  
  return(out)
}