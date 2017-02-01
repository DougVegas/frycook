#' Low Count Factor Levels Removal
#'
#' Removes levels of a factor variable with low counts
#' @param data Name of data.frame or data.table
#' @keywords factor levels
#' @export
#' @examples
#' low_count_levels_removal()


low_count_levels_removal = function(data) {

# remove factor columns not used in modeling or analysis first:

# Change character columns to factors ::
data <- as.data.frame(data)
for (i in 1:ncol(data)) {
  if (is.character(data[,i]) == TRUE) { 
    data[,i] <- as.factor(data[,i])
  }
}

# switch back to data.table
data <- data.table::as.data.table(data) 

# Create separate data frames for factor variables ::
datanew  <- data[, lapply(data, is.numeric) == FALSE, with = FALSE]

# add in a column of 1's ::
datanew$COUNT <- rep(1,nrow(data))

# Remove factor levels with low counts ::
for (i in 1:(ncol(datanew)-1)) {
  
  # set var to the factor variables ::
  var <- c(noquote(colnames(datanew)[i]))
  
  # aggregate counts by levels of factors ::
  data_temp  <- datanew[, .(sum(COUNT)), by = var]
  
  # remove levels with less than x % of total records ::
  data_merge <- data_temp[V1/sum(V1) > 0.0025]
  
  # remove count variable ::
  data_final <- data_merge[,.SD, .SDcols = -2]
  
  # set key to factor variables for joining
  setkeyv(data,var)
  setkeyv(data_final,var)
  
  # join data frames ::
  data <- data[data_final, nomatch=0]
}

data

}