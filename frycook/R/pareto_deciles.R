#' Pareto Deciles
#'
#' Creates Pareto Deciles of a given metric for each unique ID in your dataset.
#' @param data Name of data.frame or data.table
#' @param x Name of numeric vector to create Pareto deciles for; must be in quotations ""
#' @param id Unique ID of data.frame or data.table to group by
#' @keywords Pareto deciles
#' @export
#' @examples
#' pareto_deciles()

# PARETO DECILES FUNCTION---------------------

###first, create a function for creating Pareto deciles
pareto_deciles = function(data, x, id = NULL){
  ###make x argument as character for column renaming
  xchar = as.character(x)
  ###sort x descending, NAs are put last
  data = data[order(-data[ ,x], na.last = TRUE), ]
  ###create cumulative totals and cumulative % totals
  data$cume_total = cumsum(data[ ,x])
  data$cume_pct_total = data$cume_total / max(data$cume_total, na.rm = TRUE)
  ###begin binning into pareto deciles
  data$pareto_decile = ifelse(data$cume_pct_total <= .1, 10,
                              ifelse(data$cume_pct_total <= .2, 9,
                                     ifelse(data$cume_pct_total <= .3, 8,
                                            ifelse(data$cume_pct_total <= .4, 7,
                                                   ifelse(data$cume_pct_total <= .5, 6,
                                                          ifelse(data$cume_pct_total <= .6, 5,
                                                                 ifelse(data$cume_pct_total <= .7, 4,
                                                                        ifelse(data$cume_pct_total <= .8, 3,
                                                                               ifelse(data$cume_pct_total <= .9, 2,
                                                                                      ifelse(data$cume_pct_total <= 1, 1, 0))))))))))
  ###rename CUME_TOTAL, CUME_PCT_TOTAL, and PARETO_DECILE
  newColname1 = paste(xchar, "_cume_total", sep = "")
  newColname2 = paste(xchar, "_cume_pct_total", sep = "")
  newColname3 = paste(xchar, "_pareto_decile", sep = "")
  data = plyr::rename(data, c("cume_total" = newColname1))
  data = plyr::rename(data, c("cume_pct_total" = newColname2))
  data = plyr::rename(data, c("pareto_decile" = newColname3))
  data
}


#' Pareto Deciles Plot
#'
#' Creates a ggplot2 histogram of Pareto deciles on your dataset.
#' @param data Name of data.frame or data.table
#' @param x Name of numeric vector to create Pareto deciles for; must be in quotations ""
#' @param id Unique ID of data.frame or data.table to group by
#' @keywords Pareto deciles ggplot2
#' @export
#' @examples
#' pareto_deciles_ggplot()

# PARETO DECILES GGPLOT FUNCTION

pareto_deciles_ggplot = function(data, x, id = NULL) {
  ###make x argument as character for column renaming
  xchar = as.character(x)
  ###concat for plot title
  plotTitle = paste("Histogram of Pareto Deciles for", x, sep = "")
  ###call pareto_deciles() function
  plotData = pareto_deciles(data = data, x = x)
  histVar = paste(xchar, "_pareto_decile", sep = "")
  plot = ggplot2::ggplot(plotData, aes(x = plotData[ ,grepl(histVar, names(plotData))])) + geom_histogram(binwidth = 1, , color = "white", fill = "#401A50")
  plot = plot + ggtitle(label = plotTitle) + xlab("")
}

