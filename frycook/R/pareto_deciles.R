#' Pareto Deciles
#'
#' Creates Pareto Deciles of a given metric for each unique ID in your dataset.
#' @param data Name of data.frame or data.table
#' @param x Name of numeric vector to create Pareto deciles for
#' @param id Unique ID of data.frame or data.table to group by
#' @keywords Pareto deciles
#' @export
#' @examples
#' pareto_deciles()

# PARETO DECILES FUNCTION---------------------

###first, create a function for creating Pareto deciles (requires plyr, magrittr)
pareto_deciles = function(data, x) {
  ###get column name of vector
  xchar = data.frame(x) %>% names(.) %>% as.character(.)
  ###sort x descending, NAs are put last
  sorted_data = data[order(-data$x, na.last = TRUE), ]
  ###create cumulative totals and cumulative % totals
  sorted_data$cume_total = cumsum(sorted_data$x)
  sorted_data$cume_pct_total = sorted_data$cume_total / max(sorted_data$cume_total, na.rm = TRUE)
  ###begin binning into pareto deciles
  sorted_data$pareto_decile = ifelse(sorted_data$cume_pct_total <= .1, 10,
                                     ifelse(sorted_data$cume_pct_total <= .2, 9,
                                            ifelse(sorted_data$cume_pct_total <= .3, 8,
                                                   ifelse(sorted_data$cume_pct_total <= .4, 7,
                                                          ifelse(sorted_data$cume_pct_total <= .5, 6,
                                                                 ifelse(sorted_data$cume_pct_total <= .6, 5,
                                                                        ifelse(sorted_data$cume_pct_total <= .7, 4,
                                                                               ifelse(sorted_data$cume_pct_total <= .8, 3,
                                                                                      ifelse(sorted_data$cume_pct_total <= .9, 2,
                                                                                             ifelse(sorted_data$cume_pct_total <= 1, 1, 0))))))))))
  ###rename CUME_TOTAL, CUME_PCT_TOTAL, and PARETO_DECILE
  newColname1 = paste(xchar, "_cume_total", sep = "")
  newColname2 = paste(xchar, "_cume_pct_total", sep = "")
  newColname3 = paste(xchar, "_pareto_decile", sep = "")
  sorted_data = plyr::rename(sorted_data, c("cume_total" = newColname1))
  sorted_data = plyr::rename(sorted_data, c("cume_pct_total" = newColname2))
  sorted_data = plyr::rename(sorted_data, c("pareto_decile" = newColname3))
  sorted_data
}


#' Pareto Deciles
#'
#' Creates a ggplot2 histogram of Pareto deciles on your dataset.
#' @param data Name of data.frame or data.table
#' @param x Name of numeric vector to create Pareto deciles for
#' @param id Unique ID of data.frame or data.table to group by
#' @keywords Pareto deciles ggplot2
#' @export
#' @examples
#' pareto_deciles_ggplot()

# PARETO DECILES GGPLOT FUNCTION

pareto_deciles_ggplot = function(data, x) {
  ###get column name of vector
  xchar = data.frame(x) %>% names(.) %>% as.character(.)
  ###call pareto_deciles() function
  plotData = pareto_deciles(data = data, x = x)
  histVar = paste(xchar, "_pareto_decile", sep = "")
  plot = ggplot2::ggplot(plotData, aes(x = plotData[ ,grepl(histVar, names(plotData))])) + geom_histogram(binwidth = 1, , color = "white", fill = "#401A50")
  plot #= plot + 
}

