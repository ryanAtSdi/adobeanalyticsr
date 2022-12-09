# Zip file -------------------------------------------------

#' Write a data frame to a csv file and zip it gz format for validation and upload via
#' Bulk Data Insertion API
#'
#' @description
#'`bdia_df2gz` is a helper function that simplifies the process of creating a csv file from a data frame
#'and zipping it as a gz file that can be leveraged with Bulk Data Insertion API endpoints.
#'
#' @param x The data frame you want to write to csv and zip.
#' @param fileName What you want to name the file. This name should not include file extensions.
#' @param path The path within your working directory where you want the zipped file saved. Default is '/'.
#'
#' @seealso [bdia_pageview_template(), bdia_customlink_template()]
#' @family bdia
#' @import R.utils
#' @export
bdia_df2gz <- function(x, fileName, path='') {
  #append csv file extension to fileName and set path
  fileName <- paste0(fileName, '.csv')
  path <- paste(getwd(), path, fileName, sep = '/')

  #write to csv and zip
  write.csv(x, file = path, row.names = FALSE)
  gzip(path)
}
