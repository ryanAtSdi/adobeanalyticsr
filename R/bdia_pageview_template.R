# Pageview Template -------------------------------------------------

#' Creates a data frame with columns for all fields required when using the Bulk Data Insertion API
#' to push pageview data to Adobe Analytics.
#'
#' @description
#'`bdia_pageview_template` is meant to help simplify the process of creating a data set that can
#'be successfully sent to Adobe Analytics via the Bulk Data Insertion API. It will return a data frame
#'with columns for each of the required fields, and is flexible enough to accommodate all acceptable
#'page and visitor ID parameters. The data frame returned is extensible, as props, events, and evars can
#'be added to it. Adtional information on required fields can be found at:
#'https://developer.adobe.com/analytics-apis/docs/2.0/guides/endpoints/bulk-data-insertion/file-format/
#'
#' @param pageParam The type of page parameter you will be using in your data set. Use:
#'  'url' if you will be using URL,
#'  'qstring' if you will be including page information using a query string parameter,
#'  'name' if you will be using Page (default).
#' @param idParam The type of user ID you will be using in your data set. Use:
#'  'vid' if you will be using visitor ID,
#'  'ip' if you will be using IP Address,
#'  'cid' if you will be using Customer ID. If this option is chosen, you will also need to provide
#'  cidType as well.
#'  'mcid' if you will be using Markting Cloud Visitor ID (default).
#' @param cidType Is the type of customer ID you are using form Adobe Audience Manager. Learn more
#'  about using Customer IDs with the Bulk Data Insetion API here:
#'  https://developer.adobe.com/analytics-apis/docs/2.0/guides/endpoints/bulk-data-insertion/mcseed/
#'
#' @seealso [bdia_customlink_template()]
#' @family bdia
#' @import dplyr
#' @export
bdia_pageview_template <- function(pageParam = 'name', idParam = 'mcid', cidType = NULL) {
  #vars
  page <- dplyr::case_when(pageParam == 'url' ~ 'pageURL',
                    pageParam == 'qstring' ~ 'queryString',
                    pageParam == 'name' ~ 'pageName')

  id <- dplyr::case_when(idParam == 'vid' ~ 'visitorID',
                  idParam == 'ip' ~ 'IPAddress',
                  idParam == 'cid' ~ paste0('customerID.',cidType,'.id'),
                  idParam == 'mcid' ~ 'marketingCloudVisitorID')

  nc <- ifelse(idParam == 'cid', 6, 5)

  #create empty df
  df <- data.frame(matrix(ncol = nc, nrow = 0))

  #define column names
  cn <- c('reportSuiteID',
          'userAgent',
          'timestamp',
          page,
          id)

  if (idParam == 'cid')  {
    if(is.null(cidType)) {
      stop('Input "cidType" is required when selecting Customer ID as your idParam.')
    }
    else {cn <- append(cn, paste0('customerID.',cidType,'.isMCSeed'))}
  }

  #column names
  colnames(df) <- cn

  #save template to global environment
  assign('pageview_template', df, envir = .GlobalEnv)
}