# Custom Link Template -------------------------------------------------

#' Creates a data frame with columns for all fields required when using the Bulk Data Insertion API
#' to push custom link data to Adobe Analytics.
#'
#' @description
#'`bdia_customlink_template` is meant to help simplify the process of creating a data set that can
#'be successfully sent to Adobe Analytics via the Bulk Data Insertion API. It will return a data frame
#'with columns for each of the required fields, and is flexible enough to accommodate all acceptable
#'link and visitor ID parameters. The data frame returned is extensible, as props, events, and evars can
#'be added to it. Additional information on required fields can be found at:
#'https://developer.adobe.com/analytics-apis/docs/2.0/guides/endpoints/bulk-data-insertion/file-format/
#'
#' @param linkParam The type of link parameter you will be using in your data set. Use:
#'  'url' if you will be using the link URL,
#'  'qstring' if you will be including link information using a query string parameter,
#'  'name' if you will be using the link name (default).
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
#' @seealso [bdia_pageview_template()]
#' @family bdia
#' @import dplyr
#' @export
bdia_customlink_template <- function(linkParam = 'name', idParam = 'mcid', cidType = NULL) {
  #vars
  link <- case_when(linkParam == 'url' ~ 'linkURL',
                    linkParam == 'qstring' ~ 'queryString',
                    linkParam == 'name' ~ 'linkName')

  id <- case_when(idParam == 'vid' ~ 'visitorID',
                  idParam == 'ip' ~ 'IPAddress',
                  idParam == 'cid' ~ paste0('customerID.',cidType,'.id'),
                  idParam == 'mcid' ~ 'marketingCloudVisitorID')

  nc <- case_when(idParam == 'cid' && linkParam != 'qstring' ~ 7,
                  idParam != 'cid' && linkParam == 'qstring' ~ 5,
                  TRUE ~ 6)

  #create empty df
  df <- data.frame(matrix(ncol = nc , nrow = 0))

  #define column names
  cn <- c('reportSuiteID',
          'userAgent',
          'timestamp',
          'linkType',
          link,
          id)

  if (idParam == 'cid')  {
    if(is.null(cidType)) {
      stop('Input "cidType" is required when selecting Customer ID as your idParam.')
    }
    else {cn <- append(cn, paste0('customerID.',cidType,'.isMCSeed'))}
  }

  if (linkParam == 'qstring') {
    cn <- cn[cn != 'linkType']
  }

  #column names
  colnames(df) <- cn

  #save template to global environment
  assign('customlink_template', df, envir = .GlobalEnv)
}