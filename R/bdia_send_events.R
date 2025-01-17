# Send data -------------------------------------------------

#' Send event data to Adobe Analytics using the Bulk Data Insertion API.
#'
#' @description
#'`bdia_send_events` will send data to your Adobe Analytics report suite via a zipped data file (.gz)
#'posted to the Bulk Data Insertion API Events endpoint. Prior to taking this step, be sure to first
#'validate your file using bdia_validate_file().
#'
#' @param file The path of the file you want to validate.
#' @param routing The data center through which the data will be routed. Use:
#' 'g' for Global routing (default),
#' 'e' for Europe & Asia routing,
#' 'n' for North America routing.
#' @param vgid The Visitor Group ID you want to assign to the file. Defaults to vg0.
#' Learn more about using Visitor Groups with the Bulk Data Insertion API here:
#' https://developer.adobe.com/analytics-apis/docs/2.0/guides/endpoints/bulk-data-insertion/visitor-groups/
#'
#' @seealso [bdia_validate_file()]
#' @family bdia
#' @import dplyr httr
#' @export
bdia_send_events <- function(file, routing='g', vgid='vg0') {
  env_vars <- get_env_vars()
  token_config <- get_token_config(client_id = env_vars$client_id,
                                   client_secret = env_vars$client_secret)

  #vars
  e_url <- dplyr::case_when(routing == 'n' ~ 'https://analytics-collection-va7.adobe.io', #NA
                     routing == 'e' ~ 'https://analytics-collection-nld2.adobe.io', #EU/Asia
                     TRUE ~ 'https://analytics-collection.adobe.io') #Global

  #get validation response
  valid <- adobeanalyticsr::bdia_validate_file(file, routing)

  #post, but only if file was validated
  #Ask Ben about best practice for getting API key...
  if (str_detect(valid, 'file is valid')) {
    #request
    req <- httr::POST(
      config = NULL,
      url = paste0(e_url, '/aa/collect/v1/events'),
      body = list(file = httr::upload_file(file)),
      token_config,
      httr::add_headers('x-adobe-vgid'=vgid,
                        'x-api-key'=Sys.getenv("AW_API_KEY"))
    )

    #response
    res <- rawToChar(req$content)

    #print response
    print(res)
  }
  else { print('File could not be validated. No request made to the events endpoint.') }
}