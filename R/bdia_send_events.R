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
#'
#' @seealso [bdia_validate_file()]
#' @family bdia
#' @import dplyr httr
#' @export
bdia_send_events <- function(file, routing='g') {
  env_vars <- get_env_vars()
  token_config <- get_token_config(client_id = env_vars$client_id,
                                   client_secret = env_vars$client_secret)

  #vars
  e_url <- dplyr::case_when(routing == 'n' ~ 'https://analytics-collection-va7.adobe.io', #NA
                     routing == 'e' ~ 'https://analytics-collection-nld2.adobe.io', #EU/Asia
                     TRUE ~ 'https://analytics-collection.adobe.io') #Global

  #get validation response
  valid <- bdia_validate(file, routing)

  #post, but only if file was validated
  if (str_detect(valid, 'file is valid')) {
    #request
    req <- httr::POST(
      url = paste0(e_url, '/aa/collect/v1/events'),
      body = list(file = httr::upload_file(file)),
      httr::add_headers('Authorization'=token_config,
                        'x-api-key'=env_vars$client_secret)
    )

    #response
    res <- rawToChar(req$content)

    #print response
    print(res)
  }
  else { print('File could not be validated. No request made to the events endpoint.') }
}