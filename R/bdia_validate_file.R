# File validation -------------------------------------------------

#' Validate a file's structure using the Bulk Data Insertion API Validation endpoint
#'
#' @description
#'`bdia_validate_file` will send a .gz file to the Bulk Data Insertion API endpoint
#' and return the response. It should be used to validate you have your data structured
#' correctly prior to attempting to push the data to Adobe Analytics via the Bulk Data Insertion
#' API Events endpoint.
#'
#' @param file The path of the file you want to validate.
#' @param routing The data center through which the data will be routed. Use:
#' 'g' for Global routing (default),
#' 'e' for Europe & Asia routing,
#' 'n' for North America routing.
#'
#' @seealso [bdia_pageview_template(), bdia_customlink_template(), bdia_df2gz()]
#' @family bdia
#' @import httr dplyr
#' @export
bdia_validate_file <- function(file, routing = 'g') {
  env_vars <- get_env_vars()
  token_config <- get_token_config(client_id = env_vars$client_id,
                                   client_secret = env_vars$client_secret)

  #Define the data routing domain
  e_url <- dplyr::case_when(routing == 'n' ~ 'https://analytics-collection-va7.adobe.io', #NAM
                     routing == 'e' ~ 'https://analytics-collection-nld2.adobe.io', #EU/Asia
                     routing == 'g' ~ 'https://analytics-collection.adobe.io') #Global

  #Send the POST request to the validation endpoint
  req <- httr::POST(
    url = paste0(e_url, '/aa/collect/v1/events/validate'),
    body = list(file = httr::upload_file(file)),
    httr::add_headers('Authorization'=token_config,
                      'x-api-key'=env_vars$client_secret)
  )

  #Save the response into a var
  res <- rawToChar(req$content)
  #NOTE: Clean this up before printing to user

  #Print response for the user
  print(res)
}