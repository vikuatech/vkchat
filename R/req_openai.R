#' req_openai
#'
#' @description Perform a request to openAI API, with custom headers and authentication
#'
#' @param req req object to perform the request
#' @param key OpenAI secret key
#' @param beta include beta header
#' @param app_json include application/json header
#' @param dry_run print request (to inspect) instead of performing it
#'
#' @return invisible.
#'
#' @export
req_openai <- function(req, key, beta = TRUE, app_json = TRUE, dry_run = FALSE) {

  # Add Beta Header
  if(beta){
    req <- req %>% httr2::req_headers('OpenAI-Beta' = 'assistants=v1')
  }

  # Add Accept json header
  if(app_json){
    req <- req %>% httr2::req_headers("Accept" = "application/json")
  }

  # Add Bearer
  req <- req %>%
    httr2::req_auth_bearer_token(key)

  # If dry_run then print request
  if(dry_run){
    return( req %>% httr2::req_dry_run() )
  }

  # Perform Request
  resp <- req %>% httr2::req_perform()

  if(httr2::resp_is_error(resp)){
    resp
  }

  # Verifica si la solicitud fue exitosa
  if(httr2::resp_status(resp) == 200) {

    httr2::resp_body_json(resp)

  }
}



