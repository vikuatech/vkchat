#' upload_file
#'
#' @description Perform a request to openAI API regarding files.
#'
#' @inheritParams send_message
#' @param df dataframe to upload as csv
#' @param file_path path of the local csv file
#' @param ... other arguments to pass to req_openai
#'
#' @return invisible.
#'
#' @export
upload_file <- function(df, key, file_path, ...){

  # Write dataframe to path
  df %>% write.csv(file_path)

  # Check file size
  if(file.size(file_path)/1e+6 >= 512){
    stop('File size exceeds 512MB. Please reduce file size')
  }

  # Upload file to openai
  resp <- httr2::request('https://api.openai.com/v1/files') %>%
    httr2::req_body_multipart(
      purpose = "assistants",
      file = curl::form_file(file_path)
    ) %>%
    req_openai(key, beta = FALSE, app_json = FALSE, ...)

  file.remove(file_path)
  return(resp)

}

#' @export
#' @rdname upload_file
remove_file <- function(file_id){
  httr2::request('https://api.openai.com/v1/files') %>%
    httr2::req_method('DELETE') %>%
    httr2::req_url_path_append(file_id) %>%
    req_openai(openai_key, beta = FALSE, app_json = FALSE)

}

#' @export
#' @rdname upload_file
retrieve_file_content <- function(file_id, key){
  httr2::request('https://api.openai.com/v1/files') %>%
    httr2::req_url_path_append(file_id, 'content') %>%
    httr2::req_headers("Content-Type" = "octet-stream") %>%
    req_openai(key, beta = FALSE, app_json = FALSE)

}
