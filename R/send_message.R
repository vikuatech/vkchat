#' send_message
#'
#' @description Send a user message to OpenAI thread
#'
#' @inheritParams req_openai
#' @param thread_id ID of the thread
#' @param content Content Text of the message
#' @param file_id id of the file in openAI account
#' @param after_mssg,before_mssg parameters to retreive messages before or after a specific message
#'
#' @return invisible.
#'
#' @export
send_message <- function(thread_id, content, key, file_id = NULL){

  data_mssg <- list(
    role = "user",
    content = content
  )

  if(!is.null(file_id)){
    data_mssg$file_ids <- list(file_id)
  }

  # Send Message
  httr2::request('https://api.openai.com/v1/threads/') %>%
    httr2::req_url_path_append(thread_id, 'messages') %>%
    httr2::req_body_json(
      data = data_mssg
    ) %>%
    req_openai(key)

}

#' @export
#' @rdname send_message
list_messages <- function(thread_id, key, after_mssg = NULL, before_mssg = NULL){
  httr2::request('https://api.openai.com/v1/threads/') %>%
    httr2::req_url_path_append(thread_id, 'messages') %>%
    httr2::req_url_query(after = after_mssg, before = before_mssg) %>%
    req_openai(key)
}
