#' create_thread
#'
#' @description Create a Thread in OpenAI API
#'
#' @inheritParams send_message
#'
#' @return invisible.
#'
#' @export
create_thread <- function(key){
  # Create Thread
  httr2::request('https://api.openai.com/v1/threads') %>%
    httr2::req_method('POST') %>%
    req_openai(key)

}

#' @export
#' @rdname create_thread
create_thread_and_message <- function(file_id, key, content){

  # Create Thread
  httr2::request('https://api.openai.com/v1/threads') %>%
    httr2::req_body_json(
      auto_unbox = TRUE,
      data = list(
        messages = list(
          list(
            role = "user",
            content = content,
            file_ids = list(file_id)
          )
        )
      )
    ) %>%
    req_openai(key)
}
