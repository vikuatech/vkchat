#' log_response
#'
#' @description Save thread response in a csv file
#'
#' @param thread the thread object retreive via `run_thread_openai`
#' @param file_name string to identify the response
#' @param log_path path to save the log in csv format
#'
#' @return invisible.
#'
#' @export
log_response <- function(thread, file_name, log_path){
  df_response <- thread$data %>%
    purrr::keep(~.x$role == 'assistant') %>%
    purrr::map(
      ~.x[c('id', 'content', 'thread_id', 'run_id', 'created_at')]
    ) %>%
    dplyr::bind_rows() %>%
    tidyr::unnest_wider(content) %>%
    tidyr::unnest_wider(text) %>%
    dplyr::select(id, value, thread_id, run_id, created_at) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::mutate(file_name = file_name)

  if(file.exists(log_path)){
    append <- TRUE
  } else {
    append <- FALSE
  }
  df_response %>% write.csv(log_path, append = append)

}

