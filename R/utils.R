#' utils
#'
#' @description Utils to smooth workflow of openAI API communication
#'
#' @inheritParams run_thread
#' @param roles Roles to extract response from either `user` or `assistant`
#' @param project_id,dataset_id Bigquery path of the dataset to query to
#'
#' @return invisible.
#'
#' @export
extract_assistant_response <- function(content, roles = c('assistant', 'user')){
  content %>%
    purrr::pluck('data') %>%
    purrr::keep(~.x$role %in% roles) %>%
    purrr::map_chr(~purrr::pluck(.x, 'content', 1, 'text', 'value'))

}

#' @export
#' @rdname extract_assistant_response
remove_query_from_message <- function(content){

  if(!stringr::str_detect(content, '---QUERY START')){
    return(content)
  }

  start_location <- content %>%
    stringr::str_locate("---QUERY START") %>%
    .[,2]

  content %>%
    stringr::str_sub(end = start_location-14)
}

#' @export
#' @rdname extract_assistant_response
parse_query <- function(content, project_id, dataset_id){

  bq_path <- glue::glue('`{project_id}.{dataset_id}.')

  tryCatch({
    query <- content %>%
      extract_assistant_response() %>%
      stringr::str_replace_all('\\n', ' ') %>%
      stringr::str_extract('(?<=QUERY START---)(.*)(?=---QUERY END---)') %>%
      stringr::str_replace_all('\\s`', bq_path) %>%
      stringr::str_trim()

    list(
      status = 'success',
      message = query
    )
  },
  error = function(e){
    list(
      status = 'error',
      message = 'No SQL query found in assistant response.'
    )
  })

}

#' @export
#' @rdname extract_assistant_response
execute_query <- function(query, project_id){

  tryCatch({
    data_result <- bigrquery::bq_project_query(project_id, query) %>%
      bigrquery::bq_table_download()

    result_string <- knitr::kable(data_result) %>%
      as.character() %>%
      paste0(collapse = '\n')

    mssg <- glue::glue('---QUERY RESULT---\n{result_string}')

    list(
      status = 'success',
      message = mssg
    )

  },
  error = function(e){
    list(
      status = 'error',
      message = 'Can`t compile SQL query.'
    )
  })

}
