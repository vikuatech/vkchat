#' ask_openai
#'
#' @description Run flow that ask a question to openAI and perform query to Bigquery
#'
#' @inheritParams run_thread
#' @param roles Roles to extract response from either `user` or `assistant`
#' @param project_id,dataset_id Bigquery path of the dataset to query to
#'
#' @return invisible.
#'
#' @export
ask_openai <- function(content, thread_id, assistant_id, key){
  # Send user prompt to Assistant
  cat('Sending Message \n')
  message_question <- send_message(
    thread_id,
    content,
    key
  )

  # Respuesta de assistent con SQLquery
  cat('Running Thread \n')
  thread_messages <- run_thread(
    thread_id,
    assistant_id,
    key,
    before_mssg = message_question$id
  )

  # Extract SQL query from assistant response
  assistant_query <- parse_query(thread_messages)

  if(assistant_query$status == 'error'){
    error_return <- c(assistant_query, thread = list(thread_messages))
    return(error_return)
  }

  # Execute SQL query
  cat('Compiling SQL query \n')
  assistant_query_result <- execute_query(assistant_query$message)

  if(assistant_query_result$status == 'error'){
    error_return <- c(assistant_query_result, thread = list(thread_messages))
    return(error_return)
  }

  # Send response to assistant
  cat('Concluding Message \n')
  message_query_response <- send_message(
    thread_id,
    assistant_query_result$message,
    key
  )

  # Retreive assistant concluded response
  thread_messages_respond <- run_thread(
    thread_id,
    assistant_id,
    key,
    before_mssg = message_question$id
  )

  c(
    status = 'success',
    thread = list(thread_messages_respond)
  )

}

#' @export
#' @rdname ask_openai
extract_assistant_response <- function(content, roles = c('assistant', 'user')){
  content %>%
    purrr::pluck('data') %>%
    purrr::keep(~.x$role %in% roles) %>%
    purrr::map_chr(~purrr::pluck(.x, 'content', 1, 'text', 'value'))

}

#' @export
#' @rdname ask_openai
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
