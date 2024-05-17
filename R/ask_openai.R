#' ask_openai
#'
#' @description Run workflow that ask a question to openAI and perform query to Bigquery
#'
#' @inheritParams run_thread
#' @param roles Roles to extract response from either `user` or `assistant`
#' @param project_id,dataset_id Bigquery path of the dataset to query to
#'
#' @return invisible.
#'
#' @export
ask_openai <- function(content, thread_id, assistant_id, key, project_id, dataset_id){
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

  cat('Extracting Response \n')
  first_response <- thread_messages %>%
    extract_assistant_response()

  # Si el mensaje no tiene una sentencia SQL retornalo tal cual
  if(!stringr::str_detect(first_response, '---QUERY START')){
    cat('Error Status: success_nosql \n')
    nosql_return <- c(
      status = 'success_nosql',
      thread = list(thread_messages)
    )
    return(nosql_return)
  }

  # Extract SQL query from assistant response
  cat('Extracting SQL sentence \n')
  assistant_query <- parse_query(first_response, project_id, dataset_id)

  if(assistant_query$status == 'error'){
    cat('Error Status: Cant extract SQL sentence \n')
    error_return <- c(assistant_query, thread = list(thread_messages))
    return(error_return)
  }

  # Execute SQL query
  cat('Compiling SQL query \n')
  assistant_query_result <- execute_query(assistant_query$message, project_id)

  if(assistant_query_result$status == 'error'){
    cat('Error Status: Wrong SQL sentence. Cant compile \n')
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

  concluding_response <- c(
    status = 'success_sql',
    thread = list(thread_messages_respond)
  )

  return(concluding_response)

}
