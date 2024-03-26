#' run_thread
#'
#' @description Run a thread in OpenAI API, and wait for the response
#'
#' @inheritParams send_message
#' @param thread_id,assistant_id,run_id ID objects to identify and set run
#'
#' @return invisible.
#'
#' @export
run_thread <- function(thread_id, assistant_id, key, after_mssg = NULL, before_mssg = NULL){

  # Run Thread
  thread_run <- create_run(thread_id, assistant_id, key)

  # Start while loop until completed or failed
  run_id <- thread_run %>% purrr::pluck('id')
  status <- 'queued'
  security_counter <- 0
  while(status %in% c('queued', 'in_progress')){

    # Wait for openai to process the response
    Sys.sleep(3*security_counter)
    security_counter <- security_counter+1

    # Check status
    run_status <- check_run_status(thread_id, run_id, key)
    status <- run_status %>% purrr::pluck('status')

    # Stop if failed
    if(status == 'failed'){
      resp <- run_status %>% purrr::pluck('last_error')
      break
    }

    # Retreive messages if completed
    else if(status == 'completed') {
      resp <- list_messages(thread_id, key, after_mssg, before_mssg)
      break
    }

    # Retreive run response if Other status not queued or in_progress
    else if(!status %in% c('queued', 'in_progress')){
      resp <- run_status
      break
    }

    if(security_counter == 10){
      stop('Security counter reached without response')
      break
    }

    cat(glue::glue('Run: {security_counter}. Status: {status} '), '\n')

  }

  return(resp)
}

#' @export
#' @rdname run_thread
check_run_status <- function(thread_id, run_id, key){
  httr2::request('https://api.openai.com/v1/threads/') %>%
    httr2::req_url_path_append(thread_id, 'runs', run_id) %>%
    req_openai(key)

}

#' @export
#' @rdname run_thread
create_run <- function(thread_id, assistant_id, key){
  thread <- httr2::request('https://api.openai.com/v1/threads/') %>%
    httr2::req_url_path_append(thread_id, 'runs') %>%
    httr2::req_body_json(data = list(assistant_id = assistant_id)) %>%
    req_openai(key)

}




