% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_thread.R
\name{run_thread}
\alias{run_thread}
\alias{check_run_status}
\alias{create_run}
\title{run_thread}
\usage{
run_thread(thread_id, assistant_id, key, after_mssg = NULL, before_mssg = NULL)

check_run_status(thread_id, run_id, key)

create_run(thread_id, assistant_id, key)
}
\arguments{
\item{thread_id, assistant_id, run_id}{ID objects to identify and set run}

\item{key}{OpenAI secret key}

\item{after_mssg, before_mssg}{parameters to retreive messages before or after a specific message}
}
\value{
invisible.
}
\description{
Run a thread in OpenAI API, and wait for the response
}
