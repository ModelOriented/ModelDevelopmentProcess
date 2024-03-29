% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_to_json.R
\name{process_to_json}
\alias{process_to_json}
\title{Convert process to json}
\usage{
process_to_json(process)
}
\arguments{
\item{process}{list with information about process (phases, steps, and efforts)}
}
\value{
string with json representation of a process
}
\description{
Convert process to json
}
\examples{
\dontrun{
  process <- read_process("inst/processes/0general.txt")
  cat(process_to_json(process))
}

}
