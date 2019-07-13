#' Convert process to json
#'
#' @param process list with information about process (phases, steps, and efforts)
#'
#' @return string with json representation of a process
#' @export
#'
#' @examples
#' \dontrun{
#'   process <- read_process("inst/processes/0general.txt")
#'   cat(process_to_json(process))
#' }
#'
process_to_json <- function(process) {
  # Phases
  phases_json <- sapply(process$phases, function(phase) {
    paste0('{"top":"',phase[1],'", "bottom":[',paste0(paste0('"', phase[-1], '"'), collapse = ", "),']}')
  })
  phases_json <- paste0('"tbTxt": [\n',
                        paste0(phases_json, collapse = ",\n"),
                        "\n],")

  # Steps
  steps_json <- sapply(process$steps, function(steps) {
    paste0('{"main":"',steps[1],'", "sub":[',paste0(paste0('"', steps[-1], '"'), collapse = ", "),']}')
  })
  steps_json <- paste0('"leftTxt": [\n',
                        paste0(steps_json, collapse = ",\n"),
                        "\n],")

  # Effort
  effort_json <- sapply(process$effort, function(effort) {
    paste0(paste0('{"time":"A',seq_along(effort),'","value":',effort,'}'), collapse = ", ")
  })
  effort_json <- paste0('"bar": [\n',
                       paste0(paste0("[", effort_json, "]"), collapse = ",\n"),
                       "\n]")

  paste0("{\n",
         phases_json,"\n",
         steps_json,"\n",
         effort_json,"\n",
         "}\n")
}
