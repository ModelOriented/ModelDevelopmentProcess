#' Read process from a config file
#'
#' @param file name of a file with title, phases, steps, and efforts
#'
#' @return list with process detailes
#' @export
#'
#' @examples
#' process <- read_process()
#' str(process)
read_process <- function(file = "inst/processes/0general.txt") {
  lines <- readLines(file)

  # remove empty
  lines <- lines[lines != ""]
  
  # find sections
  sections <- grep(lines, pattern = "##")
  
  # Title - first section
  title <- lines[sections[1]+1]
  
  ## Phases
  phases <- lines[(sections[2]+1):(sections[3]-1)]
  phases <- strsplit(phases, split = "[:,] +")
  
  ## Steps
  steps <- lines[(sections[3]+1):(sections[4]-1)]
  steps <- strsplit(steps, split = "[:,] +")
  
  ## Effort
  effort <- lines[(sections[4]+1):length(lines)]
  effort <- strsplit(effort, split = "[ \t]+")
  
  list(title = title, 
       phases = phases, 
       steps = steps, 
       effort = effort)
}
