#' Generate website for selected processes
#'
#' @param process_files vector of files names with process definitions
#' @param website path to website scheme 
#' @param write_to path to html file in which the final website will be written
#'
#' @export
generate_process_website <- function(process_files = list.files("inst/processes", full.names = TRUE), 
                                     website = "inst/diagramTextButton.html",
                                     write_to = "diagram_out.html") {
  # read process_files
  processes <- lapply(process_files, read_process)

  # convert to jsons
  process_json <- lapply(processes, process_to_json)
  
  # read website template
  website_template = paste(readLines(website), collapse = "\n")
  
  # impute data 
  website_template <- gsub(website_template, 
                           pattern = "##PROCESS_DATA##",
                           replacement = paste0(process_json, collapse = ", \n"))
  # impute selector 
  titles <- unlist(lapply(processes, function(process) process$title))
  titles <- paste0(paste0('<text class = "select" onclick="update(', seq_along(titles), ')">',titles,'</text>'), collapse = "\n")

  website_template <- gsub(website_template, 
                           pattern = "##TITLE_DATA##",
                           replacement = titles)
  
  writeLines(website_template, write_to)
}

