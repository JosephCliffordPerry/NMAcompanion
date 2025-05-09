#' Read NMA Export File
#'
#' @description Reads a tab-delimited nuclear morphology analysis (NMA) export file
#' into a data frame.
#'
#' @param path_to_export A string giving the file path to the tab-delimited NMA export file.
#'
#' @return A data frame containing the imported data.
#'
#' @examples
#' \dontrun{
#' data <- read_NMA_export("data/NMA_output.txt")
#' head(data)
#' }
#' @importFrom utils read.table
#' @export
read_NMA_export <- function(path_to_export) {
  rawdata <- read.table(path_to_export, sep = "\t", header = TRUE)
  return(rawdata)
}
