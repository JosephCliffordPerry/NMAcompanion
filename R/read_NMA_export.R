#' Read NMA Export File
#'
#' @description Reads a tab-delimited nuclear morphology analysis (NMA) export file
#' into a data frame for analysis. The export must be produced using NMA with profiles and outlines included
#' for most package functionality.
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
  if (!hasName(rawdata,"Angle_profile_0")) {
   stop("Missing profiles check NMA export.")
  }
  if (!hasName(rawdata,"Outline_OrientedCoordinates_X_0")) {
    warning("Missing outlines check NMA export, consensus images will be unavailable.")
  }
  return(rawdata)
}
