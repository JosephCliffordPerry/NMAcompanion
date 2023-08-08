#' Bimodality detector(grouped)
#' @importFrom diptest dip.test
#' @param selected_datasets the list you want to add bimodal columns to
#' @param dataset the dataset you want to take bimodal columns from
#' this function can itterate through the colunns of a dataset and add the
#' bimodal columns to a list in blocks of 5 this is designed aroud it being used
#' on regions of the profile regions of the dataset

# a function that itterates through a dataset and adds bimodal data to a list
hartigansdipper<-function(dataset,selected_datasets){

  # Iterate over each column of the dataset
  i <- 1
  while (i <= ncol(dataset)) {
    # Perform Hartigan's Dip Test
    dip_test <- dip.test(dataset[[i]])

    # Check if the distribution is bimodal
    if (dip_test$p.value < 0.05) {
      # Get the column indices of the previous and next four columns
      prev_cols <- max(1, i - 1)
      next_cols <- min(ncol(dataset), i + 4)

      # Extract the relevant columns
      selected_dataset <- dataset[, prev_cols:next_cols]

      # Add 'CellID' column from the original dataset
      # selected_dataset$CellID <- data$CellID
      # make the dataset name
      dataset_label <- colnames(selected_dataset)

      # Get the dataset name
      dataset_name <- paste0("Dataset_", paste0(colnames(selected_dataset), collapse = "_"))

      # Append the selected dataset to the list with the dataset name as the label
      selected_datasets[[dataset_name]] <- selected_dataset


      # Update the index to the next column after the extracted columns
      i <- next_cols + 1
    } else {
      # Move to the next column
      i <- i + 1
    }
  }
  return(selected_datasets)}


#' Get columns of a data.frame that are non-unimodal
#'
#' Performs Hartigan's dip-test, and returns subset data frames containing
#' potentially non-unimodal columns plus the surrounding two columns.
#'
#' @param data the data frame to input
#' @param data.name the name of the data to use in the output
#' @param dip.test.alpha the p-value threshold for the dip test
#'
#' @return a list of data.frames, containing the columns of interest
#' @export
#'
#' @examples
#' ap <- NMA_toy_dataset %>% dplyr::select(starts_with("Angle_profile"))
#' get.dip.test.regions(ap, "Angle_profile")
#'
#' perims <- NMA_toy_dataset %>% dplyr::select(starts_with("Perimeter_microns"))
#' get.dip.test.regions(perims, "Perimeter")
#'
get.dip.test.regions <- function(data, data.name, dip.test.alpha = 0.05) {

  # run diptest across all columns, get boolean vector output
  diptest.vals <- sapply(1:ncol(data), function(c) dip.test(data[,c])$p.value<dip.test.alpha)

  # Is each index within 2 indexes of a TRUE?
  # i - the index to test
  # returns - TRUE if any index in the range (i-2 : i+2) is TRUE, FALSE otherwise
  region.is.valid <- function(i) any(diptest.vals[max(1, i-2):min(i+2, length(diptest.vals))])

  # Expand the single significant diptest results to +-2 surrounding indexes
  # Generates 'blocks' of T and F. Adjacent dip test regions will merge
  expanded.blocks <- sapply(1:length(diptest.vals), region.is.valid )

  # Find T/F block boundaries - where do we switch from T to F?
  is.block.start <- sapply(1:length(expanded.blocks), function(i) i==1 || expanded.blocks[i-1]!=expanded.blocks[i])
  is.block.end <- sapply(1:length(expanded.blocks), function(i) expanded.blocks[i+2]!=expanded.blocks[i+1] || i==length(expanded.blocks))

  # Find start and end indexes of the blocks that are of interest
  start.indexes <- which(is.block.start & expanded.blocks)
  end.indexes <- which(is.block.end & expanded.blocks)

  # Subset the input data
  result <- mapply( function(s, e) data[, s:e], start.indexes, end.indexes, SIMPLIFY = FALSE )
  if(length(result)>0){
    names(result) <- paste0(data.name, "_", start.indexes, ":", end.indexes)
  }
  result
}
