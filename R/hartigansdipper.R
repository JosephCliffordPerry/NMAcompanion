#' non-unimodality detector(grouped)
#' @importFrom diptest dip.test
#' @param dataset the dataset you want to take multimodal columns from
#' this function can itterate through the colunns of a dataset and add the
#' multimodal columns to a list in blocks of 5 this is designed aroud it being used
#' on regions of the profile regions of the dataset


# a function that itterates through a dataset and adds multimodal data to a list
hartigansdipper <- function(dataset) {
  selected_datasets <- list()

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
  return(selected_datasets)
}


#' Get columns of a data.frame that are non-unimodal
#'
#' Performs Hartigan's dip-test, and returns subset data frames containing
#' potentially non-unimodal columns plus the surrounding two columns.
#'
#' @param data the data frame to input
#' @param dip.test.alpha the p-value threshold for the dip test
#' @param is.profile whether the region is a profile or not
#'
#' @return a list of data.frames, containing the columns of interest
#' @export
#'
#'
#'
get.dip.test.regions <- function(data, dip.test.alpha = 0.05, is.profile = TRUE) {
  # run diptest across all columns, get boolean vector output
  diptest.vals <- sapply(1:NCOL(data), function(c) dip.test(data[, c])$p.value < dip.test.alpha)

  if (is.profile) {
    col1.name <- colnames(data)[1]
    data.name <- gsub("_\\d+$", "", col1.name)

    # Is each index within 2 indexes of a TRUE?
    region.is.valid <- function(i) any(diptest.vals[max(1, i - 2):min(i + 2, length(diptest.vals))])

    expanded.blocks <- sapply(1:length(diptest.vals), region.is.valid)

    is.block.start <- c(TRUE, diff(expanded.blocks) != 0)
    is.block.end <- c(diff(expanded.blocks) != 0, TRUE)

    start.indexes <- which(is.block.start & expanded.blocks)
    end.indexes <- which(is.block.end & expanded.blocks)

    result <- mapply(function(s, e) data[, s:e], start.indexes, end.indexes, SIMPLIFY = FALSE)
    if (length(result) > 0) {
      names(result) <- paste0(data.name, start.indexes - 1, ":", end.indexes - 1)
      # check if start and end indexes overlap and if so combine
      if (end.indexes[length(end.indexes)] == length(expanded.blocks) & start.indexes[1] == 1) {
        result_df <- as.data.frame(append(result[length(result)],result[[1]]))
        result[[1]]<- result_df
        names(result)[1]<-paste0(names(result[1]),":",names(result[length(result)]))
        result[[length(result)]]<- NULL
      }
      }
  } else {
    col1.name <- colnames(data)
    data.name <- gsub("_\\d+$", "", col1.name)[diptest.vals]
    result <- data[diptest.vals]
    names(result) <- paste0(data.name, which(diptest.vals) - 1)
  }

  result
}
