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
