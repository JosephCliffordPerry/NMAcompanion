# a function that itterates through a dataset and adds single column bimodal data to a list
monohartigansdipper<-function(dataset,selected_datasets){

  # Iterate over each column of the dataset
  i <- 1
  while (i <= ncol(dataset)) {
    # Perform Hartigan's Dip Test
    dip_test <- dip.test(dataset[[i]])

    # Check if the distribution is bimodal
    if (dip_test$p.value < 0.05) {

      # Extract the relevant columns
      selected_dataset <- dataset[i]

      # Add 'CellID' column from the original dataset
      # selected_dataset$CellID <- data$CellID
      # make the dataset name
      dataset_label <- colnames(selected_dataset)

      # Get the dataset name
      dataset_name <- paste0("Dataset_", paste0(colnames(selected_dataset), collapse = "_"))

      # Append the selected dataset to the list with the dataset name as the label
      selected_datasets[[dataset_name]] <- selected_dataset


      # Update the index to the next column after the extracted columns
      i <- i + 1
    } else {
      # Move to the next column
      i <- i + 1
    }
  }
  return(selected_datasets)}
