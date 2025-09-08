#' File Operations for tSNEr
#'
#' @name file_operations
#' @rdname file_operations
NULL

#' @export

#' @export
find_and_select_multiple_fcs <- function() {
  cat("\n", rep("=", 60), "\n", sep="")
  cat("MULTI-FILE FCS t-SNE/viSNE ANALYSIS TOOL\n")
  cat(rep("=", 60), "\n\n", sep="")

  # Get current working directory (R project folder)
  project_dir <- getwd()
  cat("Searching for FCS files in project folder:\n")
  cat(project_dir, "\n\n")

  # Find all FCS files recursively
  fcs_files <- list.files(path = project_dir,
                          pattern = "\\.fcs$",
                          recursive = TRUE,
                          ignore.case = TRUE,
                          full.names = TRUE)

  if (length(fcs_files) == 0) {
    cat("No FCS files found in the project folder!\n")
    cat("Please ensure your FCS files are in the project directory.\n")
    stop("No FCS files found")
  }

  # Display found files
  cat(sprintf("Found %d FCS file(s):\n", length(fcs_files)))
  cat(rep("-", 60), "\n", sep="")

  # Create cleaner display names (relative paths)
  display_names <- gsub(paste0("^", project_dir, "/"), "", fcs_files)

  for (i in seq_along(fcs_files)) {
    # Get file info
    file_info <- file.info(fcs_files[i])
    file_size <- round(file_info$size / 1024^2, 2) # Convert to MB

    # Extract just the filename for cleaner display
    filename <- basename(fcs_files[i])

    cat(sprintf("%2d. %s (%.2f MB) - %s\n", i, filename, file_size, display_names[i]))
  }

  cat(rep("-", 60), "\n\n", sep="")

  # Let user select files
  cat("Selection options:\n")
  cat("  - Enter file numbers separated by commas (e.g., 1,2,3)\n")
  cat("  - Type 'all' to select all files\n")
  cat("  - Type 'range:1-3' to select a range\n\n")

  selection <- readline(prompt = "Your selection: ")

  # Parse selection
  if (tolower(selection) == "all") {
    selected_indices <- seq_along(fcs_files)
  } else if (grepl("^range:", selection)) {
    range_str <- sub("range:", "", selection)
    range_parts <- as.numeric(strsplit(range_str, "-")[[1]])
    selected_indices <- range_parts[1]:range_parts[2]
  } else {
    selected_indices <- as.numeric(strsplit(selection, ",")[[1]])
  }

  # Validate selection
  selected_indices <- selected_indices[selected_indices >= 1 & selected_indices <= length(fcs_files)]

  if (length(selected_indices) == 0) {
    cat("Invalid selection. Please try again.\n")
    return(find_and_select_multiple_fcs())
  }

  selected_files <- fcs_files[selected_indices]

  cat(sprintf("\nSelected %d file(s):\n", length(selected_files)))
  for (i in seq_along(selected_files)) {
    cat(sprintf("  %d. %s\n", i, basename(selected_files[i])))
  }

  confirm <- readline(prompt = "\nConfirm selection? (y/n): ")
  if (tolower(confirm) != "y") {
    return(find_and_select_multiple_fcs())
  }

  return(selected_files)
}

# Function to load and combine multiple FCS files

#' @export
load_multiple_fcs <- function(file_paths) {
  cat("\n", rep("=", 60), "\n", sep="")
  cat("LOADING FCS FILES...\n")
  cat(rep("=", 60), "\n\n", sep="")

  combined_data <- NULL
  file_info_list <- list()

  for (i in seq_along(file_paths)) {
    cat(sprintf("Loading file %d/%d: %s\n", i, length(file_paths), basename(file_paths[i])))

    # Load FCS file
    fcs_data <- read.FCS(file_paths[i], transformation = FALSE)

    # Convert to data frame
    df <- as.data.frame(exprs(fcs_data))

    # Add file/group identifier
    df$FileGroup <- basename(file_paths[i])
    df$FileGroup <- gsub("\\.fcs$", "", df$FileGroup, ignore.case = TRUE)  # Remove .fcs extension
    df$FileIndex <- i

    # Store file info
    file_info_list[[i]] <- list(
      filename = basename(file_paths[i]),
      n_cells = nrow(df),
      n_markers = ncol(df) - 2,  # Exclude FileGroup and FileIndex
      group_name = df$FileGroup[1]
    )

    # Combine data
    if (is.null(combined_data)) {
      combined_data <- df
    } else {
      # Check if columns match
      common_cols <- intersect(colnames(combined_data), colnames(df))
      if (length(common_cols) < ncol(df) - 2) {
        cat("Warning: Files have different markers. Using common markers only.\n")
      }
      combined_data <- rbind(combined_data[, common_cols], df[, common_cols])
    }
  }

  # Display summary
  cat("\n", rep("-", 60), "\n", sep="")
  cat("LOADING SUMMARY:\n")
  cat(rep("-", 60), "\n", sep="")

  total_cells <- sum(sapply(file_info_list, function(x) x$n_cells))
  cat(sprintf("Total cells loaded: %d\n", total_cells))
  cat("\nCells per file:\n")

  for (info in file_info_list) {
    cat(sprintf("  %s: %d cells\n", info$group_name, info$n_cells))
  }

  cat(sprintf("\nTotal markers (common): %d\n", ncol(combined_data) - 2))
  cat(rep("-", 60), "\n", sep="")

  return(list(data = combined_data, file_info = file_info_list))
}

# Function to confirm/modify group names for multiple files
