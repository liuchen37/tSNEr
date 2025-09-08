#' Run tSNEr Analysis
#'
#' @description
#' Main function to run interactive t-SNE analysis on FCS files.
#'
#' @param working_dir Character. Directory containing FCS files (default: current directory)
#' @return A list containing analysis results, plot files, and report filename
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' result <- tSNEr()
#'
#' # Specify directory
#' result <- tSNEr(working_dir = "/path/to/fcs/files")
#' }
#'
tSNEr <- function(working_dir = NULL) {

  # Set working directory if provided
  if (!is.null(working_dir)) {
    setwd(working_dir)
  }

  tryCatch({
    cat("\n🚀 STARTING t-SNE/viSNE ANALYSIS TOOL v1.0\n")
    cat("=" , rep("=", 59), "\n\n", sep="")

    # Find and select multiple FCS files
    selected_files <- find_and_select_multiple_fcs()

    # Load all selected files
    loaded_data <- load_multiple_fcs(selected_files)
    data <- loaded_data$data
    file_info <- loaded_data$file_info

    # Confirm/modify group names
    group_info <- confirm_file_groups(data, file_info)

    # Optional: Downsample if needed
    data <- downsample_data(data)

    # Select markers for analysis
    selected_markers <- select_markers(data)

    # Get t-SNE parameters
    tsne_params <- get_tsne_params(nrow(data))

    # Run t-SNE analysis
    tsne_coords <- run_tsne_analysis(data, selected_markers, tsne_params)

    # Create visualisation plots with improved layout
    plot_result <- create_visualisation_plots(data, tsne_coords, selected_markers, group_info)

    # Generate HTML report
    output_file <- generate_html_report(plot_result, group_info, tsne_params, file_info)

    cat("\n", rep("=", 60), "\n", sep="")
    cat("🎉 ANALYSIS COMPLETE!\n")
    cat(rep("=", 60), "\n", sep="")
    cat(sprintf("\n📊 Open the report: %s\n", output_file))
    cat("✨ Enjoy exploring your data!\n\n")

    # Ask if user wants to save the combined data
    save_data <- readline(prompt = "\nSave combined data with t-SNE coordinates as CSV? (y/n): ")
    if (tolower(save_data) == "y") {
      csv_file <- paste0("tsne_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      write.csv(plot_result$data, csv_file, row.names = FALSE)
      cat(sprintf("📊 Data saved as: %s\n", csv_file))
    }

    return(list(
      data = plot_result$data,
      file = output_file,
      plot_result = plot_result,
      tsne_params = tsne_params,
      group_info = group_info,
      file_info = file_info
    ))

  }, error = function(e) {
    cat("\n", rep("=", 60), "\n", sep="")
    cat("ERROR OCCURRED:\n")
    cat(rep("=", 60), "\n", sep="")
    cat(sprintf("%s\n", e$message))
    cat("\nPlease check your input and try again.\n")
    return(NULL)
  })
}
