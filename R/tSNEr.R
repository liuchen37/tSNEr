
#' Run tSNEr Analysis
#'
#' @description
#' Main function to run interactive t-SNE/viSNE analysis on flow cytometry data.
#' This function provides an interactive interface for file selection, parameter
#' tuning, and visualization generation.
#'
#' @param working_dir Character. Optional directory containing FCS files (default: current directory)
#'
#' @return A list containing:
#' \itemize{
#'   \item data: Data frame with t-SNE coordinates and original data
#'   \item file: Path to generated HTML report
#'   \item plot_result: List of plot files and metadata
#'   \item tsne_params: Parameters used for t-SNE
#'   \item group_info: Group mapping information
#'   \item file_info: Information about loaded FCS files
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Run analysis in current directory
#' result <- tSNEr()
#'
#' # Run analysis in specific directory
#' result <- tSNEr(working_dir = "/path/to/fcs/files")
#' }
#'
tSNEr <- function(working_dir = NULL) {
  if (!is.null(working_dir)) {
    if (!dir.exists(working_dir)) {
      stop("Directory does not exist: ", working_dir)
    }
    setwd(working_dir)
  }

  # Call the main function from your original code
  main()
}

