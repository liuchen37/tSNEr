# Conversion of tSNEr.r to package format

# Read original file
original_file <- "tSNEr.r"
if (!file.exists(original_file)) {
  stop("Cannot find tSNEr.r file. Please check the path.")
}

current <- readLines(original_file)
cat("Read", length(current), "lines from", original_file, "\n")

# Remove installation section (everything before main code)
# Look for common patterns that indicate start of actual functions
patterns <- c(
  "#==========================#",
  "#--------------------------#",
  "# MAIN FUNCTIONS",
  "# Functions",
  "main\\s*<-\\s*function",
  "^[a-zA-Z_][a-zA-Z0-9_.]*\\s*<-\\s*function"
)

start_line <- NULL
for (pattern in patterns) {
  matches <- grep(pattern, current, ignore.case = TRUE)
  if (length(matches) > 0) {
    start_line <- matches[1]
    cat("Found code start at line", start_line, "with pattern:", pattern, "\n")
    break
  }
}

# If no pattern found, keep everything
if (is.null(start_line)) {
  cat("No separator found, keeping all code\n")
  functions_only <- current
} else {
  # Skip any comment lines immediately after the separator
  while (start_line <= length(current) &&
         grepl("^#|^\\s*$", current[start_line])) {
    start_line <- start_line + 1
  }
  functions_only <- current[start_line:length(current)]
}

# Add package header
header <- '# tSNEr Package Functions
# Auto-generated from tSNEr.r

#\' @import flowCore Rtsne ggplot2 dplyr tidyr gridExtra htmltools base64enc viridis knitr jsonlite
#\' @importFrom RColorBrewer brewer.pal
#\' @importFrom grDevices dev.off png pdf
#\' @importFrom stats scale
#\' @importFrom utils write.csv readline
NULL

'

# Write to package file
output_file <- "R/functions.R"
writeLines(c(header, functions_only), output_file)
cat("Written", length(functions_only), "lines to", output_file, "\n")

# Create main wrapper function
wrapper <- '
#\' Run tSNEr Analysis
#\'
#\' @description
#\' Main function to run interactive t-SNE/viSNE analysis on flow cytometry data.
#\' This function provides an interactive interface for file selection, parameter
#\' tuning, and visualization generation.
#\'
#\' @param working_dir Character. Optional directory containing FCS files (default: current directory)
#\'
#\' @return A list containing:
#\' \\itemize{
#\'   \\item data: Data frame with t-SNE coordinates and original data
#\'   \\item file: Path to generated HTML report
#\'   \\item plot_result: List of plot files and metadata
#\'   \\item tsne_params: Parameters used for t-SNE
#\'   \\item group_info: Group mapping information
#\'   \\item file_info: Information about loaded FCS files
#\' }
#\'
#\' @export
#\'
#\' @examples
#\' \\dontrun{
#\' # Run analysis in current directory
#\' result <- tSNEr()
#\'
#\' # Run analysis in specific directory
#\' result <- tSNEr(working_dir = "/path/to/fcs/files")
#\' }
#\'
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
'

writeLines(wrapper, "R/tSNEr.R")
cat("Created main wrapper function in R/tSNEr.R\n")

# Package setup
devtools::document()

# Check that everything is in place
list.files()
list.files("R")

# Run package QC
devtools::check()

# Install package
devtools::install()

# Install locally for testing
devtools::install()
