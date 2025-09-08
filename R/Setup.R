# Setup script for tSNEFlowAnalyzer package

# 1. Set up basic package infrastructure
usethis::use_mit_license("Chen Liu")
usethis::use_readme_md()
usethis::use_news_md()
usethis::use_git()
usethis::use_github()

# 2. Edit DESCRIPTION file
usethis::use_description(
  fields = list(
    Title = "Interactive t-SNE Analysis Tool for Flow Cytometry Data",
    Description = "A comprehensive tool for performing t-SNE analysis on flow cytometry
    data (.fcs) with interactive HTML reports, customizable PDF exports, and drag-and-drop
    plot arrangements. Supports multiple FCS file analysis with unified scaling across
    groups for direct comparison.",
    `Authors@R` = 'person("Chen", ":Liu", email = "chen.liu@umontreal.ca")',
    Version = "1.0.0",
    Depends = "R (>= 4.0.0)"
  )
)

# 3. Add dependencies
usethis::use_package("flowCore")
usethis::use_package("Rtsne")
usethis::use_package("ggplot2")
usethis::use_package("dplyr")
usethis::use_package("tidyr")
usethis::use_package("gridExtra")
usethis::use_package("htmltools")
usethis::use_package("RColorBrewer")
usethis::use_package("base64enc")
usethis::use_package("viridis")
usethis::use_package("knitr")
usethis::use_package("jsonlite")

# 4. Create R directory and main file
dir.create("R", showWarnings = FALSE)

# 5. Save this as your main package file
main_code <- '
#\' Run t-SNE/viSNE Analysis
#\'
#\' Main function to run complete t-SNE/viSNE analysis pipeline on flow cytometry data
#\'
#\' @param working_dir Optional. Set working directory for FCS files. Default is current directory.
#\' @return A list containing analysis results, plot files, and report filename
#\' @export
#\' @examples
#\' \\dontrun{
#\' # Run analysis in current directory
#\' result <- run_tsne_analysis()
#\'
#\' # Run analysis in specific directory
#\' result <- run_tsne_analysis("/path/to/fcs/files")
#\' }
run_tsne_analysis <- function(working_dir = NULL) {

  # Set working directory if provided
  if (!is.null(working_dir)) {
    setwd(working_dir)
  }

  # Your main function code here
  # Call the internal functions

  tryCatch({
    cat("\\n🚀 STARTING t-SNE ANALYSIS TOOL v1.0\\n")

    # Rest of your main function...

  }, error = function(e) {
    cat("\\nERROR OCCURRED:\\n")
    cat(sprintf("%s\\n", e$message))
    return(NULL)
  })
}
'

writeLines(main_code, "R/run_analysis.R")

# 6. Create a file for all your helper functions
# Copy your complete code into separate function files in the R/ directory

# 7. Document the package
devtools::document()

# 8. Create vignette
usethis::use_vignette("getting-started")

# 9. Set up testing
usethis::use_testthat()

# 10. Check the package
devtools::check()
