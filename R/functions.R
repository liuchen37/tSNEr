# tSNEr Package Functions
# Auto-generated from tSNEr.r

#' @import flowCore Rtsne ggplot2 dplyr tidyr gridExtra htmltools base64enc viridis knitr jsonlite
#' @importFrom RColorBrewer brewer.pal
#' @importFrom grDevices dev.off png pdf
#' @importFrom stats scale
#' @importFrom utils write.csv readline
NULL


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
confirm_file_groups <- function(data, file_info) {
  cat("\n", rep("=", 60), "\n", sep="")
  cat("GROUP NAMES (from file names):\n")
  cat(rep("=", 60), "\n\n", sep="")

  # Get unique groups from FileGroup column
  groups <- unique(data$FileGroup)

  cat("Current group names:\n")
  for (i in seq_along(groups)) {
    n_cells <- sum(data$FileGroup == groups[i])
    cat(sprintf("%d. %s (n=%d cells)\n", i, groups[i], n_cells))
  }

  # Ask if user wants to rename
  rename <- readline(prompt = "\nDo you want to rename any groups? (y/n): ")

  group_mapping <- setNames(groups, groups)

  if (tolower(rename) == "y") {
    for (group in groups) {
      new_name <- readline(prompt = sprintf("New name for '%s' (press Enter to keep): ", group))
      if (new_name != "") {
        group_mapping[group] <- new_name
      }
    }
  }

  cat("\nFinal group names:\n")
  for (old in names(group_mapping)) {
    n_cells <- sum(data$FileGroup == old)
    cat(sprintf("  %s -> %s (n=%d)\n", old, group_mapping[old], n_cells))
  }

  return(list(mapping = group_mapping, column = "FileGroup"))
}

# Function to select markers for t-SNE analysis
select_markers <- function(data) {
  cat("\n", rep("=", 60), "\n", sep="")
  cat("AVAILABLE MARKERS/CHANNELS:\n")
  cat(rep("=", 60), "\n\n", sep="")

  # Exclude FileGroup and FileIndex from markers
  markers <- colnames(data)[!colnames(data) %in% c("FileGroup", "FileIndex")]

  # Display all markers with index
  for (i in seq_along(markers)) {
    cat(sprintf("  %3d. %s\n", i, markers[i]))
  }

  cat("\n", rep("-", 60), "\n", sep="")
  cat("Selection options:\n")
  cat("  - Enter marker numbers separated by commas (e.g., 1,3,5,7)\n")
  cat("  - Type 'all' to select all markers\n")
  cat("  - Type 'range:1-10' to select a range\n")
  cat(rep("-", 60), "\n\n", sep="")

  selection <- readline(prompt = "Your selection: ")

  # Parse selection
  if (tolower(selection) == "all") {
    selected_markers <- markers
  } else if (grepl("^range:", selection)) {
    range_str <- sub("range:", "", selection)
    range_parts <- as.numeric(strsplit(range_str, "-")[[1]])
    selected_markers <- markers[range_parts[1]:range_parts[2]]
  } else {
    indices <- as.numeric(strsplit(selection, ",")[[1]])
    selected_markers <- markers[indices]
  }

  # Remove any NA values
  selected_markers <- selected_markers[!is.na(selected_markers)]

  # Display selected markers
  cat(sprintf("\nSelected %d markers:\n", length(selected_markers)))
  for (marker in selected_markers) {
    cat(sprintf("  - %s\n", marker))
  }

  confirm <- readline(prompt = "\nConfirm selection? (y/n): ")
  if (tolower(confirm) != "y") {
    return(select_markers(data))
  }

  return(selected_markers)
}

# Function to downsample data if needed
downsample_data <- function(data, max_cells_per_group = 10000) {
  cat("\n", rep("=", 60), "\n", sep="")
  cat("DATA SAMPLING:\n")
  cat(rep("=", 60), "\n", sep="")

  groups <- unique(data$FileGroup)
  cells_per_group <- table(data$FileGroup)

  cat("Current cell counts per group:\n")
  for (group in names(cells_per_group)) {
    cat(sprintf("  %s: %d cells\n", group, cells_per_group[group]))
  }

  # Check if downsampling is needed
  if (any(cells_per_group > max_cells_per_group)) {
    cat(sprintf("\nSome groups have more than %d cells.\n", max_cells_per_group))
    downsample <- readline(prompt = sprintf("Downsample to %d cells per group? (y/n): ", max_cells_per_group))

    if (tolower(downsample) == "y") {
      custom_max <- readline(prompt = sprintf("Enter max cells per group [%d]: ", max_cells_per_group))
      if (custom_max != "") {
        max_cells_per_group <- as.numeric(custom_max)
      }

      # Perform downsampling
      sampled_data <- NULL
      for (group in groups) {
        group_data <- data[data$FileGroup == group, ]
        n_cells <- nrow(group_data)

        if (n_cells > max_cells_per_group) {
          sampled_indices <- sample(1:n_cells, max_cells_per_group)
          group_data <- group_data[sampled_indices, ]
          cat(sprintf("  Downsampled %s: %d -> %d cells\n", group, n_cells, max_cells_per_group))
        }

        if (is.null(sampled_data)) {
          sampled_data <- group_data
        } else {
          sampled_data <- rbind(sampled_data, group_data)
        }
      }

      cat(sprintf("\nTotal cells after downsampling: %d\n", nrow(sampled_data)))
      return(sampled_data)
    }
  }

  return(data)
}

# Function to get t-SNE parameters from user
get_tsne_params <- function(n_cells) {
  cat("\n", rep("=", 60), "\n", sep="")
  cat("t-SNE/viSNE PARAMETERS\n")
  cat(rep("=", 60), "\n", sep="")
  cat("Press Enter to use default values shown in brackets\n")
  cat(rep("-", 60), "\n\n", sep="")

  params <- list()

  # Suggest perplexity based on number of cells
  suggested_perp <- min(30, floor(sqrt(n_cells/100)))
  perp <- readline(prompt = sprintf("Perplexity [%d]: ", suggested_perp))
  params$perplexity <- ifelse(perp == "", suggested_perp, as.numeric(perp))

  # Iterations
  iter <- readline(prompt = "Number of iterations [1000]: ")
  params$max_iter <- ifelse(iter == "", 1000, as.numeric(iter))

  # Learning rate (eta)
  eta <- readline(prompt = "Learning rate (eta) [200]: ")
  params$eta <- ifelse(eta == "", 200, as.numeric(eta))

  # Theta (for Barnes-Hut)
  theta <- readline(prompt = "Theta (0 for exact t-SNE) [0.5]: ")
  params$theta <- ifelse(theta == "", 0.5, as.numeric(theta))

  # Random seed
  seed <- readline(prompt = "Random seed [42]: ")
  params$seed <- ifelse(seed == "", 42, as.numeric(seed))

  # PCA preprocessing
  pca <- readline(prompt = "Initial PCA dimensions (FALSE for no PCA) [50]: ")
  if (pca == "" ) {
    params$pca <- 50
  } else if (tolower(pca) == "false") {
    params$pca <- FALSE
  } else {
    params$pca <- as.numeric(pca)
  }

  cat("\n", rep("-", 60), "\n", sep="")
  cat("Selected parameters:\n")
  for (name in names(params)) {
    cat(sprintf("  %s: %s\n", name, params[[name]]))
  }

  return(params)
}

# Function to run t-SNE analysis
run_tsne_analysis <- function(data, selected_markers, tsne_params) {
  cat("\n", rep("=", 60), "\n", sep="")
  cat("RUNNING t-SNE...\n")
  cat(rep("=", 60), "\n\n", sep="")

  # Prepare data matrix
  X <- as.matrix(data[, selected_markers])

  # Scale the data
  cat("Scaling data...\n")
  X_scaled <- scale(X)

  # Handle any NaN or infinite values
  X_scaled[!is.finite(X_scaled)] <- 0

  # Set seed for reproducibility
  set.seed(tsne_params$seed)

  # Run t-SNE
  cat("Fitting t-SNE model...\n")
  tsne_result <- Rtsne(X_scaled,
                       dims = 2,
                       perplexity = tsne_params$perplexity,
                       max_iter = tsne_params$max_iter,
                       eta = tsne_params$eta,
                       theta = tsne_params$theta,
                       pca = tsne_params$pca,
                       verbose = TRUE)

  cat("t-SNE completed!\n")
  return(tsne_result$Y)
}

# Function to create static visualisation plots
create_visualisation_plots <- function(data, tsne_coords, selected_markers, group_info) {
  cat("\n", rep("=", 60), "\n", sep="")
  cat("CREATING VISUALISATIONS...\n")
  cat(rep("=", 60), "\n\n", sep="")

  # Add t-SNE coordinates to data
  data$tSNE1 <- tsne_coords[, 1]
  data$tSNE2 <- tsne_coords[, 2]

  # Apply group mapping
  data$Group_Display <- group_info$mapping[data[[group_info$column]]]

  groups <- unique(data$Group_Display)

  # Create directory for plots
  plot_dir <- paste0("tsne_plots_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  dir.create(plot_dir, showWarnings = FALSE)

  # Limit number of markers for display
  max_markers_display <- 12
  if (length(selected_markers) > max_markers_display) {
    cat(sprintf("Note: Displaying first %d markers in visualisation\n", max_markers_display))
    display_markers <- selected_markers[1:max_markers_display]
  } else {
    display_markers <- selected_markers
  }

  # Define reversed rainbow colour palette (blue to red)
  reversed_rainbow <- c("#0000FF", "#0080FF", "#00FFFF", "#00FF80",
                        "#00FF00", "#80FF00", "#FFFF00", "#FF8000",
                        "#FF0000")

  # Calculate global colour scales for each marker
  cat("Calculating unified colour scales for each marker...\n")
  marker_scales <- list()
  for (marker in display_markers) {
    # Get all values for this marker across all groups
    all_values <- data[[marker]]
    q01 <- quantile(all_values, 0.01, na.rm = TRUE)
    q99 <- quantile(all_values, 0.99, na.rm = TRUE)
    marker_scales[[marker]] <- c(q01, q99)
    cat(sprintf("  %s: range [%.2f, %.2f]\n", marker, q01, q99))
  }

  # Generate plots for each group
  plot_files <- list()

  for (group in groups) {
    group_data <- data[data$Group_Display == group, ]
    group_files <- list()

    # Downsample for visualisation if too many points
    if (nrow(group_data) > 5000) {
      plot_indices <- sample(1:nrow(group_data), 5000)
      plot_data <- group_data[plot_indices, ]
      cat(sprintf("Downsampling %s for visualisation: %d -> 5000 points\n",
                  group, nrow(group_data)))
    } else {
      plot_data <- group_data
    }

    for (marker in display_markers) {
      # Use the global scale for this marker
      scale_limits <- marker_scales[[marker]]

      # Create plot WITHOUT title (since it's shown in HTML)
      p <- ggplot(plot_data, aes(x = tSNE1, y = tSNE2, colour = .data[[marker]])) +
        geom_point(size = 0.5, alpha = 0.7) +
        scale_color_gradientn(colours = reversed_rainbow,
                              limits = scale_limits,
                              oob = scales::squish,
                              name = "Expression") +
        labs(x = "tSNE_c_1",
             y = "tSNE_c_2") +  # Removed title here
        coord_fixed(ratio = 1) +  # Force square aspect ratio
        theme_minimal() +
        theme(axis.text = element_text(size = 8),
              axis.title = element_text(size = 10),
              legend.title = element_text(size = 9),
              legend.text = element_text(size = 8),
              legend.position = "right",
              plot.margin = margin(5, 5, 5, 5))

      # Save plot
      filename <- sprintf("%s/%s_%s.png", plot_dir,
                          gsub("[^A-Za-z0-9]", "_", group),
                          gsub("[^A-Za-z0-9]", "_", marker))
      ggsave(filename, p, width = 5, height = 4, dpi = 100)

      group_files[[marker]] <- filename
    }

    plot_files[[group]] <- group_files
  }

  cat(sprintf("\n✅ Plots saved in directory: %s\n", plot_dir))

  return(list(data = data, plot_files = plot_files, plot_dir = plot_dir,
              display_markers = display_markers, all_markers = selected_markers,
              marker_scales = marker_scales))
}

# Function to create plots with better legend placement and readable text
create_combined_plots <- function(data, all_markers, display_markers, output_dir = "tsne_plots") {
  cat("\n", rep("=", 60), "\n", sep="")
  cat("CREATING VISUALISATIONS...\n")
  cat(rep("=", 60), "\n\n", sep="")

  # Create output directory
  dir.create(output_dir, showWarnings = FALSE)

  # Get unique groups
  groups <- unique(data$Group_Display)
  cat(sprintf("📊 Creating plots for %d groups\n", length(groups)))
  cat(sprintf("📈 Plotting %d markers (out of %d total)\n",
              length(display_markers), length(all_markers)))

  # Store plot file paths
  plot_files <- list()

  # Create progress bar
  total_plots <- length(groups) * length(display_markers)
  pb <- txtProgressBar(min = 0, max = total_plots, style = 3)
  plot_counter <- 0

  # Process each group
  for(group in groups) {
    cat(sprintf("\n\n🔹 Processing group: %s\n", group))

    # Filter data for this group
    group_data <- data[data$Group_Display == group, ]
    cat(sprintf("   • Cells in group: %s\n", format(nrow(group_data), big.mark = ",")))

    # Initialize plot file list for this group
    plot_files[[group]] <- list()

    # Create plots for each marker
    for(marker in display_markers) {
      plot_counter <- plot_counter + 1
      setTxtProgressBar(pb, plot_counter)

      # Check if marker exists in data
      if(!(marker %in% colnames(group_data))) {
        cat(sprintf("\n   ⚠️  Warning: Marker '%s' not found in data for group '%s'. Skipping.\n",
                    marker, group))
        next
      }

      # Get global min and max for this marker across all groups
      global_min <- min(data[[marker]], na.rm = TRUE)
      global_max <- max(data[[marker]], na.rm = TRUE)

      # Create the plot with legend at bottom
      p <- ggplot(group_data, aes(x = tSNE_1, y = tSNE_2, color = .data[[marker]])) +
        geom_point(size = 0.5, alpha = 0.7) +
        scale_color_viridis_c(
          name = "Expression",
          option = "viridis",
          limits = c(global_min, global_max),
          breaks = pretty(c(global_min, global_max), n = 5),
          labels = function(x) format(round(x, 1), nsmall = 1)
        ) +
        coord_fixed(ratio = 1) +
        theme_minimal(base_size = 14) +  # Increased base size
        theme(
          # Increase axis text size
          axis.text = element_text(size = 11),
          axis.title = element_text(size = 12, face = "bold"),

          # Legend at bottom with larger, more readable text
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.title = element_text(size = 11, face = "bold"),
          legend.text = element_text(size = 10),
          legend.key.width = unit(2, "cm"),  # Wider color bar
          legend.key.height = unit(0.4, "cm"),
          legend.margin = margin(t = 5, b = 5),

          # Panel appearance
          panel.background = element_rect(fill = "white", color = NA),
          panel.grid.major = element_line(color = "grey90", size = 0.5),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(color = "black", fill = NA, size = 1),

          # Plot margins - reduced top/bottom since legend is now at bottom
          plot.margin = margin(t = 5, r = 10, b = 5, l = 10, unit = "pt"),

          # Aspect ratio
          aspect.ratio = 1
        ) +
        labs(
          x = "tSNE_1",
          y = "tSNE_2"
        ) +
        guides(
          color = guide_colorbar(
            title.position = "top",
            title.hjust = 0.5,
            label.position = "bottom",
            label.hjust = 0.5,
            ticks = TRUE,
            ticks.colour = "black",
            ticks.linewidth = 0.5,
            frame.colour = "black",
            frame.linewidth = 0.5,
            barwidth = 15,  # Wider bar
            barheight = 0.5
          )
        )

      # Save the plot with adjusted dimensions
      filename <- paste0(output_dir, "/",
                         gsub("[^A-Za-z0-9_-]", "_", group), "_",
                         gsub("[^A-Za-z0-9_-]", "_", marker), ".png")

      # Save with higher DPI for better text clarity
      ggsave(filename, plot = p,
             width = 6, height = 6.5,  # Slightly taller to accommodate bottom legend
             dpi = 150,  # Higher DPI for clearer text
             bg = "white")

      # Store file path
      plot_files[[group]][[marker]] <- filename
    }
  }

  close(pb)

  cat("\n\n", rep("=", 60), "\n", sep="")
  cat("✅ PLOT GENERATION COMPLETE\n")
  cat(rep("=", 60), "\n", sep="")
  cat(sprintf("\n📁 Plots saved in: %s/\n", output_dir))
  cat(sprintf("📊 Total plots created: %d\n", plot_counter))
  cat("🎨 Legend positioned at bottom center for space efficiency\n")
  cat("📝 Text size increased for better readability\n\n")

  return(list(
    plot_files = plot_files,
    plot_dir = output_dir,
    all_markers = all_markers,
    display_markers = display_markers,
    data = data
  ))
}

# Function to generate customisable PDF from arrangement
generate_custom_pdf <- function(plot_result, arrangement_json) {
  # This function will be called from JavaScript via a temporary R script
  # arrangement_json contains the user's custom layout

  arrangement <- jsonlite::fromJSON(arrangement_json)

  pdf_file <- paste0("custom_tsne_arrangement_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
  pdf(pdf_file, width = length(arrangement$columns) * 3, height = length(arrangement$rows) * 3)

  # Create grid of plots based on arrangement
  plot_list <- list()
  for(i in seq_along(arrangement$rows)) {
    for(j in seq_along(arrangement$columns)) {
      group <- arrangement$rows[i]
      marker <- arrangement$columns[j]

      if(group %in% names(plot_result$plot_files) &&
         marker %in% names(plot_result$plot_files[[group]])) {
        # Load the plot
        plot_list[[length(plot_list) + 1]] <- plot_result$plot_files[[group]][[marker]]
      }
    }
  }

  # Arrange in grid
  gridExtra::grid.arrange(grobs = plot_list,
                         nrow = length(arrangement$rows),
                         ncol = length(arrangement$columns))

  dev.off()
  return(pdf_file)
}

# Function to generate HTML report with drag-and-drop interface and direct PDF download
generate_html_report <- function(plot_result, group_info, tsne_params, file_info) {
  cat("\n", rep("=", 60), "\n", sep="")
  cat("GENERATING HTML REPORT...\n")
  cat(rep("=", 60), "\n\n", sep="")

  data <- plot_result$data
  groups <- unique(data$Group_Display)

  # File information table
  file_table <- data.frame(
    File = sapply(file_info, function(x) x$filename),
    Group = sapply(file_info, function(x) x$group_name),
    Cells = sapply(file_info, function(x) format(x$n_cells, big.mark = ",")),
    stringsAsFactors = FALSE
  )

  # Create HTML content
  html_content <- paste0('
<!DOCTYPE html>
<html>
<head>
    <title>t-SNE Analysis Report</title>
    <meta charset="UTF-8">
    <!-- Add jsPDF library for PDF generation -->
    <script src="https://cdnjs.cloudflare.com/ajax/libs/jspdf/2.5.1/jspdf.umd.min.js"></script>
    <style>
        body {
            font-family: "Segoe UI", Arial, sans-serif;
            margin: 0;
            padding: 20px;
            background: #f5f5f5;
        }
        .container {
            max-width: 1600px;
            margin: 0 auto;
            background: white;
            padding: 25px;
            border-radius: 8px;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        h1 {
            color: #2c3e50;
            border-bottom: 2px solid #3498db;
            padding-bottom: 10px;
        }
        h2, h3 {
            color: #34495e;
            margin-top: 20px;
        }
        table {
            width: 100%;
            border-collapse: collapse;
            margin: 15px 0;
        }
        th {
            background: #3498db;
            color: white;
            padding: 10px;
            text-align: left;
        }
        td {
            padding: 8px;
            border-bottom: 1px solid #ecf0f1;
        }
        .info-box {
            background: #ecf0f1;
            padding: 15px;
            border-radius: 5px;
            margin: 15px 0;
        }
        .marker-naming-section {
            background: #f8f9fa;
            padding: 15px;
            border-radius: 8px;
            margin: 20px 0;
            border: 2px solid #3498db;
        }
        .marker-inputs {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
            gap: 10px;
            margin: 15px 0;
        }
        .marker-input-group {
            display: flex;
            align-items: center;
            background: white;
            padding: 5px;
            border-radius: 4px;
        }
        .marker-label {
            min-width: 120px;
            font-weight: bold;
            color: #2c3e50;
        }
        .marker-input {
            flex: 1;
            padding: 5px;
            border: 1px solid #ddd;
            border-radius: 3px;
        }
        button {
            background: #3498db;
            color: white;
            padding: 8px 15px;
            border: none;
            border-radius: 5px;
            cursor: pointer;
            margin-right: 10px;
            transition: all 0.3s;
        }
        button:hover {
            background: #2980b9;
            transform: translateY(-2px);
            box-shadow: 0 2px 5px rgba(0,0,0,0.2);
        }
        .secondary-button {
            background: #95a5a6;
        }
        .secondary-button:hover {
            background: #7f8c8d;
        }
        .group-header {
            background: linear-gradient(135deg, #3498db, #2980b9);
            color: white;
            padding: 12px;
            cursor: pointer;
            border-radius: 8px 8px 0 0;
        }
        .group-header:hover {
            background: linear-gradient(135deg, #2980b9, #3498db);
        }
        .group-plots {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(280px, 1fr));
            gap: 15px;
            padding: 15px;
            background: #fafafa;
        }
        .plot-container {
            text-align: center;
            background: white;
            padding: 8px;
            border-radius: 5px;
            box-shadow: 0 1px 3px rgba(0,0,0,0.1);
        }
        .plot-title {
            font-weight: bold;
            margin-bottom: 5px;
            color: #2c3e50;
        }
        .arrow {
            margin-right: 10px;
            display: inline-block;
            transition: transform 0.3s;
        }

        /* Custom Arrangement Styles */
        .arrangement-section {
            background: #f0f8ff;
            padding: 20px;
            border-radius: 8px;
            margin: 30px 0;
            border: 2px solid #3498db;
        }
        .arrangement-container {
            background: white;
            padding: 15px;
            border-radius: 5px;
            margin-top: 15px;
        }
        .experiment-name-section {
            background: #e8f4f8;
            padding: 15px;
            border-radius: 5px;
            margin-bottom: 20px;
            border: 1px solid #3498db;
        }
        .experiment-input {
            width: 100%;
            max-width: 500px;
            padding: 10px;
            font-size: 16px;
            border: 2px solid #3498db;
            border-radius: 4px;
            margin-top: 10px;
        }
        .experiment-input:focus {
            outline: none;
            border-color: #2980b9;
            box-shadow: 0 0 5px rgba(52, 152, 219, 0.3);
        }
        .drag-area {
            display: flex;
            gap: 20px;
            margin: 20px 0;
        }
        .source-panel {
            flex: 0 0 200px;
            background: #f8f9fa;
            padding: 15px;
            border-radius: 5px;
            max-height: 400px;
            overflow-y: auto;
        }
        .arrangement-grid {
            flex: 1;
            background: white;
            border: 2px solid #e0e0e0;
            border-radius: 5px;
            padding: 10px;
            min-height: 300px;
            overflow-x: auto;
        }
        .draggable-item {
            background: white;
            border: 1px solid #3498db;
            padding: 8px;
            margin: 5px;
            border-radius: 4px;
            cursor: move;
            transition: all 0.3s;
            font-size: 13px;
        }
        .draggable-item:hover {
            background: #e3f2fd;
            transform: translateX(5px);
        }
        .draggable-item.dragging {
            opacity: 0.5;
        }
        .grid-table {
            width: 100%;
            border-collapse: collapse;
            min-width: 600px;
        }
        .grid-cell {
            border: 1px solid #ddd;
            padding: 10px;
            min-height: 200px;
            min-width: 200px;
            position: relative;
            background: #fafafa;
        }
        .grid-cell.dragover {
            background: #e3f2fd;
            border: 2px dashed #3498db;
        }
        .grid-header {
            background: #3498db;
            color: white;
            font-weight: bold;
            padding: 10px;
            text-align: center;
            min-width: 200px;
        }
        .grid-row-header {
            background: #2c3e50;
            color: white;
            font-weight: bold;
            padding: 10px;
            text-align: center;
            min-width: 150px;
        }
        .placed-item {
            background: linear-gradient(135deg, #3498db, #2980b9);
            color: white;
            padding: 5px;
            border-radius: 3px;
            margin: 2px;
            font-size: 12px;
            display: inline-block;
        }
        .placed-item:hover {
            background: linear-gradient(135deg, #2980b9, #3498db);
        }
        .preview-area {
            margin-top: 20px;
            padding: 15px;
            background: #f8f9fa;
            border-radius: 5px;
        }
        .preview-grid {
            display: grid;
            gap: 10px;
            margin-top: 10px;
        }
        .preview-plot {
            background: white;
            padding: 5px;
            border-radius: 3px;
            box-shadow: 0 1px 3px rgba(0,0,0,0.1);
        }
        .preview-plot img {
            width: 100%;
            height: auto;
        }
        .success-button {
            background: #27ae60;
        }
        .success-button:hover {
            background: #229954;
        }
        .remove-button {
            background: #e74c3c;
            color: white;
            border: none;
            padding: 2px 6px;
            font-size: 10px;
            margin-left: 5px;
            border-radius: 2px;
            cursor: pointer;
        }
        .remove-button:hover {
            background: #c0392b;
        }
        .column-slot, .row-slot {
            padding: 5px;
            min-height: 30px;
        }
        .pdf-download-button {
            background: linear-gradient(135deg, #e74c3c, #c0392b);
        }
        .pdf-download-button:hover {
            background: linear-gradient(135deg, #c0392b, #e74c3c);
        }
        #pdf-status {
            display: inline-block;
            margin-left: 15px;
            padding: 8px 12px;
            background: #f39c12;
            color: white;
            border-radius: 4px;
            display: none;
        }

        .footer {
            margin-top: 30px;
            padding-top: 20px;
            border-top: 1px solid #ecf0f1;
            text-align: center;
            color: #7f8c8d;
        }
    </style>
</head>
<body>
    <div class="container">
        <h1>t-SNE/viSNE Analysis Report</h1>

        <div class="info-box">
            <h2>File Information</h2>
            ', knitr::kable(file_table, format = "html"), '
        </div>

        <!-- Marker Naming Section -->
        <div class="marker-naming-section">
            <h3>🏷️ Add Marker Names (Optional)</h3>
            <p style="color: #7f8c8d;">Add biological marker names to make channels more meaningful (e.g., CD3, CD4, CD8)</p>
            <div class="marker-inputs">
')

  # Add marker input fields
  for(marker in plot_result$all_markers) {
    html_content <- paste0(html_content, '
                <div class="marker-input-group">
                    <label class="marker-label">', marker, ':</label>
                    <input type="text" class="marker-input" data-channel="', marker, '"
                           placeholder="Enter marker name">
                </div>')
  }

  html_content <- paste0(html_content, '
            </div>
            <div>
                <button onclick="updateMarkerNames()">Update Names</button>
                <button class="secondary-button" onclick="expandAll()">Expand All Groups</button>
                <button class="secondary-button" onclick="collapseAll()">Collapse All Groups</button>
                <span id="update-status" style="margin-left: 15px;"></span>
            </div>
        </div>

        <div class="info-box">
            <h2>Analysis Parameters</h2>
            <p><strong>Total cells analysed:</strong> ', format(nrow(data), big.mark = ","), '</p>
            <p><strong>Channels analysed:</strong> ', length(plot_result$all_markers), '</p>
            <p><strong>Channels displayed:</strong> ', length(plot_result$display_markers), '</p>
            <p><strong>t-SNE parameters:</strong></p>
            <ul>')

  # Add t-SNE parameters
  for(param in names(tsne_params)) {
    html_content <- paste0(html_content, '
                <li>', param, ': ', tsne_params[[param]], '</li>')
  }

  html_content <- paste0(html_content, '
            </ul>
            <p><strong>Note:</strong> Colour scales are unified across groups for each marker to enable direct comparison.</p>
        </div>

        <h2>t-SNE Visualisations</h2>
        <p>Click group headers to expand/collapse. Plots have square aspect ratio for accurate representation.</p>
')

  # Add groups with plots
  for(i in seq_along(groups)) {
    group <- groups[i]
    group_id <- paste0("group-", i)
    n_cells <- sum(data$Group_Display == group)

    html_content <- paste0(html_content, '
        <div style="margin-bottom: 25px; border: 1px solid #ddd; border-radius: 8px; overflow: hidden;">
            <div class="group-header" onclick="toggleGroup(\'', group_id, '\')">
                <div style="display: flex; justify-content: space-between; align-items: center;">
                    <div>
                        <span class="arrow" id="arrow-', group_id, '">▼</span>
                        <strong style="font-size: 18px;">', group, '</strong>
                    </div>
                    <span style="background: rgba(255,255,255,0.2); padding: 4px 8px; border-radius: 4px;">
                        ', format(n_cells, big.mark = ","), ' cells
                    </span>
                </div>
            </div>
            <div id="', group_id, '" class="group-plots">
')

    for(marker in plot_result$display_markers) {
      img_path <- plot_result$plot_files[[group]][[marker]]
      img_base64 <- base64enc::dataURI(file = img_path, mime = "image/png")

      html_content <- paste0(html_content, '
                <div class="plot-container">
                    <div class="plot-title" data-channel="', marker, '">', marker, '</div>
                    <img src="', img_base64, '" data-group="', group, '" data-marker="', marker, '"
                         style="width: 100%; height: auto; border-radius: 3px;">
                </div>')
    }

    html_content <- paste0(html_content, '
            </div>
        </div>')
  }

  # Add custom arrangement section with experiment name input
  html_content <- paste0(html_content, '

        <!-- Custom Plot Arrangement Section -->
        <div class="arrangement-section">
            <h2>📊 Custom Plot Arrangement</h2>
            <p>Enter your experiment name and create a custom arrangement by dragging groups and markers to the grid below.</p>

            <div class="arrangement-container">
                <!-- Experiment Name Input -->
                <div class="experiment-name-section">
                    <label for="experiment-name" style="font-weight: bold; color: #2c3e50; font-size: 16px;">
                        🔬 Experiment Name:
                    </label>
                    <input type="text"
                           id="experiment-name"
                           class="experiment-input"
                           placeholder="Enter your experiment name (e.g., CD4 T-cell Analysis, Patient Sample 001)"
                           value="">
                    <p style="color: #7f8c8d; font-size: 12px; margin-top: 5px;">
                        This will appear as the title in your PDF export
                    </p>
                </div>

                <div class="drag-area">
                    <!-- Source panels -->
                    <div>
                        <div class="source-panel">
                            <h4>Groups</h4>
                            <div id="groups-source">
')

  # Add draggable groups
  for(group in groups) {
    html_content <- paste0(html_content, '
                                <div class="draggable-item" draggable="true" data-type="group" data-value="',
                           group, '">', group, '</div>')
  }

  html_content <- paste0(html_content, '
                            </div>
                        </div>

                        <div class="source-panel" style="margin-top: 20px;">
                            <h4>Markers</h4>
                            <div id="markers-source">
')

  # Add draggable markers
  for(marker in plot_result$display_markers) {
    html_content <- paste0(html_content, '
                                <div class="draggable-item" draggable="true" data-type="marker" data-value="',
                           marker, '">', marker, '</div>')
  }

  html_content <- paste0(html_content, '
                            </div>
                        </div>
                    </div>

                    <!-- Arrangement Grid -->
                    <div class="arrangement-grid">
                        <h4>Arrangement Grid</h4>
                        <p style="color: #7f8c8d; font-size: 14px;">Drag groups to rows (left) and markers to columns (top)</p>

                        <table class="grid-table" id="arrangement-table">
                            <thead>
                                <tr>
                                    <th class="grid-header">Groups \\ Markers</th>
                                    <th class="grid-header" id="col-header-1" ondrop="dropColumn(event, 1)" ondragover="allowDrop(event)">
                                        <div id="col-slot-1" class="column-slot">Drop marker here</div>
                                    </th>
                                    <th class="grid-header" id="col-header-2" ondrop="dropColumn(event, 2)" ondragover="allowDrop(event)">
                                        <div id="col-slot-2" class="column-slot">Drop marker here</div>
                                    </th>
                                    <th class="grid-header" id="col-header-3" ondrop="dropColumn(event, 3)" ondragover="allowDrop(event)">
                                        <div id="col-slot-3" class="column-slot">Drop marker here</div>
                                    </th>
                                    <th class="grid-header">
                                        <button onclick="addColumn()" style="padding: 2px 8px; font-size: 12px;">+ Add Column</button>
                                    </th>
                                </tr>
                            </thead>
                            <tbody id="grid-body">
                                <tr id="grid-row-1">
                                    <td class="grid-row-header" ondrop="dropRow(event, 1)" ondragover="allowDrop(event)">
                                        <div id="row-slot-1" class="row-slot">Drop group here</div>
                                    </td>
                                    <td class="grid-cell" id="cell-1-1"></td>
                                    <td class="grid-cell" id="cell-1-2"></td>
                                    <td class="grid-cell" id="cell-1-3"></td>
                                    <td></td>
                                </tr>
                                <tr id="grid-row-2">
                                    <td class="grid-row-header" ondrop="dropRow(event, 2)" ondragover="allowDrop(event)">
                                        <div id="row-slot-2" class="row-slot">Drop group here</div>
                                    </td>
                                    <td class="grid-cell" id="cell-2-1"></td>
                                    <td class="grid-cell" id="cell-2-2"></td>
                                    <td class="grid-cell" id="cell-2-3"></td>
                                    <td></td>
                                </tr>
                                <tr id="grid-row-3">
                                    <td class="grid-row-header" ondrop="dropRow(event, 3)" ondragover="allowDrop(event)">
                                        <div id="row-slot-3" class="row-slot">Drop group here</div>
                                    </td>
                                    <td class="grid-cell" id="cell-3-1"></td>
                                    <td class="grid-cell" id="cell-3-2"></td>
                                    <td class="grid-cell" id="cell-3-3"></td>
                                    <td></td>
                                </tr>
                                <tr>
                                    <td class="grid-row-header">
                                        <button onclick="addRow()" style="padding: 2px 8px; font-size: 12px;">+ Add Row</button>
                                    </td>
                                    <td></td>
                                    <td></td>
                                    <td></td>
                                    <td></td>
                                </tr>
                            </tbody>
                        </table>

                        <div style="margin-top: 20px;">
                            <button class="pdf-download-button" onclick="generatePDF()">📥 Download as PDF</button>
                            <button class="secondary-button" onclick="clearGrid()">Clear Grid</button>
                            <button onclick="previewArrangement()">Preview Layout</button>
                            <span id="pdf-status"></span>
                        </div>

                        <div id="preview-area" class="preview-area" style="display: none;">
                            <h4>Preview</h4>
                            <div id="preview-grid" class="preview-grid"></div>
                        </div>
                    </div>
                </div>
            </div>
        </div>

        <div class="footer">
            <p>Report generated on ', format(Sys.time(), "%d %B %Y at %H:%M:%S"), '</p>
            <p style="font-size: 12px;">t-SNE Analysis Tool v2.2</p>
        </div>
    </div>

    <script>
    // Store all data
    var channelNames = ', jsonlite::toJSON(plot_result$all_markers), ';
    var displayChannels = ', jsonlite::toJSON(plot_result$display_markers), ';
    var groups = ', jsonlite::toJSON(groups), ';
    var markerMapping = {};
    var plotImages = {};

    // Store base64 images
')

  # Add image data to JavaScript
  for(group in groups) {
    for(marker in plot_result$display_markers) {
      img_path <- plot_result$plot_files[[group]][[marker]]
      img_base64 <- base64enc::dataURI(file = img_path, mime = "image/png")
      html_content <- paste0(html_content, '
    if (!plotImages["', group, '"]) plotImages["', group, '"] = {};
    plotImages["', group, '"]["', marker, '"] = "', img_base64, '";
    ')
    }
  }

  html_content <- paste0(html_content, '

    // Initialize marker mapping
    channelNames.forEach(function(channel) {
        markerMapping[channel] = channel;
    });

    // Grid arrangement data
    var gridRows = [];
    var gridColumns = [];
    var numRows = 3;
    var numCols = 3;

    // Drag and drop functions
    function allowDrop(event) {
        event.preventDefault();
        event.currentTarget.classList.add("dragover");
    }

    function drag(event) {
        event.dataTransfer.setData("text", event.target.dataset.value);
        event.dataTransfer.setData("type", event.target.dataset.type);
        event.target.classList.add("dragging");
    }

    function dropRow(event, rowIndex) {
        event.preventDefault();
        event.currentTarget.classList.remove("dragover");

        var value = event.dataTransfer.getData("text");
        var type = event.dataTransfer.getData("type");

        if (type === "group") {
            // Ensure array is large enough
            while (gridRows.length < rowIndex) {
                gridRows.push(null);
            }

            gridRows[rowIndex - 1] = value;
            document.getElementById("row-slot-" + rowIndex).innerHTML =
                \'<div class="placed-item">\' + value +
                \'<button class="remove-button" onclick="removeRow(\' + rowIndex + \')">×</button></div>\';
            updateGridCells();
        }
    }

    function dropColumn(event, colIndex) {
        event.preventDefault();
        event.currentTarget.classList.remove("dragover");

        var value = event.dataTransfer.getData("text");
        var type = event.dataTransfer.getData("type");

        if (type === "marker") {
            // Ensure the array is large enough
            while (gridColumns.length < colIndex) {
                gridColumns.push(null);
            }

            gridColumns[colIndex - 1] = value;
            var slot = document.getElementById("col-slot-" + colIndex);
            if (slot) {
                slot.innerHTML =
                    \'<div class="placed-item">\' + (markerMapping[value] || value) +
                    \'<button class="remove-button" onclick="removeColumn(\' + colIndex + \')">×</button></div>\';
            }
            updateGridCells();
        }
    }

    function removeRow(rowIndex) {
        gridRows[rowIndex - 1] = null;
        document.getElementById("row-slot-" + rowIndex).innerHTML = "Drop group here";
        updateGridCells();
    }

    function removeColumn(colIndex) {
        gridColumns[colIndex - 1] = null;
        document.getElementById("col-slot-" + colIndex).innerHTML = "Drop marker here";
        updateGridCells();
    }

    function updateGridCells() {
        // Get actual number of rows and columns from the DOM
        var tbody = document.getElementById("grid-body");
        var actualRows = tbody.rows.length - 1;  // -1 for "Add Row" row
        var headerRow = document.getElementById("arrangement-table").rows[0];
        var actualCols = headerRow.cells.length - 2;  // -2 for row header and "Add Column" button

        for (var r = 0; r < actualRows; r++) {
            for (var c = 0; c < actualCols; c++) {
                var cell = document.getElementById("cell-" + (r + 1) + "-" + (c + 1));
                if (cell) {
                    if (gridRows[r] && gridColumns[c] &&
                        plotImages[gridRows[r]] && plotImages[gridRows[r]][gridColumns[c]]) {
                        cell.innerHTML = \'<img src="\' + plotImages[gridRows[r]][gridColumns[c]] +
                                       \'" style="width: 100%; height: auto;">\';
                    } else {
                        cell.innerHTML = "";
                    }
                }
            }
        }
    }

    function addRow() {
        numRows++;
        var tbody = document.getElementById("grid-body");
        var newRow = tbody.insertRow(tbody.rows.length - 1);
        newRow.id = "grid-row-" + numRows;

        // Add row header
        var headerCell = newRow.insertCell(0);
        headerCell.className = "grid-row-header";
        headerCell.setAttribute("ondrop", "dropRow(event, " + numRows + ")");
        headerCell.setAttribute("ondragover", "allowDrop(event)");
        headerCell.innerHTML = \'<div id="row-slot-\' + numRows + \'" class="row-slot">Drop group here</div>\';

        // Add cells for existing columns
        var headerRow = document.getElementById("arrangement-table").rows[0];
        var currentCols = headerRow.cells.length - 2; // -2 for row header and add button

        for (var i = 1; i <= currentCols; i++) {
            var cell = newRow.insertCell(i);
            cell.className = "grid-cell";
            cell.id = "cell-" + numRows + "-" + i;
        }

        // Add empty cell at the end
        newRow.insertCell(currentCols + 1);
    }

    function addColumn() {
        numCols++;
        var table = document.getElementById("arrangement-table");

        // Add header
        var headerRow = table.rows[0];
        var newHeader = document.createElement("th");
        newHeader.className = "grid-header";
        newHeader.id = "col-header-" + numCols;
        newHeader.setAttribute("ondrop", "dropColumn(event, " + numCols + ")");
        newHeader.setAttribute("ondragover", "allowDrop(event)");
        newHeader.innerHTML = \'<div id="col-slot-\' + numCols + \'" class="column-slot">Drop marker here</div>\';
        headerRow.insertBefore(newHeader, headerRow.cells[headerRow.cells.length - 1]);

        // Add cells to each data row
        var tbody = document.getElementById("grid-body");
        for (var i = 0; i < tbody.rows.length - 1; i++) {  // -1 to skip the "Add Row" row
            var row = tbody.rows[i];
            var newCell = row.insertCell(row.cells.length - 1);  // Insert before the last empty cell
            newCell.className = "grid-cell";
            newCell.id = "cell-" + (i + 1) + "-" + numCols;
        }
    }

    function clearGrid() {
        gridRows = [];
        gridColumns = [];

        // Clear all row slots
        for (var r = 1; r <= numRows; r++) {
            var rowSlot = document.getElementById("row-slot-" + r);
            if (rowSlot) rowSlot.innerHTML = "Drop group here";
        }

        // Clear all column slots
        for (var c = 1; c <= numCols; c++) {
            var colSlot = document.getElementById("col-slot-" + c);
            if (colSlot) colSlot.innerHTML = "Drop marker here";
        }

        // Clear all cells
        updateGridCells();
    }

    function previewArrangement() {
        var previewArea = document.getElementById("preview-area");
        var previewGrid = document.getElementById("preview-grid");

        // Filter out null values
        var activeRows = gridRows.filter(function(r) { return r !== null && r !== undefined; });
        var activeCols = gridColumns.filter(function(c) { return c !== null && c !== undefined; });

        if (activeRows.length === 0 || activeCols.length === 0) {
            alert("Please add at least one group and one marker to preview");
            return;
        }

        previewGrid.style.gridTemplateColumns = "repeat(" + activeCols.length + ", 1fr)";
        previewGrid.innerHTML = "";

        for (var r = 0; r < activeRows.length; r++) {
            for (var c = 0; c < activeCols.length; c++) {
                var plotDiv = document.createElement("div");
                plotDiv.className = "preview-plot";

                if (plotImages[activeRows[r]] && plotImages[activeRows[r]][activeCols[c]]) {
                    plotDiv.innerHTML = \'<img src="\' + plotImages[activeRows[r]][activeCols[c]] + \'">\';
                }

                previewGrid.appendChild(plotDiv);
            }
        }

        previewArea.style.display = "block";
    }

    function generatePDF() {
        // Filter out null values
        var activeRows = gridRows.filter(function(r) { return r !== null && r !== undefined; });
        var activeCols = gridColumns.filter(function(c) { return c !== null && c !== undefined; });

        if (activeRows.length === 0 || activeCols.length === 0) {
            alert("Please create an arrangement with at least one group and one marker");
            return;
        }

        // Get experiment name
        var experimentName = document.getElementById("experiment-name").value.trim();
        var pdfTitle = experimentName ? "t-SNE Analysis: " + experimentName : "t-SNE Analysis";

        // Show status
        var statusDiv = document.getElementById("pdf-status");
        statusDiv.style.display = "inline-block";
        statusDiv.innerHTML = "Generating PDF...";

        // Initialize jsPDF
        const { jsPDF } = window.jspdf;

        // First, we need to get the actual aspect ratio from the images
        // Create a temporary image to measure dimensions
        var tempImg = new Image();
        var firstGroup = activeRows[0];
        var firstMarker = activeCols[0];

        tempImg.onload = function() {
            // Get actual image dimensions
            var imgWidth = tempImg.width;
            var imgHeight = tempImg.height;

            // The plot area is square, but the full image includes the legend
            // We need to maintain the original aspect ratio
            var aspectRatio = imgHeight / imgWidth;

            // Calculate dimensions with VERY NARROW spacing
            var plotWidth = 50;  // Width of each plot in mm
            var plotHeight = plotWidth * aspectRatio; // Maintain aspect ratio
            var margin = 10;     // Reduced margin
            var spacingX = 1;    // VERY NARROW horizontal space between plots (1mm)
            var spacingY = 1;    // VERY NARROW vertical space between plots (1mm)

            // Extra space for labels
            var labelSpaceTop = 8;   // Space for column headers
            var labelSpaceLeft = 8;  // Space for row headers

            var pageWidth = margin * 2 + labelSpaceLeft + activeCols.length * plotWidth + (activeCols.length - 1) * spacingX;
            var pageHeight = margin * 2 + labelSpaceTop + activeRows.length * plotHeight + (activeRows.length - 1) * spacingY + 10; // +10 for title

            // Create PDF with appropriate size
            var orientation = "portrait";
            var format = "a4"; // Default to A4

            // Check if we need larger paper or landscape
            if (pageWidth > 210 || pageHeight > 297) {
                if (pageWidth > pageHeight) {
                    orientation = "landscape";
                    if (pageWidth <= 297 && pageHeight <= 210) {
                        format = "a4";
                    } else {
                        format = [pageWidth, pageHeight];
                    }
                } else {
                    format = [pageWidth, pageHeight];
                }
            }

            // If still too large, scale down
            var scale = 1;
            var maxDim = 400; // Maximum dimension in mm
            if (pageWidth > maxDim || pageHeight > maxDim) {
                scale = Math.min(maxDim / pageWidth, maxDim / pageHeight);
                plotWidth *= scale;
                plotHeight *= scale;
                spacingX *= scale;
                spacingY *= scale;
                margin *= scale;
                labelSpaceTop *= scale;
                labelSpaceLeft *= scale;

                // Recalculate page dimensions
                pageWidth = margin * 2 + labelSpaceLeft + activeCols.length * plotWidth + (activeCols.length - 1) * spacingX;
                pageHeight = margin * 2 + labelSpaceTop + activeRows.length * plotHeight + (activeRows.length - 1) * spacingY + 10;

                if (pageWidth <= 297 && pageHeight <= 210) {
                    orientation = "landscape";
                    format = "a4";
                } else if (pageWidth <= 210 && pageHeight <= 297) {
                    orientation = "portrait";
                    format = "a4";
                } else {
                    format = [Math.min(pageWidth, 420), Math.min(pageHeight, 594)]; // A2 max
                }
            }

            var doc = new jsPDF({
                orientation: orientation,
                unit: "mm",
                format: format
            });

            // Get actual page dimensions
            var actualPageWidth = doc.internal.pageSize.getWidth();
            var actualPageHeight = doc.internal.pageSize.getHeight();

            // Center the content if page is larger
            var contentWidth = labelSpaceLeft + activeCols.length * plotWidth + (activeCols.length - 1) * spacingX;
            var contentHeight = labelSpaceTop + activeRows.length * plotHeight + (activeRows.length - 1) * spacingY;

            var offsetX = (actualPageWidth - contentWidth) / 2;
            var offsetY = (actualPageHeight - contentHeight - 10) / 2; // -10 for title space

            offsetX = Math.max(offsetX, margin);
            offsetY = Math.max(offsetY, margin);

            // Add title with experiment name
            doc.setFontSize(14);
            doc.setFont(undefined, "bold");
            doc.text(pdfTitle, actualPageWidth / 2, offsetY, { align: "center" });

            // Add timestamp
            doc.setFontSize(9);
            doc.setFont(undefined, "normal");
            doc.setTextColor(100);
            doc.text(new Date().toLocaleString(), actualPageWidth / 2, offsetY + 5, { align: "center" });
            doc.setTextColor(0);

            // Add column headers (marker names)
            doc.setFontSize(9);  // Smaller font for tight spacing
            for (var c = 0; c < activeCols.length; c++) {
                var markerLabel = markerMapping[activeCols[c]] || activeCols[c];
                var x = offsetX + labelSpaceLeft + c * (plotWidth + spacingX) + plotWidth / 2;
                var y = offsetY + 10;

                // Truncate long labels if needed
                if (markerLabel.length > 15) {
                    markerLabel = markerLabel.substring(0, 12) + "...";
                }

                doc.text(markerLabel, x, y, { align: "center" });
            }

            // Add plots with row headers
            for (var r = 0; r < activeRows.length; r++) {
                // Add row header (group name)
                doc.setFontSize(9);  // Smaller font for tight spacing
                var groupLabel = activeRows[r];
                var labelX = offsetX + labelSpaceLeft - 2;
                var labelY = offsetY + labelSpaceTop + 10 + r * (plotHeight + spacingY) + plotHeight / 2;

                // Save the current graphics state
                doc.saveGraphicsState();

                // Rotate for vertical text
                doc.text(groupLabel, labelX, labelY, { angle: 90, align: "center" });

                // Restore graphics state
                doc.restoreGraphicsState();

                // Add plots for this row
                for (var c = 0; c < activeCols.length; c++) {
                    if (plotImages[activeRows[r]] && plotImages[activeRows[r]][activeCols[c]]) {
                        var x = offsetX + labelSpaceLeft + c * (plotWidth + spacingX);
                        var y = offsetY + labelSpaceTop + 10 + r * (plotHeight + spacingY);

                        // Add image with proper aspect ratio
                        doc.addImage(
                            plotImages[activeRows[r]][activeCols[c]],
                            "PNG",
                            x, y,
                            plotWidth, plotHeight
                        );

                        // Optional: Add very thin border around plot
                        doc.setDrawColor(230);  // Very light gray
                        doc.setLineWidth(0.05);  // Very thin line
                        doc.rect(x, y, plotWidth, plotHeight);
                    }
                }
            }

            // Add footer
            doc.setFontSize(8);
            doc.setTextColor(150);
            var footerText = "Generated: " + new Date().toLocaleDateString() + " | " +
                            activeRows.length + " groups × " + activeCols.length + " markers";
            if (experimentName) {
                footerText += " | Experiment: " + experimentName;
            }
            doc.text(footerText, actualPageWidth / 2, actualPageHeight - 5, { align: "center" });

            // Save PDF with experiment name in filename if provided
            var timestamp = new Date().getTime();
            var filename = experimentName ?
                "tsne_" + experimentName.replace(/[^a-z0-9]/gi, "_").toLowerCase() + "_" + timestamp + ".pdf" :
                "tsne_arrangement_" + timestamp + ".pdf";

            doc.save(filename);

            // Update status
            statusDiv.innerHTML = "✓ PDF Downloaded!";
            statusDiv.style.background = "#27ae60";
            setTimeout(function() {
                statusDiv.style.display = "none";
                statusDiv.style.background = "#f39c12";
            }, 3000);
        };

        // Load the first image to get dimensions
        if (plotImages[firstGroup] && plotImages[firstGroup][firstMarker]) {
            tempImg.src = plotImages[firstGroup][firstMarker];
        } else {
            alert("Error: Could not load plot images");
            statusDiv.style.display = "none";
        }
    }

    function updateMarkerNames() {
        var inputs = document.querySelectorAll(".marker-input");
        var updated = false;

        inputs.forEach(function(input) {
            var channel = input.dataset.channel;
            var markerName = input.value.trim();
            if (markerName && displayChannels.includes(channel)) {
                markerMapping[channel] = channel + ": " + markerName;

                // Update all plot titles for this channel
                var titles = document.querySelectorAll(\'.plot-title[data-channel="\' + channel + \'"]\');
                titles.forEach(function(title) {
                    title.textContent = markerMapping[channel];
                });

                // Update draggable marker items
                var draggables = document.querySelectorAll(\'.draggable-item[data-value="\' + channel + \'"]\');
                draggables.forEach(function(item) {
                    item.textContent = markerMapping[channel];
                });

                updated = true;
            }
        });

        if (updated) {
            document.getElementById("update-status").innerHTML =
                \'<span style="color: green;">✓ Marker names updated!</span>\';
            setTimeout(function() {
                document.getElementById("update-status").innerHTML = "";
            }, 2000);

            // Update column headers if already placed
            for (var i = 0; i < gridColumns.length; i++) {
                if (gridColumns[i]) {
                    var slot = document.getElementById("col-slot-" + (i + 1));
                    if (slot) {
                        slot.innerHTML =
                            \'<div class="placed-item">\' + (markerMapping[gridColumns[i]] || gridColumns[i]) +
                            \'<button class="remove-button" onclick="removeColumn(\' + (i + 1) + \')">×</button></div>\';
                    }
                }
            }
        }
    }

    function toggleGroup(groupId) {
        var content = document.getElementById(groupId);
        var arrow = document.getElementById("arrow-" + groupId);
        if (content.style.display === "none") {
            content.style.display = "grid";
            arrow.innerHTML = "▼";
        } else {
            content.style.display = "none";
            arrow.innerHTML = "▶";
        }
    }

    function expandAll() {
        var groups = document.querySelectorAll(".group-plots");
        var arrows = document.querySelectorAll(".arrow");
        groups.forEach(function(group) {
            group.style.display = "grid";
        });
        arrows.forEach(function(arrow) {
            arrow.innerHTML = "▼";
        });
    }

    function collapseAll() {
        var groups = document.querySelectorAll(".group-plots");
        var arrows = document.querySelectorAll(".arrow");
        groups.forEach(function(group) {
            group.style.display = "none";
        });
        arrows.forEach(function(arrow) {
            arrow.innerHTML = "▶";
        });
    }

    // Add event listeners to draggable items
    document.addEventListener("DOMContentLoaded", function() {
        var draggables = document.querySelectorAll(".draggable-item");
        draggables.forEach(function(item) {
            item.addEventListener("dragstart", drag);
            item.addEventListener("dragend", function(e) {
                e.target.classList.remove("dragging");
            });
        });

        // Add dragover/dragleave event listeners
        var dropTargets = document.querySelectorAll(".grid-cell, .grid-header, .grid-row-header");
        dropTargets.forEach(function(target) {
            target.addEventListener("dragleave", function(e) {
                e.currentTarget.classList.remove("dragover");
            });
        });
    });
    </script>
</body>
</html>')

  # Save HTML
  filename <- paste0("tsne_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
  writeLines(html_content, filename)

  cat(sprintf("✅ HTML report saved as: %s\n", filename))
  cat(sprintf("📁 Plot images saved in: %s/\n", plot_result$plot_dir))
  cat(sprintf("🎯 PDF now has very narrow spacing (1mm) between plots\n"))
  cat(sprintf("📥 Compact layout maximizes plot size on each page\n"))

  return(filename)
}

# Main analysis function
main <- function() {
  tryCatch({
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

    # Create visualisation plots
    plot_result <- create_visualisation_plots(data, tsne_coords, selected_markers, group_info)

    # Generate HTML report
    output_file <- generate_html_report(plot_result, group_info, tsne_params, file_info)

    cat("\n", rep("=", 60), "\n", sep="")
    cat("ANALYSIS COMPLETE!\n")
    cat(rep("=", 60), "\n", sep="")

    # Ask if user wants to save the combined data
    save_data <- readline(prompt = "\nSave combined data with t-SNE coordinates as CSV? (y/n): ")
    if (tolower(save_data) == "y") {
      csv_file <- paste0("tsne_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      write.csv(plot_result$data, csv_file, row.names = FALSE)
      cat(sprintf("📊 Data saved as: %s\n", csv_file))
    }

    return(list(data = plot_result$data, file = output_file))

  }, error = function(e) {
    cat("\n", rep("=", 60), "\n", sep="")
    cat("ERROR OCCURRED:\n")
    cat(rep("=", 60), "\n", sep="")
    cat(sprintf("%s\n", e$message))
    cat("\nPlease check your input and try again.\n")
    return(NULL)
  })
}

# ============================================================================
# RUN THE ANALYSIS
# ============================================================================


result <- main()

