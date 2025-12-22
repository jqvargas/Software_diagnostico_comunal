# setup_r.R - Run this once to prepare portable R

# Set paths
project_path <- getwd()
portable_r_path <- file.path(project_path, "R-Portable")
portable_lib <- file.path(portable_r_path, "library")

# Create R-Portable directory if it doesn't exist
if (!dir.exists(portable_r_path)) {
  dir.create(portable_r_path, recursive = TRUE)
}

# Copy current R installation to portable location
r_home <- R.home()
message("Copying R from: ", r_home)
message("To: ", portable_r_path)

# Copy R files (this may take a few minutes)
file.copy(r_home, dirname(portable_r_path), recursive = TRUE)
file.rename(file.path(dirname(portable_r_path), basename(r_home)), portable_r_path)

# Set library path to portable R
.libPaths(portable_lib)

# List of packages your Shiny app needs
required_packages <- c(
  "shiny",
  "dplyr",
  "ggplot2",
  "sf",
  "raster",
  "viridis",
  "RColorBrewer",
  "readr",
  "readxl",
  "stringr",
  "stringi",
  "knitr",
  "fs",
  "rmarkdown",
  "webshot2"
)

# Install packages to portable R
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, lib = portable_lib, dependencies = TRUE)
  }
}

message("Portable R setup complete!")
message("Packages installed in: ", portable_lib)