library(devtools)
library(rsvg)
devtools::install_github("klutometis/roxygen")
library(roxygen2)

# install.packages(c('grid','gridSVG'))
install.packages('grImport2')
library(grImport2)
library(grid)
library(gridSVG)

setwd("C:/Users/dan.fraser/Documents")
create("NRLfastR")
devtools::document("NRLfastR")
devtools::install("NRLfastR")

library(NRLfastR)
getwd()

full_path_to_figures <- file.path(getwd(), "NRLfastR", "inst", "figures")
print(full_path_to_figures)

# List files in the full path
list.files(full_path_to_figures)

path_to_figures <- system.file("figures", package = "NRLfastR")
print(path_to_figures)

dir.create("/inst/logos", recursive = TRUE)

get_team_logo <- function(team_name) {
  # Construct the full path to the SVG file
  full_path_to_figures <- file.path(getwd(), "NRLfastR", "inst", "logos", paste0(team_name, ".svg"))
  
  # Check if the file exists
  if (file.exists(full_path_to_figures)) {
    return(full_path_to_figures)
  } else {
    stop("Logo not found.")
  }
}

file.path(getwd(), "NRLfastR", "inst", "figures")
get_team_logo("Eels")


print(logo_path)

add_team_logo <- function(plot, team_name, x, y, width = 0.1, height = 0.1) {
  logo <- get_team_logo(team_name)
  svg_image <- rsvg::rsvg(logo)  # Convert SVG to a raster object
  
  # Create a rasterGrob from the SVG
  g <- rasterGrob(svg_image, width = unit(width, "npc"), height = unit(height, "npc"))
  
  # Add the logo to the plot
  plot + annotation_custom(g, xmin = x, xmax = x + width, ymin = y, ymax = y + height)
}

add_team_logo(Eels)

