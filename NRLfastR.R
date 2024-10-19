library(devtools)
library(rsvg)
devtools::install_github("klutometis/roxygen")
library(roxygen2)

# install.packages(c('grid','gridSVG'))
# install.packages('grImport2')
library(grImport2)
library(grid)
library(gridSVG)

setwd("C:/Users/dan.fraser/Documents")
create("NRLfastR")

get_NRLteam_logo <- function(team_name) {
  NRL_logos <- data.frame(
    team = c("Broncos","Bulldogs","Cowboys","Dolphins","Dragons","Eels","Knights","Panthers","Rabbitohs", 
             "Raiders","Roosters","SeaEagles","Sharks","Storm","Titans","Warriors","West Tigers"),
    url = c("https://raw.githubusercontent.com/dfraser22/NFLfastR/refs/heads/main/png_files/Broncos.png",
            "https://raw.githubusercontent.com/dfraser22/NFLfastR/refs/heads/main/png_files/Bulldogs.png",
            "https://raw.githubusercontent.com/dfraser22/NFLfastR/refs/heads/main/png_files/Cowboys.png",
            "https://raw.githubusercontent.com/dfraser22/NFLfastR/refs/heads/main/png_files/Dolphins.png",
            "https://raw.githubusercontent.com/dfraser22/NFLfastR/refs/heads/main/png_files/Dragons.png",
            "https://raw.githubusercontent.com/dfraser22/NFLfastR/refs/heads/main/png_files/Eels.png",
            "https://raw.githubusercontent.com/dfraser22/NFLfastR/refs/heads/main/png_files/Knights.png",
            "https://raw.githubusercontent.com/dfraser22/NFLfastR/refs/heads/main/png_files/Panthers.png",
            "https://raw.githubusercontent.com/dfraser22/NFLfastR/refs/heads/main/png_files/Rabbitohs.png",
            "https://raw.githubusercontent.com/dfraser22/NFLfastR/refs/heads/main/png_files/Raiders.png",
            "https://raw.githubusercontent.com/dfraser22/NFLfastR/refs/heads/main/png_files/Roosters.png",
            "https://raw.githubusercontent.com/dfraser22/NFLfastR/refs/heads/main/png_files/SeaEagles.png",
            "https://raw.githubusercontent.com/dfraser22/NFLfastR/refs/heads/main/png_files/Sharks.png",
            "https://raw.githubusercontent.com/dfraser22/NFLfastR/refs/heads/main/png_files/Storm.png",
            "https://raw.githubusercontent.com/dfraser22/NFLfastR/refs/heads/main/png_files/Titans.png",
            "https://raw.githubusercontent.com/dfraser22/NFLfastR/refs/heads/main/png_files/Warriors.png",
            "https://raw.githubusercontent.com/dfraser22/NFLfastR/refs/heads/main/png_files/WestTigers.png"),
            stringsAsFactors = FALSE
  )
  
  logo_url <- logos$url[match(team_name, logos$team)]
  
  if (is.na(logo_url)) {
    stop("Team not found.")
  }
  
  return(logo_url)
}

NRL_logos
get_NRLteam_logo('Broncos')


# setwd("C:/Users/dan.fraser/Documents")
# create("NRLfastR")
# devtools::document("NRLfastR")
# devtools::install("NRLfastR")
# 
# library(NRLfastR)
# getwd()
# 
# full_path_to_figures <- file.path(getwd(), "NRLfastR", "inst", "figures")
# print(full_path_to_figures)
# 
# # List files in the full path
# list.files(full_path_to_figures)
# 
# path_to_figures <- system.file("figures", package = "NRLfastR")
# print(path_to_figures)
# 
# dir.create("/inst/logos", recursive = TRUE)
# 
# get_team_logo <- function(team_name) {
#   # Construct the full path to the SVG file
#   full_path_to_figures <- file.path(getwd(), "NRLfastR", "inst", "logos", paste0(team_name, ".svg"))
#   
#   # Check if the file exists
#   if (file.exists(full_path_to_figures)) {
#     return(full_path_to_figures)
#   } else {
#     stop("Logo not found.")
#   }
# }
# 
# file.path(getwd(), "NRLfastR", "inst", "figures")
# get_team_logo("Eels")
# 
# 
# print(logo_path)
# 
# add_team_logo <- function(plot, team_name, x, y, width = 0.1, height = 0.1) {
#   logo <- get_team_logo(team_name)
#   svg_image <- rsvg::rsvg(logo)  # Convert SVG to a raster object
#   
#   # Create a rasterGrob from the SVG
#   g <- rasterGrob(svg_image, width = unit(width, "npc"), height = unit(height, "npc"))
#   
#   # Add the logo to the plot
#   plot + annotation_custom(g, xmin = x, xmax = x + width, ymin = y, ymax = y + height)
# }
# 
# add_team_logo(Eels)
# 
