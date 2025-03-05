## Adjusting Colors for MLS teams (or any color really)
## by Matt Barger
## 
## Objective: maximize color contrast so every prescribed color reads well on a prescribed plot.
## 
## Step 1: Set up functions
##
## Get the libraries in there
library(tidyverse)

## This converts R-named colors to hex codes.
col2hex <- function(x, alpha = FALSE) {
  args <- as_tibble(t(col2rgb(x, alpha = alpha)))
  args <- c(args, list(names = x, maxColorValue = 255))
  do.call(rgb, args)
}

## This calculates a color's brightness. Contrast is the absolute differences in brightness between two colors.
luminance <- function(hex_code) {
  return(col2rgb(hex_code)[1] * 0.2989 + col2rgb(hex_code)[2] * 0.587 + col2rgb(hex_code)[3] * 0.114)
  }

## This function adjusts colors that are too bright or too dark.
##    - base_color: the color being adjusted
##    - bg_color:   the background base color
##    - ref_color:  the gradient reference point (template black for darker, template white for lighter)
adjust_color <- function(base_color, bg_color, ref_color, n_shades = 100, min_contrast = 80) {
  ## This creates a gradient between the color and the reference color
  color_fix <- colorRampPalette(c(base_color, ref_color))
  ## color_fix() splits the gradient into n_shades number of colors
  adjusted_shade <- tibble(shade = as.vector(color_fix(n_shades))) |> 
    rowwise() |> 
    ## this grades the darker colors against contrast with the background
    mutate(contrast = abs(luminance(shade) - luminance(bg_color)))  |>
    ## filter out anything below min_contrast
    filter(contrast > min_contrast) |> ungroup() |> 
    ## arrange by the most true to the base color
    arrange(contrast) |> select(shade) |> unlist() |> as.vector()
  ## and return the brightest color within the contrast setting.
  return(adjusted_shade[[1]])
  rm(color_fix)
  rm(adjusted_shade)
}

## Step 2: Example code. 
##
## The base colors from my template. See themes.R file for details.
## This code should work off R color names as well, but col2hex() can convert those names as well.
template_white <- col2hex('floralwhite')
template_grass <- "#e6d9ce"
template_black <- "#1a1817"

## team_index is the all_opta.teams table filtered for MLS teams. See SQL script in the data_setup.R file.
## It has with primary_color and secondary_color as fields of hex codes.
team_index_colors_fixed <- team_index |>
  rowwise() |>
  ## First, figure out how many colors are too light or too dark. I'll keep the luminance/contrast fields in for now.
  mutate(
    lum_p = luminance(primary_color),
    lum_s = luminance(secondary_color),
    contrast_lite_p = abs(lum_p - luminance(template_grass)),
    contrast_dark_p = abs(lum_p - luminance(template_black)),
    contrast_lite_s = abs(lum_p - luminance(template_grass)),
    contrast_dark_s = abs(lum_p - luminance(template_black))
  ) |>
  mutate(
    #This is where we adjust the colors for primary
    primary_color_adj = case_when(
      # Colors too dark? Adjust against template black, scale gradient to template white
      contrast_dark_p < 40  ~ adjust_color(primary_color, template_black, template_white, min_contrast = 40),
      # Colors too light? Adjust against template grass, scale gradient to template black
      contrast_lite_p < 110 ~ adjust_color(primary_color, template_grass, template_black, min_contrast = 110),
      # Keep the rest
      .default = primary_color
    ),
    #Do the same thing for secondary.
    secondary_color_adj = case_when(
      contrast_dark_s < 40  ~ adjust_color(secondary_color, template_black, template_white, min_contrast = 40),
      contrast_lite_s < 110 ~ adjust_color(secondary_color, template_grass, template_black, min_contrast = 110),
      .default = secondary_color
    )
  ) |>
  select(team_id, team_name, team_abbreviation, primary_color, primary_color_adj, secondary_color, secondary_color_adj, everything())
