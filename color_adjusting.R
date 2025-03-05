lum_fw <- 250.3295
lum_fb <- 24.4814

adj_color_darker <- function(color) {
  color_fix <- colorRampPalette(c(color, black_hues[10]))
  adjusted_shade <- tibble(shade = as.vector(color_fix(30))) |> 
    rowwise() |> 
    mutate(rgb = list(col2rgb(shade)), 
           luminance = 0.2989 * rgb[1] + 0.587 * rgb[2] + 0.114 * rgb[3], 
           contrast = abs(luminance - grass_luminance))  |>
    filter(contrast > 75) |> ungroup() |> 
    arrange(contrast) |>   select(shade) |> unlist() |> as.vector()
  return(adjusted_shade[[1]])
  rm(color_fix)
  rm(adjusted_shade)
}

adj_color_lighter <- function(color) {
  color_fix <- colorRampPalette(c(color, 'floralwhite'))
  adjusted_shade <- tibble(shade = as.vector(color_fix(30))) |> 
    rowwise() |> 
    mutate(rgb = list(col2rgb(shade)), 
           luminance = 0.2989 * rgb[1] + 0.587 * rgb[2] + 0.114 * rgb[3], 
           contrast = abs(luminance - grass_luminance))  |>
    filter(contrast < 190) |> ungroup() |> 
    arrange(-contrast) |>   select(shade) |> unlist() |> as.vector()
  return(adjusted_shade[[1]])
  rm(color_fix)
  rm(adjusted_shade)
}

grass_luminance <- col2rgb(black_hues[2])[1] * 0.2989 + col2rgb(black_hues[2])[2] * 0.587 +col2rgb(black_hues[2])[3] * 0.114 

color_start <- team_index |> 
  rowwise() |> 
  mutate(rgb_primary = list(col2rgb(primary_color)), 
         rgb_secondary = list(col2rgb(secondary_color))) |> 
  mutate(luminance_primary = 0.2989 * rgb_primary[1] + 0.587 * rgb_primary[2] + 0.114 * rgb_primary[3], 
         luminance_secondary = 0.2989 * rgb_secondary[1] + 0.587 * rgb_secondary[2] + 0.114 * rgb_secondary[3], 
         luminance_floralwhite = grass_luminance, 
         luminance_floralblack = lum_fb) |> 
  mutate(contrast_prim = abs(luminance_primary -luminance_floralwhite), 
         contrast_seco = abs(luminance_secondary - luminance_floralwhite)) |>
  mutate(adjust_primary = ifelse(contrast_prim > 190, "too dark", 
                                 ifelse(contrast_prim < 80, 'too light', NA)),
         adjust_secondary = ifelse(contrast_seco > 190, "too dark", 
                                   ifelse(contrast_seco < 80, 'too light', NA)),
         primary_adjusted = ifelse(is.na(adjust_primary), primary_color,
                                   ifelse(adjust_primary == "too dark", 
                                          adj_color_lighter(primary_color),
                                          adj_color_darker(primary_color))),
         secondary_adjusted = ifelse(is.na(adjust_secondary), secondary_color,
                                     ifelse(adjust_secondary == "too dark", 
                                            adj_color_lighter(secondary_color),
                                            adj_color_darker(secondary_color)))) |>
  select(team_id, team_name, primary_color, secondary_color, tertiary_color, primary_adjusted, secondary_adjusted)
