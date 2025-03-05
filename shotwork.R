library(tidyverse)

### Isolate possessions where shots occurred
shot_poss <- xgoals_mls25 |>
  select(game_id, event_id, team_id, player_id = shooter_id, xg_shooter) |>
  inner_join(events_mls25) |>
  distinct(game_id, possession_chain_id, possession_team_id) |>
  inner_join(events_mls25) |>
  left_join(game_index |>  mutate(game_date = date(date_time_utc)) |> select(game_id, game_date)) |> 
  left_join(xgoals_mls25 |> select(player_id = shooter_id, game_id, team_id, event_id, pattern_of_play, xg_shooter, distance_from_goal_yds)) |>
  left_join(player_index) |>
  left_join(team_index) |>
  left_join(type_index) |>
  mutate(type_name_adj = ifelse(type_name %in% c('Attempt Saved', 'Goal','Miss','Post'), 'Shot', type_name),
         shot_outcome = ifelse(type_name_adj != "Shot", NA,
                               ifelse(type_name != 'Attempt Saved', type_name,
                                      ifelse(!is.na(blocked), 'Blocked','Saved')))
         )

##summarize posses per shot
passes_per_shot_poss <- shot_poss |>
  group_by(game_id, possession_team_id, possession_chain_id) |>
  summarize(
    sequence_comp_passes = sum(type_name_adj == "Pass" & outcome == 1, NA, na.rm = T),
    sequence_shots = sum(type_name_adj == "Shot", na.rm = T),
    sequence_xg = 1 - prod(1 - xg_shooter, na.rm = T),
    sequence_closest_shot = min(distance_from_goal_yds, na.rm = T)
  ) 
sp1 |> filter(type_name_adj == "Shot") |> ungroup() |> summarize(n(), sum(xg_shooter))

sp1 <- shot_poss |>
  left_join(team_index |> left_join(color_start) |>
              select(possession_team_id = team_id, possession_team = team_name, poss_abbv = team_abbreviation,
                                 poss_primary = primary_adjusted, 
                     poss_secondary = secondary_adjusted)) |>
  left_join(passes_per_shot_poss) |> 
  filter(poss_abbv == "MTL") |> filter(game_date == max(game_date)) |>
  arrange(-sequence_xg, asa_in_game_order) |> 
  #filter(x != x2 & y != y2) |> 
  group_by(possession_chain_id) |> 
  mutate(across(c(x, y, x2, y2), ~ ifelse(possession_team_id == team_id, ., 100 - .)),
         x2_adj = ifelse(x2 != lead(x) & y2 != lead(y), lead(x), x2),
         y2_adj = ifelse(x2 != lead(x) & y2 != lead(y), lead(y), y2),
         ) |> 
  select(asa_in_game_order, sequence_xg, possession_chain_id, possession_team_id, minute, second, xg_shooter, team_name, player_name, type_name_adj, outcome, shot_outcome, 
         x, y, x2, y2, x2_adj, y2_adj, primary_color, secondary_color, poss_primary, poss_secondary) |>
  mutate(shot_outcome_shape = ifelse(shot_outcome == "Goal", 22,
                                     ifelse(shot_outcome == "Blocked", 15,
                                            ifelse(shot_outcome == "Saved", 16, 19))))

sp1 |> ungroup() |> group_by(possession_chain_id) |> 
  summarize(xg = max(sequence_xg), shots = sum(type_name_adj == "Shot")) |>
  ungroup() |>
  summarize(sum(xg), sum(shots))


createPitch(x = 115, y = 80, grass_colour = black_hues[1], line_colour = black_hues[6]) +
  #ggplot() +
  geom_segment(data = sp1 |> filter(type_name_adj != "Shot"),
    aes(x = x*1.15, y = y*0.8, xend = x2*1.15, yend = y2*0.8, 
        color = poss_primary, 
        alpha = sequence_xg), size = 1.5,
               arrow = arrow(length = unit(1, "mm"))) +
  geom_segment(data = sp1 |> filter(type_name_adj != "Shot"),
               aes(x = x2*1.15, y = y2*0.8, xend = x2_adj*1.15, yend = y2_adj*0.8,
                   color = poss_primary, 
                   alpha = sequence_xg), size = 1.5,
               arrow = arrow(length = unit(1, "mm"))) +
  geom_point(data = sp1 |> filter(type_name_adj == "Shot" & shot_outcome!= "Goal"),
             aes(x = x*1.15, y = y*0.8, size = xg_shooter, shape = shot_outcome, color = poss_primary), stroke = 2, fill = black_hues[2]) +
  geom_point(data = sp1 |> filter(type_name_adj == "Shot" & shot_outcome == "Goal"),
             aes(x = x*1.15, y = y*0.8, size = xg_shooter, color = poss_secondary,fill = poss_primary), shape = 21,
             stroke = 2)+
  scale_size_binned(breaks = c(0, 0.06, 0.15, 0.33,1),range = c(2, 8), limits = c(0, 1)) +
  scale_alpha_binned(breaks = c(0, 0.06, 0.15, 0.33,1)) +
  scale_shape_manual(values = c(22,23,24, 25)) +
  scale_fill_identity() +
  scale_color_identity() +
  coord_flip() + scale_y_reverse() +
  guides(alpha = 'none', size = 'none') +
  theme_matt +
  theme(aspect.ratio = 105/80,
        axis.text = element_blank(), axis.title = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = c(0.87, 0.12),
        plot.caption = element_text(hjust = c(0,1))
        ) +
  labs(title = "Minnesota: Loons lively and direct",
       subtitle = 'Possessions that created 1.94 xG from 14 shots, mapped',
       caption = c('Points sized and lines shaded by shot xG','Viz: Matt Barger. Source: Opta'))
                   
  
  View()

  xgoals_mls25 |>
    mutate(xg = round(xg_shooter, digits = 2)) |>
    filter(pattern_of_play != "Penalty") |>
    group_by(xg) |>
    summarize(n = n()) |>
    ungroup() |>
    arrange(xg) |>
    mutate(xg_pct = 100*cumsum(n/sum(n))) |>
    ggplot(aes(x = xg, y = xg_pct)) +
    geom_rect(xmin = 0.15, xmax = 0.31, ymin = 75, ymax = 90, color = NA, fill = black_hues[3], alpha = 0.1) +
    geom_rect(xmin = 0.03, xmax = 0.15, ymin = 25, ymax = 75, color = NA, fill = black_hues[4], alpha = 0.1) +
    geom_hline(yintercept = c(0, 25, 50, 75, 90, 100), color = black_hues[5]) +
    geom_vline(xintercept = c(0, 0.03, 0.07, 0.15, 0.31), color = black_hues[5]) +
    geom_step(size = 1) +
    theme_matt +
    scale_x_continuous(breaks = c(0.0,0.03,0.07,0.15,0.31), limits = c(0, 0.5),
                       position = 'top') +
    scale_y_continuous(breaks = c(0,25,50,75,90, 100))+
    labs(
      title = "Good and Bad xG",
      subtitle = "All 364 non-penalty chances ranked through MLS Matchday 1",
      caption = c("Viz: Matt Barger (@mattbarger.bsky.social)","Data: Opta via American Soccer Analysis."),
      y = "Percentile (%)",
      x = "xG"
    ) +
    theme(
      axis.title.x = element_text(hjust = 0),
      axis.title.y = element_text(hjust = 0),
      plot.caption = element_text(hjust = c(0,1))
    )
