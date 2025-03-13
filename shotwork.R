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
                               ifelse(type_name == "Post", "Miss", 
                                      ifelse(type_name != 'Attempt Saved', type_name,
                                             ifelse(!is.na(blocked), 'Blocked','Saved'))))
  )
##summarize posses per shot
passes_per_shot_poss <- shot_poss |>
  group_by(game_id, possession_team_id, possession_chain_id) |>
  summarize(
    sequence_comp_passes = sum(type_name_adj == "Pass" & outcome == 1, NA, na.rm = T),
    sequence_shots = sum(type_name_adj == "Shot", na.rm = T),
    goal_scored = sum(shot_outcome == "Goal", na.rm = T),
    sequence_xg = 1 - prod(1 - xg_shooter, na.rm = T),
    sequence_closest_shot = min(distance_from_goal_yds, na.rm = T)
  )  |>
  mutate(sequence_xg_adj = ifelse(goal_scored > 0, 1, sequence_xg))

sp1 <- shot_poss |>
  left_join(team_index |> left_join(color_start) |>
              select(possession_team_id = team_id, possession_team = team_name, poss_abbv = team_abbreviation,
                     poss_primary = primary_adjusted, 
                     poss_secondary = secondary_adjusted)) |>
  left_join(passes_per_shot_poss) |> 
  filter(poss_abbv == "CHI") |> filter(game_date == max(game_date)) |>
  arrange(-sequence_xg, asa_in_game_order) |> 
  #filter(x != x2 & y != y2) |> 
  group_by(possession_chain_id) |> 
  filter(!type_name_adj %in% c('Out','Contentious Referee Decision','Injury Time Announcement','Deleted After Review')) |>
  mutate(across(c(x, y, x2, y2), ~ ifelse(possession_team_id == team_id, ., 100 - .)),
         x2_adj = ifelse(x2 != lead(x) & y2 != lead(y), lead(x), x2),
         y2_adj = ifelse(x2 != lead(x) & y2 != lead(y), lead(y), y2),
  ) |>
  select(game_id,asa_in_game_order, sequence_shots, sequence_xg, sequence_xg_adj, possession_chain_id, possession_team_id, minute, second, pattern_of_play, xg_shooter, possession_team, team_name, player_name, type_name_adj, outcome, shot_outcome, 
         x, y, x2, y2, x2_adj, y2_adj, primary_color, secondary_color, poss_primary, poss_secondary) |>
  mutate(shot_outcome_shape = case_when(
    shot_outcome == "Goal" ~ 21,
    shot_outcome == "Saved" ~ 23,
    shot_outcome == "Blocked" ~ 22,
    shot_outcome == "Miss" ~ 24,
    .default = NA
  ),
  shot_outcome_fill = ifelse(shot_outcome == "Goal", poss_primary, black_hues[2])
  ) |>
  ungroup()

title <- paste(sp1 |>  distinct(possession_team) |> unlist() |> as.vector(), 
               ": Attacking Buildup", sep = "")
subtitle <- game_index |> 
  filter(game_id == sp1 |> distinct(game_id) |> unlist()) |>
  left_join(team_index |> select(home_team_id = team_id, home_abbv = team_short_name)) |>
  left_join(team_index |> select(away_team_id = team_id, away_abbv = team_short_name)) |>
  mutate(game_date =  date(date_time_utc),
         opponent_id = ifelse(
           sp1 |> distinct(possession_team_id) |> unlist() != home_team_id, home_team_id, away_team_id),
         scoreline = paste(home_abbv, " ", home_score, "-", away_score, " ", away_abbv, sep = "")
  ) |>
  left_join(team_index |> select(opponent_id = team_id, opponent = team_abbreviation)) |>
  select(game_id, game_date, opponent_id, opponent, scoreline) |>
  mutate(
    subtitle1 = paste('From ', scoreline,', ', game_date, sep = "")
  ) |>
  left_join(
    sp1 |>
      distinct(game_id, possession_team_id, sequence_xg, sequence_shots) |>
      group_by(game_id) |>
      summarize(xG = round(sum(sequence_xg), digits = 2), shots = sum(sequence_shots)) |>
      mutate(subtitle2 = paste("Possessions that generated ", xG, " xG from ", shots, " shots, mapped",sep =""))
  ) |>
  mutate(subtitle = paste(subtitle1, subtitle2, sep = "\n"))

team_primary <- sp1 |> distinct(poss_primary) |> unlist()
team_secondary <- sp1 |> distinct(poss_secondary) |> unlist()

createPitch(x = 115, y = 80, grass_colour = black_hues[1], line_colour = black_hues[6]) +
  #ggplot() +
  geom_segment(data = sp1 |> filter(type_name_adj != "Shot"),
    aes(x = x*1.15, y = y*0.8, xend = x2*1.15, yend = y2*0.8, 
        color = poss_primary, 
        alpha = sequence_xg_adj), size = 1.25,
               arrow = arrow(length = unit(1, "mm"))) +
  geom_segment(data = sp1 |> filter(type_name_adj != "Shot"),
               aes(x = x2*1.15, y = y2*0.8, xend = x2_adj*1.15, yend = y2_adj*0.8,
                   color = poss_primary, 
                   alpha = sequence_xg_adj), size = 1.25,
               arrow = arrow(length = unit(1, "mm"))) +
  geom_point(data = sp1 |> filter(type_name_adj == "Shot") |> 
               arrange(desc(factor(shot_outcome, levels = c("Goal", "Saved", "Miss", "Blocked")))),
             aes(x = x*1.15, y = y*0.8, 
                 size = xg_shooter, 
                 shape = shot_outcome, fill = shot_outcome_fill,
                 color = poss_secondary), stroke = 2,
             show.legend = T) +
  scale_size_binned(breaks = c(0, 0.06, 0.15, 0.33,1),range = c(2, 8), limits = c(0, 1)) +
  scale_alpha_binned(breaks = c(0, 0.06, 0.15, 0.33,1), range = c(0.1, 1), limits = c(0,1)) +
  scale_shape_manual(values = c("Goal" = 21, "Saved" = 23, "Miss" = 24, "Blocked" = 22), drop = F,
                     limits = c("Goal", "Saved", "Miss", "Blocked"),
                     guide = guide_legend(title = NA, 
                                          override.aes = list(
                                            size = c(5, 3, 3, 3),
                                            color = team_secondary,
                                            fill = c(team_primary, 'floralwhite', 'floralwhite', 'floralwhite')
                                          ))) +
  scale_fill_identity() +
  scale_color_identity() +
  coord_flip() + scale_y_reverse() +
  guides(alpha = 'none', size = 'none') +
  theme_matt +
  theme(aspect.ratio = 105/80,
        axis.text = element_blank(), axis.title = element_blank(),
        panel.grid.major = element_blank(),
        legend.direction = "horizontal",
        legend.justification = 'right',
        legend.position = c(0.99, 0.98), 
        legend.box.margin=margin(0,0,0,0),
        legend.title = element_blank(),
        plot.caption = element_text(hjust = c(0,1))
        ) +
  labs(title = title,
       subtitle = subtitle$subtitle,
       caption = c('Points sized by shot xG\nNon-goal possession chains shaded by sequence xG','Source: American Soccer Analysis\nViz: Matt Barger'))
