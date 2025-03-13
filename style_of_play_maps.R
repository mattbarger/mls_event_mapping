events_mls24 <- dbGetQuery(
  conn = conn,
  "SELECT *
  FROM mls.events e
  WHERE e.game_id = ANY (
      SELECT game_id
      FROM mls.games
      WHERE season_name = '2024' 
      AND stage_name = 'Regular Season' 
      AND home_score IS NOT NULL
  )"
)

xgoals_mls24 <- dbGetQuery(
  conn = conn,
  "SELECT *
  FROM mls.xgoals xg
  WHERE xg.game_id = ANY (
      SELECT game_id
      FROM mls.games
      WHERE season_name = '2024' 
      AND stage_name = 'Regular Season' 
      AND home_score IS NOT NULL
  )"
)


events_mls24 |>
  left_join(team_index |> select(possession_team_id = team_id, possession_team_name = team_abbreviation)) |>
  group_by(game_id, possession_chain_id, possession_team_name) |>
  filter(possession_team_id == team_id) |>
  left_join(type_index) |>
  mutate(elapsed_time = minute * 60 + second,
         progress = ifelse(type_name == "Pass"|type_name == "Carry" & outcome == 1, x2 - x, 0)) |>
  summarize(passes = sum(type_name == "Pass" & outcome == 1),
            shots = sum(type_name %in% c('Attempt Saved','Goal','Miss','Post')),
            progress = sum(progress),
            start_time = min(elapsed_time),
            end_time = max(elapsed_time),
            out_of_play = sum(out_of_play_secs, na.rm = T),
  ) |> #filter(passes != 0) |>
  mutate(time = end_time - start_time - out_of_play) |> filter(time > 0) |>
  ungroup() |> View()

<<<<<<< HEAD
styles_attack_season <- events_mls24 |>
  left_join(team_index |> select(possession_team_id = team_id, possession_team_name = team_abbreviation)) |>
  group_by(game_id, possession_chain_id, possession_team_id, possession_team_name) |>
=======
events_mls24 |>
  left_join(team_index |> select(possession_team_id = team_id, possession_team_name = team_abbreviation)) |>
  group_by(game_id, possession_chain_id, possession_team_name) |>
>>>>>>> 8c55757 (cleaned up charts for automation.)
  filter(possession_team_id == team_id) |>
  left_join(type_index) |>
  mutate(elapsed_time = minute * 60 + second,
         progress = ifelse(type_name == "Pass"|type_name == "Carry" & outcome == 1, x2 - x, 0)) |>
  summarize(passes = sum(type_name == "Pass" & outcome == 1),
            shots = sum(type_name %in% c('Attempt Saved','Goal','Miss','Post')),
            progress = sum(progress),
            start_time = min(elapsed_time),
            end_time = max(elapsed_time),
            out_of_play = sum(out_of_play_secs, na.rm = T),
<<<<<<< HEAD
  ) |> #filter(passes != 0) |>
  mutate(time = end_time - start_time - out_of_play) |> filter(time > 0) |>
  ungroup() |> 
  group_by(possession_team_id, 
           possession_team_name) |> 
  #mutate(direct_speed = progress * 1.15/time) |>
  summarize(passes_per_sequence = mean(passes), shots = sum(shots),
            direct_velocity = sum(progress) * 1.15/sum(time)) |>
  ungroup() |>
  mutate(z_speed = (direct_velocity - mean(direct_velocity))/sd(direct_velocity),
         z_passes = (passes_per_sequence - mean(passes_per_sequence))/sd(passes_per_sequence),
         attack_style = z_speed - z_passes/2) |>
  select(possession_team_id, possession_team_name, z_speed, z_passes, attack_style) |>
  arrange(attack_style) 

styles_attack_season <- events_mls24 |>
  left_join(team_index |> select(possession_team_id = team_id, possession_team_name = team_abbreviation)) |>
  group_by(game_id, possession_chain_id, possession_team_id, possession_team_name) |>
  filter(possession_team_id == team_id) |>
  left_join(type_index) |>
  mutate(elapsed_time = minute * 60 + second,
         progress = ifelse(type_name == "Pass"|type_name == "Carry" & outcome == 1, x2 - x, 0)) |>
  summarize(passes = sum(type_name == "Pass" & outcome == 1),
            shots = sum(type_name %in% c('Attempt Saved','Goal','Miss','Post')),
            progress = sum(progress),
            start_time = min(elapsed_time),
            end_time = max(elapsed_time),
            out_of_play = sum(out_of_play_secs, na.rm = T),
  ) |> #filter(passes != 0) |>
  mutate(time = end_time - start_time - out_of_play) |> filter(time > 0) |>
  ungroup() |> 
  group_by(game_id,possession_team_id, 
           possession_team_name) |> 
  #mutate(direct_speed = progress * 1.15/time) |>
  summarize(passes_per_sequence = mean(passes), shots = sum(shots),
            direct_velocity = sum(progress) * 1.15/sum(time)) |>
  ungroup() |>
  mutate(z_speed = (direct_velocity - mean(direct_velocity))/sd(direct_velocity),
         z_passes = (passes_per_sequence - mean(passes_per_sequence))/sd(passes_per_sequence),
         attack_style = z_speed - z_passes/2) |>
  select(game_id, team_id = possession_team_id, possession_team_name, z_speed, z_passes, attack_style) |>
  arrange(attack_style) 




ppda_per_game <- events_mls24 |>
  left_join(game_index |> select(game_id, home_team_id, away_team_id)) |>
  left_join(type_index) |>
  filter(!is.na(possession_team_id)) |>
  mutate(defensive_team_id = ifelse(possession_team_id == home_team_id, away_team_id, home_team_id)) |>
  group_by(game_id, defensive_team_id, .drop = F) |>
  summarize(passes_allowed = sum(type_name == "Pass" & outcome == 1 & team_id != defensive_team_id & x <= 60, na.rm = T)) |> 
  left_join(def_actions, by = c('defensive_team_id' = 'team_id', 'game_id')) |>
  mutate(ppda = passes_allowed/defensive_actions) |>
  left_join(team_index |> select(team_id, team_name, team_abbreviation), by = c("defensive_team_id" = "team_id")) |>
  arrange(ppda)
  select(game_id, team_id = defensive_team_id, team_name, ppda, passes_allowed, defensive_actions)
  
  
  left_join(styles_attack_season) |>
  ggplot(aes(x = ppda,y = attack_style)) + geom_point()

def_actions <- events_mls24 |>
  left_join(type_index) |>
  group_by(team_id, game_id) |>
  filter(x >= 40) |>
  summarize(t = sum(type_name == "Tackle"),
            i = sum(type_name == "Interception"),
            f = sum(type_name == "Foul" & outcome == 0) ,
            c = sum(type_name == "Challenge"),
            b = sum(type_name == "Blocked Pass"),
            defensive_actions = t + i + c + b + f)


ppda_per_game |> group_by(team_name) |> 
  summarize(passes_allowed = sum(passes_allowed),
            defensive_actions = sum(defensive_actions),
            ppda = passes_allowed/defensive_actions) |>
  arrange(ppda)



styles_attack_season_2 <- styles_attack_season |> ungroup() |>
  mutate(z_speed = (direct_velocity - mean(direct_velocity))/sd(direct_velocity),
         z_passes = (passes_per_sequence - mean(passes_per_sequence))/sd(passes_per_sequence),
         attack_style = z_speed - z_passes/2) |>
  select(possession_team_id, possession_team_name, z_speed, z_passes, attack_style) |>
  arrange(attack_style) 


styles_attack_season |>
=======
            ) |> #filter(passes != 0) |>
  mutate(time = end_time - start_time - out_of_play) |> filter(time > 0) |>
  ungroup() |> 
  group_by(possession_team_name) |> 
  #mutate(direct_speed = progress * 1.15/time) |>
  summarize(passes_per_sequence = mean(passes), shots = sum(shots),
            direct_velocity = sum(progress) * 1.15/sum(time)) |> 
  #mutate( highlight_team = ifelse(possession_team_name == "LAFC", possession_team_name, "zzz")) |>
  arrange(passes_per_sequence) |>
>>>>>>> 8c55757 (cleaned up charts for automation.)
  ggplot(aes(x = passes_per_sequence, y = direct_velocity)) +
  geom_hline(yintercept = 1.89, color = black_hues[6], size = 1) +
  geom_vline(xintercept = 4.2, color = black_hues[6], size = 1) +
  geom_point(
    #aes(color = highlight_team, alpha = highlight_team, size = highlight_team)
    ) +
  geom_text_repel(aes(label = possession_team_name), family = "Inter") +
  scale_x_continuous(breaks = c(2.9, 3.5, 4.2, 4.8, 5.5)) +
  scale_y_continuous(breaks = c(1.34, 1.62, 1.89, 2.17, 2.44)) +
  scale_alpha_manual(values = c(1, 0.2)) +
  scale_color_manual(values = c(claret_hues[5], teal_hues[4])) +
  scale_size_manual(values = c(4,1)) +
  theme_matt +
  labs(title = "Style of Play Chart, First Attempt",
       x = "Average Passes per Sequence", y = "Direct Velocity per Sequence (m/s)") +
    guides(color = "none", alpha = "none", size = 'none')

styles_attack_season <- events_mls24 |>
  left_join(team_index |> select(possession_team_id = team_id, possession_team_name = team_abbreviation)) |>
  group_by(game_id, possession_chain_id, possession_team_name) |>
  filter(possession_team_id == team_id) |>
  left_join(type_index) |>
  mutate(elapsed_time = minute * 60 + second,
         progress = ifelse(type_name == "Pass"|type_name == "Carry" & outcome == 1, x2 - x, 0)) |>
  summarize(passes = sum(type_name == "Pass" & outcome == 1),
            shots = sum(type_name %in% c('Attempt Saved','Goal','Miss','Post')),
            progress = sum(progress),
            start_time = min(elapsed_time),
            end_time = max(elapsed_time),
            out_of_play = sum(out_of_play_secs, na.rm = T),
  ) |> #filter(passes != 0) |>
  mutate(time = end_time - start_time - out_of_play) |> filter(time > 0) |>
  ungroup() |> 
  group_by(possession_team_name) |> 
  #mutate(direct_speed = progress * 1.15/time) |>
  summarize(passes_per_sequence = mean(passes), shots = sum(shots),
            direct_velocity = sum(progress) * 1.15/sum(time))

xgoals_mls24 |>
  group_by(game_id, team_id) |>
  summarize(shots = n(),
            xg = sum(xg_shooter),
            xg_great = sum(xg_shooter > 0.33),
            xg_good = sum(xg_shooter > 0.15 & xg_shooter <= 0.33),
            xg_average = sum(xg_shooter > 0.06 & xg_shooter <= 0.15),
            xg_poor = sum(xg_shooter <= 0.06)) |>
  left_join(styles_attack_season) |>
  ungroup() |> group_by(possession_team_name) |>
  mutate(attack_tendency = mean(attack_style)) |>
  ggplot(aes(y = xg, x = attack_style)) + geom_point()
