events_mls24 <- dbGetQuery(
  conn = conn,
  "SELECT *
  FROM mls.events e
  WHERE e.game_id = ANY (
      SELECT game_id
      FROM mls.games
      WHERE season_name = '2024'
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
  ungroup() |> 
  group_by(game_id, possession_team_name) |> 
  mutate(direct_speed = progress * 1.15/time) |>
  summarize(passes_per_sequence = mean(passes), shots = sum(shots),
            direct_velocity = sum(progress) * 1.15/sum(time)) |> 
  arrange(passes_per_sequence) |>
  ggplot(aes(x = passes_per_sequence, y = direct_velocity)) +
  geom_hline(yintercept = 1.89, color = black_hues[6], size = 1) +
  geom_vline(xintercept = 4.2, color = black_hues[6], size = 1) +
  geom_point() +
  #geom_text_repel(aes(label = possession_team_name), family = "Inter") +scale_x_reverse(breaks = c(2.9, 3.5, 4.2, 4.8, 5.5)) +
  scale_y_continuous(breaks = c(1.34, 1.62, 1.89, 2.17, 2.44)) +
  theme_matt +
  labs(title = "Style of Play Chart, First Attempt",
       x = "Average Passes per Sequence", y = "Direct Velocity per Sequence (m/s)")

events_mls25$time