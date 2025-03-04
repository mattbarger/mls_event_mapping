library(tidyverse)
library(worldfootballR)
library(itscalledsoccer)
library(ggplot2)
library(ggrepel)
library(ggbeeswarm)
library(janitor)
devtools::install_github('American-soccer-analysis/itscalledsoccer-r')
devtools::install_github('JaseZiv/worldfootballR')

asa <- AmericanSoccerAnalysis$new()

mls_players <- asa$get_players(leagues = 'mls') |> mutate(birth_date = as.Date.character(birth_date))
mls_teams <- asa$get_teams(leagues = 'mls')
mls_salaries <- asa$get_player_salaries(leagues = 'mls') |>
  group_by(player_id, team_id, season_name) |> 
  filter(mlspa_release == max(mlspa_release)) |> 
  ungroup()


mls_goals_added <- asa$get_player_goals_added(
  leagues = 'mls',
  split_by_teams = T,
  split_by_seasons = T,
  stage_name = "Regular Season"
) |> unnest(data) |>
  mutate(season_name = as.integer(season_name))

?AmericanSoccerAnalysis
difftime()

Salary_GA <- mls_goals_added |> 
  group_by(player_id, team_id, season_name, general_position, minutes_played) |> 
  summarize(goals_added_raw = sum(goals_added_raw), 
            goals_added_above_avg = sum(goals_added_above_avg)) |> 
  mutate(ga96 = goals_added_raw * 96/minutes_played, 
         gaaa96 = goals_added_above_avg * 96/minutes_played) |> 
  left_join(mls_teams) |> 
  left_join(mls_players |> select(1:4)) |> 
  left_join(mls_salaries |> 
              ungroup() |> 
              select(player_id, team_id, season_name, mlspa_release,
                     base_salary, guaranteed_compensation)) |>
  select(season_name, team_name, player_name, team_short_name, team_abbreviation,
         general_position, minutes_played, 
         base_salary, guaranteed_compensation, goals_added_above_avg, gaaa96, everything()) |>
  mutate(player_age = interval(birth_date, mlspa_release) %/% years())

Salary_GA_24 <- Salary_GA |> ungroup() |> filter(!is.na(base_salary), season_name == 2024) |>
  mutate(mp_pct = percent_rank(minutes_played),
         sal_outlier = base_salary > quantile(base_salary, probs = 0.75, na.rm = FALSE) + 1.5 * IQR(base_salary),
         cap_hit = ifelse(base_salary >= 683750, 683750, base_salary)
  ) 


quantile(Salary_GA_24$base_salary, probs = 0.75, na.rm = F) + 1.5 * IQR(Salary_GA_24$base_salary)

Salary_GA_24 |> 
  mutate(salary_group = case_when(
    base_salary > 683750 ~ "$684k+",
    base_salary > 350000 & base_salary <= 683750 ~ "$350k - $684k",
    base_salary > 100000 & base_salary <= 350000 ~ "$100k - $350k",
    .default = " $67k - $100k")) |>
  group_by(salary_group) |>
  #filter(minutes_played > 450) |>
  summarize(n = n())

Salary_GA_24 |> 
  ungroup() |>
  filter(!is.na(base_salary)) |>
  mutate(salary_group = case_when(
    base_salary > 683750 ~ "$684k+",
    base_salary > 350000 & base_salary <= 683750 ~ "$350k - $684k",
    base_salary > 100000 & base_salary <= 350000 ~ "$100k - $350k",
    .default = " $67k - $100k")) |>
  group_by(salary_group) |>
  mutate(base_salary_pct = percent_rank(base_salary) * 100) |> ungroup() |>
  group_by(general_position) |> 
  mutate(gaaa96_pct = pnorm(gaaa96, mean = mean(gaaa96), sd = sd(gaaa96)),
         sounders = ifelse(team_name == "Charlotte FC", player_name, NA)) |>
  ggplot(aes(x = salary_group, y = gaaa96_pct, color = general_position, size = minutes_played)) + 
  ggbeeswarm::geom_beeswarm() +
  ggrepel::geom_label_repel(aes(label = sounders))

Salary_GA_24 |> ungroup() |> filter(minutes_played > 900) |>  ggplot() + geom_histogram(aes(x = gaaa96))

Salary_GA_24 |> select(team_name, player_name, minutes_played, gaaa96) |> filter(minutes_played > 900) |> 
  mutate(GA_Z = (gaaa96 - mean(gaaa96))/sd(gaaa96)) |> View() 
  

Salary_GA_23 |>
  ungroup() |>
  filter(!is.na(base_salary)) |>
  mutate(mp_pct = percent_rank(minutes_played),
         sal_outlier = base_salary > quantile(base_salary, probs = 0.75, na.rm = FALSE) + 1.5 * IQR(base_salary),
         cap_hit = ifelse(base_salary >= 651250, 651250, base_salary)
         ) |>
  group_by(cap_hit) |> summarize(n = n_distinct(player_id)) |> arrange(cap_hit) |> print(n = 20)
  mutate(mean_salary2 = mean(base_salary[sal_outlier == F]),
         sd_salary2 = sd(base_salary[sal_outlier == F]),
         salary_norm = ifelse(sal_outlier == F,
                              (base_salary - mean_salary2)/sd_salary2,
                              10
                              ),
         sal_pnorm = pnorm(salary_norm)
         ) |>
  group_by(general_position) |>
  mutate(gaaa96_norm = pnorm(gaaa96, mean = mean(gaaa96), sd = sd(gaaa96)),
         high_asset = ifelse(sal_outlier == F, "Rest of MLS", "DP/High-Paid TAM")) |>
  ggplot(aes(x = sal_pnorm, y = gaaa96_norm, color = high_asset)) + geom_point()
  arrange(-salary_norm, -base_salary)



  arrange(-sal_sd) |> print(n = 30, scipen = 6)
  filter(minutes_played >= 180) |>
  group_by(general_position) |>
  mutate(gaaa96_mean = mean(gaaa96),
         gaaa96_sd   = sd(gaaa96),
         gaaa96_pct = pnorm(gaaa96, mean = gaaa96_mean, sd = gaaa96_sd),
         gaaa96_sigma = (gaaa96-gaaa96_mean)/gaaa96_sd) |>
  ggplot(aes(x = sal_pct, y = gaaa96_sigma)) + geom_point()
