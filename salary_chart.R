sg24 |>
  ungroup() |>
  mutate(sal_outlier = base_salary > quantile(base_salary, 0.75) + 1.5 * IQR(base_salary),
         upper_fence = quantile(base_salary, 0.75) + 1.5 * IQR(base_salary)) |>
  filter(sal_outlier == F) |>
  group_by(general_position, upper_fence) |>
  summarize(mean_salary = mean(base_salary), 
            sd_salary = sd(base_salary)) |>
  right_join(sg24) |>
  mutate(ga_salary_est = mean_salary + gaaa96_sd * sd_salary,
         mkt_distort   = ifelse(base_salary > upper_fence, base_salary - upper_fence, 0)) |>
  ungroup() |>
  select(player_name, team_abbreviation, general_position, upper_fence, gaaa96_sd,  base_salary, ga_salary_est, mkt_distort) |>
  mutate(value_add = ga_salary_est - min(base_salary, upper_fence)) |>
  arrange(-ga_salary_est)

sg24_1 |>
  mutate(onfield_salary = ifelse(ga_salary_est < 71401, 71401, ga_salary_est)) |>
  select(player_name, team_abbreviation, base_salary, ga_salary_est) |>
  arrange(-base_salary) 

team_push <- "DCU"
incoming <- c('Lukas MacNaughton', 'Randall Leal', 'Brandon Servania','Rida Zouhir','Derek Dodson','Hosei Kijima')
outgoing <- c('Theodore Ku-Dipietro', 'Mateusz Klich', 'Matai Akimboni','Russell Canouse','Steve Birnbaum','Cristian Dájome', 'Christopher McVey')

sg24_2 <- Salary_GA_24 |>
  ungroup() |>
  mutate(sal_outlier = base_salary > quantile(base_salary, 0.75) + 1.5 * IQR(base_salary),
         upper_fence = quantile(base_salary, 0.75) + 1.5 * IQR(base_salary)) |>
  filter(sal_outlier == F) |>
  group_by(general_position, upper_fence) |>
  summarize(mean_salary = mean(base_salary), 
            sd_salary = sd(base_salary)) |>
  right_join(Salary_GA_24 |> filter(minutes_played >= 500)) |>
  mutate(gaaa96_sd = (goals_added_above_avg - mean(goals_added_above_avg))/sd(goals_added_above_avg),
         ga_salary_est = mean_salary + gaaa96_sd * sd_salary,
         mkt_distort   = ifelse(base_salary > upper_fence, base_salary - upper_fence, 0)
         ) |>
  ungroup() |>
  select(player_name, team_abbreviation, minutes_played, general_position, upper_fence, gaaa96_sd,  base_salary, ga_salary_est, mkt_distort) |>
  rowwise() |>
  mutate(
    value_add = ga_salary_est - min(base_salary, upper_fence), 
    va_less_lux = ga_salary_est - base_salary,
    floor_ga_sal = ifelse(ga_salary_est < 71401, 71401, ga_salary_est),
    ceiling_base_sal = ifelse(base_salary < upper_fence, base_salary, upper_fence)
    ) |> 
  ungroup() |>
  mutate(
    highlight_player = ifelse(team_abbreviation == team_push | player_name %in% incoming, player_name, NA),
    highlight_team = ifelse(team_abbreviation == team_push | player_name %in% incoming, team_abbreviation, "zzz"),
    highlight_status = factor(ifelse(team_abbreviation == team_push | player_name %in% incoming,
                                     ifelse(player_name %in% outgoing,
                                            'outgoing', 
                                            ifelse(player_name %in% incoming,
                                                   'incoming',
                                                   'staying')),
                                     "zzz"), levels = c('staying','incoming','zzz','outgoing'))
  ) |> arrange(highlight_status)

sg24_2 |> filter(!is.na(highlight_player)) |> select(player_name, highlight_status)
sg24_2  |>
  ggplot(aes(x = ceiling_base_sal/1000, y = floor_ga_sal/1000)) + 
  geom_point(data = sg24_2 |> mutate(is.na(highlight_player)),
             aes(size = minutes_played),
             color = teal_hues[4], alpha = 0.1) +
  geom_point(
    data = sg24_2 |> filter(!is.na(highlight_player)),
    aes(color = highlight_status, 
        #alpha = highlight_status, 
        size = minutes_played
        )
    ) + 
  geom_abline(slope = 1, lty = 2, color = black_hues[10]) +
  geom_vline(xintercept = c(100, 350, 690, 1600)) +
  geom_label_repel(
    data = sg24_2 |> filter(!is.na(highlight_player)),
    aes(color = highlight_status, 
        label = highlight_player,
        #size = minutes_played
        #size = highlight_team
        ),
    fill = 'floralwhite',
    family = 'Fira Mono', fontface = "bold",size = 3
  ) +
  scale_color_manual(values = c('#000000', plot_colors[5], black_hues[5])) +
  #scale_size_manual(values = c(5, 5, 3)) +
  scale_x_log10(breaks = c(100, 350, 690, 1600)) +
  scale_y_log10(breaks = c(100, 350, 690, 1600)) +
  guides(color = "none", alpha = "none", size = "none") +
  labs(
    title = 'DC United: Where will Benteke find support?',
    subtitle = 'Base Salary x Performance-Driven Compensation, 2024 Season',
    caption = c('Onfield value normalizes goals-added contribution to position-average salaries.\nGreen denotes 2024 squad players, Gold incoming 2025 squad players, Gray outgoing squad players.\nPlayer salaries capped at outlier fence ($1.65M per IQR rule)..',
                'Viz: Matt Barger (@mattbarger.bsky.social)\nSource: American Soccer Analysis\nSample limited to 543 players with a minimum of 500 minutes played'),
    x = 'Base Salary ($ 000)',
    y = 'On-Field Value ($ 000)'
  ) +
  theme_matt +
  theme(plot.caption = element_text(hjust = c(0,1)), axis.title = element_text(hjust = 1))
  


team_push <- "CHI"
incoming <- c('Jack Elliott','Omar González')
outgoing <- c('Federico Navarro','Xherdan Shaqiri','Gastón Giménez', 'Fabian Herbers','Arnaud Souquet','Ariel Lassiter', 'Wyatt Omsberg')

sg24_2 <- Salary_GA_24 |>
  ungroup() |>
  mutate(sal_outlier = base_salary > quantile(base_salary, 0.75) + 1.5 * IQR(base_salary),
         upper_fence = quantile(base_salary, 0.75) + 1.5 * IQR(base_salary)) |>
  filter(sal_outlier == F) |>
  group_by(general_position, upper_fence) |>
  summarize(mean_salary = mean(base_salary), 
            sd_salary = sd(base_salary)) |>
  right_join(Salary_GA_24 |> filter(minutes_played >= 500)) |>
  mutate(gaaa96_sd = (goals_added_above_avg - mean(goals_added_above_avg))/sd(goals_added_above_avg),
         ga_salary_est = mean_salary + gaaa96_sd * sd_salary,
         mkt_distort   = ifelse(base_salary > upper_fence, base_salary - upper_fence, 0)
  ) |>
  ungroup() |>
  select(player_name, team_abbreviation, minutes_played, general_position, upper_fence, gaaa96_sd,  base_salary, ga_salary_est, mkt_distort) |>
  rowwise() |>
  mutate(
    value_add = ga_salary_est - min(base_salary, upper_fence), 
    va_less_lux = ga_salary_est - base_salary,
    floor_ga_sal = ifelse(ga_salary_est < 71401, 71401, ga_salary_est),
    ceiling_base_sal = ifelse(base_salary < upper_fence, base_salary, upper_fence)
  ) |> 
  ungroup() |>
  mutate(
    highlight_player = ifelse(team_abbreviation == team_push | player_name %in% incoming, player_name, NA),
    highlight_team = ifelse(team_abbreviation == team_push | player_name %in% incoming, team_abbreviation, "zzz"),
    highlight_status = factor(ifelse(team_abbreviation == team_push | player_name %in% incoming,
                                     ifelse(player_name %in% outgoing,
                                            'outgoing', 
                                            ifelse(player_name %in% incoming,
                                                   'incoming',
                                                   'staying')),
                                     "zzz"), levels = c('staying','incoming','zzz','outgoing'))
  ) |> arrange(highlight_status)

sg24_2 |> filter(!is.na(highlight_player)) |> select(player_name, highlight_status)
sg24_2  |>
  ggplot(aes(x = ceiling_base_sal/1000, y = floor_ga_sal/1000)) + 
  geom_point(data = sg24_2 |> mutate(is.na(highlight_player)),
             aes(size = minutes_played),
             color = teal_hues[4], alpha = 0.1) +
  geom_point(
    data = sg24_2 |> filter(!is.na(highlight_player)),
    aes(color = highlight_status, 
        #alpha = highlight_status, 
        size = minutes_played
    )
  ) + 
  geom_abline(slope = 1, lty = 2, color = black_hues[10]) +
  geom_vline(xintercept = c(100, 350, 690, 1600)) +
  geom_label_repel(
    data = sg24_2 |> filter(!is.na(highlight_player)),
    aes(color = highlight_status, 
        label = highlight_player,
        #size = minutes_played
        #size = highlight_team
    ),
    fill = 'floralwhite',
    family = 'Fira Mono', fontface = "bold",size = 3
  ) +
  scale_color_manual(values = c('#FF0000', plot_colors[5], black_hues[5])) +
  #scale_size_manual(values = c(5, 5, 3)) +
  scale_x_log10(breaks = c(100, 350, 690, 1600)) +
  scale_y_log10(breaks = c(100, 350, 690, 1600)) +
  guides(color = "none", alpha = "none", size = "none") +
  labs(
    title = 'Chicago Fire: Acosta, Cuypers need to raise levels with the new DP imports.',
    subtitle = 'Base Salary x Performance-Driven Compensation, 2024 Season',
    caption = c('Onfield value normalizes goals-added contribution to position-average salaries.\nGreen denotes 2024 squad players, Gold incoming 2025 squad players, Gray outgoing squad players.\nPlayer salaries capped at outlier fence ($1.65M per IQR rule)..',
                'Viz: Matt Barger (@mattbarger.bsky.social)\nSource: American Soccer Analysis\nSample limited to 543 players with a minimum of 500 minutes played'),
    x = 'Base Salary ($ 000)',
    y = 'On-Field Value ($ 000)'
  ) +
  theme_matt +
  theme(plot.caption = element_text(hjust = c(0,1)), axis.title = element_text(hjust = 1))







sg24 |> filter(team_abbreviation == "SEA") |> distinct(player_name)

  Salary_GA_24 |>
  group_by(general_position) |>
  arrange(general_position,ga96) |>
  mutate(pct_cum = cumsum(minutes_played/sum(minutes_played)),
         pct_cum_rd = round(100 * pct_cum)) |> 
  select(player_name, general_position, minutes_played, pct_cum, pct_cum_rd, ga96) |>
  filter(pct_cum_rd == 5) |>
  mutate(correct = abs(pct_cum - 0.05)) |>
  filter(correct == min(correct))
  mutate(minabs(pct_cum - 0.5))


Salary_GA_24 |>
  group_by(general_position) |>
  arrange(general_position,ga96) |>
  mutate(pct_cum = cumsum(minutes_played/sum(minutes_played))) |> 
  select(player_name, general_position, minutes_played, pct_cum, ga96) |> View()
  filter(pct_cum <= 0.05) |>
  summarize(n = n(), sum(ga96),sum(ga96)/n())

  
Salary_GA_24 |>
  group_by(general_position) |>
  summarize(quantile(base_salary,0.05))


min.comp <- 71401 # senior minimum in 2019
lm.basic.int <- lm(log(pmax(0.1, base_salary - min.comp)) ~ -1 + Value_AR, # "-1" removes the intercept if desired (i.e. sets it at 0)
                   data = player.values.data %>%
                     mutate(Value_AR = pmin(10, Value_AR))) #optionally put a cap on g+ to avoid influential data points in the model