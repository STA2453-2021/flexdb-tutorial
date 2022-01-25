
# load libraries ----------------------------------------------------------
library(tidyverse)
library(nbastatR)
library(future)
library(janitor)

# the seasons we want to extract data for
seasons <- 1948:2020

# for parallel processing
plan(multiprocess)

nba_season_data <- game_logs(seasons = seasons,
                             result_types = "team",
                             season_types = "Regular Season")
nba_playoff_data <- game_logs(seasons = seasons,
                              result_types = "team",
                              season_types = "Playoffs")



# clean var names
nba_season_data <- clean_names(nba_season_data)
nba_playoff_data <- clean_names(nba_playoff_data)

# which teams made the playoffs each season
nba_made_playoffs <- nba_playoff_data %>%
  distinct(year_season, id_team, name_team) %>%
  mutate(made_playoffs = 1)

# add some additional variables
nba_season_data <- nba_season_data%>%
  group_by(year_season, name_team) %>%
  arrange(date_game) %>%
  mutate(game_number = 1:n(),
         wins = cumsum(is_win),
         losses = game_number - wins,
         record = paste0(wins, "-", losses)) %>%
  select(name_team, year_season, slug_season,
         number_game_team_season, date_game,
         id_team, location_game,
         slug_matchup,
         url_team_season_logo,
         wins, losses, record, plus_minus = plusminus_team) %>%
  ungroup()


# join season data with playoffs data and create playoff indicator
nba_season_data <- nba_season_data %>%
  left_join(nba_made_playoffs, by = c("name_team", "year_season", "id_team")) %>%
  mutate(made_playoffs = ifelse(is.na(made_playoffs), 0, 1))


# add some season long information into the data
nba_season_data <- nba_season_data %>%
  group_by(id_team, year_season) %>%
  arrange(date_game) %>%
  mutate(final_record = record[n()],
         final_wins = wins[n()],
         final_losses = losses[n()],
         final_pm = sum(plus_minus),
         plus_minus_pre = cumsum(plus_minus) - plus_minus,
         plus_minus_post = cumsum(plus_minus)) %>%
  ungroup()

# write as .csv file
readr::write_csv(nba_season_data, here::here('data', 'nba_season_data.csv'))



