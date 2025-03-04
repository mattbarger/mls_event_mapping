
### Setup Files for Analysis
### By Matt Barger, 3 March 2025
### Run this first. Make sure the .Renviron files are correct
library(odbc)
library(tidyverse)
library(DBI)
library(RPostgres)
options(scipen = 9999)

#Connect to the database
conn <- DBI::dbConnect(RPostgres::Postgres(), 
                       dbname = Sys.getenv("ASA_dbname"), 
                       host = Sys.getenv("ASA_HOST"), 
                       port = 25060, 
                       user = Sys.getenv("ASA_USERNAME") , 
                       password = Sys.getenv("ASA_PASSWORD"))

#Events - Events for all matches from the 2025 season
events_mls25 <- dbGetQuery(
  conn = conn,
  "SELECT *
  FROM mls.events e
  WHERE e.game_id = ANY (
      SELECT game_id
      FROM mls.games
      WHERE season_name = '2025'
      AND home_score IS NOT NULL
  )"
)

#xgoals_mls25 - Information for all shots for all matches in the 2025 season
xgoals_mls25 <- dbGetQuery(
  conn = conn,
  "SELECT *
  FROM mls.xgoals xg 
  WHERE xg.game_id = ANY (
      SELECT game_id
      FROM mls.games
      WHERE season_name = '2025' 
      AND home_score IS NOT NULL
  )"
)

#xgoals_mls25 - Information for all shots for all matches in the 2025 season
gplus_mls25 <- dbGetQuery(
  conn = conn,
  "SELECT *
  FROM mls.goals_added ga
  WHERE ga.game_id = ANY (
      SELECT game_id
      FROM mls.games
      WHERE season_name = '2025' 
      AND home_score IS NOT NULL
  )"
)

#Player Index - all Player Information
player_index <- dbGetQuery(
  conn = conn,
  "SELECT *
  FROM all_opta.players
  WHERE player_id = ANY(
  	SELECT DISTINCT PLAYER_ID  
  	FROM mls.events e
  	WHERE e.game_id = ANY (
  	    SELECT game_id
  	    FROM mls.games
  	    WHERE season_name = '2025' 
  	    AND home_score IS NOT NULL
  )
  )"
)

team_index <- dbGetQuery(
  conn = conn,
  "SELECT *
    FROM all_opta.teams
    WHERE team_id = ANY(
      SELECT DISTINCT TEAM_ID 
      FROM mls.events e
      WHERE e.game_id = ANY (
        SELECT game_id
        FROM mls.games
        WHERE season_name = '2025' 
        AND home_score IS NOT NULL
      )
    )"
)

game_index <- dbGetQuery(
  conn = conn,
  "SELECT *
  FROM mls.games g
  WHERE season_name = '2025'
  and home_score IS NOT NULL"
)

type_index <- dbGetQuery(
  conn = conn,
  "select * from all_opta.ref_event_types ret"
)