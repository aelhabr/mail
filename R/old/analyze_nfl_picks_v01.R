
#'
#'
#'
#+ include = FALSE
rm(list = ls())
setwd("O:/_other/projects/gmail/")

#'
#'
#'
# Packages. ----
library("dplyr")
library("stringr")
# library("readr")
library("readxl")
# library("lubridate")
library("tidyr")

#'
#'
#'
# Parameters. ----
dir_import <- "O:/_other/projects/nfl/"
filename_import <- "data/db_nfl.xlsm"
filepath_import <-
  str_c(dir_import, filename_import)

# season_curr <- 2017
# week_curr <- 0

#'
#'
#'
excel_sheets(filepath_import)

# Import game picks. ----
ws_game_results <- "nfl_game_results"
game_results <-
  filepath_import %>% 
  read_excel(sheet = ws_game_results)

# game_results <-
#   game_results %>%
#   separate(game_results_name, c("season", "wk", "tm_away", "tm_home"))
# 
# game_results <-
#   game_results %>%
#   mutate_at(vars(season, wk), funs(as.numeric))

# Import game picks. ----
ws_game_picks <- "nfl_game_picks"

# Change the n_max paramter because the first non-blank tm_pick_straight is not
# seen in the firs 1000 rows.
game_picks <-
  filepath_import %>% 
  read_excel(sheet = ws_game_picks, guess_max = 2000)
game_picks %>% tail()

# game_picks <-
#   game_picks %>% 
#   filter(person != "k")
# game_picks <-
#   game_picks %>%
#   separate(game_results_name, c("season", "wk", "tm_away", "tm_home"))
# game_picks <-
#   game_picks %>%
#   mutate_at(vars(season, wk), funs(as.numeric)) %>% 
#   mutate_at(vars(person), funs(as.factor))

# # Join. ----
# # cols_join <- c("season", "wk", "tm_home", "tm_away")
# game_picks_join <-
#   game_picks %>%
#   # left_join(game_results, by = c( "game_results_id" = "id"))
#   left_join(game_results, by = c( "game_results_id" = "id"))

# Visualize. ----
last_xl_row <-
  game_results %>% 
  filter(!is.na(season) & !is.na(wk)) %>% 
  mutate(rn = row_number()) %>% 
  arrange(desc(rn)) %>% 
  mutate(rn = row_number()) %>% 
  slice(1)

season_curr <- pull(last_xl_row, season)
week_curr <- pull(last_xl_row, wk)
week_prev <- week_curr - 1
weeks_trail_display <- 5
weeks_in_rs <- 17

cols_date <- c("season", "wk")
cols_separate <- c(cols_date, "tm_away", "tm_home")
cols_display <- c("game_results_id", cols_separate, "person", "line_home", "line_home_pick")

picks_tidy <-
  game_picks %>% 
  select(-one_of(cols_date)) %>% # remove the existing season and wk columns
  mutate(line_home = ifelse(is.na(line_home_open), line_home_close, line_home_open)) %>% 
  # mutate(which_line = ifelse(is.na(line_home_open), "close", "open")) %>% 
  separate(game_results_name, cols_separate) %>%
  mutate_at(vars(cols_date), as.numeric) %>% 
  select(one_of(cols_display)) %>% 
  spread(person, line_home_pick, fill = 0) %>% 
  select(-k) %>% # remove Kid's picks
  filter(wk <= weeks_in_rs) %>% # filter for only regular sesaon games
  arrange(game_results_id)
picks_tidy

determine_winner <-
  function(x_num,
           y_num,
           x_char = "a",
           y_char = "t",
           default_char = "tie") {
    ifelse(abs(x_num) < abs(y_num),
           x_char,
           ifelse(abs(x_num) > abs(y_num), y_char,
           default_char))
  }

picks_calc <-
  picks_tidy %>%
  mutate_at(vars(a, t), funs(diff = line_home - .)) %>% 
  mutate(winner_1g = determine_winner(a, t))
picks_calc

picks_calc_summary_bywk <-
  picks_calc %>% 
  group_by_at(c(cols_date, "winner_1g")) %>% 
  summarise(winner_1g_cnt = n())
picks_calc_summary_bywk %>% tail()

picks_calc_summary_bywk_spread <-
  picks_calc_summary_bywk %>% 
  group_by(season, winner_1g) %>% 
  spread(winner_1g, winner_1g_cnt) %>% 
  arrange(season, wk)
picks_calc_summary_bywk_spread %>% tail(weeks_trail_display)

picks_calc_summary_bywk_spread_calc <-
  picks_calc_summary_bywk_spread %>% 
  mutate(winner_1wk = determine_winner(a, t))
picks_calc_summary_bywk_spread_calc

picks_calc_summary_byseason <-
  picks_calc_summary_bywk_spread_calc %>% 
  group_by(season, winner_1wk) %>% 
  summarise(winner_1wk_cnt = n())
picks_calc_summary_byseason %>% tail()

picks_calc_summary_byseason_spread <-
  picks_calc_summary_byseason %>%
  spread(winner_1wk, winner_1wk_cnt)
picks_calc_summary_byseason_spread %>% tail()

#'
#'
#'
# TODO
# season_2017 <- nflscrapR::season_games(2017)
# 
# url <- "http://www.thepredictiontracker.com/prednfl.html"
# library("rvest")
# 
# filename_html <- str_c("temp")
# dir_scrape <- str_c(getwd(), "/")
# filepath_html <- str_c(dir_scrape, filename_html, ".html")
# filepath_html
# 
# download.file(url, destfile = filepath_html)
# 
# raw <-
#   filepath_html %>%
#   read_html() %>%
#   html_nodes(xpath = "/html/body/pre/pre[1]/text()[2]")
#   # html_nodes("pre") %>%
#   # html_table(header = TRUE) %>%
#   # as.data.frame() %>%
#   # tbl_df()
# 
# edited <- raw[raw != ""]
# edited2 <- str_trim(edited)
# edited2 %>% str_replace_all("\\s*[::alphanum::]", "\\|")
# edited2 %>% strsplit("\\s(?=[^,]+$)", perl = TRUE)
# 
# 
# pg <- read_html("https://en.wikipedia.org/wiki/Main_Page")
# class(pg)
# pg
# links <- html_nodes(pg, "a")
# library(XML)
# X <- xmlTreeParse(url)

