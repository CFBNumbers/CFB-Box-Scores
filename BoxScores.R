library(tidyverse)
library(dplyr)
library(na.tools)
library(ggimage)
library(espnscrapeR)
library(gt)
library(ggrepel)
library(jsonlite)
library(glue)
library(moderndive)
library(devtools)
library(png)
library(grid)
library(pracma)
library(zoo)
library(ggtext)
library(elo)
library(gridExtra)
library(rvest)
library(ggthemes)
library(ranger)
library(xgboost)
library(cfbfastR)
library(cfbplotR)
Sys.setenv(CFBD_API_KEY = "JPnkMqBIdRbAYVyM8y24N4hsY6IdXno2pwl2hCNZ98smfwFvGU5sRTrGFFDC7fQZ")
setwd("~/Desktop")
cfblogos <- read.csv("https://raw.githubusercontent.com/CFBNumbers/logos/main/cfblogos.csv")

#Game Box Score
create_advanced_box_score <- function(yr,wk,team_name){
  ## Pull all data from cfbfastR first
  # PBP
  data <- cfbd_pbp_data(year = yr, week = wk, team = team_name, epa_wpa = TRUE, season_type = "regular")
  # Game Info
  game_info <- cfbd_game_info(year = yr, week = wk, team = team_name, season_type = "regular") %>%
    filter(!is.na(home_points))
  g_id <- game_info %>% pull(first(game_id))
  home_pg <- game_info %>% select(home_team, home_post_win_prob) %>%
    rename(offense_play = home_team, 
           pg = home_post_win_prob) %>%
    mutate(pg = as.numeric(pg))
  away_pg <- game_info %>% select(away_team, away_post_win_prob) %>%
    rename(offense_play = away_team, 
           pg = away_post_win_prob) %>%
    mutate(pg = as.numeric(pg))
  pg <- rbind(home_pg, away_pg)
  # Adv Box Score
  m <- cfbd_game_box_advanced(game_id = g_id)
  data <- data %>%
    mutate(id = paste0(id_play, "_", game_id, drive_id, play_text))
  data <- distinct(data, id, .keep_all = TRUE)
  df <- data %>% filter(pass == 1 | rush == 1, !is.na(EPA)) %>%
    mutate(explo_play = ifelse(EPA > 0.8, 1, 0)) %>%
    group_by(offense_play) %>%
    summarise(
      epa_play = mean(EPA),
      success = mean(success),
      n_dropbacks = sum(pass),
      n_rush = sum(rush),
      epa_per_pass = sum(EPA * pass) / n_dropbacks,
      epa_per_rush = sum(EPA * rush) / n_rush,
      plays = n(),
      yards = sum(yards_gained),
      ypp = yards / plays,
      epa_turn = sum(EPA[turnover_vec == 1]),
      explo_rate = mean(explo_play)) %>%
    dplyr::select(offense_play,
                  ypp,
                  epa_play,
                  success,
                  epa_per_pass,
                  epa_per_rush,
                  epa_turn, 
                  explo_rate) %>%
    mutate_if(is.numeric, round, digits = 3)
  # adv box score stuff
  m <- m %>% dplyr::select(team, rushing_stuff_rate, rushing_line_yds_avg)
  # join table together
  df <- df %>% left_join(m, by = c("offense_play" = "team"))
  df <- df %>% left_join(cfblogos, by = c("offense_play" = "school"))
  df <- df %>% left_join(pg, by = c("offense_play"))
  df <-
    df %>% dplyr::select(
      logo,
      offense_play,
      pg, 
      epa_play,
      ypp,
      success,
      explo_rate,
      epa_per_pass,
      epa_per_rush,
      epa_turn, 
      rushing_stuff_rate,
      rushing_line_yds_avg
    ) %>%
    mutate(pg = round(pg, 3))
  # create table subtitle
  game_score_df <- game_info %>% dplyr::select(home_team,home_points,away_team,away_points)
  home_win <- game_score_df$home_points > game_score_df$away_points
  home_score_text <- glue::glue("{game_score_df$home_team} {game_score_df$home_points}")
  away_score_text <- glue::glue("{game_score_df$away_team} {game_score_df$away_points}")
  table_subtitle <- glue::glue("{away_score_text}, {home_score_text}")
  if(home_win){
    table_subtitle <- glue::glue("{home_score_text}, {away_score_text}")
  }
  # create GT table 
  
  gt_theme_538 <- function(data,...) {
    data %>%
      opt_all_caps()  %>%
      opt_table_font(
        font = list(
          google_font("Chivo"),
          default_fonts()
        )
      ) %>%
      tab_style(
        style = cell_borders(
          sides = "bottom", color = "#00000000", weight = px(2)
        ),
        locations = cells_body(
          columns = everything(),
          # This is a relatively sneaky way of changing the bottom border
          # Regardless of data size
          rows = nrow(data$`_data`)
        )
      )  %>%
      tab_options(
        column_labels.background.color = "white",
        table.border.top.width = px(3),
        table.border.top.color = "#00000000",
        table.border.bottom.color = "#00000000",
        table.border.bottom.width = px(3),
        column_labels.border.top.width = px(3),
        column_labels.border.top.color = "#00000000",
        column_labels.border.bottom.width = px(3),
        column_labels.border.bottom.color = "black",
        data_row.padding = px(3),
        source_notes.font.size = 12,
        table.font.size = 16,
        heading.align = "left",
        ...
      )
  }
  
  tbl <-
    df %>% gt() %>%
    cols_align(align = "center") %>% 
    text_transform(
      locations = cells_body(vars(logo)),
      fn = function(x) {
        web_image(url = df$logo,
                  height = px(30))
      }
    ) %>%
    tab_header(title = md("**Advanced Box Score**"),
               subtitle = table_subtitle) %>%
    cols_label(
      logo = "",
      offense_play = "",
      pg = "Post Game Win Expectancy",
      ypp = "Yards/Play",
      epa_play = "EPA/Play",
      success = "Success Rate",
      epa_per_pass = "EPA/Pass",
      epa_per_rush = "EPA/Rush",
      explo_rate = "Explosive Play Rate",
      epa_turn = "EP Lost Turnovers",
      rushing_stuff_rate = "Rush Stuff Rate",
      rushing_line_yds_avg = "OL Yards"
    ) %>%
    fmt_percent(
      columns = c(pg, success, explo_rate, rushing_stuff_rate),
      decimals = 1
    ) %>%
    data_color(
      columns = c(3:10,12), 
      colors = scales::col_numeric(
        palette = paletteer::paletteer_d(
          palette = "ggsci::green_material"
        ) %>% as.character(),
        domain = NULL
      )
    ) %>%
    data_color( 
      columns = 11, 
      colors = scales::col_numeric(reverse = TRUE,
        palette = paletteer::paletteer_d(
          palette = "ggsci::green_material"
        ) %>% as.character(),
        domain = NULL
      )
    ) %>%
    tab_source_note(source_note = md("**Post Game Win Expectancy:** Based on Advanced Stats (Not Including Penalties)")) %>%
    tab_source_note(source_note = md("**Explosive Play**: EPA > 0.8 (75th Percentile)")) %>%
    tab_source_note(source_note = md("**OL Yards**: Yards credited to OL on rushes")) %>%
    tab_source_note(source_note = md("**Table**: @CFBNumbers | **Data**: @CFB_Data with @cfbfastR")) %>%
    gt_theme_538() %>% 
    opt_align_table_header(align = "center")
  return(tbl)
}

tbl <- create_advanced_box_score(2023, 14, "Florida State")

tbl %>% 
  gtsave("ACC.png", expand = 10)

#QB Box Score

create_qb_box_score <- function(yr,wk,team_name){
  gt_theme_538 <- function(data,...) {
    data %>%
      opt_all_caps()  %>%
      opt_table_font(
        font = list(
          google_font("Chivo"),
          default_fonts()
        )
      ) %>%
      tab_style(
        style = cell_borders(
          sides = "bottom", color = "#00000000", weight = px(2)
        ),
        locations = cells_body(
          columns = everything(),
          # This is a relatively sneaky way of changing the bottom border
          # Regardless of data size
          rows = nrow(data$`_data`)
        )
      )  %>%
      tab_options(
        column_labels.background.color = "white",
        table.border.top.width = px(3),
        table.border.top.color = "#00000000",
        table.border.bottom.color = "#00000000",
        table.border.bottom.width = px(3),
        column_labels.border.top.width = px(3),
        column_labels.border.top.color = "#00000000",
        column_labels.border.bottom.width = px(3),
        column_labels.border.bottom.color = "black",
        data_row.padding = px(3),
        source_notes.font.size = 12,
        table.font.size = 16,
        heading.align = "left",
        ...
      )
  }
  
  
cfblogos <- read.csv("https://raw.githubusercontent.com/CFBNumbers/logos/main/cfblogos.csv")

data <- cfbd_pbp_data(year = yr, week = wk, team = team_name, epa_wpa = TRUE, season_type  = "regular")
data <- data %>%
  mutate(id = paste0(id_play, "_", game_id, drive_id, play_text))
data <- distinct(data, id, .keep_all = TRUE)
data <- data %>% mutate(explo_play = ifelse(EPA > 0.80, 1, 0))

pass <- data %>% filter(pass == 1, !is.na(EPA)) %>% 
  group_by(passer_player_name, offense_play) %>%
  summarise(epa_pass = sum(EPA[turnover == 0 & sack == 0]), 
            sr_pass = sum(success), 
            explo_pass = sum(explo_play),
            plays_pass = n(),
            epa_lost_sacks = sum(EPA[sack == 1 & turnover == 0]),
            epa_lost_to = sum(EPA[turnover == 1]),
            epa_to_sack = sum(epa_lost_sacks, epa_lost_to))
rush <- data %>% filter(rush == 1, !is.na(EPA)) %>% 
  group_by(rusher_player_name) %>%
  summarise(epa_rush = sum(EPA[turnover == 0]), 
            sr_rush = sum(success), 
            explo_rush = sum(explo_play),
            epa_lost_to_rush = sum(EPA[turnover == 1]),
            plays_rush = n()) 

df <- pass %>% left_join(rush, by = c("passer_player_name" = "rusher_player_name")) %>%
  replace_na(list(epa_rush = 0, sr_rush = 0, explo_rush = 0, plays_rush = 0, epa_to_sack = 0, epa_lost_to_rush = 0)) %>%
  group_by(passer_player_name) %>% 
  mutate(total_plays = sum(plays_pass,plays_rush),
         epa_lost = sum(epa_to_sack, epa_lost_to_rush),
         total_epa = sum(epa_pass,epa_rush,epa_lost),
         epa_play = total_epa/total_plays,
         sr = sum(sr_pass,sr_rush)/total_plays,
         explo_rate = sum(explo_pass,explo_rush)/total_plays) %>%
  filter(plays_pass > 8) %>%
  left_join(cfblogos, by = c("offense_play" = "school")) %>%
  select(offense_play, passer_player_name, epa_pass, epa_rush, epa_lost,total_plays,epa_play, sr, explo_rate) %>%
  arrange(-epa_play) %>%
  mutate_if(is.numeric, round, 3) %>%
  ungroup()
home_team <- unique(data$home)
away_team <- unique(data$away)
year <- yr

table_title <- glue::glue("{home_team} vs {away_team} QB Box Score")

tbl <- df %>% gt() %>%
  cols_align(align = "center") %>% 
  tab_header(title = html(glue::glue("<strong>{home_team} vs {away_team} QB Box Score<strong>")),
             subtitle = html(glue::glue("<em>{yr} Season<em>"))) %>%
  gt_fmt_cfb_logo(columns = offense_play, height = 40) %>%
  cols_label(offense_play = "", 
             passer_player_name = "", 
             epa_pass = "Pass EPA", 
             epa_rush = "Rush EPA",
             epa_lost = "EPA Lost (Sacks, Turnovers)",
             total_plays = "Plays",
             epa_play = "EPA/Play",
             sr = "Success Rate",
             explo_rate = "Explosive Play Rate") %>%
  data_color(
    columns = 3:9, 
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  tab_style(
    cell_borders(
      side = c("all"),
      color = "black",
      weight = px(3)),
    locations = cells_body(
      columns = c(3:9)
    )
  ) %>%
  fmt_percent(
    columns = c(sr, explo_rate),
    decimals = 1
  ) %>%
  tab_options(heading.title.font.size = 35,
              heading.title.font.weight = "bolder") %>%
  tab_source_note(source_note = md("**Explosive Play:** Plays where the Expected Points Added > 0.80 (75th Percentile)")) %>% 
  tab_source_note(source_note = md("**Table**: @CFBNumbers | **Data**: @CFB_Data with @cfbfastR")) %>% 
  gt_theme_538() %>% 
  opt_align_table_header(align = "center")
return(tbl)
}

tbl <- create_qb_box_score(2023, 7, "Washington")

tbl %>% 
  gtsave("BoMichael.png", expand = 10)



#QB Weekly
pbp <- cfbd_pbp_data(year = 2023, week = 1, epa_wpa = TRUE, season_type = "regular")
pbp <- pbp %>%
  mutate(exp = ifelse(EPA > 0.8, 1, 0))

pass <- pbp %>% filter(pass == 1, !is.na(EPA)) %>% 
  group_by(passer_player_name, offense_play) %>%
  summarise(epa_pass = sum(EPA),
            plays_pass = n(),
            success_pass = sum(success),
            exp_pass = sum(exp)) 
rush <- pbp %>% filter(rush == 1, !is.na(EPA)) %>% 
  group_by(rusher_player_name) %>%
  summarise(epa_rush = sum(EPA),
            plays_rush = n(),
            success_rush = sum(success),
            exp_rush = sum(exp)) 

qb <- pass %>% left_join(rush, by = c("passer_player_name" = "rusher_player_name")) %>%
  filter(!is.na(epa_pass), plays_pass > 5) %>%
  replace_na(list(epa_rush = 0, plays_rush = 0, success_rush = 0, exp_rush = 0, exp_pass = 0)) %>%
  group_by(passer_player_name, offense_play) %>%
  summarise(epa_total = sum(epa_pass,epa_rush),
            succ_total = sum(success_pass, success_rush),
            exp_total = sum(exp_pass, exp_rush),
            plays_total = sum(plays_pass,plays_rush), 
            epa_play = epa_total/plays_total,
            sr = succ_total/plays_total,
            er = exp_total/plays_total)

table <- qb %>% filter(plays_total > 10) %>%
  arrange(-epa_play) %>% head(15) %>%
  ungroup() %>% 
  mutate(rank = 1:n()) %>%
  select(rank, offense_play, passer_player_name, epa_play, sr, er, plays_total) %>%
  mutate_if(is.numeric, round, 3) %>% ungroup()

qbr <- get_college_qbr(season = 2022, type = "weekly")
qbr <- qbr %>% filter(week == 1)
qbr <- qbr %>% select(team_id, display_name, qbr_total, epa_total, qb_plays) %>%
  mutate(epa_play_espn = epa_total/qb_plays,
         team_id = as.double(team_id)) %>%
  left_join(cfblogos, by = c("team_id"))

qb <- qb %>% left_join(qbr, by = c("passer_player_name" = "display_name")) %>%
  filter(!is.na(qbr_total))

qb <- qb %>% left_join(cfblogos, by = c("offense_play" = "school"))
P5 <- qb %>% filter(conference %in% c("ACC", "Pac-12", "Big 12", "Big Ten", 
                                      "SEC", "FBS Independents")) %>%
  filter(!(offense_play %in% c("Army", "Liberty", "UMass", "BYU",
                               "New Mexico State"))) %>%
  filter(epa_play > -0.8, qbr_total > 25)

qbr <- qbr %>%
  filter(team_id != 23)

qbr %>%
  ggplot(aes(x=epa_play_espn, y=qbr_total)) +
  geom_cfb_logos(aes(team = school), width = 0.045, alpha = 0.7) +
  geom_vline(xintercept = mean(qbr$epa_play_espn), linetype = "dashed") +
  geom_hline(yintercept = mean(qbr$qbr_total), linetype = "dashed") +
  geom_text_repel(aes(label = display_name), force = 0.4, point.padding = 0.3, segment.size = 0.2, segment.alpha = 1.5, size = 3) +
  stat_smooth(geom = 'line', se = FALSE, color = "black", alpha = 0.5, method = "lm") +
  labs(y = "ESPN QBR",
       x = "Expected Points Added (EPA) Per Play (ESPN's Model)", 
       subtitle = "<em>2022 Season | EPA/Play = Passes and Rushes<em>", 
       caption = "**Missing Player?**: No QBR/Missing Data/Play Minimum | **Figure**: @CFBNumbers | **Data**: ESPN",
       title = "**QB Play: Week 0-1 (Thru Thurs.)**") +
  theme_fivethirtyeight() +
  theme(
    axis.text = element_text(size = 10),
    strip.text.x = element_text(size = 12, face = "bold"), 
    axis.title.x = element_text(size = 9, face = "bold"),
    legend.position = "none",
    legend.title = element_markdown(size = 9, face = "bold"), 
    axis.title.y = element_text(size = 9, face = "bold"),
    plot.title = element_markdown(size = 14, hjust = 0.5, face = "italic"),
    plot.subtitle = element_markdown(size = 9, hjust = 0.5, face = "italic"),
    plot.caption = element_markdown(size = 8, hjust = 0.5))

ggsave("QBPlayWeek0-1.5.png", dpi = 600, height = 7, width = 9)






