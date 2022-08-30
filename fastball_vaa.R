library(RMySQL)
library(DBI)
library(dplyr)
library(tidyverse)
library(Lahman)
library(baseballr)
library(REdaS)
library(ggthemes)
library(ggplot2)
theme_set(theme_bw())

# Connect to DB
statcast_db <- DBI::dbConnect(RMySQL::MySQL(),
                              dbname = "statcast",
                              user = <username>,
                              password = <password>,
                              host = <host>,
                              port = <port>)

dbGetQuery(statcast_db,
           "DESCRIBE statcast;")

# Get Raw Data
rawdata <- dbGetQuery(statcast_db,
           "SELECT player_name, pitcher, game_year, fielding_team, pitch_type, pitch_name, release_pos_z, release_pos_x,
           release_extension, release_speed, release_spin_rate, ax, az, ay,
           vy0, vz0, sz_top, sz_bot, plate_x, plate_z, pfx_x, pfx_z, spin_axis, description, events, type
           FROM statcast")


# Calculate Vertical Approach Angle
rawdata <- rawdata %>%
  mutate(vy_f = -sqrt(vy0^2 - (2 * ay *(50 - (17/12)))),
         t = (vy_f-vy0) / ay,
         vz_f = vz0 + (az * t),
         vaa = -atan(vz_f/vy_f) * (180 / pi))

# Bucket events for data cleaning
hitevents <- c("single", "double", "triple", "home_run")

swingevents <- c("hit_into_play", "swinging_strike", "foul",
                 "foul_tip", "swinging_strike_blocked", "swinging_pitchout",
                 "foul_pitchout")

whiffevents <- c("swinging_strike", "swinging_pitchout", "swinging_strike_blocked", "foul_tip", "missed_bunt")


data <- rawdata %>%
  mutate(ball = ifelse(type == "B", 1, 0),
         strike = ifelse(type == "S", 1, 0),
         swings = ifelse(description %in% swingevents, 1, 0),
         callstrike = ifelse(description == "called_strike", 1, 0),
         swingstrike = ifelse(description %in% whiffevents, 1, 0),
         BB = ifelse(events == "walk", 1, 0),
         SO = ifelse(events == "strikeout", 1, 0),
         BIP = ifelse(description == "hit_into_play", 1, 0),
         hit = ifelse(events %in% hitevents, 1, 0),
         single = ifelse(events == "single", 1, 0),
         double = ifelse(events == "double", 1, 0),
         triple = ifelse(events == "triple", 1, 0),
         HR = ifelse(events == "home_run", 1, 0)) 

# Season Totals and Averages
data <- data %>%
  group_by(pitcher, game_year, pitch_name) %>%
  drop_na(release_pos_z, release_pos_x, release_extension,
          release_speed, release_spin_rate, vaa) %>%
  mutate(avg_release_pos_z = mean(release_pos_z),
         avg_release_pos_x = mean(release_pos_x),
         avg_release_ext = mean(release_extension),
         avg_plate_x = mean(plate_x),
         avg_plate_z = mean(plate_z),
         avg_spin_axis = mean(spin_axis),
         avg_hor = mean(pfx_x),
         avg_vert = mean(pfx_z),
         avg_vaa = mean(vaa),
         pitch_total = n(),
         swings = sum(swings),
         callstrikes = sum(callstrike),
         swingstrikes = sum(swingstrike),
         BB = sum(BB),
         BIP = sum(BIP),
         hits = sum(hit),
         singles = sum(single),
         doubles = sum(double),
         triples = sum(triple),
         HR = sum(HR)) %>%
  group_by(pitcher, pitch_name, game_year) %>%
  mutate(avg_pitch_velo = mean(release_speed),
           avg_release_spin = mean(release_spin_rate),
           whiff_rate = swingstrikes/swings) %>%
           select(game_year, pitcher, fielding_team, avg_release_pos_z, avg_release_pos_x,
                  avg_release_ext, pitch_name, avg_pitch_velo,
                  avg_release_spin, avg_vaa, avg_spin_axis, avg_vert, avg_hor, avg_plate_x,
                  avg_plate_z, pitch_total, swings, callstrikes,
                  swingstrikes, whiff_rate, BB, BIP, hits, singles, doubles, triples, HR) %>%
           drop_na(pitch_name) %>%
           distinct(pitcher, .keep_all = TRUE) %>%
           filter(pitch_name !="null")

# Get Player Names and MLBAM IDs
# For the query, Statcast pulls the batter name. This will help get pitcher name by pairing it with the MLBAMID
people <- read_csv("https://raw.githubusercontent.com/chadwickbureau/register/master/data/people.csv")

mlbam <- people %>%
  mutate(player_name = paste(name_first, name_last)) %>%
  select(player_name, key_mlbam, key_bbref, key_fangraphs) 

# Pull heights from Chadwick first
# This will be used to calculate Estimated Arm Angle
heights <- read_csv("https://raw.githubusercontent.com/chadwickbureau/baseballdatabank/master/core/People.csv") %>%
  mutate(player_name = paste(nameFirst, nameLast),
         height = height / 12) %>%
  select(bbrefID, height, weight, throws) %>%
  distinct(bbrefID, .keep_all = TRUE)

# Pull MLBAM ID's from Chadwick
mlbam <- people %>%
  mutate(player_name = paste(name_first, name_last)) %>%
  select(player_name, key_mlbam, key_bbref, key_fangraphs)

names(mlbam)[names(mlbam) == 'key_mlbam'] <- 'pitcher'
names(mlbam)[names(mlbam) == 'key_bbref'] <- 'bbrefID'
names(mlbam)[names(mlbam) == 'key_fangraphs'] <- 'fgID'

mlbam <- right_join(mlbam, heights, by = 'bbrefID')

drops <- c("bbrefID", "weight")
mlbam <- mlbam[, !(names(mlbam) %in% drops)]

data <- left_join(data, mlbam, by ="pitcher")

# Estimated Arm Angle
data <- data %>%
  ungroup() %>%
  group_by(player_name, game_year) %>%
  mutate(adj = (avg_release_pos_z - height*0.7),
         opp = abs(avg_release_pos_x), # dist. from middle of chest to arm
         hyp = sqrt(opp^2 + adj^2),
         arm_angle = rad2deg(acos((adj^2 + hyp^2 - opp^2) / (2*(adj*hyp)))),
         arm_slot = case_when(arm_angle >= 90 ~ "Submarine",
                              arm_angle >= 70 & arm_angle < 90 ~ "Side Arm",
                              arm_angle < 70 & arm_angle >= 30 ~ "Three-Quarters",
                              arm_angle < 30 ~ "Overhand")) %>%
  drop_na(arm_angle) %>%
  select(game_year, player_name, fgID, height, arm_angle, arm_slot, throws,
         pitch_name, pitch_total, avg_pitch_velo, avg_release_spin, avg_release_ext, 
         avg_release_pos_x, avg_release_pos_z, avg_spin_axis, avg_hor, avg_vert,
         avg_plate_x, avg_plate_z, avg_vaa, swings, callstrikes, whiff_rate, swingstrikes,
         BB, BIP, hits, singles, doubles, triples, HR) %>%
  mutate_if(is.numeric, round, digits = 3) 

data$whiff_rate[is.na(data$whiff_rate)] = 0
data <- data[!apply(data == " ", 1, all), ]

data <- data %>% filter(pitch_name !="")

ff <- data %>% select(game_year, player_name, fgID, height, arm_angle, arm_slot, throws,
                      pitch_name, pitch_total, avg_plate_x, avg_plate_z, avg_vert, avg_hor,
                      avg_spin_axis, avg_release_ext, avg_release_pos_x, avg_release_pos_z, 
                      avg_pitch_velo, avg_release_spin, avg_vaa, swings, callstrikes, whiff_rate, swingstrikes,
                      BB, BIP, hits, singles, doubles, triples, HR) %>%
  filter(pitch_name == "4-Seam Fastball") %>%
  filter(whiff_rate != 0) %>%
  filter(pitch_total >= 50)

# Plot
ggplot(ff, aes(x=avg_plate_z, y=avg_vaa)) +
  geom_point(aes(color = whiff_rate)) +
  scale_colour_gradient(low = "navyblue", high = "red", name = "Whiff Rate") +
  xlab("Average Vertical Position") +
  ylab("Average Vertical Approach Angle") +
  labs(caption = "Data Source: Baseball Savant")
