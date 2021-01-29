# Libraries
library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(rvest)
library(scales)

# Attempt to make a measure of Wide Receiver Efficiency
# What makes a good wide receiver:
# 1 - Route Running & Separation
# 2 - Catching the football
# 3 - YAC
# 4 - Impact Plays (Converting 3rd downs, big plays (20+ yds), Red Zone and big play TD's)
# Credit to Tyler Brandt of PFF for the idea for some of the factors going into this 

########################################################
# PART I - Data Collection #
########################################################
# Getting the play by play data and organising it
seasons <- 2020
pbp <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  )
})

pbp_rp <- data %>%
  filter(pass == 1, !is.na(epa))

# I have to manually enter the players and routes run as a data frame because I don't have a paid subscription
# to any of the sites that have this data readily available
# PlayerProfiler has this data for 2020 and 2019, but there page is very complex (but nice looking) and I am a 
# novice when it comes to web scraping, and am not sure how to scrape from the tables on their page
# Can see all the data PlayerProfiler has for Davante Adams here:
# url <- "https://www.playerprofiler.com/nfl/davante-adams/"

Routes_Run = data.frame("player_short_name" = c("S.Diggs","D.Adams","D.Hopkins", "D.Waller", "T.Kelce", "A.Robinson", "K.Allen", "T.Lockett", "J.Smith-Schuster", "R.Anderson", "A.Cooper", "C.Kupp", 
                                                "C.Ridley", "R.Woods", "Di.Johnson", "J.Jefferson", "T.Hill", "T.McLaurin", "D.Metcalf", "C.Beasley", "B.Cooks", "T.Boyd", "C.Samuel","M.Jones", 
                                                "A.Thielen", "C.Lamb", "J.Landry", "L.Thomas", "R.Gage","AJ.Brown", "M.Evans", "T.Higgins", "T.Hockenson", "DJ.Moore", "S.Shepard", "C.Davis", 
                                                "C.Godwin", "E.Engram", "C.Claypool", "D.Parker", "D.Schultz", "N.Fant", "D.Mooney", "E.Sanders", "B.Aiyuk", "H.Henry", "J.Crowder", "J.Meyers", 
                                                "M.Gallup", "M.Andrews", "M.Brown", "E.Ebron", "H.Hurst", "H.Renfrow", "T.Hilton", "L.Fitzgerald", "G.Ward", "M.Gesicki", "W.Fuller", "J.Jeudy",
                                                "J.Reynolds", "R.Tonyan", "J.Jones", "T.Patrick", "D.Slayton", "J.Graham", "A.Miller", "K.Bourne", "C.Kirk", "G.Kittle", "N.Agholor", "A.Green",
                                                "D.Byrd", "M.Williams", "A.Hooper", "D.Amendola", "D.Goedert", "A.Brown", "D.Robinson", "R.Gronkowski"
                                                  ),
                        RR = c(595,462, 595, 505, 540, 608, 497, 605, 629, 548, 605, 506, 553, 579, 518, 528, 564, 583, 615, 451, 543, 502, 433, 597, 501, 514, 384, 576, 502, 389, 552, 489, 
                               452, 525, 356, 368, 433, 487, 432, 507, 529, 382, 517, 325, 431, 460, 378, 302, 628, 331, 428, 444, 509, 358, 428, 423, 499, 440, 372, 497, 488, 328, 286, 412, 
                               527, 378, 431, 444, 486, 216, 425, 508, 446, 464, 290, 313, 300, 233, 463, 361))

# Add some new variables
# We want only non-penalty pass plays for the regular season
# Explosive plays are any reception that goes for >20 yards
# RZ TD's defeind by TD's that happen inside the 20
# Third Down Conversion defined by plays where it was third down and it was converted to a first
# divided the number of third down targets the receiver had
# Summarise and get the rest of the data we want 
wr_pbp <- pbp_rp %>%
  filter(play_type=='pass', pass == 1, down <=4, week <= 17, !is.na(receiver_player_name), !is.na(cp)) %>%
  group_by(receiver_player_id) %>%
  mutate(rec = ifelse(incomplete_pass==0 & interception==0, 1, 0),
         explosiveplay = ifelse(complete_pass==1 & yards_gained >= 20 , 1, 0),
         explosivetdplay = ifelse(explosiveplay==1 & touchdown==1, 1, 0),
         ThirdDTargets = ifelse(down==3, 1, 0),
         ThirdDConversion = ifelse(down==3 & third_down_converted==1, 1, 0),
         RedZoneTDs = ifelse(yardline_100<=20 & complete_pass==1 & touchdown==1, 1, 0)
  ) %>%
  summarise(player_short_name = last(receiver_player_name), team= last(posteam), target=n(), rec = sum(rec), epa = mean(epa), 
            qbepa = mean(qb_epa), complete_pass = sum(complete_pass), 
            incomplete_pass = sum(incomplete_pass),
            int = sum(interception), yards = sum(yards_gained), ypr = sum(yards_gained)/sum(rec), TDs = sum(touchdown),
            explosiveplay = sum(explosiveplay), explosiveTD = sum(explosivetdplay), 
            ThirdDownConversion = sum(ThirdDConversion)/sum(ThirdDTargets), RedZoneTDs = sum(RedZoneTDs), air_yards = mean(air_yards), 
            yac_epa = mean(comp_yac_epa), cp = mean(cp), 
            xyac_epa = mean(xyac_epa, na.rm = TRUE), xyac_success = mean(xyac_success, na.rm = TRUE), 
            xyac_mean_yardage = mean(xyac_mean_yardage, na.rm=TRUE), xyac_median_yardage = mean(xyac_median_yardage, na.rm=TRUE)
  ) %>%
  filter(rec >= 30) %>%
  arrange(-rec) %>%
  ungroup()

# Need to fix up some of the player names that are different from the NGS tables
wr_pbp[33, "player_short_name"] <- "AJ.Brown"
wr_pbp[37, "player_short_name"] <- "DJ.Moore"

# Get receiving data from NFL Next Gen Stats
ngs_wr2 <- nflfastR:::load_ngs(2020, "receiving") %>%
  filter(week <= 0, receptions >= 30) %>%
  group_by(player_short_name) %>%
  arrange(-receptions) %>%
  ungroup()

# Once again modify some names so they are uniform in both
ngs_wr2[16, "player_short_name"] <- "Di.Johnson"
ngs_wr2[30, "player_short_name"] <- "AJ.Brown"
ngs_wr2[35, "player_short_name"] <- "DJ.Moore"

# Merge the play by play summarized data and NGS data
wr_pbpmaster <- merge(wr_pbp, ngs_wr2, by="player_short_name") %>% arrange(-rec)

# Add team colors 
wr_pbpmaster <- wr_pbpmaster %>%
  left_join(teams_colors_logos, by = c('team' = 'team_abbr'))

# Filter for receivers with 45 or more receptions (because that keeps the charts not to crowded, and that's
# all the patience I had for entering Routes Run data manually )
wr_pbpmaster_55R <- wr_pbpmaster %>% filter(rec>= 45)

# merge the Routes Run Data to the final data frame
wr_pbpmaster_55R <- merge(wr_pbpmaster_55R, Routes_Run, by="player_short_name")


########################################################
# PART II - Simple Model #
########################################################
# Our most simple model will just be the EPA/tar for the wide receiver vs the EPA/tar
# for the quarterback when targeting that WR. Obviously the QB play has a large affect
# here, but good WR play can sometimes elevate QB's and make up for their mistakes, so 
# it'll be interesting to see how they correlate

wr_pbpmaster %>% 
  ggplot(aes(x = epa, y = qbepa)) +
  #horizontal line with mean EPA
  geom_hline(yintercept = mean(wr_pbpmaster$epa), color = "red", linetype = "dashed", alpha=0.5) +
  #vertical line with mean CPOE
  geom_vline(xintercept =  mean(wr_pbpmaster$qbepa), color = "red", linetype = "dashed", alpha=0.5) +
  #add points for the QBs with the right colors
  #cex controls point size and alpha the transparency (alpha = 1 is normal)
  geom_point(color = wr_pbpmaster$team_color, cex=wr_pbpmaster$target/25, alpha = .6) +
  #add names using ggrepel, which tries to make them not overlap
  geom_text_repel(aes(label=player_short_name), max.overlaps = Inf) +
  #add a smooth line fitting cpoe + epa
  stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm')+
  #titles and caption
  labs(x = "WR EPA per target",
       y = "QB EPA per target",
       title = "Wide Receiver Efficiency, 2020",
       caption = "Data: @nflfastR") +
  #uses the black and white ggplot theme
  theme_bw() +
  #center title with hjust = 0.5
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) +
  #make ticks look nice
  #if this doesn't work, `install.packages('scales')`
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

# This is alot of data points, which keeps things messy
# We can cut the number of WR's in half by looking at only WR's with
# 55 or more receptions
wr_pbpmaster_55R %>% 
  ggplot(aes(x = epa, y = qbepa)) +
  #horizontal line with mean EPA
  geom_hline(yintercept = mean(wr_pbpmaster_55R$epa), color = "red", linetype = "dashed", alpha=0.5) +
  #vertical line with mean CPOE
  geom_vline(xintercept =  mean(wr_pbpmaster_55R$qbepa), color = "red", linetype = "dashed", alpha=0.5) +
  #add points for the QBs with the right colors
  #cex controls point size and alpha the transparency (alpha = 1 is normal)
  geom_point(color = wr_pbpmaster_55R$team_color, cex=wr_pbpmaster_55R$target/25, alpha = .6) +
  #add names using ggrepel, which tries to make them not overlap
  geom_text_repel(aes(label=player_short_name), max.overlaps = Inf) +
  #add a smooth line fitting cpoe + epa
  stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm')+
  #titles and caption
  labs(x = "WR EPA per target",
       y = "QB EPA per target",
       title = "Wide Receiver Efficiency, 2020 (45+ Rec)",
       caption = "Data: @nflfastR") +
  #uses the black and white ggplot theme
  theme_bw() +
  #center title with hjust = 0.5
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) +
  #make ticks look nice
  #if this doesn't work, `install.packages('scales')`
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

########################################################
# PART III - New Model #
########################################################
# # # 
# All of the factors used in these formulas are that factor/mean for the 2020 season of all WR's
# above the 45 reception limit. So each value is actually how many times above the mean the player is
# # #
# 1 - Route Running & Separation
# Separation Score = TAYPRR * AvgSeparation
#      TAYPRR  = Targeted Air Yards per Route Run  (Average Depth of Target) x (Targets/Routes Run) 
#      AvgSeparation = From NGS, calculates the distance from nearest defender to receiver at time of catch
# This is similar to Yards Per Route Run, while also taking into account targets
# Targets/Route Run is an way to estimate how effective a receiver is at getting open, or how important
# they are to an offense in being incorporated into plays. 
# This has the downside of hurting receivers who are teams with deep WR depth (like Dallas), with lots to go around
# Adding ADOT is adding the yards per route run, and takes into account if WR is a multi-level threat
# Separation gives an idea of how far away a defender in man or zone coverage is from the receiver at the catch point

# 2 - Catching the Football
# Hands Score = Catches/Targets
# This is pretty self explanatory. Using Catchable Balls instead of targets is preferable
# as targets can include bad QB passes, but I don't have a subscription to any of the sites that
# has catchable balls for WR's available 

# 3 - YAC
# YAC Score = 0.25*Average YAC above expectation
# How many yards you pick up more than what was expected based on position, defenders trajectories, etc.
# It is diminished by a factor of 1/4, because I read that YAC usually accounts for 1/3 of a wide receiver's
# total yards, so it seemed like a good way to weigh it more appropiately. But at 1/3, YAC factor tended to 
# dominate the total score so I lowered it to 1/4

# 4 - Impact Plays
# The tricky part for this was how to weight each aspect. Explosive Plays are weighted mroe than Explosive TD's
# as I didn't want to double count them, but still wanted to add the importance of getting 6. Because
# 3rd Downs and RZ plays are a smaller percentage of overall snaps I lowered their weight to 0.05, where as an
# explosive play is possible anywhere other than inside the 20 yard line. 
# How to weight these is one of my bigger question marks I have. Maybe to weight them closer to the total percentage
# of actual RZ/3D plays that occurred to total routes run on those downs..? This is just me spit balling some numbers
# ImpactPlayScore = ypr + 0.035*(ExplosivePlays/Rec) + 0.015*(ExplosiveTDs) + 0.05*(Third Down Conversion) + 
#                     0.05*(Red Zone TD's)

# 5 - WR Efficiency Rating
# WRER = SeparationScore + HandsScore + YAC_Score + ImpactPlayScore
# Finally to get our final number we add it all up. If a WR is the average for every single value, then they score a 
# 3.13. So anything above 4.0 I would consider quite good, and close to 5 would be quite elite

wr_pbpmaster_55R <- wr_pbpmaster_55R %>%
  mutate(TAYPRR = yards.x*target/RR, 
         catch_rate = rec/target
  ) %>%
  arrange(-epa)

# Mean values to use to compare to the WR's numbers
means <- wr_pbpmaster_55R %>% summarise(targets_m = mean(target), rec_m = mean(rec), 
                                        epa_m = mean(epa), qbepa_m = mean(qbepa), yards_m = mean(yards.x),
                                        ypr_m = mean(ypr), tds_m = mean(TDs), explplay_m = mean(explosiveplay),
                                        expltd_m = mean(explosiveTD),
                                        yac_epa_m = mean(yac_epa), avg_separation_m = mean(avg_separation), 
                                        avg_yac_above_expectation_m = mean(avg_yac_above_expectation),
                                        RR_m = mean(RR), TAYPRR_m = mean(TAYPRR), catch_rate_m = mean(catch_rate),
                                        ThirdDConv_m = mean(ThirdDownConversion), RZTD_m = mean(RedZoneTDs)
)

# Make the WRER Score
wr_pbpmaster_55R_Fin <- wr_pbpmaster_55R %>% 
  summarise(Name=player_short_name,
            Team = team,
            epa = epa,
    HandsScore = catch_rate,
    YAC_Score = 0.25*(avg_yac_above_expectation/means$avg_yac_above_expectation_m),
    ImpactPlayScore = (ypr/means$ypr_m + 0.035*(explosiveplay/rec)/(means$explplay_m/means$rec_m) + 0.015*(explosiveTD/means$expltd_m) + 
                             0.05*(ThirdDownConversion/means$ThirdDConv_m) + 0.05*(RedZoneTDs/means$RZTD_m)),
    SeparationScore = (TAYPRR*avg_separation)/(means$TAYPRR_m*means$avg_separation_m),
    WRER = SeparationScore + HandsScore + YAC_Score + ImpactPlayScore
  ) %>%
  arrange(-WRER)

wr_pbpmaster_55R_Fin <- wr_pbpmaster_55R_Fin %>%
  left_join(teams_colors_logos, by = c('Team' = 'team_abbr'))

wr_pbpmaster_55R_Fin %>% 
  ggplot(aes(x = WRER, y = epa)) +
  #horizontal line with mean EPA
  geom_hline(yintercept = mean(wr_pbpmaster_55R_Fin$epa), color = "red", linetype = "dashed", alpha=0.5) +
  #vertical line with mean CPOE
  geom_vline(xintercept =  mean(wr_pbpmaster_55R_Fin$WRER), color = "red", linetype = "dashed", alpha=0.5) +
  #add points for the QBs with the right colors
  #cex controls point size and alpha the transparency (alpha = 1 is normal)
  geom_point(color = wr_pbpmaster_55R_Fin$team_color, cex=wr_pbpmaster_55R$target/25, alpha = .6) +
  #add names using ggrepel, which tries to make them not overlap
  geom_text_repel(aes(label=Name), max.overlaps = Inf) +
  #add a smooth line fitting cpoe + epa
  stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm')+
  #titles and caption
  labs(x = "WR Efficiency Rating (Hands, Separation, YAC, Impact Plays)",
       y = "EPA per target",
       title = "Wide Receiver Efficiency, 2020 (45+ Rec)",
       caption = "Data: @nflfastR and @NextGenStats") +
  #uses the black and white ggplot theme
  theme_bw() +
  #center title with hjust = 0.5
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) +
  #make ticks look nice
  #if this doesn't work, `install.packages('scales')`
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))


# Improvements:
#     - First, collect more data from past seasons to use for the mean values to give a better idea of 
#         how the WR is performing above/below to historical averages
#     - Include all players who had a reception, not just 45+
#     - Find a more analytically sound way to weight the factors rather than just eye balling something that
#         feels right
#     - What factors am I missing? I wanted to add a contested catch component, because some WR's play style is more
#         accustomed to winning 50/50 jump balls, and ove the middel in traffic as opposed to creating separation via
#         route running, but I couldn't divine a way to calculate such a score without PFF's data that I don't pay for