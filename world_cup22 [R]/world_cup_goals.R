#Uploading packages: 
# Function 'library()' to upload installed package.
library(tidyverse)
library(lubridate)

# Uploading database in .csv,
# Function 'read_csv()' package: readr
world_cup_goals <- read_csv(file = "world_cup_goals.csv", delim=";")

world_cup_goals <- read.table(file = "world_cup_goals.csv", 
                        header = TRUE, 
                        sep = ";",
                        encoding = "UTF-8") 

# Vizualise estructure:
# Function 'glimpse()' of the dplyr package (part of tidyverse)
glimpse(world_cup_goals)

# We need the values of "Minute" as type: <factor>, not <Character>
# function mutate() of the dplyr package 
world_cup_goals <- world_cup_goals %>% 
                      mutate(Minute = as.factor(Minute))
  

glimpse(world_cup_goals)


world_cup_goals %>% 
  filter(League != "NA")%>%
  group_by(League) %>%
  summarise(n = n())%>%
  arrange(desc(n)) %>%
  print(n=27)

# WE observe a total of 26 professional leagues, the top 5 leagues of the world are
# the top scorer. The order was: Premier League (41 goals), Ligue 1 (32), Serie A (24),
# La Liga (21), Bundesliga (16)


# To observe the top scores teams, it will be a kind of similar process...
world_cup_goals %>% 
  filter(Player_team != "NA")%>%
  group_by(Player_team) %>%
  summarise(n = n())%>%
  arrange(desc(n)) %>%
  print(n=10)

# A total of 78 different teams were represented when national team score a goal
# PSG was longer the most representative with 22 goals. Second, Barcelona and Man.
# City with 8, followed by 7 goals (AC Milan, Chelsea, Man. Utd), Tottenham with 6,
# and the top 10 finished with 5 goals (Atl. Madrid, Benfica, Juventus).


# To analyse the goals by their origin:
world_cup_goals %>% 
  filter(Goal_origin != "NA")%>%
  group_by(Goal_origin) %>%
  summarise(n = n())%>%
  arrange(desc(n))

# To observe those value with relative frequencies and percent
world_cup_goals %>% 
  filter(Goal_origin != "NA")%>%
  group_by(Goal_origin) %>%
  summarise(n = n())%>%
  mutate(n_rel = n / sum(n)) %>% 
  mutate('percent' = round(100*(n / sum(n)),2)) %>%
  arrange(desc(n))

# Since football is a multi-player game, it is natural to see <Assistance> as the most
# important way to score. It will be necessary another edition's sample 
# to compare those values.

# # To observe the matches with more goals:
world_cup_goals %>% 
  filter(Goal != "NA")%>%
  group_by(Match) %>%
  summarise(n = n())%>%
  arrange(desc(n))

# We observe the matches with penalty shootouts as the top games in goals.
# The final (Argentina-France) was a game with 12 goals, followed by the games:
# Netherlands-Argentina and Croatia-Brazil
# In group stages, we had games as England 6-2 Iran, Portugal 6-1 Switzerland and
# Spain 7-0 Costa Rica


# Now, we can see the top scorers:
world_cup_goals %>% 
  filter(Goal != "NA")%>%
  group_by(Player, Country) %>%
  summarise(n = n())%>%
  arrange(desc(n))

#We had 135 different players scoring on World cup. The top was:
# Mbappé (9), Messi (9). This statistic includes penalty shootout stage.


# To filter (exclude) this kind of goal:
world_cup_goals %>% 
  filter(Goal_origin != "Penalty shootout") %>%
  group_by(Player, Country) %>%
  summarise(n = n())%>%
  arrange(desc(n))
# This the official list, where Mbappé (8 goals) was selected the golden boat

# To see the top scorer countries:
world_cup_goals %>% 
  filter(Goal != "NA")%>%
  group_by(Country) %>%
  summarise(n = n())%>%
  arrange(desc(n)) %>%
  print(n=34)
# 34 Countries scores at least one goal.

# To see with group and stage were more significant:
world_cup_goals %>% 
  filter(Goal != "NA")%>%
  group_by(Stage) %>%
  summarise(n = n())%>%
  arrange(desc(n)) %>%
  print(n=34)
# Between group, Group E was the most important with 22 goals, while Groups D and F
# scored 11 each (group stage are 6 game each.
# the 1/8 finals (8 games) was important with 35 goals (2 matches defined in penalties)

# # To observe the dates with more goals:
world_cup_goals %>% 
  filter(Goal != "NA")%>%
  group_by(Date) %>%
  summarise(n = n())%>%
  arrange(desc(n))
# It was the date Dec/09 with games in Quarter finals stage 
# Second, Nov/28 with final stages in groups G and H (double games)
# Third, Dec/01, also a double games in group stages E and F

