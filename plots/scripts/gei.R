
# game excitement index

gei_df <- pbp_df |> 
  mutate(
    abs_wpa = abs(ifelse(is.na(wpa),0,wpa))
  ) |> 
  select(game_id, wpa, abs_wpa) |> 
  group_by(game_id) |> 
  summarise(
    gei = sum(abs_wpa, na.rm = T)
  ) |> 
  arrange(desc(gei)) |> 
  filter(grepl('SF',game_id))

{
gei_index <- 12
page <- 1

gei_games <- gei_df |> 
  slice(((gei_index*page)-(gei_index-1)):(gei_index*page))

pbp_df |> 
  filter(game_id  %in% gei_games$game_id) |> 
  ggplot(aes(x=game_seconds_remaining,y=home_wp)) + 
  geom_line() + 
  scale_x_reverse() +
  facet_wrap(facets = 'game_id')
  }
