SELECT "season", "game_id", "play_id", "posteam", "defteam", "receiver", "receiver_player_id", "receiver_id", "air_yards", "yards_gained", "complete_pass", "pass_location", "cp", "pass_touchdown"
FROM "nflfastR_pbp"
WHERE ("season" = 2020.0 AND "pass_attempt" = 1.0 AND "season_type" = 'REG' AND "two_point_attempt" = 0.0 AND NOT((("receiver_id") IS NULL)))
GROUP BY "posteam"
ALTER TABLE "nflfastR_pbp"
ADD target INT(255),
ADD game_played INT(255);
GROUP BY "game_id", "posteam"
Add game_played ifelse(row_number()==1,1,0)