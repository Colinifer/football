SELECT "season", "game_id", "play_id", "posteam", "defteam", "receiver", "receiver_player_id", "receiver_id", "air_yards", "yards_gained", "complete_pass", "pass_location", "cp", "pass_touchdown", 0 AS "target", 0 AS "game_played"
FROM "nflfastR_pbp"
WHERE ("season" = 2020.0 AND "pass_attempt" = 1.0 AND "season_type" = 'REG' AND "two_point_attempt" = 0.0 AND NOT((("receiver_id") IS NULL)))
--GROUP BY "posteam"
LIMIT 10