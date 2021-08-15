defteam AS team, 
COUNT(DISTINCT game_id) AS games,
COUNT(game_id) AS targets_allowed,
COUNT(game_id) / COUNT(DISTINCT game_id) AS targets_allowed_pg,
SUM(yards_gained) AS tot_rec_yards,
SUM(yards_gained) / COUNT(DISTINCT game_id) AS rec_yards_allowed_pg,
SUM(air_yards) AS tot_air_yards,
SUM(air_yards) / COUNT(DISTINCT game_id) AS air_yards_allowed_pg,
SUM(yards_gained) / SUM(air_yards) AS racr_allowed,
SUM(complete_pass) AS receptions_allowed,
SUM(air_yards) / COUNT(game_id) AS adot_allowed,
SUM(pass_touchdown) AS td_allowed, 
SUM(pass_touchdown) /  COUNT(DISTINCT game_id) AS td_allowed_pg
FROM "nflfastR_pbp"
WHERE (season = 2020.0 AND pass_attempt = 1.0 AND season_type = 'REG' AND two_point_attempt = 0.0 AND NOT(((receiver_id) IS NULL)))
GROUP BY team
ORDER BY td DESC;