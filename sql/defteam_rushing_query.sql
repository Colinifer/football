SELECT defteam AS team, 
COUNT(DISTINCT game_id) AS games,
COUNT(game_id) AS rushes_allowed,
COUNT(game_id) / COUNT(DISTINCT game_id) AS rush_attempts_allowed_pg,
SUM(yards_gained) AS tot_rush_yards_allowed,
SUM(yards_gained) / COUNT(DISTINCT game_id) AS rush_yards_allowed_pg,
SUM(rush_touchdown) AS tot_rush_td_allowed, 
SUM(rush_touchdown) /  COUNT(DISTINCT game_id) AS rush_td_allowed_pg
FROM "nflfastR_pbp"
WHERE (season = 2020.0 AND rush_attempt = 1.0 AND season_type = 'REG' AND two_point_attempt = 0.0 AND NOT(((rusher_player_id) IS NULL)))
GROUP BY team
ORDER BY rush_td_allowed DESC;