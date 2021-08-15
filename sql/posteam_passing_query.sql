SELECT posteam AS team, 
COUNT(DISTINCT 
game_id) AS games,
COUNT(game_id) AS targets,
COUNT(game_id) / COUNT(DISTINCT game_id) AS targets_pg,
SUM(yards_gained) AS tot_rec_yards,
SUM(yards_gained) / COUNT(DISTINCT game_id) AS rec_yards_pg,
SUM(air_yards) AS tot_air_yards,
SUM(air_yards) / COUNT(DISTINCT game_id) AS air_yards_pg,
SUM(yards_gained) / SUM(air_yards) AS racr,
SUM(complete_pass) AS receptions,
SUM(air_yards) / COUNT(game_id) AS adot,
SUM(pass_touchdown) AS td, 
SUM(pass_touchdown) /  COUNT(DISTINCT game_id) AS td_pg
FROM "nflfastR_pbp"
GROUP BY team
ORDER BY td DESC;