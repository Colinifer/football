SELECT posteam,
    season,
    COUNT(DISTINCT game_id) AS games,
    COUNT(game_id) AS rush_attempts,
    COUNT(game_id) / COUNT(DISTINCT game_id) AS rush_attempts_pg,
    SUM(yards_gained) AS tot_rush_yards,
    SUM(yards_gained) / COUNT(DISTINCT game_id) AS rush_yards_pg,
    SUM(rush_touchdown) AS tot_rush_td,
    SUM(rush_touchdown) / COUNT(DISTINCT game_id) AS rush_td_pg,
    AVG(epa) AS avg_rush_epa,
    SUM(epa) AS tot_rush_epa
FROM "nflfastR_pbp"
WHERE (
        rush_attempt = 1.0
        AND season_type = 'REG'
        AND two_point_attempt = 0.0
        AND NOT(((rusher_player_id) IS NULL))
    )
GROUP BY season,
    posteam;