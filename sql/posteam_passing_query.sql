SELECT posteam AS team,
    season,
    COUNT(DISTINCT game_id) AS games,
    COUNT(game_id) AS targets,
    COUNT(game_id) / COUNT(DISTINCT game_id) AS targets_pg,
    SUM(yards_gained) AS tot_pass_yards,
    SUM(yards_gained) / COUNT(DISTINCT game_id) AS pass_yards_pg,
    SUM(air_yards) AS tot_air_yards,
    SUM(air_yards) / COUNT(DISTINCT game_id) AS air_yards_pg,
    SUM(yards_gained) / SUM(air_yards) AS racr,
    SUM(complete_pass) AS completions,
    SUM(air_yards) / COUNT(game_id) AS adot,
    SUM(pass_touchdown) AS tot_pass_td,
    SUM(pass_touchdown) / COUNT(DISTINCT game_id) AS pass_td_pg,
    AVG(cpoe) AS avg_cpoe,
    AVG(epa) AS avg_epa,
    SUM(epa) AS tot_epa
FROM "nflfastR_pbp"
WHERE (
        pass_attempt = 1.0
        AND season_type = 'REG'
        AND two_point_attempt = 0.0
        AND NOT(((receiver_player_id) IS NULL))
    )
GROUP BY season,
    team;