SELECT defteam,
    season,
    COUNT(DISTINCT game_id) AS games,
    COUNT(game_id) AS targets_allowed,
    COUNT(game_id) / COUNT(DISTINCT game_id) AS targets_allowed_pg,
    SUM(yards_gained) AS tot_pass_yards_allowed,
    SUM(yards_gained) / COUNT(DISTINCT game_id) AS pass_yards_allowed_pg,
    SUM(air_yards) AS tot_air_yards_allowed,
    SUM(air_yards) / COUNT(DISTINCT game_id) AS air_yards_allowed_pg,
    SUM(yards_gained) / SUM(air_yards) AS racr_allowed,
    SUM(complete_pass) AS completions_allowed,
    SUM(air_yards) / COUNT(game_id) AS adot_allowed,
    SUM(pass_touchdown) AS tot_pass_td_allowed,
    SUM(pass_touchdown) / COUNT(DISTINCT game_id) AS pass_td_allowed_pg,
    AVG(cpoe) AS avg_cpoe_allowed,
    AVG(epa) AS avg_pass_epa_allowed,
    SUM(epa) AS tot_pass_epa_allowed
FROM "nflfastR_pbp"
WHERE (
        pass_attempt = 1.0
        AND season_type = 'REG'
        AND two_point_attempt = 0.0
        AND NOT(((receiver_player_id) IS NULL))
    )
GROUP BY season,
    defteam;