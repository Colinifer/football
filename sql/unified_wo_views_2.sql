SELECT 
    --a posteam_passing
    a.posteam,
    a.season,
    COUNT(DISTINCT a.game_id) AS games,
    COUNT(a.game_id) AS targets,
    COUNT(a.game_id) / COUNT(DISTINCT a.game_id) AS targets_pg,
    SUM(a.yards_gained) AS tot_pass_yards,
    SUM(a.yards_gained) / COUNT(DISTINCT a.game_id) AS pass_yards_pg,
    SUM(a.air_yards) AS tot_air_yards,
    SUM(a.air_yards) / COUNT(DISTINCT a.game_id) AS air_yards_pg,
    SUM(a.yards_gained) / SUM(a.air_yards) AS racr,
    SUM(a.complete_pass) AS completions,
    SUM(a.air_yards) / COUNT(a.game_id) AS adot,
    SUM(a.pass_touchdown) AS tot_pass_td,
    SUM(a.pass_touchdown) / COUNT(DISTINCT a.game_id) AS pass_td_pg,
    AVG(a.cpoe) AS avg_cpoe,
    AVG(a.epa) AS avg_pass_epa,
    SUM(a.epa) AS tot_pass_epa,
    --b defteam_passing
    b.targets_allowed,
    b.targets_allowed_pg,
    b.tot_pass_yards_allowed,
    b.pass_yards_allowed_pg,
    b.tot_air_yards_allowed,
    b.air_yards_allowed_pg,
    b.racr_allowed,
    b.completions_allowed,
    b.adot_allowed,
    b.tot_pass_td_allowed,
    b.pass_td_allowed_pg,
    b.avg_cpoe_allowed,
    b.avg_pass_epa_allowed,
    b.tot_pass_epa_allowed,
    --c posteam_rushing
    c.rush_attempts,
    c.rush_attempts_pg,
    c.tot_rush_yards,
    c.rush_yards_pg,
    c.tot_rush_td,
    c.rush_td_pg,
    c.avg_rush_epa,
    c.tot_rush_epa,
    --d defteam_rushing
    d.rushes_allowed,
    d.rush_attempts_allowed_pg,
    d.tot_rush_yards_allowed,
    d.rush_yards_allowed_pg,
    d.tot_rush_td_allowed,
    d.rush_td_allowed_pg,
    d.avg_rush_epa_allowed,
    d.tot_rush_epa_allowed
FROM "nflfastR_pbp" AS a
    LEFT JOIN (
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
                season = 2020.0
                AND pass_attempt = 1.0
                AND season_type = 'REG'
                AND two_point_attempt = 0.0
                AND NOT(((receiver_player_id) IS NULL))
            )
        GROUP BY season,
            defteam
    ) AS b ON a.posteam = b.defteam
    AND a.season = b.season
    LEFT JOIN (
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
                season = 2020.0
                AND rush_attempt = 1.0
                AND season_type = 'REG'
                AND two_point_attempt = 0.0
                AND NOT(((rusher_player_id) IS NULL))
            )
        GROUP BY season,
            posteam
    ) AS c ON a.posteam = c.posteam
    AND a.season = c.season
    LEFT JOIN (
        SELECT defteam,
            season,
            COUNT(DISTINCT game_id) AS games,
            COUNT(game_id) AS rushes_allowed,
            COUNT(game_id) / COUNT(DISTINCT game_id) AS rush_attempts_allowed_pg,
            SUM(yards_gained) AS tot_rush_yards_allowed,
            SUM(yards_gained) / COUNT(DISTINCT game_id) AS rush_yards_allowed_pg,
            SUM(rush_touchdown) AS tot_rush_td_allowed,
            SUM(rush_touchdown) / COUNT(DISTINCT game_id) AS rush_td_allowed_pg,
            AVG(epa) AS avg_rush_epa_allowed,
            SUM(epa) AS tot_rush_epa_allowed
        FROM "nflfastR_pbp"
        WHERE (
                season = 2020.0
                AND rush_attempt = 1.0
                AND season_type = 'REG'
                AND two_point_attempt = 0.0
                AND NOT(((rusher_player_id) IS NULL))
            )
        GROUP BY season,
            defteam
) AS d ON a.posteam = d.defteam
AND a.season = d.season
WHERE (a.season = 2020.0)
GROUP BY a.posteam, a.season;