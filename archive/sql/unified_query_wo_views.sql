SELECT a.posteam AS team, 
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
SUM(a.pass_touchdown) /  COUNT(DISTINCT a.game_id) AS pass_td_pg,
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
b.pass_td_allowed_pg
FROM "nflfastR_pbp" AS a
LEFT JOIN
    (SELECT bb.defteam AS team, 
    bb.season,
    COUNT(DISTINCT bb.game_id) AS games,
    COUNT(bb.game_id) AS targets_allowed,
    COUNT(bb.game_id) / COUNT(DISTINCT bb.game_id) AS targets_allowed_pg,
    SUM(bb.yards_gained) AS tot_pass_yards_allowed,
    SUM(bb.yards_gained) / COUNT(DISTINCT bb.game_id) AS pass_yards_allowed_pg,
    SUM(bb.air_yards) AS tot_air_yards_allowed,
    SUM(bb.air_yards) / COUNT(DISTINCT bb.game_id) AS air_yards_allowed_pg,
    SUM(bb.yards_gained) / SUM(bb.air_yards) AS racr_allowed,
    SUM(bb.complete_pass) AS completions_allowed,
    SUM(bb.air_yards) / COUNT(bb.game_id) AS adot_allowed,
    SUM(bb.pass_touchdown) AS tot_pass_td_allowed, 
    SUM(bb.pass_touchdown) /  COUNT(DISTINCT bb.game_id) AS pass_td_allowed_pg
    FROM "nflfastR_pbp" AS bb
    WHERE (bb.season = 2020.0 AND bb.pass_attempt = 1.0 AND bb.season_type = 'REG' AND bb.two_point_attempt = 0.0 AND NOT(((bb.receiver_player_id) IS NULL)))
    GROUP BY bb.season, team
    ) AS b
    ON team = b.team AND a.season = b.season
WHERE (a.season = 2020.0 AND a.pass_attempt = 1.0 AND a.season_type = 'REG' AND a.two_point_attempt = 0.0 AND NOT(((a.receiver_player_id) IS NULL)))
GROUP BY a.season, a.posteam;