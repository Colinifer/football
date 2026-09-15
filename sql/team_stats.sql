-------------
-- offense --
-------------

WITH pass_off AS (
	SELECT
		posteam,
		season,
		COUNT(DISTINCT game_id) AS games,
		COUNT(game_id) AS targets,
		ROUND(COUNT(game_id)::NUMERIC / COUNT(DISTINCT game_id)::NUMERIC,
			2) AS targets_pg,
		SUM(yards_gained) AS tot_pass_yards,
		ROUND(SUM(yards_gained)::NUMERIC / COUNT(DISTINCT game_id)::NUMERIC,
			2) AS pass_yards_pg,
		SUM(air_yards) AS tot_air_yards,
		ROUND(SUM(air_yards)::NUMERIC / COUNT(DISTINCT game_id)::NUMERIC,
			2) AS air_yards_pg,
		ROUND(SUM(yards_gained)::NUMERIC / SUM(air_yards)::NUMERIC,
			2) AS racr,
		SUM(complete_pass) AS completions,
		ROUND(SUM(air_yards)::NUMERIC / COUNT(game_id)::NUMERIC,
			2) AS adot,
		SUM(pass_touchdown) AS tot_pass_td,
		ROUND(SUM(pass_touchdown)::NUMERIC / COUNT(DISTINCT game_id)::NUMERIC,
			2) AS pass_td_pg,
		ROUND(AVG(cpoe)::NUMERIC,
			2) AS avg_cpoe,
		ROUND(AVG(epa)::NUMERIC,
			2) AS avg_pass_epa,
		ROUND(SUM(epa)::NUMERIC,
			2) AS tot_pass_epa
	FROM
		"nflfastR_pbp"
	WHERE (pass = 1.0
			AND season_type = 'REG'
			AND two_point_attempt = 0.0
			AND NOT(((receiver_player_id) IS NULL)))
	GROUP BY
		season,
		posteam
),
rush_off AS (
	SELECT
		posteam,
		season,
		COUNT(DISTINCT game_id) AS games,
		COUNT(game_id) AS rush_attempts,
		ROUND(COUNT(game_id)::NUMERIC / COUNT(DISTINCT game_id)::NUMERIC,
			2) AS rush_attempts_pg,
		SUM(yards_gained) AS tot_rush_yards,
		ROUND(SUM(yards_gained)::NUMERIC / COUNT(DISTINCT game_id)::NUMERIC,
			2) AS rush_yards_pg,
		SUM(rush_touchdown) AS tot_rush_td,
		ROUND(SUM(rush_touchdown)::NUMERIC / COUNT(DISTINCT game_id)::NUMERIC,
			2) AS rush_td_pg,
		ROUND(AVG(epa)::NUMERIC,
			2) AS avg_rush_epa,
		ROUND(SUM(epa)::NUMERIC,
			2) AS tot_rush_epa
	FROM
		"nflfastR_pbp"
	WHERE (rush = 1.0
			AND season_type = 'REG'
			AND two_point_attempt = 0.0
			AND NOT(((rusher_player_id) IS NULL)))
	GROUP BY
		season,
		posteam
),
-------------
-- defense --
-------------
pass_def AS (
	SELECT
		defteam,
		season,
		COUNT(DISTINCT game_id) AS games,
		COUNT(game_id) AS targets_against,
		ROUND(COUNT(game_id)::NUMERIC / COUNT(DISTINCT game_id)::NUMERIC,
			2) AS targets_against_pg,
		SUM(yards_gained) AS tot_pass_yards_against,
		ROUND(SUM(yards_gained)::NUMERIC / COUNT(DISTINCT game_id)::NUMERIC,
			2) AS pass_yards_against_pg,
		SUM(air_yards) AS tot_air_yards_against,
		ROUND(SUM(air_yards)::NUMERIC / COUNT(DISTINCT game_id)::NUMERIC,
			2) AS air_yards_against_pg,
		ROUND(SUM(yards_gained)::NUMERIC / SUM(air_yards)::NUMERIC,
			2) AS racr_against,
		SUM(complete_pass) AS completions_against,
		ROUND(SUM(air_yards)::NUMERIC / COUNT(game_id)::NUMERIC,
			2) AS adot_against,
		SUM(pass_touchdown) AS tot_pass_td_against,
		ROUND(SUM(pass_touchdown)::NUMERIC / COUNT(DISTINCT game_id)::NUMERIC,
			2) AS pass_td_against_pg,
		ROUND(AVG(cpoe)::NUMERIC,
			2) AS avg_cpoe_against,
		ROUND(AVG(epa)::NUMERIC,
			2) AS avg_pass_epa_against,
		ROUND(SUM(epa)::NUMERIC,
			2) AS tot_pass_epa_against
	FROM
		"nflfastR_pbp"
	WHERE (pass = 1.0
			AND season_type = 'REG'
			AND two_point_attempt = 0.0
			AND NOT(((receiver_player_id) IS NULL)))
	GROUP BY
		season,
		defteam
),
rush_def AS (
	SELECT
		defteam,
		season,
		COUNT(DISTINCT game_id) AS games,
		COUNT(game_id) AS rush_attempts_against,
		ROUND(COUNT(game_id)::NUMERIC / COUNT(DISTINCT game_id)::NUMERIC,
			2) AS rush_attempts_against_pg,
		SUM(yards_gained) AS tot_rush_yards_against,
		ROUND(SUM(yards_gained)::NUMERIC / COUNT(DISTINCT game_id)::NUMERIC,
			2) AS rush_yards_against_pg,
		SUM(rush_touchdown) AS tot_rush_td_against,
		ROUND(SUM(rush_touchdown)::NUMERIC / COUNT(DISTINCT game_id)::NUMERIC,
			2) AS rush_td_against_pg,
		ROUND(AVG(epa)::NUMERIC,
			2) AS avg_rush_epa_against,
		ROUND(SUM(epa)::NUMERIC,
			2) AS tot_rush_epa_against
	FROM
		"nflfastR_pbp"
	WHERE (rush = 1.0
			AND season_type = 'REG'
			AND two_point_attempt = 0.0
			AND NOT(((rusher_player_id) IS NULL)))
	GROUP BY
		season,
		defteam
)
-- Temp table end
-- Query start
SELECT
	pass_off.posteam,
	pass_off.season,
	pass_off.games,
	pass_off.targets,
	pass_off.targets_pg,
	pass_off.tot_pass_yards,
	pass_off.pass_yards_pg,
	pass_off.tot_air_yards,
	pass_off.air_yards_pg,
	pass_off.racr,
	pass_off.completions,
	pass_off.adot,
	pass_off.tot_pass_td,
	pass_off.pass_td_pg,
	pass_off.avg_cpoe,
	pass_off.avg_pass_epa,
	pass_off.tot_pass_epa,
	rush_off.rush_attempts,
	rush_off.rush_attempts_pg,
	rush_off.tot_rush_yards,
	rush_off.rush_yards_pg,
	rush_off.tot_rush_td,
	rush_off.rush_td_pg,
	rush_off.avg_rush_epa,
	rush_off.tot_rush_epa,
	pass_def.targets_against,
	pass_def.targets_against_pg,
	pass_def.tot_pass_yards_against,
	pass_def.pass_yards_against_pg,
	pass_def.tot_air_yards_against,
	pass_def.air_yards_against_pg,
	pass_def.racr_against,
	pass_def.completions_against,
	pass_def.adot_against,
	pass_def.tot_pass_td_against,
	pass_def.pass_td_against_pg,
	pass_def.avg_cpoe_against,
	pass_def.avg_pass_epa_against,
	pass_def.tot_pass_epa_against,
	rush_def.rush_attempts_against,
	rush_def.rush_attempts_against_pg,
	rush_def.tot_rush_yards_against,
	rush_def.rush_yards_against_pg,
	rush_def.tot_rush_td_against,
	rush_def.rush_td_against_pg,
	rush_def.avg_rush_epa_against,
	rush_def.tot_rush_epa_against
FROM
	pass_off
	INNER JOIN rush_off ON pass_off.posteam = rush_off.posteam
		AND pass_off.season = rush_off.season
		AND pass_off.games = rush_off.games
	INNER JOIN pass_def ON pass_off.posteam = pass_def.defteam
		AND pass_off.season = pass_def.season
		AND pass_off.games = pass_def.games
	INNER JOIN rush_def ON pass_off.posteam = rush_def.defteam
		AND pass_off.season = rush_def.season
		AND pass_off.games = rush_def.games