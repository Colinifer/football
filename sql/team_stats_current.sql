-------------
-- offense --
-------------

WITH pass_off AS (
	SELECT
		posteam,
		season,
		COUNT(DISTINCT game_id) AS games,
		-- dropbacks
		SUM(qb_dropback) AS dropbacks,
		-- attempts
		SUM(complete_pass + incomplete_pass + interception) AS attempts,
		-- completions
		SUM(complete_pass) AS completions,
		-- interceptions
		SUM(interception) AS interceptions,
		ROUND(SUM(interception)::NUMERIC / COUNT(DISTINCT game_id)::NUMERIC,
			2) AS interceptions_pg,
		-- sacks
		SUM(sack) AS sacks,
		ROUND(SUM(sack)::NUMERIC / COUNT(DISTINCT game_id)::NUMERIC,
			2) AS sacks_pg,
		-- sack yards
		SUM(1 * yards_gained * sack) AS sack_yards,
		-- passing yards
		SUM(passing_yards) AS pass_yards,
		ROUND(SUM(passing_yards)::NUMERIC / COUNT(DISTINCT game_id)::NUMERIC,
			2) AS pass_yards_pg,
		-- passing touchdowns
		SUM(pass_touchdown) AS pass_td,
		ROUND(SUM(pass_touchdown)::NUMERIC / COUNT(DISTINCT game_id)::NUMERIC,
			2) AS pass_td_pg,
		-- air yards
		SUM(air_yards) AS air_yards,
		ROUND(SUM(air_yards)::NUMERIC / COUNT(DISTINCT game_id)::NUMERIC,
			2) AS air_yards_pg,
		-- yards after catch
		SUM(passing_yards::NUMERIC - air_yards::NUMERIC * complete_pass::NUMERIC) AS passing_yards_after_catch,
		ROUND((SUM(passing_yards)::NUMERIC - SUM(air_yards)::NUMERIC) / COUNT(DISTINCT game_id)::NUMERIC,
		2) AS passing_yards_after_catch_pg,
		-- pacr
		ROUND(SUM(passing_yards)::NUMERIC / SUM(air_yards)::NUMERIC,
			2) AS pacr,
		-- adot
		ROUND(SUM(air_yards)::NUMERIC / COUNT(game_id)::NUMERIC,
			2) AS adot,
		-- cpoe
		ROUND(AVG(cpoe)::NUMERIC,
			2) AS avg_cpoe,
		-- passing first downs
		SUM(first_down_pass) AS first_down_pass,
		-- pass epa
		ROUND(SUM(epa)::NUMERIC,
			2) AS pass_epa,
		ROUND(AVG(epa)::NUMERIC,
			2) AS avg_pass_epa,
		-- sack epa
		ROUND(SUM(1 * epa * sack)::NUMERIC,
			2) AS sack_epa,
		ROUND(SUM(1 * epa * sack)::NUMERIC / SUM(sack)::NUMERIC,
			2) AS avg_sack_epa,
		-- int epa
		ROUND(SUM(1 * epa * interception)::NUMERIC,
			2) AS int_epa,
		ROUND(SUM(1 * epa * interception)::NUMERIC / SUM(interception)::NUMERIC,
			2) AS avg_int_epa
	FROM
		"nflfastR_pbp"
	WHERE (play_type IN('pass',
			'qb_spike')
		AND season = (
			SELECT
				max(season)
			FROM
				"nflfastR_pbp")
			AND season_type = 'REG'
			AND two_point_attempt = 0.0
			--AND NOT(receiver_player_id IS NULL)
)
	GROUP BY
		season,
		posteam
),
rush_off AS (
	SELECT
		posteam,
		season,
		COUNT(DISTINCT game_id) AS games,
		-- rushes
		COUNT(game_id) AS rush_attempts,
		ROUND(COUNT(game_id)::NUMERIC / COUNT(DISTINCT game_id)::NUMERIC,
			2) AS rush_attempts_pg,
		-- rushing yards
		SUM(yards_gained) AS rush_yards,
		ROUND(SUM(yards_gained)::NUMERIC / COUNT(DISTINCT game_id)::NUMERIC,
			2) AS rush_yards_pg,
		-- rushings touchdowns
		SUM(rush_touchdown) AS rush_td,
		ROUND(SUM(rush_touchdown)::NUMERIC / COUNT(DISTINCT game_id)::NUMERIC,
			2) AS rush_td_pg,
		-- epa
		ROUND(SUM(epa)::NUMERIC,
			2) AS rush_epa,
		ROUND(AVG(epa)::NUMERIC,
			2) AS avg_rush_epa,
		-- rushing first downs
		SUM(
			CASE WHEN first_down_rush = 1
				AND lateral_rusher_player_id IS NOT NULL THEN
				1
			ELSE
				0
			END) AS first_down_rush,
		-- hvts
		SUM(
			CASE WHEN yardline_100 <= 10 THEN
				1
			ELSE
				0
			END) AS hvts
	FROM
		"nflfastR_pbp"
	WHERE (play_type IN('run',
			'qb_kneel')
		AND season = (
			SELECT
				max(season)
			FROM
				"nflfastR_pbp")
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
		-- dropbacks
		SUM(qb_dropback) AS dropbacks_against,
		-- attempts
		SUM(complete_pass + incomplete_pass + interception) AS attempts_against,
		-- completions
		SUM(complete_pass) AS completions_against,
		-- interceptions
		SUM(interception) AS interceptions_against,
		ROUND(SUM(interception)::NUMERIC / COUNT(DISTINCT game_id)::NUMERIC,
			2) AS interceptions_pg_against,
		-- sacks
		SUM(sack) AS sacks_against,
		ROUND(SUM(sack)::NUMERIC / COUNT(DISTINCT game_id)::NUMERIC,
			2) AS sacks_pg_against,
		-- sack yards
		SUM(- 1 * yards_gained * sack) AS sack_yards_against,
		-- passing yards
		SUM(passing_yards) AS pass_yards_against,
		ROUND(SUM(passing_yards)::NUMERIC / COUNT(DISTINCT game_id)::NUMERIC,
			2) AS pass_yards_pg_against,
		-- passing touchdowns
		SUM(pass_touchdown) AS pass_td_against,
		ROUND(SUM(pass_touchdown)::NUMERIC / COUNT(DISTINCT game_id)::NUMERIC,
			2) AS pass_td_pg_against,
		-- air yards
		SUM(air_yards) AS air_yards_against,
		ROUND(SUM(air_yards)::NUMERIC / COUNT(DISTINCT game_id)::NUMERIC,
			2) AS air_yards_pg_against,
		-- yards after catch
		SUM(passing_yards::NUMERIC - air_yards::NUMERIC * complete_pass::NUMERIC) AS passing_yards_after_catch_against,
		ROUND((SUM(passing_yards)::NUMERIC - SUM(air_yards)::NUMERIC) / COUNT(DISTINCT game_id)::NUMERIC,
		2) AS passing_yards_after_catch_pg_against,
		-- pacr
		ROUND(SUM(passing_yards)::NUMERIC / SUM(air_yards)::NUMERIC,
			2) AS pacr_against,
		-- adot
		ROUND(SUM(air_yards)::NUMERIC / COUNT(game_id)::NUMERIC,
			2) AS adot_against,
		-- cpoe
		ROUND(AVG(cpoe)::NUMERIC,
			2) AS avg_cpoe_against,
		-- passing first downs
		SUM(first_down_pass) AS first_down_pass_against,
		-- pass epa
		ROUND(SUM(epa)::NUMERIC,
			2) AS pass_epa_against,
		ROUND(AVG(epa)::NUMERIC,
			2) AS avg_pass_epa_against,
		-- sack epa
		ROUND(SUM(- 1 * epa * sack)::NUMERIC,
			2) AS sack_epa_against,
		ROUND(SUM(- 1 * epa * sack)::NUMERIC / SUM(sack)::NUMERIC,
			2) AS avg_sack_epa_against,
		-- int epa
		ROUND(SUM(- 1 * epa * interception)::NUMERIC,
			2) AS int_epa_against,
		ROUND(SUM(- 1 * epa * interception)::NUMERIC / SUM(interception)::NUMERIC,
			2) AS avg_int_epa_against
	FROM
		"nflfastR_pbp"
	WHERE (play_type IN('pass',
			'qb_spike')
		AND season = (
			SELECT
				max(season)
			FROM
				"nflfastR_pbp")
			AND season_type = 'REG'
			AND two_point_attempt = 0.0
			--AND NOT(((receiver_player_id) IS NULL))
)
	GROUP BY
		season,
		defteam
),
rush_def AS (
	SELECT
		defteam,
		season,
		COUNT(DISTINCT game_id) AS games,
		-- rushes
		COUNT(game_id) AS rush_attempts_against,
		ROUND(COUNT(game_id)::NUMERIC / COUNT(DISTINCT game_id)::NUMERIC,
			2) AS rush_attempts_pg_against,
		-- rushing yards
		SUM(yards_gained) AS rush_yards_against,
		ROUND(SUM(yards_gained)::NUMERIC / COUNT(DISTINCT game_id)::NUMERIC,
			2) AS rush_yards_pg_against,
		-- rushings touchdowns
		SUM(rush_touchdown) AS rush_td_against,
		ROUND(SUM(rush_touchdown)::NUMERIC / COUNT(DISTINCT game_id)::NUMERIC,
			2) AS rush_td_pg_against,
		-- epa
		ROUND(SUM(epa)::NUMERIC,
			2) AS rush_epa_against,
		ROUND(AVG(epa)::NUMERIC,
			2) AS avg_rush_epa_against,
		-- rushing first downs
		SUM(
			CASE WHEN first_down_rush = 1
				AND lateral_rusher_player_id IS NOT NULL THEN
				1
			ELSE
				0
			END) AS first_down_rush_against,
		-- hvts
		SUM(
			CASE WHEN yardline_100 <= 10 THEN
				1
			ELSE
				0
			END) AS hvts_against
	FROM
		"nflfastR_pbp"
	WHERE (play_type IN('run',
			'qb_kneel')
		AND season = (
			SELECT
				max(season)
			FROM
				"nflfastR_pbp")
			AND season_type = 'REG'
			AND two_point_attempt = 0.0
			AND NOT(((rusher_player_id) IS NULL)))
	GROUP BY
		season,
		defteam
) -- Temp table end
-- Query start
SELECT
	pass_off.posteam,
	pass_off.season,
	pass_off.games,
	pass_off.dropbacks,
	pass_off.attempts,
	pass_off.completions,
	pass_off.interceptions,
	pass_off.interceptions_pg,
	pass_off.sacks,
	pass_off.sacks_pg,
	pass_off.sack_yards,
	pass_off.pass_yards,
	pass_off.pass_yards_pg,
	pass_off.pass_td,
	pass_off.pass_td_pg,
	pass_off.air_yards,
	pass_off.air_yards_pg,
	pass_off.passing_yards_after_catch,
	pass_off.passing_yards_after_catch_pg,
	pass_off.pacr,
	pass_off.adot,
	pass_off.avg_cpoe,
	pass_off.avg_pass_epa,
	pass_off.first_down_pass,
	pass_off.pass_epa,
	pass_off.avg_pass_epa,
	pass_off.sack_epa,
	pass_off.avg_sack_epa,
	pass_off.int_epa,
	pass_off.avg_int_epa,
	rush_off.rush_attempts,
	rush_off.rush_attempts_pg,
	rush_off.rush_yards,
	rush_off.rush_yards_pg,
	rush_off.rush_td,
	rush_off.rush_td_pg,
	rush_off.avg_rush_epa,
	rush_off.rush_epa,
	rush_off.first_down_rush,
	rush_off.hvts,
	pass_def.dropbacks_against,
	pass_def.attempts_against,
	pass_def.completions_against,
	pass_def.interceptions_against,
	pass_def.interceptions_pg_against,
	pass_def.sacks_against,
	pass_def.sacks_pg_against,
	pass_def.sack_yards_against,
	pass_def.pass_yards_against,
	pass_def.pass_yards_pg_against,
	pass_def.pass_td_against,
	pass_def.pass_td_pg_against,
	pass_def.air_yards_against,
	pass_def.air_yards_pg_against,
	pass_def.passing_yards_after_catch_against,
	pass_def.passing_yards_after_catch_pg_against,
	pass_def.pacr_against,
	pass_def.adot_against,
	pass_def.avg_cpoe_against,
	pass_def.avg_pass_epa_against,
	pass_def.first_down_pass_against,
	pass_def.pass_epa_against,
	pass_def.avg_pass_epa_against,
	pass_def.sack_epa_against,
	pass_def.avg_sack_epa_against,
	pass_def.int_epa_against,
	pass_def.avg_int_epa_against,
	rush_def.rush_attempts_against,
	rush_def.rush_attempts_pg_against,
	rush_def.rush_yards_against,
	rush_def.rush_yards_pg_against,
	rush_def.rush_td_against,
	rush_def.rush_td_pg_against,
	rush_def.avg_rush_epa_against,
	rush_def.rush_epa_against,
	rush_def.first_down_rush_against,
	rush_def.hvts_against
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