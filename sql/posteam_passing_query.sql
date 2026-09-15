SELECT
	posteam,
	season,
	COUNT(DISTINCT game_id) AS games,
	COUNT(game_id) AS targets,
	ROUND(COUNT(game_id)::numeric / COUNT(DISTINCT game_id)::numeric,2) AS targets_pg,
	SUM(yards_gained) AS tot_pass_yards,
	ROUND(SUM(yards_gained)::numeric / COUNT(DISTINCT game_id)::numeric, 2) AS pass_yards_pg,
	SUM(air_yards) AS tot_air_yards,
	ROUND(SUM(air_yards)::numeric / COUNT(DISTINCT game_id)::numeric,2) AS air_yards_pg,
	ROUND(SUM(yards_gained)::numeric / SUM(air_yards)::numeric,2) AS racr,
	SUM(complete_pass) AS completions,
	ROUND(SUM(air_yards)::numeric / COUNT(game_id)::numeric,2) AS adot,
	SUM(pass_touchdown) AS tot_pass_td,
	ROUND(SUM(pass_touchdown)::numeric / COUNT(DISTINCT game_id)::numeric,2) AS pass_td_pg,
	ROUND(AVG(cpoe)::numeric,2) AS avg_cpoe,
	ROUND(AVG(epa)::numeric,2) AS avg_pass_epa,
	ROUND(SUM(epa)::numeric,2) AS tot_pass_epa
FROM
	"nflfastR_pbp"
WHERE (pass_attempt = 1.0
	AND season = (
		SELECT
			max(season)
		FROM
			"nflfastR_pbp")
		AND season_type = 'REG'
		AND two_point_attempt = 0.0
		AND NOT(((receiver_player_id) IS NULL)))
GROUP BY
	season,
	posteam;