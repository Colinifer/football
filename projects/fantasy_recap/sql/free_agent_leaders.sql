WITH latest_game AS (
    SELECT season as max_season, week as max_week
    FROM "nflfastR_player_stats_week"
    ORDER BY season DESC, week DESC
    LIMIT 1
),
ranked_players AS (
    SELECT 
        p.player_id,
        p.player_name,
        p.position,
        p.season,
        p.week,
        p.fantasy_points,
        p.fantasy_points_ppr,
        (p.fantasy_points + p.fantasy_points_ppr) / 2.0 AS fantasy_points_half_ppr,
        r.franchise_name,
        ROW_NUMBER() OVER(PARTITION BY p.position ORDER BY ((p.fantasy_points + p.fantasy_points_ppr) / 2.0) DESC) as rn
    FROM "nflfastR_player_stats_week" p
    JOIN latest_game lg ON p.season = lg.max_season AND p.week = lg.max_week
    LEFT JOIN "nflfastR_players" np ON p.player_id = np.gsis_id
    LEFT JOIN "ffscrapR_rosters" r ON r.player_id::text = np.espn_id
    WHERE p.position IN ('QB', 'RB', 'WR', 'TE')
        AND r.franchise_name IS NULL
)
SELECT 
    player_id,
    player_name,
    position,
    franchise_name,
    season,
    week,
    fantasy_points,
    fantasy_points_half_ppr,
    fantasy_points_ppr
FROM ranked_players
WHERE rn <= 5
ORDER BY position, fantasy_points_half_ppr DESC;
