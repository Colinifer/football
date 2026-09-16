WITH latest_projections AS (
    SELECT DISTINCT ON (fpid)
        fpid,
        name,
        season,
        week,
        scoring,
        _fetched_at,
        (stats->>'points')::float as points_std,
        (stats->>'points_half')::float as points_half,
        (stats->>'points_ppr')::float as points_ppr
    FROM "ffpros_projections"
    WHERE week = 0
    ORDER BY fpid, _fetched_at DESC
),
ranked_projections AS (
    SELECT 
        lp.*,
        fp.position_id,
        r.franchise_name,
        ROW_NUMBER() OVER(PARTITION BY fp.position_id ORDER BY lp.points_half DESC) as rn
    FROM latest_projections lp
    JOIN "ffpros_players" fp ON lp.fpid = fp.player_id
    LEFT JOIN "ffscrapR_rosters" r ON fp.player_name = r.player_name
    WHERE r.franchise_name IS NULL
)
SELECT 
    fpid,
    name,
    position_id,
    franchise_name,
    season,
    week,
    points_std,
    points_half,
    points_ppr,
    _fetched_at
FROM ranked_projections
WHERE rn <= 5
ORDER BY position_id, points_half DESC;
