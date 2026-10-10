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
)
SELECT 
    lp.fpid,
    lp.name AS projection_name,
    fp.player_id AS ffpros_player_id,
    fp.player_name AS ffpros_player_name,
    fp.position_id,
    lp.points_half,
    lp._fetched_at
FROM latest_projections lp
LEFT JOIN "ffpros_players" fp 
    ON lp.fpid = fp.player_id
LEFT JOIN "ffscrapR_rosters" r 
    ON fp.player_name = r.player_name
WHERE r.player_name IS NULL
    AND fp.position_id <> 'DST'
ORDER BY fp.position_id, lp.points_half DESC NULLS LAST;