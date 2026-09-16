SELECT DISTINCT
    h.date,
    p.name,
    p.position,
    p.team,
    h.scoring_format,
    h.teams,
    h.adp
FROM "adp_history" h
JOIN "adp_players" p ON h.player_id = p.player_id
WHERE p.name ILIKE '%Jahmyr Gibbs%'
    AND h.teams = 10
    AND h.scoring_format = 'half-ppr'
ORDER BY h.date ASC, h.scoring_format, h.teams;
