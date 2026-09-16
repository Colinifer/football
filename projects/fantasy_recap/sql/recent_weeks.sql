SELECT season, week, count(*) 
FROM "nflfastR_player_stats_week" 
GROUP BY season, week 
ORDER BY season DESC, week DESC 
LIMIT 5;
