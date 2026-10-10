"""Statistical metrics, consistency calculations, and comparison utilities."""

from __future__ import annotations

from typing import Any

import numpy as np
import pandas as pd


def compute_player_distribution_stats(weekly_df: pd.DataFrame, player_name: str) -> dict[str, Any]:
    """Compute detailed distribution metrics (mean, median, floor, ceiling, std dev, consistency) for a player."""
    p_df = weekly_df[weekly_df["player_name"] == player_name]
    if p_df.empty:
        return {}

    games = len(p_df)
    pts_half = p_df["points_half"].values
    pts_ppr = p_df["points_ppr"].values
    pts_std = p_df["points_std"].values

    targets = p_df["targets"].values
    receptions = p_df["receptions"].values
    rec_yds = p_df["receiving_yards"].values if "receiving_yards" in p_df else p_df["rec_yds"].values
    rush_yds = p_df["rushing_yards"].values if "rushing_yards" in p_df else p_df["rush_yds"].values
    air_yards = p_df["receiving_air_yards"].values if "receiving_air_yards" in p_df else p_df["air_yards"].values
    adot_vals = p_df["adot"].values

    mean_half = float(np.mean(pts_half))
    std_half = float(np.std(pts_half)) if games > 1 else 0.0
    median_half = float(np.median(pts_half))
    p25_half = float(np.percentile(pts_half, 25))
    p75_half = float(np.percentile(pts_half, 75))
    p90_half = float(np.percentile(pts_half, 90))

    # Consistency Index (Coefficient of variation: Std / Mean). Lower means more consistent.
    cv = (std_half / mean_half) if mean_half > 0 else 0.0
    if cv < 0.35:
        consistency_rating = "Very High (Rock Solid)"
    elif cv < 0.55:
        consistency_rating = "Moderate (Standard Starter)"
    elif cv < 0.85:
        consistency_rating = "Volatile (Boom / Bust)"
    else:
        consistency_rating = "Extremely Volatile"

    return {
        "player_name": player_name,
        "games": games,
        "team": p_df["team"].iloc[-1] if "team" in p_df else "",
        "pos": p_df["position"].iloc[-1] if "position" in p_df else "",
        # Scoring
        "ppg_half": round(mean_half, 2),
        "median_half": round(median_half, 2),
        "floor_half": round(p25_half, 2),
        "ceiling_half": round(p75_half, 2),
        "boom_half": round(p90_half, 2),
        "std_half": round(std_half, 2),
        "consistency_rating": consistency_rating,
        "ppg_ppr": round(float(np.mean(pts_ppr)), 2),
        "ppg_std": round(float(np.mean(pts_std)), 2),
        # Volume
        "targets_pg": round(float(np.mean(targets)), 2),
        "rec_pg": round(float(np.mean(receptions)), 2),
        "rec_yds_pg": round(float(np.mean(rec_yds)), 2),
        "rush_yds_pg": round(float(np.mean(rush_yds)), 2),
        "scrimmage_yds_pg": round(float(np.mean(rec_yds + rush_yds)), 2),
        "air_yds_pg": round(float(np.mean(air_yards)), 2),
        "avg_adot": round(float(np.mean(adot_vals)), 2),
        "target_share_pct": round(float(np.mean(p_df["target_share"].values)) * 100.0, 1),
        "air_yards_share_pct": round(float(np.mean(p_df["air_yards_share"].values)) * 100.0, 1),
        "wopr": round(float(np.mean(p_df["wopr"].values)), 3),
        "total_tds": int(np.sum(p_df["total_tds"].values)),
    }


def compute_swap_deltas(player_drop_stats: dict[str, Any], player_add_stats: dict[str, Any]) -> dict[str, Any]:
    """Calculate the expected delta (+/-) when dropping one player and adding another."""
    if not player_drop_stats or not player_add_stats:
        return {}

    ppg_delta = round(player_add_stats["ppg_half"] - player_drop_stats["ppg_half"], 2)
    floor_delta = round(player_add_stats["floor_half"] - player_drop_stats["floor_half"], 2)
    ceiling_delta = round(player_add_stats["ceiling_half"] - player_drop_stats["ceiling_half"], 2)
    target_share_delta = round(player_add_stats["target_share_pct"] - player_drop_stats["target_share_pct"], 1)
    air_yards_delta = round(player_add_stats["air_yds_pg"] - player_drop_stats["air_yds_pg"], 1)
    wopr_delta = round(player_add_stats["wopr"] - player_drop_stats["wopr"], 3)

    # Simple qualitative recommendation
    if ppg_delta > 2.0 and target_share_delta >= 0:
        verdict = "🔥 High-Value Upgrade: Significant increase in scoring and opportunity."
    elif ppg_delta > 0:
        verdict = "✅ Positive Upside Swap: Modest statistical upgrade."
    elif ppg_delta > -1.5 and ceiling_delta > 3.0:
        verdict = "⚡ High Ceiling Gamble: Lower floor but higher boom potential."
    elif ppg_delta < -2.0:
        verdict = "⚠️ Downgrade Risk: Player to drop provides superior volume and scoring."
    else:
        verdict = "⚖️ Lateral Move: Similar overall production and role."

    return {
        "ppg_delta": ppg_delta,
        "floor_delta": floor_delta,
        "ceiling_delta": ceiling_delta,
        "target_share_delta": target_share_delta,
        "air_yards_delta": air_yards_delta,
        "wopr_delta": wopr_delta,
        "verdict": verdict,
    }
