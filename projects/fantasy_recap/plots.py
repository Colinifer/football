"""Interactive visualization components using Plotly for the Fantasy Football Dashboard."""

from __future__ import annotations

import numpy as np
import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
from scipy import stats

# Modern color palette for player comparisons
COLORS = [
    "#3B82F6",  # Blue
    "#EF4444",  # Red
    "#10B981",  # Emerald
    "#F59E0B",  # Amber
    "#8B5CF6",  # Purple
    "#EC4899",  # Pink
    "#06B6D4",  # Cyan
    "#84CC16",  # Lime
]


def plot_density_distributions(
    weekly_df: pd.DataFrame,
    metric_col: str,
    metric_label: str,
    players: list[str],
    bandwidth_adj: float = 1.0,
) -> go.Figure:
    """Create overlaid Gaussian KDE density distribution curves for selected players.

    Displays probability density, mean markers, and quartile ranges.
    """
    fig = go.Figure()

    if weekly_df.empty or not players:
        fig.update_layout(title="No data available for density plot")
        return fig

    # Filter for selected players and valid metric values
    df_filtered = weekly_df[weekly_df["player_name"].isin(players)].copy()
    df_filtered = df_filtered.dropna(subset=[metric_col])

    if df_filtered.empty:
        fig.update_layout(title=f"No data available for metric '{metric_label}'")
        return fig

    all_vals = df_filtered[metric_col].values
    min_val = max(0, float(np.min(all_vals)) - 2.0)
    max_val = float(np.max(all_vals)) + 2.0

    # Ensure evaluation grid
    x_grid = np.linspace(min_val, max_val, 300)

    for i, player in enumerate(players):
        player_df = df_filtered[df_filtered["player_name"] == player]
        vals = player_df[metric_col].values
        color = COLORS[i % len(COLORS)]

        if len(vals) == 0:
            continue

        mean_val = float(np.mean(vals))

        if len(vals) >= 2 and np.std(vals) > 1e-4:
            try:
                kde = stats.gaussian_kde(vals, bw_method=lambda s: s.scotts_factor() * bandwidth_adj)
                y_density = kde(x_grid)
            except Exception:
                # Fallback if covariance matrix is singular
                y_density, _ = np.histogram(vals, bins=10, density=True)
                x_grid = np.linspace(min_val, max_val, len(y_density))
        else:
            # For 1 game or identical values, render a sharp bell/spike
            y_density = np.exp(-0.5 * ((x_grid - mean_val) / 0.5) ** 2) / (0.5 * np.sqrt(2 * np.pi))

        # Density Filled Curve
        fig.add_trace(
            go.Scatter(
                x=x_grid,
                y=y_density,
                mode="lines",
                name=f"{player} (Avg: {mean_val:.1f})",
                line=dict(color=color, width=2.5),
                fill="tozeroy",
                fillcolor=f"rgba({int(color[1:3], 16)}, {int(color[3:5], 16)}, {int(color[5:7], 16)}, 0.25)",
                hovertemplate=f"<b>{player}</b><br>{metric_label}: %{{x:.2f}}<br>Density: %{{y:.3f}}<extra></extra>",
            )
        )

        # Vertical Mean Line
        fig.add_vline(
            x=mean_val,
            line_width=2,
            line_dash="dash",
            line_color=color,
            annotation_text=f"{player.split()[-1]} Mean: {mean_val:.1f}",
            annotation_position="top right",
            annotation_font=dict(color=color, size=10),
        )

        # Rug plot / scatter points at bottom showing individual games
        fig.add_trace(
            go.Scatter(
                x=vals,
                y=[-0.005 * (i + 1)] * len(vals),
                mode="markers",
                name=f"{player} Games",
                showlegend=False,
                marker=dict(symbol="line-ns", size=10, color=color, line=dict(width=2, color=color)),
                hovertemplate=f"<b>{player} Game Output</b>: %{{x:.1f}} {metric_label}<extra></extra>",
            )
        )

    fig.update_layout(
        title=dict(
            text=f"<b>Density Distribution: {metric_label}</b>",
            font=dict(size=18, family="Inter, sans-serif"),
        ),
        xaxis=dict(
            title=f"<b>{metric_label}</b>",
            gridcolor="rgba(200, 200, 200, 0.2)",
            zeroline=True,
        ),
        yaxis=dict(
            title="<b>Probability Density</b>",
            gridcolor="rgba(200, 200, 200, 0.2)",
            showticklabels=False,
        ),
        template="plotly_dark",
        height=480,
        legend=dict(orientation="h", yanchor="bottom", y=1.02, xanchor="right", x=1),
        margin=dict(l=40, r=40, t=60, b=40),
        hovermode="x",
    )

    return fig


def plot_box_violin_distribution(
    weekly_df: pd.DataFrame,
    metric_col: str,
    metric_label: str,
    players: list[str],
) -> go.Figure:
    """Create interactive Box / Violin plot with jittered points showing game distributions."""
    df_filtered = weekly_df[weekly_df["player_name"].isin(players)].copy()
    if df_filtered.empty:
        return go.Figure()

    fig = go.Figure()

    for i, player in enumerate(players):
        player_df = df_filtered[df_filtered["player_name"] == player]
        if player_df.empty:
            continue
        color = COLORS[i % len(COLORS)]

        fig.add_trace(
            go.Violin(
                y=player_df[metric_col],
                x=[player] * len(player_df),
                name=player,
                box_visible=True,
                meanline_visible=True,
                points="all",
                jitter=0.2,
                pointpos=-0.3,
                marker=dict(color=color, size=7, opacity=0.8),
                line=dict(color=color, width=1.8),
                fillcolor=f"rgba({int(color[1:3], 16)}, {int(color[3:5], 16)}, {int(color[5:7], 16)}, 0.3)",
                hovertext=[
                    f"Week {row['week']} vs {row['opponent_team']}: {row[metric_col]:.1f}"
                    for _, row in player_df.iterrows()
                ],
                hoverinfo="text",
            )
        )

    fig.update_layout(
        title=dict(
            text=f"<b>Violin & Box Plot: {metric_label} Distribution</b>",
            font=dict(size=18, family="Inter, sans-serif"),
        ),
        yaxis=dict(title=f"<b>{metric_label}</b>", gridcolor="rgba(200, 200, 200, 0.2)"),
        xaxis=dict(title="<b>Player</b>"),
        template="plotly_dark",
        height=450,
        showlegend=False,
        margin=dict(l=40, r=40, t=60, b=40),
    )

    return fig


def plot_weekly_trendlines(
    weekly_df: pd.DataFrame,
    metric_col: str,
    metric_label: str,
    players: list[str],
) -> go.Figure:
    """Create week-by-week trajectory lines with rolling trendlines."""
    df_filtered = weekly_df[weekly_df["player_name"].isin(players)].copy()
    if df_filtered.empty:
        return go.Figure()

    fig = go.Figure()

    for i, player in enumerate(players):
        player_df = df_filtered[df_filtered["player_name"] == player].sort_values(by=["season", "week"])
        if player_df.empty:
            continue
        color = COLORS[i % len(COLORS)]

        # Game labels (e.g. S26 W1 vs TEN)
        x_labels = [f"W{row['week']} ({row['opponent_team']})" for _, row in player_df.iterrows()]
        y_vals = player_df[metric_col].values

        # Actual game points
        fig.add_trace(
            go.Scatter(
                x=x_labels,
                y=y_vals,
                mode="lines+markers",
                name=f"{player}",
                line=dict(color=color, width=2.5),
                marker=dict(size=9, color=color),
                hovertemplate=f"<b>{player}</b><br>Game: %{{x}}<br>{metric_label}: %{{y:.1f}}<extra></extra>",
            )
        )

        # 3-game rolling average if >= 3 games
        if len(y_vals) >= 3:
            rolling_avg = pd.Series(y_vals).rolling(window=3, min_periods=1).mean().values
            fig.add_trace(
                go.Scatter(
                    x=x_labels,
                    y=rolling_avg,
                    mode="lines",
                    name=f"{player} (3-G Trend)",
                    line=dict(color=color, width=1.5, dash="dot"),
                    showlegend=True,
                    hovertemplate=f"<b>{player} 3-Game Avg</b>: %{{y:.1f}}<extra></extra>",
                )
            )

    fig.update_layout(
        title=dict(
            text=f"<b>Game-by-Game Trajectory: {metric_label}</b>",
            font=dict(size=18, family="Inter, sans-serif"),
        ),
        xaxis=dict(title="<b>Game / Week</b>", gridcolor="rgba(200, 200, 200, 0.2)"),
        yaxis=dict(title=f"<b>{metric_label}</b>", gridcolor="rgba(200, 200, 200, 0.2)"),
        template="plotly_dark",
        height=450,
        legend=dict(orientation="h", yanchor="bottom", y=1.02, xanchor="right", x=1),
        margin=dict(l=40, r=40, t=60, b=40),
        hovermode="x unified",
    )

    return fig


def plot_target_air_yards_quadrant(
    summary_df: pd.DataFrame,
    selected_players: list[str] | None = None,
) -> go.Figure:
    """Create a Target Share (%) vs. Air Yards Share (%) quadrant scatter plot."""
    if summary_df.empty:
        return go.Figure()

    df = summary_df.copy()
    fig = px.scatter(
        df,
        x="avg_target_share_pct",
        y="avg_air_yards_share_pct",
        color="position",
        size="ppg_half",
        hover_name="player_name",
        hover_data={
            "team": True,
            "ppg_half": ":.1f",
            "targets_pg": ":.1f",
            "avg_adot": ":.1f",
            "avg_wopr": ":.3f",
            "avg_target_share_pct": False,
            "avg_air_yards_share_pct": False,
        },
        labels={
            "avg_target_share_pct": "Target Share (%)",
            "avg_air_yards_share_pct": "Air Yards Share (%)",
            "position": "Pos",
            "ppg_half": "Half-PPR PPG",
        },
        template="plotly_dark",
        height=520,
    )

    # Highlight selected players with rings / stars
    if selected_players:
        selected_rows = df[df["player_name"].isin(selected_players)]
        for _, row in selected_rows.iterrows():
            fig.add_annotation(
                x=row["avg_target_share_pct"],
                y=row["avg_air_yards_share_pct"],
                text=f"<b>★ {row['player_name']}</b>",
                showarrow=True,
                arrowhead=2,
                arrowsize=1,
                arrowwidth=2,
                arrowcolor="#F59E0B",
                font=dict(size=12, color="#F59E0B"),
                yshift=10,
            )

    # Quadrant dividing lines (e.g. 15% target share, 20% air yards share)
    fig.add_hline(y=20.0, line_dash="dash", line_color="rgba(255,255,255,0.3)")
    fig.add_vline(x=15.0, line_dash="dash", line_color="rgba(255,255,255,0.3)")

    # Quadrant annotations
    fig.add_annotation(x=25, y=38, text="🔥 Alpha Volume (High Targets + Deep Air Yards)", showarrow=False, font=dict(color="rgba(255,255,255,0.5)", size=11))
    fig.add_annotation(x=5, y=38, text="🚀 Field Stretchers (Low Targets, High ADOT)", showarrow=False, font=dict(color="rgba(255,255,255,0.5)", size=11))
    fig.add_annotation(x=25, y=8, text="🎯 Possession / PPR Machines (High Target Share)", showarrow=False, font=dict(color="rgba(255,255,255,0.5)", size=11))

    fig.update_layout(
        title=dict(
            text="<b>Target Share vs. Air Yards Share Opportunity Map</b>",
            font=dict(size=18, family="Inter, sans-serif"),
        ),
        margin=dict(l=40, r=40, t=60, b=40),
    )

    return fig


def plot_radar_comparison(
    summary_df: pd.DataFrame,
    players: list[str],
) -> go.Figure:
    """Create normalized radar chart comparing volume, efficiency, air yards, and scoring."""
    df_filtered = summary_df[summary_df["player_name"].isin(players)].copy()
    if df_filtered.empty or len(players) == 0:
        return go.Figure()

    radar_metrics = [
        ("ppg_half", "Fantasy PPG", 25.0),
        ("targets_pg", "Targets/G", 12.0),
        ("rec_pg", "Receptions/G", 10.0),
        ("rec_yds_pg", "Rec Yards/G", 100.0),
        ("air_yds_pg", "Air Yards/G", 120.0),
        ("avg_adot", "ADOT", 16.0),
        ("avg_wopr", "WOPR (x100)", 0.8),
    ]

    categories = [label for _, label, _ in radar_metrics]
    fig = go.Figure()

    for i, player in enumerate(players):
        player_row = df_filtered[df_filtered["player_name"] == player]
        if player_row.empty:
            continue
        row = player_row.iloc[0]
        color = COLORS[i % len(COLORS)]

        values = []
        hover_texts = []
        for col, label, max_val in radar_metrics:
            val = float(row.get(col, 0.0) or 0.0)
            if col == "avg_wopr":
                norm_val = min(100.0, (val / max_val) * 100.0)
                hover_texts.append(f"{label}: {val:.3f}")
            else:
                norm_val = min(100.0, (val / max_val) * 100.0)
                hover_texts.append(f"{label}: {val:.1f}")
            values.append(norm_val)

        # Close the radar polygon
        values.append(values[0])
        closed_cats = [*categories, categories[0]]

        fig.add_trace(
            go.Scatterpolar(
                r=values,
                theta=closed_cats,
                fill="toself",
                name=player,
                line=dict(color=color, width=2.5),
                fillcolor=f"rgba({int(color[1:3], 16)}, {int(color[3:5], 16)}, {int(color[5:7], 16)}, 0.2)",
                text=[*hover_texts, hover_texts[0]],
                hoverinfo="text",
            )
        )

    fig.update_layout(
        polar=dict(
            radialaxis=dict(visible=True, range=[0, 100], showticklabels=False),
            angularaxis=dict(direction="clockwise"),
        ),
        title=dict(
            text="<b>Multi-Dimensional Player Profile Comparison (Normalized)</b>",
            font=dict(size=18, family="Inter, sans-serif"),
        ),
        template="plotly_dark",
        height=480,
        legend=dict(orientation="h", yanchor="bottom", y=1.05, xanchor="right", x=1),
        margin=dict(l=40, r=40, t=60, b=40),
    )

    return fig
