"""Fantasy Football Free Agent & Roster Analytics Dashboard with Density Distributions."""

from __future__ import annotations

import os
import sys

import pandas as pd
import streamlit as st
from dotenv import find_dotenv, load_dotenv

# Ensure shared package and local files are importable
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))
sys.path.append(os.path.abspath(os.path.dirname(__file__)))

from data import (
    get_all_active_players_summary,
    get_franchises,
    get_free_agents_data,
    get_player_weekly_stats,
    get_roster_players,
    get_seasons,
)
from metrics import compute_player_distribution_stats, compute_swap_deltas
from plots import (
    plot_box_violin_distribution,
    plot_density_distributions,
    plot_radar_comparison,
    plot_target_air_yards_quadrant,
    plot_weekly_trendlines,
)

load_dotenv(find_dotenv())

# Configure Streamlit page settings
st.set_page_config(
    page_title="Fantasy Football Waiver & Density Analytics",
    page_icon="🏈",
    layout="wide",
    initial_sidebar_state="expanded",
)

# Custom CSS styling for dark modern theme
st.markdown(
    """
    <style>
    .main-header {
        font-size: 2.2rem;
        font-weight: 800;
        background: linear-gradient(90deg, #3B82F6, #10B981, #F59E0B);
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
        margin-bottom: 0.2rem;
    }
    .sub-header {
        color: #94A3B8;
        font-size: 1.05rem;
        margin-bottom: 1.5rem;
    }
    .metric-card {
        background: #1E293B;
        border: 1px solid #334155;
        border-radius: 10px;
        padding: 16px;
        box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1);
    }
    .metric-val {
        font-size: 1.8rem;
        font-weight: 700;
        color: #F8FAFC;
    }
    .metric-label {
        font-size: 0.85rem;
        font-weight: 600;
        color: #94A3B8;
        text-transform: uppercase;
        letter-spacing: 0.05em;
    }
    .stTabs [data-baseweb="tab-list"] {
        gap: 8px;
    }
    .stTabs [data-baseweb="tab"] {
        padding: 10px 20px;
        font-weight: 600;
        border-radius: 6px;
    }
    </style>
    """,
    unsafe_allow_html=True,
)


def main():
    # Header Banner
    st.markdown('<div class="main-header">🏈 Fantasy Football Waiver & Density Analytics</div>', unsafe_allow_html=True)
    st.markdown(
        '<div class="sub-header">Explore unrostered free agents, compare weekly distributions (KDE density, ADOT, air yards), and optimize your fantasy roster.</div>',
        unsafe_allow_html=True,
    )

    # -------------------------------------------------------------
    # SIDEBAR CONTROLS
    # -------------------------------------------------------------
    st.sidebar.header("⚙️ League & Filter Settings")

    franchises = get_franchises()
    selected_franchise = st.sidebar.selectbox(
        "🏆 Select Your Franchise",
        options=franchises,
        index=0,
        help="Select your fantasy team from PostgreSQL",
    )

    seasons = get_seasons()
    selected_season = st.sidebar.selectbox(
        "📅 Season",
        options=seasons if seasons else [2026, 2025],
        index=0,
    )

    scoring_format = st.sidebar.selectbox(
        "🎯 Scoring Format",
        options=["Half-PPR", "PPR", "Standard"],
        index=0,
    )

    pos_filter = st.sidebar.multiselect(
        "🏃 Position Filter",
        options=["QB", "RB", "WR", "TE"],
        default=["QB", "RB", "WR", "TE"],
    )

    min_games = st.sidebar.slider("Min Games Played", min_value=1, max_value=10, value=1, step=1)

    bandwidth_adj = st.sidebar.slider(
        "🌊 Density Smoothing (KDE Bandwidth)",
        min_value=0.4,
        max_value=2.0,
        value=1.0,
        step=0.1,
        help="Adjust Gaussian Kernel Density smoothing width",
    )

    # -------------------------------------------------------------
    # LOAD DATA
    # -------------------------------------------------------------
    with st.spinner("Loading player statistics and free agent database..."):
        df_roster = get_roster_players(selected_franchise, season=selected_season)
        df_free_agents = get_free_agents_data(
            season=selected_season,
            scoring_format=scoring_format,
            min_games=min_games,
        )
        df_all_catalog = get_all_active_players_summary(season=selected_season)

    # Filter free agents by position
    if pos_filter:
        df_free_agents_filtered = df_free_agents[df_free_agents["position"].isin(pos_filter)]
    else:
        df_free_agents_filtered = df_free_agents

    # Get roster player names
    roster_names = df_roster["player_name"].dropna().tolist()

    # -------------------------------------------------------------
    # MAIN TABS
    # -------------------------------------------------------------
    tab_roster, tab_free_agents, tab_deep_dive, tab_swap = st.tabs(
        [
            "🏆 My Roster",
            "🔍 Free Agent Explorer",
            "📊 Player Deep-Dive & Density Comparison",
            "🔄 Roster vs. Waiver Swap Analyzer",
        ]
    )

    # =============================================================
    # TAB 1: MY ROSTER
    # =============================================================
    with tab_roster:
        st.subheader(f"📋 Current Roster: {selected_franchise}")

        if df_roster.empty:
            st.warning("No players found on this roster.")
        else:
            # Join with season performance
            roster_perf = pd.merge(
                df_roster,
                df_all_catalog,
                left_on="gsis_id",
                right_on="player_id",
                how="left",
                suffixes=("", "_perf"),
            )

            # High-level KPIs
            c1, c2, c3, c4 = st.columns(4)
            total_players = len(df_roster)
            avg_roster_ppg = roster_perf["ppg_half"].mean() if "ppg_half" in roster_perf else 0.0
            top_target_earner = (
                roster_perf.loc[roster_perf["targets_pg"].idxmax()]["player_name"]
                if not roster_perf["targets_pg"].dropna().empty
                else "N/A"
            )
            top_scorer = (
                roster_perf.loc[roster_perf["ppg_half"].idxmax()]["player_name"]
                if not roster_perf["ppg_half"].dropna().empty
                else "N/A"
            )

            c1.metric("Roster Size", f"{total_players} Players")
            c2.metric("Team Avg PPG (Half-PPR)", f"{avg_roster_ppg:.1f} pts")
            c3.metric("Top Scorer", top_scorer)
            c4.metric("Top Target Earner", top_target_earner)

            st.write("---")

            # Display Roster Table
            display_cols = [
                "roster_pos",
                "player_name",
                "nfl_team",
                "games",
                "ppg_half",
                "targets_pg",
                "rec_pg",
                "rec_yds_pg",
                "avg_target_share_pct",
                "avg_wopr",
                "carries_pg",
                "rush_yds_pg",
                "total_tds",
            ]
            available_cols = [c for c in display_cols if c in roster_perf.columns]
            roster_display = roster_perf[available_cols].rename(
                columns={
                    "roster_pos": "Pos",
                    "player_name": "Player",
                    "nfl_team": "Team",
                    "games": "Games",
                    "ppg_half": "Half-PPR PPG",
                    "targets_pg": "Targets/G",
                    "rec_pg": "Rec/G",
                    "rec_yds_pg": "Rec Yds/G",
                    "avg_target_share_pct": "Tgt Share %",
                    "avg_wopr": "WOPR",
                    "carries_pg": "Carries/G",
                    "rush_yds_pg": "Rush Yds/G",
                    "total_tds": "TDs",
                }
            )
            st.dataframe(
                roster_display,
                use_container_width=True,
                hide_index=True,
            )

    # =============================================================
    # TAB 2: FREE AGENTS EXPLORER
    # =============================================================
    with tab_free_agents:
        st.subheader("🔍 Available Free Agents (Sorted by Performance)")

        col_search, col_sort, col_count = st.columns([3, 2, 1])
        search_query = col_search.text_input("🔎 Search by Player Name or Team", placeholder="e.g. Shakir, BUF, QB...")

        sort_col = col_sort.selectbox(
            "Sort Table By",
            options=[
                "fantasy_ppg",
                "proj_points",
                "targets_pg",
                "rec_pg",
                "rec_yds_pg",
                "air_yds_pg",
                "avg_target_share_pct",
                "avg_wopr",
                "carries_pg",
                "rush_yds_pg",
                "scrimmage_yds_pg",
                "ceiling_half",
                "floor_half",
            ],
            format_func=lambda x: {
                "fantasy_ppg": "Fantasy PPG (Actual)",
                "proj_points": "Projected Next Week Pts",
                "targets_pg": "Targets / Game",
                "rec_pg": "Receptions / Game",
                "rec_yds_pg": "Receiving Yards / Game",
                "air_yds_pg": "Air Yards / Game",
                "avg_target_share_pct": "Target Share (%)",
                "avg_wopr": "WOPR (Weighted Opportunity)",
                "carries_pg": "Carries / Game",
                "rush_yds_pg": "Rushing Yards / Game",
                "scrimmage_yds_pg": "Scrimmage Yards / Game",
                "ceiling_half": "75th Percentile Ceiling",
                "floor_half": "25th Percentile Floor",
            }.get(x, x),
            index=0,
        )

        # Apply search filter
        df_fa_view = df_free_agents_filtered.copy()
        if search_query:
            q = search_query.strip().lower()
            df_fa_view = df_fa_view[
                df_fa_view["player_name"].str.lower().str.contains(q, na=False)
                | df_fa_view["team"].str.lower().str.contains(q, na=False)
                | df_fa_view["position"].str.lower().str.contains(q, na=False)
            ]

        # Sort
        df_fa_view = df_fa_view.sort_values(by=sort_col, ascending=False)
        col_count.metric("Free Agents Available", f"{len(df_fa_view)} Players")

        # Table Display
        fa_cols = [
            "player_name",
            "position",
            "team",
            "games",
            "fantasy_ppg",
            "floor_half",
            "ceiling_half",
            "proj_points",
            "targets_pg",
            "rec_pg",
            "rec_yds_pg",
            "air_yds_pg",
            "avg_adot",
            "avg_target_share_pct",
            "avg_wopr",
            "carries_pg",
            "rush_yds_pg",
            "scrimmage_yds_pg",
            "total_tds",
        ]
        fa_display = df_fa_view[fa_cols].rename(
            columns={
                "player_name": "Player",
                "position": "Pos",
                "team": "Team",
                "games": "GP",
                "fantasy_ppg": f"PPG ({scoring_format})",
                "floor_half": "Floor (P25)",
                "ceiling_half": "Ceiling (P75)",
                "proj_points": "Proj Pts",
                "targets_pg": "Tgt/G",
                "rec_pg": "Rec/G",
                "rec_yds_pg": "Rec Yds/G",
                "air_yds_pg": "Air Yds/G",
                "avg_adot": "ADOT",
                "avg_target_share_pct": "Tgt %",
                "avg_wopr": "WOPR",
                "carries_pg": "Rush/G",
                "rush_yds_pg": "Rush Yds/G",
                "scrimmage_yds_pg": "Scrimmage Yds/G",
                "total_tds": "TDs",
            }
        )

        st.dataframe(
            fa_display,
            use_container_width=True,
            hide_index=True,
        )

        st.write("---")
        st.subheader("🎯 Free Agent Opportunity Quadrant Map (Target Share vs. Air Yards Share)")
        fig_quad = plot_target_air_yards_quadrant(df_fa_view.head(50))
        st.plotly_chart(fig_quad, use_container_width=True)

    # =============================================================
    # TAB 3: PLAYER DEEP-DIVE & DENSITY COMPARISONS
    # =============================================================
    with tab_deep_dive:
        st.subheader("📊 Multi-Player Statistical & Density Comparison")
        st.markdown("Select any combination of **Rostered Players** and **Free Agents** to compare distribution curves and efficiency profiles.")

        # Candidate choices
        all_candidates = df_all_catalog["player_name"].dropna().tolist()

        # Defaults: Pick top 2 free agents + top 1 rostered player if available
        default_fa = df_free_agents_filtered["player_name"].head(2).tolist() if not df_free_agents_filtered.empty else []
        default_roster = [roster_names[0]] if roster_names else []
        default_selection = (default_roster + default_fa)[:4]

        selected_compare_players = st.multiselect(
            "Select Players to Compare",
            options=all_candidates,
            default=[p for p in default_selection if p in all_candidates],
            help="Select 1 to 6 players for overlaid density distributions and statistical comparisons",
        )

        if not selected_compare_players:
            st.info("Please select at least 1 player above to visualize statistics and density curves.")
        else:
            # Fetch weekly logs for selected players
            selected_ids = df_all_catalog[df_all_catalog["player_name"].isin(selected_compare_players)]["player_id"].tolist()
            weekly_stats_df = get_player_weekly_stats(selected_ids, season=selected_season)

            # Metric selector for plots
            metric_options = {
                "receptions": ("Receptions per Game", "receptions"),
                "scrimmage_yards": ("Scrimmage Yards per Game", "scrimmage_yards"),
                "receiving_yards": ("Receiving Yards per Game", "rec_yds"),
                "rushing_yards": ("Rushing Yards per Game", "rush_yds"),
                "air_yards": ("Air Yards per Game", "air_yards"),
                "adot": ("ADOT (Average Depth of Target)", "adot"),
                "targets": ("Targets per Game", "targets"),
                "carries": ("Carries per Game", "carries"),
                "target_share": ("Target Share (Fraction)", "target_share"),
                "points_half": ("Fantasy Points (Half-PPR)", "points_half"),
                "points_ppr": ("Fantasy Points (PPR)", "points_ppr"),
                "points_std": ("Fantasy Points (Standard)", "points_std"),
                "total_epa": ("Total EPA per Game", "total_epa"),
            }

            col_metric, _ = st.columns([3, 2])
            chosen_metric_key = col_metric.selectbox(
                "📈 Select Metric to Analyze",
                options=list(metric_options.keys()),
                format_func=lambda k: metric_options[k][0],
                index=0,  # Receptions per game
            )
            metric_label, metric_col_name = metric_options[chosen_metric_key]

            # -------------------------------------------------------------
            # 1. SUMMARY STATS CARDS
            # -------------------------------------------------------------
            st.write("### 📌 Player Statistical Profiles & Consistency")
            stat_cols = st.columns(len(selected_compare_players))

            for idx, p_name in enumerate(selected_compare_players):
                p_stats = compute_player_distribution_stats(weekly_stats_df, p_name)
                if p_stats:
                    with stat_cols[idx]:
                        st.markdown(
                            f"""
                            <div class="metric-card">
                                <div style="font-size:1.15rem; font-weight:700; color:#38BDF8;">{p_stats['player_name']}</div>
                                <div style="color:#94A3B8; font-size:0.85rem; margin-bottom:8px;">{p_stats['pos']} - {p_stats['team']} ({p_stats['games']} GP)</div>
                                <div class="metric-val">{p_stats['ppg_half']} <span style="font-size:0.9rem; color:#94A3B8;">PPG</span></div>
                                <div style="font-size:0.85rem; color:#F1F5F9; margin-top:6px;">
                                    <b>Floor (P25):</b> {p_stats['floor_half']} pts<br>
                                    <b>Median:</b> {p_stats['median_half']} pts<br>
                                    <b>Ceiling (P75):</b> {p_stats['ceiling_half']} pts<br>
                                    <b>Rec/G:</b> {p_stats['rec_pg']} | <b>Tgt/G:</b> {p_stats['targets_pg']}<br>
                                    <b>Air Yds/G:</b> {p_stats['air_yds_pg']} | <b>ADOT:</b> {p_stats['avg_adot']}<br>
                                    <b>Tgt Share:</b> {p_stats['target_share_pct']}% | <b>WOPR:</b> {p_stats['wopr']}<br>
                                    <b>Consistency:</b> {p_stats['consistency_rating']}
                                </div>
                            </div>
                            """,
                            unsafe_allow_html=True,
                        )

            st.write("---")

            # -------------------------------------------------------------
            # 2. VISUALIZATION SUB-TABS
            # -------------------------------------------------------------
            viz_tab1, viz_tab2, viz_tab3, viz_tab4, viz_tab5 = st.tabs(
                [
                    "🌊 KDE Density Distributions",
                    "🎻 Box & Violin Plots",
                    "📈 Weekly Game Trajectory",
                    "🕸️ Multi-Metric Radar",
                    "📋 Game Logs Table",
                ]
            )

            with viz_tab1:
                st.markdown(f"#### Overlaid Density Distribution: `{metric_label}`")
                st.caption("Visualizes probability density curves with mean lines and individual game output markers.")
                fig_density = plot_density_distributions(
                    weekly_stats_df,
                    metric_col=metric_col_name,
                    metric_label=metric_label,
                    players=selected_compare_players,
                    bandwidth_adj=bandwidth_adj,
                )
                st.plotly_chart(fig_density, use_container_width=True)

            with viz_tab2:
                st.markdown(f"#### Box & Violin Distribution: `{metric_label}`")
                fig_violin = plot_box_violin_distribution(
                    weekly_stats_df,
                    metric_col=metric_col_name,
                    metric_label=metric_label,
                    players=selected_compare_players,
                )
                st.plotly_chart(fig_violin, use_container_width=True)

            with viz_tab3:
                st.markdown(f"#### Weekly Game-by-Game Trajectory: `{metric_label}`")
                fig_trend = plot_weekly_trendlines(
                    weekly_stats_df,
                    metric_col=metric_col_name,
                    metric_label=metric_label,
                    players=selected_compare_players,
                )
                st.plotly_chart(fig_trend, use_container_width=True)

            with viz_tab4:
                st.markdown("#### Multi-Dimensional Radar Comparison")
                fig_radar = plot_radar_comparison(df_all_catalog, selected_compare_players)
                st.plotly_chart(fig_radar, use_container_width=True)

            with viz_tab5:
                st.markdown("#### Game-by-Game Boxscores")
                log_cols = [
                    "player_name",
                    "season",
                    "week",
                    "opponent_team",
                    "points_half",
                    "points_ppr",
                    "receptions",
                    "targets",
                    "rec_yds",
                    "air_yards",
                    "adot",
                    "target_share",
                    "wopr",
                    "carries",
                    "rush_yds",
                    "total_tds",
                ]
                st.dataframe(
                    weekly_stats_df[log_cols].rename(
                        columns={
                            "player_name": "Player",
                            "season": "Season",
                            "week": "Week",
                            "opponent_team": "Opp",
                            "points_half": "Half-PPR",
                            "points_ppr": "PPR",
                            "receptions": "Rec",
                            "targets": "Tgt",
                            "rec_yds": "Rec Yds",
                            "air_yards": "Air Yds",
                            "adot": "ADOT",
                            "target_share": "Tgt Share",
                            "wopr": "WOPR",
                            "carries": "Rush Att",
                            "rush_yds": "Rush Yds",
                            "total_tds": "TDs",
                        }
                    ),
                    use_container_width=True,
                    hide_index=True,
                )

    # =============================================================
    # TAB 4: ROSTER VS. WAIVER SWAP ANALYZER
    # =============================================================
    with tab_swap:
        st.subheader("🔄 Waiver Wire Add / Drop Swap Analyzer")
        st.markdown("Select a player from your current roster to **Drop**, and a free agent to **Add** to calculate expected point and volume deltas.")

        col_drop, col_add = st.columns(2)

        drop_player = col_drop.selectbox(
            "🔻 Player to Drop (From My Roster)",
            options=roster_names if roster_names else all_candidates,
            index=0,
        )

        fa_names = df_free_agents_filtered["player_name"].dropna().tolist()
        add_player = col_add.selectbox(
            "🟢 Free Agent to Add (Waiver Wire)",
            options=fa_names if fa_names else all_candidates,
            index=0,
        )

        if drop_player and add_player:
            # Fetch stats for both
            pair_ids = df_all_catalog[df_all_catalog["player_name"].isin([drop_player, add_player])]["player_id"].tolist()
            pair_weekly_df = get_player_weekly_stats(pair_ids, season=selected_season)

            stats_drop = compute_player_distribution_stats(pair_weekly_df, drop_player)
            stats_add = compute_player_distribution_stats(pair_weekly_df, add_player)
            deltas = compute_swap_deltas(stats_drop, stats_add)

            if deltas:
                st.write("---")
                st.markdown(f"### 💡 Decision Verdict: `{deltas['verdict']}`")

                # Delta metric columns
                d1, d2, d3, d4, d5 = st.columns(5)
                d1.metric(
                    "Half-PPR PPG Delta",
                    f"{stats_add['ppg_half']} pts",
                    delta=f"{deltas['ppg_delta']:+.2f} pts/g",
                )
                d2.metric(
                    "25th Percentile Floor Delta",
                    f"{stats_add['floor_half']} pts",
                    delta=f"{deltas['floor_delta']:+.2f} pts",
                )
                d3.metric(
                    "75th Percentile Ceiling Delta",
                    f"{stats_add['ceiling_half']} pts",
                    delta=f"{deltas['ceiling_delta']:+.2f} pts",
                )
                d4.metric(
                    "Target Share Delta",
                    f"{stats_add['target_share_pct']}%",
                    delta=f"{deltas['target_share_delta']:+.1f}%",
                )
                d5.metric(
                    "Air Yards / G Delta",
                    f"{stats_add['air_yds_pg']} yds",
                    delta=f"{deltas['air_yards_delta']:+.1f} yds/g",
                )

                st.write("---")
                st.markdown("#### 🌊 Head-to-Head Density Distribution Overlay (Fantasy Points)")
                fig_swap_density = plot_density_distributions(
                    pair_weekly_df,
                    metric_col="points_half",
                    metric_label="Fantasy Points (Half-PPR)",
                    players=[drop_player, add_player],
                    bandwidth_adj=bandwidth_adj,
                )
                st.plotly_chart(fig_swap_density, use_container_width=True)


if __name__ == "__main__":
    main()
