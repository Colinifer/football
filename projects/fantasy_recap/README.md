# Fantasy Recap CLI

A Python CLI application built with `uv` that connects to your PostgreSQL football database using parent `.env` configuration and executes SQL queries from the `sql/` directory.

## Setup

1. Ensure the parent `.env` file is configured in the root `football/` directory.
2. Install dependencies:
   ```bash
   uv sync
   ```

## Interactive Streamlit Dashboard

Launch the interactive Free Agent, Roster Analytics, and Density Distribution Dashboard:

```bash
uv run streamlit run app.py
```

### Dashboard Features
- **My Roster:** View all players on your fantasy team, their positional breakdown, weekly averages, and floor/ceiling.
- **Free Agent Explorer:** Filter and sort available free agents by key metrics (Target Share, Air Yards Share, WOPR, ADOT, Fantasy PPG, Projections) with an interactive Target Share vs. Air Yards opportunity map.
- **Player Deep-Dive & Density Comparison:** Compare multiple players with overlaid **KDE density distribution curves**, **Box/Violin plots**, **Game Trajectory line charts**, and **Multi-Dimensional Radar charts** across receptions, total yards, air yards, ADOT, targets, and fantasy points.
- **Waiver Swap Analyzer:** Select a player from your roster to drop and a free agent to add to calculate expected point/volume deltas and decision recommendations.

---

## Usage (CLI SQL Execution)

## Example: Reproducing Free Agent Projections

To retrieve top free agent projections for `week = 0` using half-PPR scoring:

```bash
uv run main.py free_agent_projections.sql
```
