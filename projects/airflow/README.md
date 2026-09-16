GitHub Copilot

Here is a brief README tailored for setting up and running the script using **`uv`**, including an example for reproducing the team tiers plot.

---

# NFL Team Tiers Visualization

This tool queries PostgreSQL play-by-play data, calculates offensive and defensive EPA per play for NFL teams, and generates custom dark-themed scatter plots featuring team logos.

## Prerequisites

- [uv](https://github.com/astral-sh/uv) installed on your system.
- A running PostgreSQL database containing `nflfastR_pbp` data.

---

## Example: Reproducing Results with `uv`

### 1. Configure Environment Variables
Create a `.env` file in your workspace root with your PostgreSQL connection details:
```env
DB_USER=your_username
DB_PASSWORD=your_password
DB_HOST=localhost
DB_PORT=5432
DB_NAME=football_db
```

### 2. Run Using `uv run`
You can run the script directly using `uv` without manually creating or activating a virtual environment. `uv` will automatically resolve and install dependencies defined in your project's `pyproject.toml`:

```bash
uv run python projects/airflow/plot_team_tiers.py --season 2023
```

### 3. Output
The generated scatter plot will be saved to:
```text
output/plots/team_tiers/team_tiers_2023.png
```