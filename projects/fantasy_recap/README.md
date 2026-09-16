# Fantasy Recap CLI

A Python CLI application built with `uv` that connects to your PostgreSQL football database using parent `.env` configuration and executes SQL queries from the `sql/` directory.

## Setup

1. Ensure the parent `.env` file is configured in the root `football/` directory.
2. Install dependencies:
   ```bash
   uv sync
   ```

## Usage

Run any SQL script from the `sql/` folder by passing its filename as an argument:

```bash
uv run main.py <filename.sql>
```

## Example: Reproducing Free Agent Projections

To retrieve top free agent projections for `week = 0` using half-PPR scoring:

```bash
uv run main.py free_agent_projections.sql
```
