cd# Project Overview

This repository serves as a comprehensive football analytics project, primarily leveraging R for in-depth data processing, analysis, and visualization of both NFL and college football play-by-play data. It builds upon established R packages such as `nflfastR` and `cfbfastR`, augmented by a custom `initR` package for streamlined operations. Additionally, the project incorporates a Python-based Streamlit web application, designed for interactive data exploration and fantasy football management, which interfaces with a PostgreSQL database.

# Building and Running

## R Environment Setup

The core R environment is initialized and managed through the `init.R` file. This script is responsible for:
*   Loading a wide array of R packages essential for data manipulation, statistical analysis, and advanced plotting.
*   Sourcing various R scripts that define plot themes, utility functions, and data modification routines.
*   Updating the `nflfastR_pbp` and `cfbfastR_pbp` databases, which are central to the project's data foundation.
*   Establishing connections to ESPN fantasy football leagues via the `ffscrapr` package.

To set up and run the R components:
1.  Open the `football.Rproj` file in RStudio.
2.  Execute the `init.R` script. Ensure that any necessary R packages, especially those from GitHub (as indicated by commented `devtools::install_github` lines in `init.R`), are installed.

## Python Streamlit Application

An interactive web application, built with Streamlit, resides in the `streamlit/` directory. This application utilizes local modules like `fantasypros`, `yaml` for configuration, and `psycopg2` for connecting to the PostgreSQL database.

To run the Streamlit application:
1.  Navigate to the `streamlit/` directory in your terminal.
2.  Execute the command: `streamlit run main.py`

## Database Interaction

The project relies on a PostgreSQL database for data storage. Both the R and Python components interact with this database.
*   R scripts use `RPostgres` and `initR::fx.db_con` for database connectivity.
*   The Python Streamlit application uses `psycopg2` and `dbconfig.py` for its database operations.
*   Database connection parameters are likely configured in `database.ini` and `streamlit/db_config.yaml`.
*   SQL queries used for data retrieval and manipulation are located in the `sql/` directory.

# Development Conventions

## R Coding Style

R scripts within this project adhere to `tidyverse` principles, emphasizing data manipulation with `dplyr` and `purrr`, and leveraging the `%>%` pipe operator for readable code.

## Python Coding Style

Python code in this project follows specific conventions enforced by `ruff` for linting and formatting, as configured in `pyproject.toml`. Key aspects include:
*   **Python Version:** Targeting Python 3.13.
*   **Line Length:** A maximum line length of 79 characters.
*   **Docstring Convention:** Google-style docstrings are enforced (`pydocstyle`).
*   **Pylint Limits:** Specific limits are set for `pylint` regarding the maximum number of statements, returns, arguments, and nested blocks to maintain code complexity.

## General Conventions

*   **Centralized Data:** A PostgreSQL database serves as the central repository for all analytical data, ensuring consistency across R and Python components.
*   **Modular Structure:** The project is organized into logical directories (e.g., `R/`, `python/`, `fantasy_football/`, `plots/`, `sql/`, `streamlit/`) to promote modularity and maintainability.