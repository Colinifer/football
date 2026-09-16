"""Database utility functions for connection and query execution."""

import logging
import os

import pandas as pd
from dotenv import find_dotenv, load_dotenv
from sqlalchemy import create_engine, text

load_dotenv(find_dotenv())
logger = logging.getLogger(__name__)


def build_connection(
    db_user: str = os.getenv("DB_USER", ""),
    db_pass: str = os.getenv("DB_PASS", ""),
    db_host: str = os.getenv("DB_HOST_LOCAL", ""),
    db_port: str = os.getenv("DB_PORT_LOCAL", ""),
    db_database: str = os.getenv("DB_NAME", "football"),
) -> str:
    """Build and return the PostgreSQL database connection URL."""
    logger.debug("Establishing connection to PostgreSQL")
    db_url = f"postgresql://{db_user}:{db_pass}@{db_host}:{db_port}/{db_database}"
    logger.debug("Database URL built successfully")
    return db_url


def execute_query(
    db_connection_string: str,
    query: str,
) -> pd.DataFrame:
    """Execute a SQL query using SQLAlchemy.

    Return results as a DataFrame.
    """
    engine = create_engine(db_connection_string)

    with engine.connect() as conn:
        result = conn.execute(text(query))
        return pd.DataFrame(
            result.fetchall(),
            columns=list(result.keys()),
        )
