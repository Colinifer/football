# nflpro/query.py
"""Utility functions for querying data from a PostgreSQL database.

It includes functions for:
- Establishing a database connection using psycopg2.
- Executing SQL queries.
- Returning query results as Pandas DataFrames.
- Writing Pandas DataFrames to PostgreSQL tables.

These functions abstract away the complexities of database interaction,
allowing for easier data retrieval and manipulation within the nflpro package.

Functions:
    create_connection:
        Creates a connection to a PostgreSQL database.

    execute_query:
        Executes a SQL query and returns the result as a list of tuples.

    query_to_dataframe:
        Executes a SQL query and returns the result as a Pandas DataFrame.

    write_dataframe_to_postgres:
        Writes a Pandas DataFrame to a PostgreSQL database table.
"""

import logging
import os

import pandas as pd
import psycopg2
from dotenv import load_dotenv


def create_connection(
    config: dict[str, str] | None,
) -> psycopg2.extensions.connection | None:
    """Create a connection to the PostgreSQL database.

    Args:
        config (Dict[str, str]): A dictionary containing \
            the database configuration.
            Must include keys:
                - "host",
                - "database",
                - "username",
                - "password",
                - "port"

    Returns:
        A psycopg2 connection object if the connection is successful, \
            otherwise None.

    """
    load_dotenv()  # Load environment variables from .env file

    # Load configuration from environment variables as a base
    env_config = {
        "host": os.environ.get("DB_HOST"),
        "database": os.environ.get("DB_NAME"),
        "username": os.environ.get("DB_USER"),
        "password": os.environ.get("DB_PASS"),
        "port": os.environ.get("DB_PORT"),
    }

    # Override with values from the config dictionary if provided
    if config:
        env_config.update(config)

    logging.info(env_config)

    # Check for missing values
    if not all(env_config.values()):
        logging.error(
            "Not all database configuration values are set \
                (either in config or environment).",
        )
        return None  # Or raise an exception

    try:
        conn = psycopg2.connect(
            host=env_config["host"],
            database=env_config["database"],
            user=env_config["username"],
            password=env_config["password"],
            port=env_config["port"],
        )

    except psycopg2.Error as e:
        msg = f"Error connecting to database: {e}"
        logging.exception(msg)
        return None
    else:
        return conn


def execute_query(
    conn: psycopg2.extensions.connection,
    query: str,
    params: tuple | None = None,
) -> list[tuple[any, ...]] | None:
    """Execute a SQL query against the database.

    Args:
        conn (psycopg2.extensions.connection): A psycopg2 connection object.
        query (str): The SQL query to execute.
        params (tuple, optional):  The parameters to pass to \
            the query. Defaults to None

    Returns:
        A list of tuples containing the result rows, \
            or None if an error occurs.

    """
    cur = None  # Initialize cur outside the try block
    try:
        cur = conn.cursor()
        cur.execute(query, params)
        rows = cur.fetchall()
    except psycopg2.Error as e:
        msg = f"Error executing query: {e}"
        logging.exception(msg)
        return None
    else:
        return rows
    finally:
        if cur:  # Check if cur is defined before attempting to close
            cur.close()


def query_to_dataframe(
    conn: psycopg2.extensions.connection,
    query: str,
    params: tuple | None = None,
    print_query: bool | None = None,
) -> pd.DataFrame:
    """Execute a SQL query and returns the result as a Pandas DataFrame.

    Args:
        conn (psycopg2.extensions.connection): A psycopg2 connection object.
        query (str): The SQL query to execute.
        params (tuple, optional):  The parameters to pass to \
            the query. Defaults to None
        print_query (bool | none, optional): Prints the query if selected.
            Defaults to None

    Returns:
        A Pandas DataFrame of tuples containing the result \
            rows, or None if an error occurs.

    """
    rows = execute_query(conn, query, params)
    if rows is None:
        return pd.DataFrame()

    try:
        # Create a new cursor to get column names, without re-executing query
        cur = conn.cursor()
        cur.execute(query, params)
        if print_query:
            logging.info(query)
        colnames = [desc[0] for desc in cur.description]  # Get column names
    except psycopg2.Error as e:
        msg = f"Error getting column names: {e}"
        logging.exception(msg)
        return pd.DataFrame()
    finally:
        if cur:
            cur.close()

    return pd.DataFrame(rows, columns=colnames)


def write_dataframe_to_postgres(
    df: pd.DataFrame,
    config: dict,
    table_name: str,
) -> None:
    """Write a DataFrame to PostgreSQL database table, overwriting if needed.

    Args:
        df (pd.DataFrame): The DataFrame to write.
        config (dict): Database connection configuration.
        table_name (str): The name of the table to write to.

    Returns:
        None

    Example:
    >>> config = {
    ...     "host": "localhost",
    ...     "database": "mydatabase",
    ...     "username": "myuser",
    ...     "password": "mypassword",
    ...     "port": "5432",
    ... }
    >>> write_dataframe_to_postgres(plays_df, config, "plays_with_schedule")

    """
    try:
        conn = create_connection(config=config)
        if conn is not None:
            {
                {
                    df.to_sql(
                        name=table_name,  # Table name in PostgreSQL
                        con=conn,  # Database connection
                        # Replace the table if it already exists
                        if_exists="replace",
                        # Do not write DataFrame index as a column
                        index=False,
                    ),
                },
            }
            msg = (
                "DataFrame written to PostgreSQL table"
                f"'{table_name}' successfully!",
            )
            logging.info(msg)
        else:
            logging.warning("Failed to create database connection.")
    except Exception as e:
        msg = f"Error writing to PostgreSQL: {e}"
        logging.exception(msg)
    finally:
        if conn:
            conn.close()
