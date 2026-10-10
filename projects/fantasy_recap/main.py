import os
import sys

import pandas as pd
from dotenv import load_dotenv
from sqlalchemy import create_engine, text

# Load .env from the parent football directory
dotenv_path = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", ".env"))
load_dotenv(dotenv_path=dotenv_path)

def get_db_engine():
    host = os.getenv("DB_HOST_LOCAL", "localhost")
    port = os.getenv("DB_PORT_LOCAL", "5432")
    database = os.getenv("DB_NAME", "football")
    user = os.getenv("DB_USER", "postgres")
    password = os.getenv("DB_PASS", "")

    print(f"Connecting to database {database} at {host}:{port} as {user}...")
    connection_string = f"postgresql://{user}:{password}@{host}:{port}/{database}"
    engine = create_engine(connection_string)
    return engine

def main():
    if len(sys.argv) < 2:
        print("Usage: uv run main.py <sql_filename>")
        print("Example: uv run main.py sample.sql")
        sys.exit(1)

    sql_filename = sys.argv[1]
    
    # Check if path is provided directly or inside sql/ directory
    if os.path.exists(sql_filename):
        sql_path = sql_filename
    else:
        sql_path = os.path.join(os.path.dirname(__file__), "sql", sql_filename)
        if not os.path.exists(sql_path) and not sql_path.endswith(".sql"):
            sql_path += ".sql"

    if not os.path.exists(sql_path):
        print(f"Error: SQL file not found at '{sql_filename}' or '{sql_path}'")
        sys.exit(1)

    print(f"Reading SQL from {sql_path}...")
    with open(sql_path) as f:
        query = f.read()

    try:
        engine = get_db_engine()
        with engine.connect() as connection:
            df = pd.read_sql(text(query), connection)
        print("Query executed successfully. Results:")
        print(df)
        engine.dispose()
    except Exception as e:
        print(f"Error connecting to database or executing query: {e}")

if __name__ == "__main__":
    main()
