import yaml
import psycopg2

def convert_to_snake_case(s):
    """
    Converts a string to snake case and lower case.

    Args:
        s (str): The input string.

    Returns:
        str: The input string converted to snake case and lower case.
    """

    # Replace all non-alphanumeric characters with spaces
    s = ''.join(e if e.isalnum() else ' ' for e in s)

    # Split the string into words
    words = s.split()

    # Convert each word to lower case and join with underscores
    snake_case = '_'.join(word.lower() for word in words)

    return snake_case

def load_config(file_path):
    with open(file_path, "r") as file:
        config = yaml.safe_load(file)
        return config

def load_db_config(config):
    return config["database"]
    
def load_ff_config(config):
    return config["ff_conn"]


def create_connection(config):
    try:
        conn = psycopg2.connect(
            host=config["host"],
            database=config["database"],
            user=config["username"],
            password=config["password"],
            port=config["port"],
        )
        return conn
    except psycopg2.Error as e:
        print(f"Error: {e}")


def execute_query(conn, query):
    try:
        cur = conn.cursor()
        cur.execute(query)
        rows = cur.fetchall()
        return rows
    except psycopg2.Error as e:
        print(f"Error: {e}")
    finally:
        cur.close()
