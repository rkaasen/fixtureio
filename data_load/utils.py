import pandas as pd
import requests
from io import StringIO
from sqlalchemy import create_engine
import os

# Fetch data from URL and return as a DataFrame
def fetch_data_from_url(url):
    response = requests.get(url)
    response.raise_for_status()
    return pd.read_csv(StringIO(response.text))

# Upload DataFrame to PostgreSQL
def upload_df_to_postgres(df, table_name):
    db_host = os.getenv("DB_HOST")
    db_name = os.getenv("DB_NAME")
    db_user = os.getenv("DB_USER")
    db_password = os.getenv("DB_PASSWORD")
    db_port = os.getenv("DB_PORT", 5432)

    print(db_host)

    # Create the SQLAlchemy engine
    engine = create_engine(f"postgresql+psycopg2://{db_user}:{db_password}@{db_host}:{db_port}/{db_name}")
    df.to_sql(table_name, engine, schema="public", if_exists="replace", index=False)
    print(f"Data uploaded to PostgreSQL table '{table_name}' successfully.")
    engine.dispose()
    
def get_data_from_db(table_name_to_query):
    # Set up environment variables or replace these with your database details
    db_host = os.getenv("DB_HOST", "your_db_host")
    db_name = os.getenv("DB_NAME", "your_db_name")
    db_user = os.getenv("DB_USER", "your_db_user")
    db_password = os.getenv("DB_PASSWORD", "your_db_password")
    db_port = os.getenv("DB_PORT", "5432")  # Default PostgreSQL port is 5432

    # Create the SQLAlchemy engine to connect to the database
    engine = create_engine(f"postgresql+psycopg2://{db_user}:{db_password}@{db_host}:{db_port}/{db_name}")

    # Define the query (adjust the table name to your actual table)
    query = f"SELECT * FROM {table_name_to_query} "

    # Fetch data into a pandas DataFrame
    try:
        with engine.connect() as connection:
            df = pd.read_sql(query, connection)
            print("Data fetched successfully:")
    except Exception as e:
        print(f"Error querying the database: {e}")
    finally:
        # Dispose of the engine connection
        engine.dispose()
    return df


def append_df_to_postgres(df, table_name):
    """
    Append a pandas DataFrame to an existing PostgreSQL table in the 'public' schema.
    Creates the table automatically if it doesn't exist.
    """

    db_host = os.getenv("DB_HOST")
    db_name = os.getenv("DB_NAME")
    db_user = os.getenv("DB_USER")
    db_password = os.getenv("DB_PASSWORD")
    db_port = os.getenv("DB_PORT", 5432)

    # sanity check
    if not all([db_host, db_name, db_user, db_password]):
        raise ValueError("Missing one or more required DB environment variables.")

    # Create the SQLAlchemy engine
    engine = create_engine(
        f"postgresql+psycopg2://{db_user}:{db_password}@{db_host}:{db_port}/{db_name}"
    )

    # Append instead of replace
    df.to_sql(
        table_name,
        engine,
        schema="public",
        if_exists="append",
        index=False,
        method="multi"   # uses batch inserts, much faster
    )