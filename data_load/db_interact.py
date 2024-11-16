# %%
from sqlalchemy import create_engine, inspect
import os
from dotenv import load_dotenv

# Load database credentials from environment variables
load_dotenv()
db_host = os.getenv("DB_HOST")
db_name = os.getenv("DB_NAME")
db_user = os.getenv("DB_USER")
db_password = os.getenv("DB_PASSWORD")
db_port = os.getenv("DB_PORT", 5432)

# Create the SQLAlchemy engine
engine = create_engine(f"postgresql+psycopg2://{db_user}:{db_password}@{db_host}:{db_port}/{db_name}")

# List all schemas
def list_schemas():
    inspector = inspect(engine)
    return inspector.get_schema_names()

# List tables in a specific schema
def list_tables(schema):
    inspector = inspect(engine)
    return inspector.get_table_names(schema=schema)

# Check the available schemas
schemas = list_schemas()
print("Schemas in the database:")
for schema in schemas:
    print(f"- {schema}")

# List tables for each schema
for schema in schemas:
    print(f"\nTables in schema '{schema}':")
    tables = list_tables(schema)
    for table in tables:
        print(f"  - {table}")

# Close the engine connection
engine.dispose()


# %%
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
import os
import pandas as pd
from sqlalchemy import create_engine

#define table to query
table_name_to_query = "premier_league_data_current_lambda"
table_name_to_query = "premier_league_data_historical"
table_name_to_query = "premier_league_fixtures_current_lambda"
table_name_to_query = "users"
# table_name_to_query = "bets"
table_name_to_query = "premier_league_fixtures_historical_enriched"
# table_name_to_query = "team_name_translate"


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
        print(df)
except Exception as e:
    print(f"Error querying the database: {e}")
finally:
    # Dispose of the engine connection
    engine.dispose()


# %%
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Table deletion

from sqlalchemy import create_engine, text
import os

# Define the table name you want to delete
table_name = "premier_league_fixtures_current_enriched"


import psycopg2
import os

# Set up your database connection details
db_host = os.getenv("DB_HOST")
db_name = os.getenv("DB_NAME")
db_user = os.getenv("DB_USER")
db_password = os.getenv("DB_PASSWORD")
db_port = os.getenv("DB_PORT", 5432)

# Connect to the PostgreSQL database
conn = psycopg2.connect(
    host=db_host,
    database=db_name,
    user=db_user,
    password=db_password,
    port=db_port
)

# Delete the table
try:
    with conn.cursor() as cursor:
        cursor.execute(f"DROP TABLE IF EXISTS {table_name}")
        print(f"Table '{table_name}' has been deleted.")
        conn.commit()
except Exception as e:
    print(f"Error deleting table: {e}")
finally:
    conn.close()
# %%
