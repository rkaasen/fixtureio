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

    # Create the SQLAlchemy engine
    engine = create_engine(f"postgresql+psycopg2://{db_user}:{db_password}@{db_host}:{db_port}/{db_name}")
    df.to_sql(table_name, engine, schema="public", if_exists="replace", index=False)
    print(f"Data uploaded to PostgreSQL table '{table_name}' successfully.")
    engine.dispose()