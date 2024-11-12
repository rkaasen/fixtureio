

import os
import pandas as pd
from sqlalchemy import create_engine


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

# Lambda handler

def lambda_handler(event, context):
    print("Lambda function started")
    
    #define table to query
    table_name_to_query = "users"

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
            df_users = pd.read_sql(query, connection)
            print("Data fetched successfully:")
    except Exception as e:
        print(f"Error querying the database: {e}")
    finally:
        # Dispose of the engine connection
        engine.dispose()
    

    df_users['bets_available'] = 10
    df_users['bets_week_starting'] = 10
    
    df_users['TimeStamp_Bets_Updated'] = pd.Timestamp.now()

    table_name = "users"
    upload_df_to_postgres(df_users, table_name)





    print("Lambda function completed without errors.")
    return {"statusCode": 200, "body": "Data upload completed successfully"}
