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


def f_select_columns_historical_data(df):
    # Selecting and renaming columns
    df_out = pd.concat([
        df.iloc[:, [0]].rename(columns={df.columns[0]: "Div"}),  # Renaming the first column to "Div"
        df[[
            "Date", "Time", "HomeTeam", "AwayTeam", 
            "FTHG", "FTAG", "FTR",  # Goals and result
            "Referee",  # Referee
            "HS", "AS", "HST", "AST",  # Shots / on target
            "Season_ending_year"  # Season indicator
        ]].copy()
    ], axis=1)
    
    # Convert selected columns to numeric
    df_out = df_out.assign(
        FTHG=pd.to_numeric(df_out["FTHG"], errors='coerce'),
        FTAG=pd.to_numeric(df_out["FTAG"], errors='coerce'),
        HS=pd.to_numeric(df_out["HS"], errors='coerce'),
        AS=pd.to_numeric(df_out["AS"], errors='coerce'),
        HST=pd.to_numeric(df_out["HST"], errors='coerce'),
        AST=pd.to_numeric(df_out["AST"], errors='coerce'),
        Season_ending_year=pd.to_numeric(df_out["Season_ending_year"], errors='coerce')
    )
    
    return df_out



# Lambda handler
def lambda_handler(event, context):
    print("Lambda function started")
    
    
    
    
    
    table_name_to_query = "bets"
    df_bets_raw = get_data_from_db(table_name_to_query)

    table_name_to_query = "premier_league_data_current_lambda"
    historical_data_this_season = get_data_from_db(table_name_to_query)
    
    # Count the number of rows in the DataFrame
    num_rows = historical_data_this_season.shape[0]
    
    # if number of rows is greater than 20
    if num_rows > 20:
        df_historical_use = f_select_columns_historical_data(historical_data_this_season)
        
        # create an id column as HomeTeam + AwayTeam
        df_historical_use['id'] = df_historical_use['HomeTeam'] + "-" +df_historical_use['AwayTeam']
        
    else:
        table_name_to_query = "premier_league_data_historical"
        historical_data_additional = get_data_from_db(table_name_to_query)
        
        # take only 30 most recent rows based on Date
        historical_data_additional['Date'] = pd.to_datetime(historical_data_additional['Date'], format='%d/%m/%Y')
        historical_data_additional = historical_data_additional.sort_values('Date', ascending=False).head(30)
        historical_data_additional = f_select_columns_historical_data(historical_data_additional)
        
        # calculate date for the 'current' data set
        historical_data_this_season['Date'] = pd.to_datetime(historical_data_this_season['Date'], format='%d/%m/%Y')

        #combine datasets
        df_historical_use = pd.concat([historical_data_this_season, historical_data_additional]).sort_values('Date', ascending=False)
        
        # create an id column as HomeTeam + AwayTeam
        df_historical_use['id'] = df_historical_use['HomeTeam'] + "-" +df_historical_use['AwayTeam']
        
        # take only latests row for each id based on date
        df_historical_use = df_historical_use.sort_values('Date', ascending=False).drop_duplicates(subset='id')

    df_bets = df_bets_raw.copy()

    # filter away cancelled bets (cancelled = True)
    df_bets = df_bets[df_bets['cancelled'] == False]

    # filter to bets where bet_concluded is NaN
    df_bets = df_bets[df_bets['bet_concluded'].isna()] 


    # Split column match_id on '-' and keep both parts as new columns
    df_bets[['HomeTeam', 'AwayTeam', 'season_ending']] = df_bets['match_id'].str.split('-', expand=True)
    
    # Keep only record where season_ending the max value of the whole df (i.e. the current season)
    df_bets = df_bets[df_bets['season_ending'] == df_bets['season_ending'].max()]
    
    # make match_id based on only HomeTeam and AwayTeam
    df_bets['match_id_short'] = df_bets['HomeTeam'] + "-" + df_bets['AwayTeam']
    
    # join with df_historical_use
    df_bets = df_bets.merge(df_historical_use, how='left', left_on='match_id_short', right_on='id')
    
    #keep only records with a match
    df_bets = df_bets.dropna(subset=['Referee'])
    
    # make a column with the name of the winning team based on the FTR column. H = HomeTeam, A = AwayTeam, D = "DRAW"
    df_bets['winning_team'] = df_bets.apply(lambda row: row['HomeTeam_x'] if row['FTR'] == 'H' else row['AwayTeam_x'] if row['FTR'] == 'A' else 'DRAW',axis=1)
    
    # set column 'bet_concluded' to the odds if the winning_team is the same as the bet. else set -1
    df_bets['bet_concluded'] = df_bets.apply(lambda row: row['odds'] if row['winning_team'] == row['bet'] else -1, axis=1)
    
    # select columns ['bet', 'odds', 'placed', 'bet_concluded', 'bet_id', 'match_id','user_id', 'cancelled']:
    df_bets_updated = df_bets[['bet_concluded', 'bet_id']]
    
    #remove column bet_concluded from df_bets_raw
    df_bets_raw = df_bets_raw.drop(columns=['bet_concluded']) 
    
    # now merge the df_bets_updated with df_bets_raw and update only the column_bet_concluded:
    df_bets_to_push = df_bets_raw.merge(df_bets_updated, how='left', on='bet_id')
    
    table_name = "bets"
    upload_df_to_postgres(df_bets_to_push, table_name)
    

    
    
    print("Lambda function completed without errors.")
    return {"statusCode": 200, "body": "Data upload completed successfully"}
