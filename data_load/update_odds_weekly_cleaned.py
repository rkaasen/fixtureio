# %%

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CONFIGS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


game_stats_config = {
    "m1": {"metric": "Points_lx", "n_matches": 10, "h2h_teams": "SKIP", "home_away": False, "weight": 1.0},
    "m2": {"metric": "Goals_Scored_lx", "n_matches": 10, "h2h_teams": "SKIP", "home_away": False, "weight": 1.0},
    "m3": {"metric": "Goals_Conceded_lx", "n_matches": 10, "h2h_teams": "SKIP", "home_away": False, "weight": 1.0},
    "m4": {"metric": "Shots_OT_For_lx", "n_matches": 10, "h2h_teams": "SKIP", "home_away": False, "weight": 1.0},
    "m5": {"metric": "Shots_OT_Conceded_lx", "n_matches": 10, "h2h_teams": "SKIP", "home_away": False, "weight": 1.0},
    
    #h2h:
    "m9": {"metric": "Points_lx", "n_matches": 5, "h2h_teams": ["home_team", "away_team"], "home_away": False, "weight": 0.5},
    "m10": {"metric": "Goals_Scored_lx", "n_matches": 5, "h2h_teams": ["home_team", "away_team"], "home_away": False, "weight": 0.5},
    "m11": {"metric": "Goals_Conceded_lx", "n_matches": 5, "h2h_teams": ["home_team", "away_team"], "home_away": False, "weight": 0.5},
    
    #home_away:
    "m6": {"metric": "Points_lx", "n_matches": 5, "h2h_teams": "SKIP", "home_away": True, "weight": 0.5},
    "m7": {"metric": "Goals_Scored_lx", "n_matches": 5, "h2h_teams": "SKIP", "home_away": True, "weight": 0.5},
    "m8": {"metric": "Goals_Conceded_lx", "n_matches": 5, "h2h_teams": "SKIP", "home_away": True, "weight": 0.5},
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FUNCTIONS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


import pandas as pd
from sqlalchemy import create_engine
from datetime import datetime
from datetime import timedelta
import os


# Upload DataFrame to PostgreSQL
def upload_df_to_postgres(df, table_name, if_exists_rule= "replace"):
    db_host = os.getenv("DB_HOST")
    db_name = os.getenv("DB_NAME")
    db_user = os.getenv("DB_USER")
    db_password = os.getenv("DB_PASSWORD")
    db_port = os.getenv("DB_PORT", 5432)

    # Create the SQLAlchemy engine
    engine = create_engine(f"postgresql+psycopg2://{db_user}:{db_password}@{db_host}:{db_port}/{db_name}")
    df.to_sql(table_name, engine, schema="public", if_exists=if_exists_rule, index=False)
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



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
def f_team_cols_rename(df, home_metrics=True):
    if home_metrics:
        df = df.rename(columns={
            "FTHG": "FT_Scored",
            "FTAG": "FT_Conceded",
            "HS": "Shots_For",
            "AS": "Shots_Conceded",
            "HST": "Shots_OT_For",
            "AST": "Shots_OT_Conceded"
        })
    else:
        df = df.rename(columns={
            "FTAG": "FT_Scored",
            "FTHG": "FT_Conceded",
            "AS": "Shots_For",
            "HS": "Shots_Conceded",
            "AST": "Shots_OT_For",
            "HST": "Shots_OT_Conceded"
        })

    df["Date"] = pd.to_datetime(df["Date"], format="%d/%m/%Y")
    df["Result"] = df.apply(
        lambda row: "W" if row["FT_Scored"] > row["FT_Conceded"] else ("L" if row["FT_Scored"] < row["FT_Conceded"] else "D"),
        axis=1
    )
    df["Points"] = df["Result"].apply(lambda x: 3 if x == "W" else (1 if x == "D" else 0))

    return df

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
def f_part_calculate_scores(df, n_matches=5):
    # Sort by 'Team' and 'Date' in descending order to get the latest matches
    df = df.sort_values(by=['Team', 'Date'], ascending=[True, False])
    
    # Select the latest `n_matches` for each team
    df = df.groupby('Team').head(n_matches)
    
    # Calculate metrics for each team and add a column to indicate the number of matches actually used
    df_out = df.groupby('Team', as_index=False).agg({
        'FT_Scored': 'sum',
        'FT_Conceded': 'sum',
        'Points': 'sum',
        'Shots_For': 'sum',
        'Shots_Conceded': 'sum',
        'Shots_OT_For': 'sum',
        'Shots_OT_Conceded': 'sum',
    })
    
    # Count the actual number of matches used for each team
    df_out['n_matches_in_sample'] = df.groupby('Team').size().reset_index(name='count')['count']
    
    # Rename columns to match the expected output
    df_out = df_out.rename(columns={
        'FT_Scored': 'Goals_Scored_lx',
        'FT_Conceded': 'Goals_Conceded_lx',
        'Points': 'Points_lx',
        'Shots_For': 'Shots_For_lx',
        'Shots_Conceded': 'Shots_Conceded_lx',
        'Shots_OT_For': 'Shots_OT_For_lx',
        'Shots_OT_Conceded': 'Shots_OT_Conceded_lx',
    })
    
    return df_out


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
def f_part_calculate_ranks(df):
    df_out = df.assign(
        Rank_Goals_Scored_lx=df['Goals_Scored_lx'].rank(method='min', ascending=False),
        Rank_Goals_Conceded_lx=df['Goals_Conceded_lx'].rank(method='min', ascending=True),
        Rank_Points_lx=df['Points_lx'].rank(method='min', ascending=False),
        Rank_Shots_For_lx=df['Shots_For_lx'].rank(method='min', ascending=False),
        Rank_Shots_Conceded_lx=df['Shots_Conceded_lx'].rank(method='min', ascending=True),
        Rank_Shots_OT_For_lx=df['Shots_OT_For_lx'].rank(method='min', ascending=False),
        Rank_Shots_OT_Conceded_lx=df['Shots_OT_Conceded_lx'].rank(method='min', ascending=True)
    )
    return df_out


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
def convert_to_r_format(df):
    # Add 'Home_Away' column if it does not exist, defaulting to 'Combined'
    if 'Home_Away' not in df.columns:
        df['Home_Away'] = 'Combined'

    # Melt the DataFrame to a long format
    df_long = df.melt(
        id_vars=['Team', 'n_matches_in_sample', 'Home_Away'],
        var_name='metric',
        value_name='value'
    )
    
    # Ensure the 'value' column is explicitly of type float
    df_long['value'] = df_long['value'].astype(float)

    # Define ranking order for each metric
    metric_order = {
        'Goals_Scored_lx': False,       # Descending order for higher is better
        'Goals_Conceded_lx': True,      # Ascending order for lower is better
        'Points_lx': False,             # Descending order for higher is better
        'Shots_For_lx': False,          # Descending order for higher is better
        'Shots_Conceded_lx': True,      # Ascending order for lower is better
        'Shots_OT_For_lx': False,       # Descending order for higher is better
        'Shots_OT_Conceded_lx': True    # Ascending order for lower is better
    }
    
    # Apply ranking for each metric based on its specified order within each 'Home_Away' group
    df_long['rank'] = df_long.groupby(['Home_Away', 'metric'], group_keys=False).apply(
        lambda x: x['value'].rank(
            method='min',
            ascending=metric_order.get(x.name[1], False)  # Get the order based on 'metric', default False if not found
        )
    ).astype(int)  # Forcefully cast ranks to int
    
    # Rearrange columns to match the desired output format
    df_long = df_long[['Team', 'Home_Away', 'n_matches_in_sample', 'rank', 'value', 'metric']]
    
    # Sort for readability
    df_long = df_long.sort_values(by=['metric', 'rank']).reset_index(drop=True)
    
    return df_long




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Main function to calculate league metrics
def f_metrics_league(data, n_matches=3, h2h_teams="SKIP", home_away=False):
    # Step 1: Check for incompatible parameters
    if h2h_teams != "SKIP" and home_away:
        print("Can't run home/away and head-to-head at the same time")
        return None

    # Step 2: Get teams for the current season
    latest_season = data['Season_ending_year'].max()
    this_season_teams = pd.concat([
        data[data['Season_ending_year'] == latest_season]['HomeTeam'],
        data[data['Season_ending_year'] == latest_season]['AwayTeam']
    ]).unique()
    team_list = pd.DataFrame(this_season_teams, columns=["Team"])

    if h2h_teams != "SKIP":
        # Step 3a: H2H calculations for specified teams
        h2h_matches = data[(data['HomeTeam'].isin(h2h_teams)) & (data['AwayTeam'].isin(h2h_teams))]
        
        df_h2h_home = (
            team_list[team_list['Team'].isin(h2h_teams)]
            .merge(h2h_matches, left_on="Team", right_on="HomeTeam", how="left")
            .rename(columns={"AwayTeam": "Opponent"})
        )
        df_h2h_home = f_team_cols_rename(df_h2h_home, home_metrics=True)
        
        df_h2h_away = (
            team_list[team_list['Team'].isin(h2h_teams)]
            .merge(h2h_matches, left_on="Team", right_on="AwayTeam", how="left")
            .rename(columns={"HomeTeam": "Opponent"})
        )
        df_h2h_away = f_team_cols_rename(df_h2h_away, home_metrics=False)
        
        # Combine home and away results for H2H teams
        df_h2h_combined = pd.concat([df_h2h_home, df_h2h_away], ignore_index=True)
        df_h2h_results = f_part_calculate_scores(df_h2h_combined, n_matches=n_matches)

        # Step 3b: Base calculations for non-H2H teams
        non_h2h_teams = team_list[~team_list['Team'].isin(h2h_teams)]
        
        df_non_h2h_home = non_h2h_teams.merge(data, left_on="Team", right_on="HomeTeam", how="left")
        df_non_h2h_away = non_h2h_teams.merge(data, left_on="Team", right_on="AwayTeam", how="left")
        
        df_non_h2h_home = f_team_cols_rename(df_non_h2h_home, home_metrics=True)
        df_non_h2h_away = f_team_cols_rename(df_non_h2h_away, home_metrics=False)
        
        df_non_h2h_combined = pd.concat([df_non_h2h_home, df_non_h2h_away], ignore_index=True)
        df_non_h2h_results = f_part_calculate_scores(df_non_h2h_combined, n_matches=n_matches)

        # Step 4: Combine H2H and non-H2H results
        df_results = pd.concat([df_h2h_results, df_non_h2h_results], ignore_index=True)

    elif home_away:
        # Separate calculations for Home and Away
        df_home = (
            team_list.merge(data, left_on="Team", right_on="HomeTeam", how="left")
            .assign(Home_Away="Home")
            .rename(columns={"AwayTeam": "Opponent"})
        )
        df_home = f_team_cols_rename(df_home, home_metrics=True)
        df_home = f_part_calculate_scores(df_home, n_matches=n_matches)
        df_home["Home_Away"] = "Home"

        df_away = (
            team_list.merge(data, left_on="Team", right_on="AwayTeam", how="left")
            .assign(Home_Away="Away")
            .rename(columns={"HomeTeam": "Opponent"})
        )
        df_away = f_team_cols_rename(df_away, home_metrics=False)
        df_away = f_part_calculate_scores(df_away, n_matches=n_matches)
        df_away["Home_Away"] = "Away"

        df_results = pd.concat([df_home, df_away], ignore_index=True)

    else:
        # Process all matches combined (Base case)
        df_home = team_list.merge(data, left_on="Team", right_on="HomeTeam", how="left")
        df_away = team_list.merge(data, left_on="Team", right_on="AwayTeam", how="left")
        
        df_home = f_team_cols_rename(df_home, home_metrics=True)
        df_away = f_team_cols_rename(df_away, home_metrics=False)
        
        df_combined = pd.concat([df_home, df_away], ignore_index=True)
        df_results = f_part_calculate_scores(df_combined, n_matches=n_matches)
        df_results["Home_Away"] = "Combined"

    # Step 5: Rank metrics
    df_results = f_part_calculate_ranks(df_results)
    return df_results



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# METRIC FUNCTION PROBABILITY

def calculate_match_probability(metrics_df, home_team, away_team, metric):
    # Filter the DataFrame for the specific metric and teams
    filtered_df = metrics_df[
        (metrics_df['Team'].isin([home_team, away_team])) &
        (metrics_df['metric'] == metric)
    ]

    # Check if we're dealing with a Home/Away scenario by examining the unique values in 'Home_Away'
    if 'Home_Away' in filtered_df.columns and len(filtered_df['Home_Away'].unique()) > 1:
        # Home/Away scenario: get home rank for home team, away rank for away team
        home_rank = float(filtered_df[(filtered_df['Team'] == home_team) & (filtered_df['Home_Away'] == "Home")]['rank'].values[0])
        away_rank = float(filtered_df[(filtered_df['Team'] == away_team) & (filtered_df['Home_Away'] == "Away")]['rank'].values[0])
    else:
        # Combined scenario, or if we have single entries for each team
        home_rank = float(filtered_df[filtered_df['Team'] == home_team]['rank'].values[0])
        away_rank = float(filtered_df[filtered_df['Team'] == away_team]['rank'].values[0])

    # Calculate the rank range and spread
    rank_range = float(metrics_df['rank'].max() - metrics_df['rank'].min())
    rank_spread = abs(away_rank - home_rank)

    # Calculate probabilities
    pool_for_teams = (rank_range - rank_spread) / 4
    pool_for_draw = (rank_range - rank_spread) / 2

    perc_for_best = 100 * (pool_for_teams + rank_spread) / rank_range
    perc_for_worst = 100 * pool_for_teams / rank_range
    perc_for_draw = 100 * pool_for_draw / rank_range

    # Determine which team has the higher probability based on rank comparison
    if home_rank < away_rank:
        home_perc = perc_for_best
        away_perc = perc_for_worst
    elif home_rank > away_rank:
        home_perc = perc_for_worst
        away_perc = perc_for_best
    else:
        home_perc = perc_for_best
        away_perc = perc_for_worst

    # Return only the calculated probabilities for Home, Away, and Draw as Python floats
    return {
        "Home": float(home_perc),
        "Away": float(away_perc),
        "Draw": float(perc_for_draw)
    }


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FULL MATCH

def calculate_expected_outcome(home_team, away_team, game_stats_config, df_result_basic_long, df_result_home_away_long, df_input):
    expected_outcome = {
        "Home": 0,
        "Away": 0,
        "Draw": 0
    }

    # Iterate over each configuration in the game_stats_config
    for key, config in game_stats_config.items():
        metric = config["metric"]
        n_matches = config["n_matches"]
        h2h_teams = config["h2h_teams"]
        home_away = config["home_away"]
        weight = config["weight"]

        case = "Basic"
        
        # Select the appropriate precomputed data frame based on `home_away`
        if home_away:
            metrics_df = df_result_home_away_long
            case = "Home/Away"
        else:
            metrics_df = df_result_basic_long

        # Filter to h2h if specified (i.e., h2h_teams != "SKIP")
        if h2h_teams != "SKIP":
            filtered_df = f_metrics_league(df_input, n_matches=n_matches, h2h_teams=[home_team, away_team], home_away=False)
            metrics_df = convert_to_r_format(filtered_df)
            case = "Head-to-Head"

        # Filter for the specific metric and teams
        filtered_metrics_df = metrics_df[
            (metrics_df["Team"].isin([home_team, away_team])) & 
            (metrics_df["metric"] == metric)
        ]

        # Check if the number of matches in the sample meets the required n_matches
        home_n_matches = filtered_metrics_df.loc[filtered_metrics_df["Team"] == home_team, "n_matches_in_sample"].values
        away_n_matches = filtered_metrics_df.loc[filtered_metrics_df["Team"] == away_team, "n_matches_in_sample"].values
        
        # If either team's actual matches are less than the required n_matches, skip this metric
        if (len(home_n_matches) == 0 or home_n_matches[0] < n_matches) or (len(away_n_matches) == 0 or away_n_matches[0] < n_matches):
            
            #print(f"Skipping {metric} for {home_team} vs {away_team} due to insufficient matches. In {case}")
            
            continue  # Skip this metric if the match count requirement isn't met

        # Calculate the match probabilities for the current metric
        match_prob = calculate_match_probability(
            metrics_df=metrics_df,
            home_team=home_team,
            away_team=away_team,
            metric=metric
        )

        # print(f"Case: {case}, {metric}: {match_prob}")

        # Weight the probabilities, converting to Python float
        weighted_home_prob = float(match_prob["Home"] * weight)
        weighted_away_prob = float(match_prob["Away"] * weight)
        weighted_draw_prob = float(match_prob["Draw"] * weight)

        # Accumulate probabilities
        expected_outcome["Home"] += weighted_home_prob
        expected_outcome["Away"] += weighted_away_prob
        expected_outcome["Draw"] += weighted_draw_prob

    # Normalize to ensure total probability is 100%
    total_prob = expected_outcome["Home"] + expected_outcome["Away"] + expected_outcome["Draw"]
    if total_prob > 0:
        expected_outcome["Home"] = float((expected_outcome["Home"] / total_prob) * 100)
        expected_outcome["Away"] = float((expected_outcome["Away"] / total_prob) * 100)
        expected_outcome["Draw"] = float((expected_outcome["Draw"] / total_prob) * 100)

    return expected_outcome



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOAD DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Lambda handler
def lambda_handler(event, context):
    print("Lambda function started")


    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # HISTORIC DATA
    
    #define table to query
    table_name_to_query1 = "premier_league_data_current_lambda"
    table_name_to_query2 = "premier_league_data_historical"

    df1 = get_data_from_db(table_name_to_query1)
    df2 = get_data_from_db(table_name_to_query2)
    
    # bind the 2 tables after applying the function f_select_columns_historical_data to both 
    df1 = f_select_columns_historical_data(df1)
    df2 = f_select_columns_historical_data(df2)
    df_raw = pd.concat([df1, df2])

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # SCHEDULE DATA

    #define table to query
    table_name_to_query_schedules = "premier_league_fixtures_current_lambda"
    table_name_to_query_translations = "team_name_translate"
   
    df_schedule = get_data_from_db(table_name_to_query_schedules)
    df_translate = get_data_from_db(table_name_to_query_translations)




    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # RUN CODE
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    # Convert raw data into a DataFrame
    df_input = pd.DataFrame(df_raw)

    # Calculate the three main data frames based on the configurations
    df_result_basic = f_metrics_league(df_input, n_matches=10, h2h_teams="SKIP", home_away=False)
    df_result_basic_long = convert_to_r_format(df_result_basic)

    df_result_home_away = f_metrics_league(df_input, n_matches=5, h2h_teams="SKIP", home_away=True)
    df_result_home_away_long = convert_to_r_format(df_result_home_away)

    # Convert the 'Date' column to datetime format
    df_schedule['Date'] = pd.to_datetime(df_schedule['Date'], format='%d/%m/%Y %H:%M')

    # Get the current date and time
    now = datetime.now()
    one_weeks_ahead = now + timedelta(weeks=1)
    two_weeks_ahead = now + timedelta(weeks=2)

    # Define a new DataFrame to store the upcoming games with calculated probabilities
    upcoming_games = df_schedule[(df_schedule['Date'] > one_weeks_ahead) & (df_schedule['Date'] <= two_weeks_ahead)].copy()
    
    # drop column team_short
    df_translate = df_translate.drop(columns=['team_short'])
    
    # Join the team name translations HOME
    upcoming_games = upcoming_games.merge(df_translate, left_on="Home Team", right_on="team_name_from_schedule_data", how="left")
    
    #rename column team to HomeTeam and drop team_name_from_schedule_data
    upcoming_games = upcoming_games.rename(columns={"team": "HomeTeam"})    
    upcoming_games = upcoming_games.drop(columns=['team_name_from_schedule_data'])
    
    # Join the team name translations AWAY
    upcoming_games = upcoming_games.merge(df_translate, left_on="Away Team", right_on="team_name_from_schedule_data", how="left")
    upcoming_games = upcoming_games.rename(columns={"team": "AwayTeam"})    
    upcoming_games = upcoming_games.drop(columns=['team_name_from_schedule_data'])
    

    # Initialize empty lists to store the probabilities for Home, Away, and Draw
    home_probs = []
    away_probs = []
    draw_probs = []

    # Initialize a counter to track progress
    total_games = len(upcoming_games)
    counter = 0

    # Iterate over each upcoming game
    for _, row in upcoming_games.iterrows():
        home_team = row['HomeTeam']
        away_team = row['AwayTeam']
        
        # Calculate expected outcome
        outcome = calculate_expected_outcome(
            home_team=home_team,
            away_team=away_team,
            game_stats_config=game_stats_config,
            df_result_basic_long=df_result_basic_long,
            df_result_home_away_long=df_result_home_away_long,
            df_input=df_input
        )
        

        # check if outcome home is lower than 15% and if so, set it to 15% + random
        if outcome["Home"] < 15:
            outcome["Home"] = 16 + pd.RangeIndex(0, 1000).to_series().sample(1).iloc[0].item() / 500
        else:
            outcome["Home"] = outcome["Home"] + 2

        # check if outcome Away is lower than 15% and if so, set it to 15% + random
        if outcome["Away"] < 15:
            outcome["Away"] = 16 + pd.RangeIndex(0, 1000).to_series().sample(1).iloc[0].item() / 500
        else:
            outcome["Away"] = outcome["Away"] + 2
            
         # check if outcome Draw is lower than 19% and if so, set it to 19% + random
        if outcome["Draw"] < 19:
            outcome["Draw"] = 20 + pd.RangeIndex(0, 1000).to_series().sample(1).iloc[0].item() / 500
        else:
            outcome["Draw"] = outcome["Draw"] + 1
            
        
        # Append the calculated probabilities to the respective lists
        home_probs.append(outcome["Home"])
        away_probs.append(outcome["Away"])
        draw_probs.append(outcome["Draw"])
        
        # Increment the counter and print progress
        counter += 1
        print(f"Processed game {counter} of {total_games} - {home_team} vs {away_team}")


    # Add the probabilities as new columns in the DataFrame
    upcoming_games['Home Win Probability (%)'] = home_probs
    upcoming_games['Away Win Probability (%)'] = away_probs
    upcoming_games['Draw Probability (%)'] = draw_probs
    
    # Set 0 as the "changed_since_start_xx"
    upcoming_games['changed_since_start_home'] = 0
    upcoming_games['changed_since_start_away'] = 0
    upcoming_games['changed_since_start_draw'] = 0


    upcoming_games['TimeStamp_Uploaded'] = pd.Timestamp.now()

    table_name = "premier_league_fixtures_historical_enriched"
    
    upload_df_to_postgres(upcoming_games, table_name, if_exists_rule = "append")





    print("Lambda function completed without errors.")
    return {"statusCode": 200, "body": "Data upload completed successfully"}

# %%
