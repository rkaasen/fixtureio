# %%
# function to help load data

import pandas as pd

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

# GET DATA

# %%
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
import os
import pandas as pd
from sqlalchemy import create_engine

#define table to query
table_name_to_query1 = "premier_league_data_current_lambda"
table_name_to_query2 = "premier_league_data_historical"

# Set up environment variables or replace these with your database details
db_host = os.getenv("DB_HOST", "your_db_host")
db_name = os.getenv("DB_NAME", "your_db_name")
db_user = os.getenv("DB_USER", "your_db_user")
db_password = os.getenv("DB_PASSWORD", "your_db_password")
db_port = os.getenv("DB_PORT", "5432")  # Default PostgreSQL port is 5432

# Create the SQLAlchemy engine to connect to the database
engine = create_engine(f"postgresql+psycopg2://{db_user}:{db_password}@{db_host}:{db_port}/{db_name}")

# Define the query (adjust the table name to your actual table)
query1 = f"SELECT * FROM {table_name_to_query1} "

# Fetch data into a pandas DataFrame
try:
    with engine.connect() as connection:
        df1 = pd.read_sql(query1, connection)
        print("Data fetched successfully:")
except Exception as e:
    print(f"Error querying the database: {e}")
finally:
    # Dispose of the engine connection
    engine.dispose()
    
    
# Define the query (adjust the table name to your actual table)
query2 = f"SELECT * FROM {table_name_to_query2} "

# Fetch data into a pandas DataFrame
try:
    with engine.connect() as connection:
        df2 = pd.read_sql(query2, connection)
        print("Data fetched successfully:")
except Exception as e:
    print(f"Error querying the database: {e}")
finally:
    # Dispose of the engine connection
    engine.dispose()

# bind the 2 tables after applying the function f_select_columns_historical_data to both 
df1 = f_select_columns_historical_data(df1)
df2 = f_select_columns_historical_data(df2)
df_raw = pd.concat([df1, df2])




# %%
import pandas as pd

# Helper functions (kept as is from previously working versions)
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

def f_part_calculate_scores(df, n_matches=5):
    df = df.sort_values(by=['Team', 'Date'], ascending=[True, False])
    df = df.groupby('Team').head(n_matches)
    df_out = df.groupby('Team', as_index=False).agg({
        'FT_Scored': 'sum',
        'FT_Conceded': 'sum',
        'Points': 'sum',
        'Shots_For': 'sum',
        'Shots_Conceded': 'sum',
        'Shots_OT_For': 'sum',
        'Shots_OT_Conceded': 'sum'
    })
    df_out = df_out.rename(columns={
        'FT_Scored': 'Goals_Scored_lx',
        'FT_Conceded': 'Goals_Conceded_lx',
        'Points': 'Points_lx',
        'Shots_For': 'Shots_For_lx',
        'Shots_Conceded': 'Shots_Conceded_lx',
        'Shots_OT_For': 'Shots_OT_For_lx',
        'Shots_OT_Conceded': 'Shots_OT_Conceded_lx'
    })
    df_out['n_matches_in_sample'] = n_matches
    return df_out

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

import pandas as pd

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
    ).astype(int)
    
    # Rearrange columns to match the desired output format
    df_long = df_long[['Team', 'Home_Away', 'n_matches_in_sample', 'rank', 'value', 'metric']]
    
    # Sort for readability
    df_long = df_long.sort_values(by=['metric', 'rank']).reset_index(drop=True)
    
    return df_long






# %%


# Convert to DataFrame
df = pd.DataFrame(df_raw)
print(df)

# Test Case 1: Basic Usage without h2h_teams and home_away

df_result_1 = f_metrics_league(df, n_matches=3, h2h_teams="SKIP", home_away=False)
print(df_result_1)

df_result_1_long = convert_to_r_format(df_result_1)
print(df_result_1_long)

df_filtered = df_result_1_long.query('metric == "Points_lx"').sort_values(by='rank')
print(df_filtered)




# %%
# Test Case 2: With home_away=True

# Convert to DataFrame
df = pd.DataFrame(df_raw)
print(df)

# Test Case 1: Basic Usage without h2h_teams and home_away

df_result_2 = f_metrics_league(df, n_matches=3, h2h_teams="SKIP", home_away=True)
print(df_result_2)


df_result_2_long = convert_to_r_format(df_result_2)
print(df_result_2_long)


df_filtered2 = df_result_2_long.query('metric == "Points_lx"').sort_values(by='rank')

# Display the filtered and sorted DataFrame
print(df_filtered2)



# %%
# Test Case 3: With home_away=True

# Convert to DataFrame
df = pd.DataFrame(df_raw)
print(df)

# Test Case 1: Basic Usage without h2h_teams and home_away

df_result_3 = f_metrics_league(df, n_matches=3, h2h_teams=["Man United", "Fulham"], home_away=False)
print(df_result_3)

df_result_3_long = convert_to_r_format(df_result_3)
print(df_result_3_long)

df_filtered3 = df_result_3_long.query('metric == "Points_lx"').sort_values(by='rank')

# Display the filtered and sorted DataFrame
print(df_filtered3)





# %%
def calculate_match_probability(metrics_df, home_team, away_team, metric):
    # Filter the DataFrame for the specific metric and teams
    filtered_df = metrics_df[
        (metrics_df['Team'].isin([home_team, away_team])) &
        (metrics_df['metric'] == metric)
    ]

    # Check if we're dealing with a Home/Away scenario by examining the unique values in 'Home_Away'
    if 'Home_Away' in filtered_df.columns and len(filtered_df['Home_Away'].unique()) > 1:
        # Home/Away scenario: get home rank for home team, away rank for away team
        home_rank = filtered_df[(filtered_df['Team'] == home_team) & (filtered_df['Home_Away'] == "Home")]['rank'].values[0]
        away_rank = filtered_df[(filtered_df['Team'] == away_team) & (filtered_df['Home_Away'] == "Away")]['rank'].values[0]
    else:
        # Combined scenario, or if we have single entries for each team
        home_rank = filtered_df[filtered_df['Team'] == home_team]['rank'].values[0]
        away_rank = filtered_df[filtered_df['Team'] == away_team]['rank'].values[0]

    # Calculate the rank range and spread
    rank_range = metrics_df['rank'].max() - metrics_df['rank'].min()
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

    # Return only the calculated probabilities for Home, Away, and Draw
    return {
        "Home": home_perc,
        "Away": away_perc,
        "Draw": perc_for_draw
    }



# %%
# Example usage
result = calculate_match_probability(metrics_df=df_result_2_long, 
                                     home_team="Man United", away_team="Fulham", 
                                     metric="Points_lx")
print(result)

# Filter df_result_2_long for "Man United", "Fulham" and "Points_lx" metric
filtered_df = df_result_2_long[
    (df_result_2_long['Team'].isin(["Man United", "Fulham"])) &
    (df_result_2_long['metric'] == "Points_lx")
]

print(filtered_df)

# %%