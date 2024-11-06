# %%
# Upload Historical EPL Data
from utils import fetch_data_from_url, upload_df_to_postgres
import pandas as pd

CSV_URL_EPL_2024 = "https://www.football-data.co.uk/mmz4281/2324/E0.csv"
CSV_URL_EPL_2023 = "https://www.football-data.co.uk/mmz4281/2223/E0.csv"
CSV_URL_EPL_2022 = "https://www.football-data.co.uk/mmz4281/2122/E0.csv"

df_EPL_2024 = fetch_data_from_url(CSV_URL_EPL_2024)
df_EPL_2024['Season_ending_year'] = 2024

df_EPL_2023 = fetch_data_from_url(CSV_URL_EPL_2023)
df_EPL_2023['Season_ending_year'] = 2023

df_EPL_2022 = fetch_data_from_url(CSV_URL_EPL_2022)
df_EPL_2022['Season_ending_year'] = 2022

df_3_seasons = pd.concat([df_EPL_2024, df_EPL_2023, df_EPL_2022], ignore_index=True)
df_3_seasons['TimeStamp_Uploaded'] = pd.Timestamp.now()  

print(df_3_seasons)

# %%
# Upload the data to the database

table_name = "premier_league_data_historical"
upload_df_to_postgres(df_3_seasons, table_name)

