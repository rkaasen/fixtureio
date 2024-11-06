# %%
# TEST THE UPLOADING

from utils import fetch_data_from_url, upload_df_to_postgres
import pandas as pd

CSV_URL = "https://www.football-data.co.uk/mmz4281/2425/E0.csv"
df = fetch_data_from_url(CSV_URL)
df['TimeStamp_Uploaded'] = pd.Timestamp.now()    
df = df.head(10)

print(df.head())
# Test the data uploading function

table_name = "premier_league_data_current"
upload_df_to_postgres(df, table_name)

# %%
# %%
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Upload EPL schedule data
 
from utils import fetch_data_from_url, upload_df_to_postgres
import pandas as pd
 
CSV_URL_FIXTURE_CURRENT = "https://fixturedownload.com/download/epl-2024-GMTStandardTime.csv"

df_fixtures_current = fetch_data_from_url(CSV_URL_FIXTURE_CURRENT)
df_fixtures_current['TimeStamp_Uploaded'] = pd.Timestamp.now()
print(df_fixtures_current)

table_name = "premier_league_fixtures_current"
upload_df_to_postgres(df_fixtures_current, table_name)

