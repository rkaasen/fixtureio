
# %% 

import pandas as pd
from utils import append_df_to_postgres


# Create one-row DataFrame for Sunderland
new_row = pd.DataFrame([{
    "team": "Sunderland",
    "team_short": "SUN",
    "team_name_from_schedule_data": "Sunderland"
}])

print(new_row)


# %%

from dotenv import load_dotenv
load_dotenv()

append_df_to_postgres(new_row, "team_name_translate")
# %%
