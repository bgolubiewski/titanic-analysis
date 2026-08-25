# /python/fix_titanic_raw.py
import pandas as pd
import requests
from bs4 import BeautifulSoup
from pathlib import Path

# Determine base and data paths
TITANIC_ANALISYS_PATH = Path(__file__).resolve().parent.parent
DATA_PATH = TITANIC_ANALISYS_PATH / "data" / "titanic_raw.csv"

OUTPUT_PATH = TITANIC_ANALISYS_PATH / "data" / "titanic_fixed.csv"


# Wikipedia URL and headers
URL = "https://en.wikipedia.org/wiki/Passengers_of_the_Titanic"
HEADERS = {
    "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"
}

# Get the page content
response = requests.get(URL, headers=HEADERS)
soup = BeautifulSoup(response.text, 'html.parser')

data_from_wiki = []

# Iterate through selected passenger tables on the page
for table_header in ('First class', 'Second class', 'Third class'):
    
    section_header = soup.find('h3', string=table_header)
    target_table = section_header.find_next("table")

    for row in target_table.find_all("tr"):
        cells = row.find_all("td")
        if not cells:
            continue # skip rows without cells with data (e.g, headers)

        passenger_name = cells[0].text.strip()
        row_html = str(row).lower()

        # Check for specific color hex in a row's code and set 'Survived_wiki' value accordingly
        if 'style="background:#9bddff;' in row_html:
            data_from_wiki.append({'Name_wiki': passenger_name, "Survived_wiki": 1})
        else:
            data_from_wiki.append({'Name_wiki': passenger_name, "Survived_wiki": 0})


df_wiki = pd.DataFrame(data_from_wiki).drop_duplicates(subset=["Name_wiki"])

df_raw = pd.read_csv(DATA_PATH)
df_raw = pd.merge(df_raw, df_wiki, on='Name_wiki', how='left')

df_raw["Survived"] = df_raw["Survived"].fillna(df_raw["Survived_wiki"])
df_raw = df_raw.drop(columns=["Survived_wiki"])

df_raw.to_csv(OUTPUT_PATH, index=False)
print(f"Saved to: {OUTPUT_PATH}")
    
