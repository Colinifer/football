import io
import streamlit as st
import pandas as pd
import numpy as np

import matplotlib as mpl
import matplotlib.pyplot as plt
import matplotlib.style as style
import matplotlib.font_manager as fm
import scipy.stats as stats

from matplotlib import patches as mpatches

import fantasypros as fpros

import yaml
import psycopg2

import dbconfig as db

# Load the database configuration
config_file = "db_config.yaml"
db_config = db.load_db_config(db.load_config(config_file))

# Create a connection to the database
conn = db.create_connection(db_config)

def selectbox_query() :
    return """
        select 
            distinct posteam
        from "nflfastR_pbp"
        where season = (select max(season) from "nflfastR_pbp")
            and posteam is not null
        order by 1
    """

selectbox_results = db.execute_query(conn, selectbox_query())
selectbox_results = list(zip(*selectbox_results))[0]

with st.sidebar:

    selected_league = st.selectbox(
        label="Fantasy League",
        options=db.load_ff_config(db.load_config(config_file))["leagues"]
    )

    selected_team = st.selectbox(
        label="Team",
        options=selectbox_results,
        # index=None,
        # placeholder="Select a team...",
    )


st.title(f"{selected_team} Stats")
st.markdown("This app helps a fantasy football manager evaluate which ")
st.markdown("####")
st.image(
    f"https://a.espncdn.com/combiner/i?img=/i/teamlogos/nfl/500/{selected_team}.png&h=100&w=100",
)

st.markdown(db.load_ff_config(db.load_config(config_file))["team_name"][selected_league])

st.write("You selected:", selected_team)

# st.markdown(selectbox_results)