import io
import streamlit as st
import pandas as pd
import numpy as np

import matplotlib as mpl
import matplotlib.pyplot as plt
import matplotlib.style as style
import matplotlib.font_manager as fm
import seaborn as sns

import scipy.stats as stats

import yaml
import psycopg2

import dbconfig as db
from utils.gamerecap import (
    gamerecap_selectbox_query,
    gamerecap_pbp_query,
    plot_wp_dist,
    plot_wp_worm,
    plot_wp,
)

# Load the database configuration
config_file = "db_config.yaml"
db_config = db.load_db_config(db.load_config(config_file))

# Create connection
conn = db.create_connection(db_config)


# Session State Initialize
if "select_season" not in st.session_state:
    st.session_state["select_season"] = 2024

# Create UI
with st.sidebar:

    selected_season = st.selectbox(
        label="Season", options=reversed([i for i in range(2020, 2025)])
    )

    selectbox_results = pd.read_sql_query(
        gamerecap_selectbox_query(selected_season), conn
    )["game_id"]
    # selectbox_results = list(zip(*selectbox_results))[0]

    selected_game = st.selectbox(
        label="Team",
        options=selectbox_results,
    )

st.header(f"{selected_game}")

col1, col2 = st.columns(2)

# with col1:
#     st.pyplot(
#         plot_wp_worm(
#             df=pd.read_sql_query(gamerecap_pbp_query(selected_game), conn),
#             game_id=selected_game,
#         )
#     )

# with col2:
#     st.pyplot(
#         plot_wp_dist(
#             df=pd.read_sql_query(gamerecap_pbp_query(selected_game), conn),
#             game_id=selected_game,
#         )
#     )

st.pyplot(
    plot_wp(
        df=pd.read_sql_query(gamerecap_pbp_query(selected_game), conn),
        game_id=selected_game,
        conn=conn,
    )
)
