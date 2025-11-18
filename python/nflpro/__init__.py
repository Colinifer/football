"""The nflpro package provides tools for interacting with NFL data.

It includes modules for:
- Accessing the NFL Pro API (nflpro.nflapi)
- Downloading NFL film footage (nflpro.film)
- Utility functions (nflpro.utils)
"""

from .film import (
    download_film,
    download_film_from_dataframe,
    download_m3u8,
    download_play_film,
    download_playlist,
    get_highest_bandwidth_stream_url,
)
from .nflapi import NFLProAPI
from .query import (
    create_connection,
    execute_query,
    query_to_dataframe,
    write_dataframe_to_postgres,
)
from .utils import create_week_slug

__all__ = [
    # nflapi.py
    "NFLProAPI",
    # film.py
    "download_playlist",
    "download_m3u8",
    "get_highest_bandwidth_stream_url",
    "download_play_film",
    "download_film",
    "download_film_from_dataframe",
    # query.py
    "create_connection",
    "execute_query",
    "query_to_dataframe",
    "write_dataframe_to_postgres",
    # utils.py
    "create_week_slug",
]
