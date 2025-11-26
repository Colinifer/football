# nflpro/nflapi.py
"""NFLProAPI: a wrapper for accessing data from the NFL Pro API.

The NFLProAPI class handles authentication, data retrieval, and data \
    formatting for various NFL-related data endpoints.
It includes methods for fetching and cleaning data related to:

- Fantasy football statistics
- Passing, rushing, and receiving statistics
- Team offense and defense statistics
- Game schedules and weeks
- Play-by-play data
- Coaches film data
- Season insights

The module uses the `requests` library for making HTTP requests, \
    the `pandas` library for data manipulation, and the `dotenv` \
        library for managing API credentials.

Dependencies:
- requests
- pandas
- python-dotenv

"""

import logging
import os
import time
from pathlib import Path
from typing import Any

import pandas as pd
import requests
from dotenv import load_dotenv


class NFLProAPI:
    """A Python wrapper for the NFL Pro API."""

    def __init__(self) -> None:
        """Initialize the API wrapper, load credentials and set up config.

        Raises:
            ValueError: If API credentials (key, token, or cookie) are missing
                from the .env file.

        """
        load_dotenv()
        self.api_key: str | None = os.getenv("NFLPRO_API_KEY")
        self.login_token: str | None = os.getenv("NFLPRO_LOGIN_TOKEN")
        self.cookie: str | None = os.getenv("NFLPRO_COOKIE")

        self.clientKey: str | None = os.getenv("CLIENT_KEY")
        self.clientSecret: str | None = os.getenv("CLIENT_SECRET")
        self.deviceId: str | None = os.getenv("DEVICE_ID")
        self.deviceInfo: str | None = os.getenv("DEVICE_INFO")
        self.refreshToken: str | None = os.getenv("REFRESH_TOKEN")

        if not all([self.api_key, self.login_token, self.cookie]):
            msg = "API credentials (key, token, or cookie) \
                are missing from the .env file."
            raise ValueError(msg)

        self.base_url: str = "https://pro.nfl.com"
        self.session: requests.Session = requests.Session()
        self.access_token: str | None = None
        self.expires_at: float | None = None

        # Centralized endpoint configuration
        self.endpoints: dict[str, dict[str, Any]] = {
            "fantasy_week": {
                "url": "/api/secured/stats/fantasy/season",
                "sortKey": "fpHalfPPR",
                "limit": 70,
                "data_key": "players",
                "important_cols": [
                    "displayName",
                    "position",
                    "teamId",
                    "fpStd",
                    "fpHalfPPR",
                    "fpPPR",
                    "fpPass",
                    "fpRush",
                    "fpRecStd",
                ],
            },
            "fantasy_season": {
                "url": "/api/secured/stats/players-fantasy/season",
                "sortKey": "fpHalfPPR",
                "limit": 70,
                "data_key": "players",
                "important_cols": [
                    "displayName",
                    "position",
                    "teamId",
                    "fpStd",
                    "fpHalfPPR",
                    "fpPPR",
                    "fpPass",
                    "fpRush",
                    "fpRecStd",
                ],
            },
            "passing_week": {
                "url": "/api/secured/stats/players-offense/passing/week",
                "sortKey": "yds",
                "limit": 35,
                "data_key": "passers",
                "important_cols": [
                    "displayName",
                    "position",
                    "teamId",
                    "jerseyNumber",
                    "cmp",
                    "att",
                    "yds",
                    "td",
                    "int",
                    "rating",
                ],
            },
            "rushing_week": {
                "url": "/api/secured/stats/players-offense/rushing/week",
                "sortKey": "yds",
                "limit": 35,
                "data_key": "rushers",
                "important_cols": [
                    "displayName",
                    "position",
                    "teamId",
                    "jerseyNumber",
                    "att",
                    "yds",
                    "td",
                    "ypc",
                    "epa",
                ],
            },
            "receiving_week": {
                "url": "/api/secured/stats/players-offense/receiving/week",
                "sortKey": "yds",
                "limit": 35,
                "data_key": "receivers",
                "important_cols": [
                    "displayName",
                    "position",
                    "teamId",
                    "jerseyNumber",
                    "rec",
                    "yds",
                    "td",
                    "tgt",
                    "ypr",
                ],
            },
            "team_offense_week": {
                "url": "/api/secured/stats/team-offense/overview/week",
                "sortKey": "yds",
                "limit": 35,
                "data_key": "offense",
                "important_cols": [
                    "teamAbbr",
                    "teamId",
                    "gp",
                    "ppg",
                    "ypg",
                    "passYpg",
                    "rushYpg",
                ],
            },
            "team_defense_week": {
                "url": "/api/secured/stats/team-defense/overview/week",
                "sortKey": "yds",
                "limit": 35,
                "data_key": "defense",
                "important_cols": [
                    "teamAbbr",
                    "teamId",
                    "gp",
                    "oppPpg",
                    "oppYpg",
                    "oppPassYpg",
                    "oppRushYpg",
                ],
            },
            "play_by_play": {
                "url": "/api/secured/plays/playlist/game",
                "sortKey": "playId",
                "limit": 75,
                "data_key": "plays",
                "important_cols": [
                    "gameId",
                    "playId",
                    "playDescription",
                    "possessionTeam",
                    "down",
                    "yardsToGo",
                    "quarter",
                ],
                "unnest": [
                    "nflIds",
                    "offense",
                    "defense",
                    "passInfo",
                    "recInfo",
                ],
            },
            "play_summary": {
                "url": "/api/plays/summaryPlay",
                "sortKey": None,
                "limit": None,
                "data_key": None,
                "important_cols": [],
            },
            "coaches": {
                "url": "/api/secured/videos/coaches",
                "sortKey": None,
                "limit": None,
                "data_key": "items",
                "important_cols": [],
            },
            "play_film": {
                "url": "/play/v1/asset",
                "sortKey": None,
                "limit": None,
                "data_key": None,
                "important_cols": [],
            },
            "weeks": {
                "url": "/api/schedules/weeks",
                "sortKey": None,
                "limit": None,
                "data_key": "weeks",
                "important_cols": [
                    "season",
                    "seasonType",
                    "week",
                    "dateBegin",
                    "dateEnd",
                    "weekType",
                    "weekSlug",
                    "weekSlug",
                    "text",
                    "seasonTypeWeek",
                ],
            },
            "games": {
                "url": "/api/schedules/games",
                "sortKey": None,
                "limit": None,
                "data_key": "games",
                "important_cols": [
                    "id",
                    "homeTeamId",
                    "awayTeamId",
                    "date",
                    "time",
                    "week",
                ],
            },
            "season_insights": {
                "url": "/api/content/insights/season",
                "sortKey": None,
                "limit": 100,
                "data_key": None,  # Data is directly in the response
                "important_cols": [
                    "nflId",
                    "playerName",
                    "teamAbbr",
                    "title",
                    "date",
                ],
            },
        }

    def _get_account_info(self) -> dict[str, Any] | None:
        """Fetch account information using APIKey and login_token.

        Returns:
            Optional[Dict[str, Any]]: Account information as a dictionary,
                or None if an error occurs.

        """
        url = "https://auth-id.nfl.com/accounts.getAccountInfo"
        headers = {
            "Content-Type": "application/x-www-form-urlencoded",
            "Cookie": self.cookie,
        }
        data = {
            "APIKey": self.api_key,
            "login_token": self.login_token,
            "include": "profile,data",
            "lang": "en",
            "sdk": "js_latest",
            "authMode": "cookie",
            "format": "json",
        }
        try:
            response = self.session.post(url, headers=headers, data=data)
            response.raise_for_status()
            return response.json()
        except requests.exceptions.RequestException as e:
            msg = f"Error fetching account info: {e}"
            logging.exception(msg)
            return None

    def _refresh_token(self, print_token: bool | None = None) -> None:
        """Refresh the access token using client credentials and signatures.

        Args:
        print_token (Optional[bool]): Whether to print the access token.
            Defaults to None.

        """
        account_info = self._get_account_info()
        if not account_info:
            logging.warning(
                "Failed to get account info.Cannot refresh token.",
            )
            return

        url = "https://api.nfl.com/identity/v3/token/refresh"
        payload = {
            "clientKey": self.clientKey,
            "clientSecret": self.clientSecret,
            "deviceId": self.deviceId,
            "deviceInfo": self.deviceInfo,
            "networkType": "other",
            "signatureTimestamp": account_info.get("signatureTimestamp"),
            "uid": account_info.get("UID"),
            "uidSignature": account_info.get("UIDSignature"),
            "refreshToken": self.refreshToken,
        }
        headers = {"Content-Type": "application/json"}
        try:
            response = self.session.post(
                url,
                headers=headers,
                json=payload,
            )
            response.raise_for_status()
            token_data = response.json()
            self.access_token = token_data.get("accessToken")
            self.expires_at = time.time() + token_data.get("expiresIn", 0)
            if print_token is not None:
                logging.info(self.access_token)
        except requests.exceptions.RequestException as e:
            msg = f"Error refreshing token: {e}"
            logging.exception(msg)

    def _make_api_call(
        self,
        endpoint_key: str,
        params: dict[str, Any],
        headers: dict[str, Any] | None = None,
        print_url: bool | None = None,
        json: dict[str, Any] | None = None,
    ) -> dict[str, Any] | None:
        """Centralized method to make a secured API call.

        Args:
            endpoint_key (str): Key for the endpoint configuration.
            params (Dict[str, Any]): Query parameters for the API call.
            headers (Optional[Dict[str, Any]]): Custom headers.
            print_url (Optional[bool]): Whether to print the URL.
            json (Optional[Dict[str, Any]]): JSON payload for POST.

        Returns:
            JSON response from the API, or None if error.

        """
        if not self.access_token or self.expires_at <= time.time():
            logging.info("Refreshing Token")
            self._refresh_token()
            if not self.access_token:
                logging.info("Failed to get a valid access token.")
                return None

        config = self.endpoints.get(endpoint_key, {})
        url = self.base_url + config.get("url", "")

        if print_url:
            logging.info(url)

        default_headers = {
            "Authorization": f"Bearer {self.access_token}",
            "Accept": "application/json, text/plain, */*",
        }

        # Update default headers with optional headers, if any
        if headers:
            default_headers.update(headers)

        params = {
            **params,
        }

        try:
            response = (
                self.session.post(
                    url,
                    headers=headers,
                    json=json,
                    params=params,
                )
                if json
                else self.session.get(
                    url,
                    headers=default_headers,
                    params=params,
                )
            )
            response.raise_for_status()
            return response.json()
        except requests.exceptions.RequestException as e:
            msg = f"Error fetching stats from {url}: {e}"
            logging.exception(msg)
            return None

    def get_play_film(  # noqa: PLR0915
        self,
        mcp_playback_id: str | int,
        params: dict[str, Any] | None = None,
        headers: dict[str, Any] | None = None,
        json: dict[str, Any] | None = None,
        *,
        print_url: bool = True,
        playlist_filename: str | bool = False,
        **kwargs: dict[str, str],
    ) -> dict[str, Any] | None:
        """Fetch Play Film data for a given MCP playback ID.

        Args:
            mcp_playback_id (Union[str, int]): The MCP playback ID.
            params (Optional[Dict[str, Any]]): Additional parameters for the
                request.
                Defaults to None.
            headers (Optional[Dict[str, Any]): Custom headers for the request.
                Defaults to None.
            json (Optional[Dict[str, Any]]): JSON payload for the request.
                Defaults to None.
            print_url (bool): Whether to print the request URL.
                Defaults to True.
            playlist_filename (Union[str, bool]): If a string, \
                the path to save the playlist; if True, saves \
                    with a default name.
                Defaults to False.
            **kwargs (Optional[Dict[str, Any]]): Additional keyword \
                arguments to pass to the API request.

        Returns:
            Optional[Dict[str, Any]]: The JSON response from the API, \
                or None if an error occurs.

        """
        if not self.access_token or self.expires_at <= time.time():
            self._refresh_token()
            if not self.access_token:
                logging.warning(
                    "Failed to get a valid access token. Cannot fetch stats.",
                )
                return None

        if isinstance(mcp_playback_id, int):
            mcp_playback_id = str(mcp_playback_id)

        config = self.endpoints.get("play_film", {})
        url = (
            "https://api.nfl.com"
            + config.get("url", "")
            + f"/{mcp_playback_id}"
        )

        if print_url:
            logging.info(url)

        default_headers = {
            "Authorization": f"Bearer {self.access_token}",
            "Accept": "application/json, text/plain, */*",
        }
        default_params: dict[str, Any] = {}

        # Update default headers with optional headers, if any
        if headers:
            default_headers.update(headers)

        if params:
            default_params.update(params)

        default_params.update(kwargs)

        try:
            response = (
                self.session.post(
                    url,
                    headers=default_headers,
                    json=json,
                    params=default_params,
                )
                if json
                else self.session.post(
                    url,
                    headers=default_headers,
                    params=default_params,
                )
            )
            response.raise_for_status()

            if playlist_filename:
                self._save_playlist(
                    response,
                    playlist_filename,
                    mcp_playback_id,
                )

            return response.json()

        except requests.exceptions.RequestException as e:
            msg = f"Error fetching stats from {url}: {e}"
            logging.exception(msg)
            return None

    def _save_playlist(
        self,
        response: requests.Response,
        playlist_filename: str | bool,
        mcp_playback_id: str,
    ) -> None:
        """Save the playlist content to a file."""
        if isinstance(playlist_filename, bool):
            playlist_filename = (
                f"playlist_{mcp_playback_id}.m3u8"  # Default filename
            )

        playlist_path = Path(playlist_filename)
        output_dir = playlist_path.parent

        if output_dir and not Path.exists(output_dir):
            Path.makedirs(output_dir, exist_ok=True)

        with Path(playlist_filename).open("w") as f:
            f.write(response.text)
        msg = f"Playlist saved to {playlist_filename}"
        logging.info(msg)

    def get_fantasy_stats(
        self,
        season: str = "2025",
        week: int | None = None,
        headers: dict[str, Any] | None = None,
        **kwargs: dict[str, str],
    ) -> dict[str, Any] | None:
        """Fetch fantasy stats for a given season and week.

        Args:
            season (str): The season for which to fetch stats. \
                Defaults to "2025".
            week (Optional[int]): The week for which to fetch stats. \
                Defaults to None.
            headers (Optional[Dict[str, Any]]): Custom headers for \
                the request.
                Defaults to None.
            **kwargs (Optional[Dict[str, Any]]): Additional keyword arguments.

        Returns:
            Optional[Dict[str, Any]]: The JSON response from the API, \
                or None if an error occurs.

        """
        endpoint_key = "fantasy_week" if week else "fantasy_season"
        params = {"season": season}
        if week:
            params["week"] = week
        params.update(kwargs)

        return self._make_api_call(endpoint_key, params, headers)

    def get_passing_stats(
        self,
        season: str = "2025",
        week: int | None = None,
        headers: dict[str, Any] | None = None,
        **kwargs: dict[str, str],
    ) -> dict[str, Any] | None:
        """Fetch passing stats for a given season and week.

        Args:
            season (str): The season for which to fetch stats. \
                Defaults to "2025".
            week (Optional[int]): The week for which to fetch stats. \
                Defaults to None.
            headers (Optional[Dict[str, Any]]): Custom headers for \
                the request. Defaults to None.
            **kwargs (Optional[Dict[str, Any]]): Additional keyword arguments.

        Returns:
            Optional[Dict[str, Any]]: The JSON response from the API, \
                or None if an error occurs.

        """
        params = {"season": season}
        if week:
            params["week"] = week
        params.update(kwargs)

        return self._make_api_call("passing_week", params, headers)

    def get_rushing_stats(
        self,
        season: str = "2025",
        week: int | None = None,
        headers: dict[str, Any] | None = None,
        **kwargs: dict[str, Any],
    ) -> dict[str, Any] | None:
        """Fetch rushing stats for a given season and week.

        Args:
            season (str): The season for which to fetch stats.
                Defaults to "2025".
            week (Optional[int]): The week for which to fetch stats.
                Defaults to None.
            headers (Optional[Dict[str, Any]]): Custom headers for \
                the request.
                Defaults to None.
            **kwargs (Optional[Dict[str, Any]]): Additional keyword arguments.

        Returns:
            Optional[Dict[str, Any]]: The JSON response from the API, \
                or None if an error occurs.

        """
        params = {"season": season}
        if week:
            params["week"] = week
        params.update(kwargs)

        return self._make_api_call("rushing_week", params, headers)

    def get_receiving_stats(
        self,
        season: str = "2025",
        week: int | None = None,
        headers: dict[str, Any] | None = None,
        **kwargs: dict[str, str],
    ) -> dict[str, Any] | None:
        """Fetch receiving stats for a given season and week.

        Args:
            season (str): The season for which to fetch stats. \
                Defaults to "2025".
            week (Optional[int]): The week for which to fetch stats.
                Defaults to None.
            headers (Optional[Dict[str, Any]]): Custom headers for \
                the request.
                Defaults to None.
            **kwargs (Optional[Dict[str, Any]]): Additional keyword arguments.

        Returns:
            Optional[Dict[str, Any]]: The JSON response from the API, \
                or None if an error occurs.

        """
        params = {"season": season}
        if week:
            params["week"] = week
        params.update(kwargs)

        return self._make_api_call("receiving_week", params, headers)

    def get_team_offense_stats(
        self,
        season: str = "2025",
        week: int | None = None,
        headers: dict[str, Any] | None = None,
        **kwargs: dict[str, str],
    ) -> dict[str, Any] | None:
        """Fetch team offense stats for a given season and week.

        Args:
            season (str): The season for which to fetch stats. \
                Defaults to "2025".
            week (Optional[int]): The week for which to fetch stats.
                Defaults to None.
            headers (Optional[Dict[str, Any]]): Custom headers for \
                the request.
                Defaults to None.
            **kwargs (Optional[Dict[str, Any]]): Additional keyword arguments.

        Returns:
            Optional[Dict[str, Any]]: The JSON response from the API, \
                or None if an error occurs.

        """
        params = {"season": season}
        if week:
            params["week"] = week
        params.update(kwargs)

        return self._make_api_call("team_offense_week", params, headers)

    def get_team_defense_stats(
        self,
        season: str = "2025",
        week: int | None = None,
        headers: dict[str, Any] | None = None,
        **kwargs: dict[str, str],
    ) -> dict[str, Any] | None:
        """Fetch team defense stats for a given season and week.

        Args:
            season (str): The season for which to fetch stats. \
                Defaults to "2025".
            week (Optional[int]): The week for which to fetch stats.
                Defaults to None.
            headers (Optional[Dict[str, Any]]): Custom headers for the request.
                Defaults to None.
            **kwargs (Optional[Dict[str, Any]]): Additional keyword arguments.

        Returns:
            Optional[Dict[str, Any]]: The JSON response from the API, \
                or None if an error occurs.

        """
        params = {"season": season}
        if week:
            params["week"] = week
        params.update(kwargs)

        return self._make_api_call("team_defense_week", params, headers)

    def get_weeks(
        self,
        season: str = "2025",
        headers: dict[str, Any] | None = None,
        **kwargs: dict[str, str],
    ) -> dict[str, Any] | None:
        """Fetch weeks for a specific season.

        Args:
            season (str): The season for which to fetch the schedule.
                Defaults to "2025".
            headers (Optional[Dict[str, Any]]): Custom headers for \
                the request.
                Defaults to None.
            **kwargs (Optional[Dict[str, Any]]): Additional keyword arguments.

        Returns:
            Optional[Dict[str, Any]]: The JSON response from the API, \
                or None if an error occurs.

        """
        params = {"season": season}
        params.update(kwargs)

        return self._make_api_call("weeks", params, headers)

    def get_schedule(
        self,
        season: str = "2025",
        season_type: str = "REG",
        week: int | None = None,
        headers: dict[str, Any] | None = None,
        **kwargs: dict[str, str],
    ) -> dict[str, Any] | None:
        """Fetch game schedule for a specific season, season type, and week.

        Args:
            season (str): The season for which to fetch the schedule.
                Defaults to "2025".
            season_type (str): The type of season \
                (e.g., "REG", "PRE", "POST").
                Defaults to "REG".
            week (Optional[int]): The week for which to fetch the schedule.
                Defaults to None.
            headers (Optional[Dict[str, Any]]): Custom headers for the request.
                Defaults to None.
            **kwargs (Optional[Dict[str, Any]]): Additional keyword arguments.

        Returns:
            Optional[Dict[str, Any]]: The JSON response from the API, \
                or None if an error occurs.

        """
        params = {"season": season, "seasonType": season_type}
        if week:
            params["week"] = week
        params.update(kwargs)

        return self._make_api_call("games", params, headers)

    def get_play_by_play_stats(
        self,
        game_id: str | None = None,
        **kwargs: dict[str, str],
    ) -> dict[str, Any] | None:
        """Fetch play-by-play stats for a specific game.

        Args:
            game_id (str): The ID of the game for which to fetch \
                play-by-play stats.
            **kwargs (Optional[Dict[str, Any]]): Additional keyword arguments.

        Returns:
            Optional[Dict[str, Any]]: The JSON response from the API, \
                or None if an error occurs.

        Raises:
            ValueError: If `game_id` is None.

        """
        if not game_id:
            msg = "game_id is a required parameter for play-by-play data."
            raise ValueError(msg)
        params = {"gameId": game_id}
        params.update(kwargs)

        return self._make_api_call("play_by_play", params)

    def get_play_summary(
        self,
        game_id: str | None = None,
        play_id: str | None = None,
        headers: dict[str, Any] | None = None,
        **kwargs: dict[str, str],
    ) -> dict[str, Any] | None:
        """Fetch a Play Summary.

        Args:
            game_id (str): the hashed NFL Pro Game ID
            play_id (str): the hashed NFL Pro Play ID
            headers (Dict[str, Any]): Custom headers for the API request.
                Defaults to None.
            **kwargs (Optional[Dict[str, Any]]): Additional keyword arguments.

        Returns:
            (Dict[str, Any): The JSON Response from the API or \
                None if an error occurs

        """
        params = {"gameId": game_id, "playId": play_id}
        params.update(kwargs)

        return self._make_api_call("play_summary", params, headers)

    def get_coaches(
        self,
        game_id: str | None = None,
        play_id: str | None = None,
        headers: dict[str, Any] | None = None,
        **kwargs: dict[str, str],
    ) -> dict[str, Any] | None:
        """Fetch Coaches film data for a specific game and play.

        Args:
            game_id (str) : the hashed NFL Pro Game ID
            play_id (str) : the hashed NFL Pro Play ID
            headers (Dict[str, Any]): Custom headers for the API request.
                Defaults to None.
            **kwargs (Optional[Dict[str, Any]]): Additional keyword arguments.

        Returns:
            Optional[Dict[str, Any]: The JSON response from the API, or None if
                an error occurs.

        """
        params = {"gameId": game_id, "playId": play_id}
        params.update(kwargs)

        return self._make_api_call("coaches", params, headers)

    def get_season_insights(
        self,
        season: str = "2025",
        tags: str | None = None,
        headers: dict[str, Any] | None = None,
        **kwargs: dict[str, str],
    ) -> dict[str, Any] | None:
        """Fetch season insights with specified tags.

        Args:
            season (str): The season for which to fetch insights.
                Defaults to "2025".
            tags (str): Comma-separated list of tags to filter insights.
                Defaults to None.
            headers (Dict[str, Any]): Custom headers for the API request.
                Defaults to None.
            **kwargs: Additional keyword arguments.

        Returns:
            Optional[Dict[str, Any]]: The JSON response from the API, \
                or None if an error occurs.

        """
        print_url = kwargs.pop("print_url", None)
        params = {"season": season}
        if tags:
            params["tags"] = tags
        params.update(kwargs)

        return self._make_api_call(
            "season_insights",
            params,
            headers,
            print_url,
        )

    def _raw_to_dataframe(
        self,
        data_list: list[dict[str, Any]],
        important_cols: list[str],
    ) -> pd.DataFrame:
        """Create and format a DataFrame.

        Args:
            data_list (List[Dict[str, Any]]): List of dictionaries containing
                the data.
            important_cols (List[str]): List of columns to prioritize in the
                DataFrame.

        Returns:
            pd.DataFrame: Formatted DataFrame.  Returns an empty DataFrame if
                input `data_list` is empty.

        """
        if not data_list:
            return pd.DataFrame()
        formatted_df = pd.DataFrame(data_list)
        remaining_cols = [
            col for col in formatted_df.columns if col not in important_cols
        ]
        ordered_cols = [
            col for col in important_cols if col in formatted_df.columns
        ] + remaining_cols
        return formatted_df[ordered_cols]

    def _pbp_to_dataframe(
        self,
        data_list: list[dict[str, Any]],
        important_cols: list[str],
    ) -> pd.DataFrame:
        """Convert nested play-by-play JSON data to a flat pandas DataFrame.

        Args:
            data_list (List[Dict[str, Any]): List of dictionaries containing
                play-by-play data.
            important_cols (List[str]): List of columns to prioritize in the
                DataFrame.

        Returns:
            pd.DataFrame: Flattened DataFrame. Returns an empty DataFrame if
                input `data_list` is empty.

        """
        if not data_list:
            return pd.DataFrame()

        formatted_df = pd.DataFrame(data_list)
        remaining_cols = [
            col for col in formatted_df.columns if col not in important_cols
        ]
        ordered_cols = [
            col for col in important_cols if col in formatted_df.columns
        ] + remaining_cols
        return formatted_df[ordered_cols]

    def stats_to_dataframe(
        self,
        stats_data: dict[str, Any] | None,
    ) -> pd.DataFrame:
        """Convert NFL stats JSON response to a pandas DataFrame.

        Args:
            stats_data (Optional[Dict[str, Any]]): JSON response containing
                NFL stats data.

        Returns:
            pd.DataFrame: A pandas DataFrame containing the stats data. Returns
                an empty DataFrame if input `stats data` is empty or the data
                format is unrecognized.

        """
        if not stats_data:
            return pd.DataFrame()

        formatted_df = None
        for endpoint_key in self.endpoints:
            config = self.endpoints[endpoint_key]
            data_key = config.get("data_key")

            if endpoint_key == "play_by_play":
                # formatted_df = self._pbp_to_dataframe(
                formatted_df = self.clean_play_by_play_data(
                    stats_data["plays"],
                    # config.get("important_cols", []),
                )
                break

            if endpoint_key == "coaches" and "items" in stats_data:
                formatted_df = self._raw_to_dataframe(
                    stats_data["items"],
                    config.get("important_cols", []),
                )
                break  # Exit loop after finding the data

            if endpoint_key in ["play_summary"] and stats_data:
                formatted_df = self._raw_to_dataframe(
                    stats_data,
                    config.get("important_cols", []),
                )
                break

            if data_key is None and endpoint_key == "season_insights":
                formatted_df = self._raw_to_dataframe(
                    stats_data,
                    config.get("important_cols", []),
                )
                break

            if data_key in stats_data:
                formatted_df = self._raw_to_dataframe(
                    stats_data[data_key],
                    config.get("important_cols", []),
                )
                break

        if formatted_df is None:
            logging.warning("Unrecognized stats data format.")
            return pd.DataFrame()

        return formatted_df

    def clean_weeks_data(
        self,
        weeks_dict: dict[str, Any],
    ) -> pd.DataFrame:
        """Flattens weeks JSON data into a pandas DataFrame.

        Args:
            weeks_dict (Dict[str, Any]): Dictionary containing weeks data
                    with a 'weeks' key.

        Returns:
            pd.DataFrame: Flattened DataFrame with schedule information.

        """
        weeks_list = weeks_dict[
            "weeks"
        ]  # Access the list of game dictionaries
        weeks_data: list[
            dict[str, Any]
        ] = []  # Initialize an empty list to hold the data

        for week in weeks_list:
            row: dict[str, Any] = {
                "season": week["season"],
                "season_type": week["seasonType"],
                "week": week["week"],
                "date_begin": week["dateBegin"],
                "date_end": week["dateEnd"],
                "week_type": week["weekType"],
                "week_slug": week["weekSlug"],
                "text": week["text"],
            }

            weeks_data.append(row)

        return pd.DataFrame(weeks_data)

    def clean_schedule_data(
        self,
        schedule_dict: dict[str, Any],
    ) -> pd.DataFrame:
        """Flattens schedule JSON data into a pandas DataFrame.

        Args:
            schedule_dict (Dict[str, Any]): Dictionary containing \
                schedule data with a 'games' key.

        Returns:
            pd.DataFrame: Flattened DataFrame with schedule information.

        """
        games_list = schedule_dict["games"]
        schedule_data = []

        for game in games_list:
            home_team, away_team, venue, external_ids = (
                game["homeTeam"],
                game["awayTeam"],
                game["venue"],
                game["externalIds"],
            )

            row = {
                "game_id_nflpro": game["id"],
                "date": game["date"],
                "time": game["time"],
                "season": game["season"],
                "season_type": game["seasonType"],
                "week": game["week"],
                "status": game["status"],
                "datetime_ampm": game["dateTimeAmPm"],
                "home_team_id": home_team["id"],
                "home_team_full_name": home_team["fullName"],
                "home_team_team_id": home_team["teamId"],
                "away_team_id": away_team["id"],
                "away_team_fullName": away_team["fullName"],
                "away_team_teamId": away_team["teamId"],
                "venue_id": venue["id"],
                "venue_name": venue["name"],
                "venue_city": venue["city"],
                "venue_country": venue["country"],
                "game_id_elias": external_ids[0]["id"],
                "game_id_gsis": external_ids[1]["id"],
            }
            schedule_data.append(row)

        return pd.DataFrame(schedule_data)

    def clean_coaches_data(
        self,
        coaches_dict: dict[str, Any],
    ) -> pd.DataFrame:
        """Flattens coaches film JSON data into a pandas DataFrame.

        Args:
            coaches_dict (Dict[str, Any]): Dictionary containing \
                coaches film data with an 'items' key.

        Returns:
            pd.DataFrame: Flattened DataFrame with coaches film information.

        """
        items_list = coaches_dict["items"]  # Access the list of video items
        coaches_data: list[dict[str, Any]] = []

        for item in items_list:
            row: dict[str, Any] = {
                "type": item["type"],
                "camera_source": item["cameraSource"],
                "mcp_playback_id": item["mcpPlaybackId"],
                "title": item["title"],
                "description": item["description"],
                "duration": item["duration"],
                "external_id": item["externalId"],
                "publish_date": item["publishDate"],
                "sub_type": item["subType"],
            }

            # Extract IDs
            ids = item["ids"]
            row["game_id"] = ids["gameId"]
            row["away_team_id"] = ids["awayTeamId"]
            row["home_team_id"] = ids["homeTeamId"]
            row["play_id"] = ids["playId"]

            # Extract thumbnail URL
            thumbnail = item["thumbnail"]
            row["thumbnail_url"] = thumbnail["thumbnailUrl"]

            coaches_data.append(row)

        return pd.DataFrame(coaches_data)

    def clean_play_by_play_data(
        self,
        pbp_dict: dict[str, Any],
    ) -> pd.DataFrame:
        """Flattens play-by-play JSON data into a pandas DataFrame.

        Args:
            pbp_dict (Dict[str, Any]): Dictionary containing play-by-play data
                    with a 'plays' key.

        Returns:
            pd.DataFrame: Flattened DataFrame with play-by-play information.
        """
        plays_list = pbp_dict.get(
            "plays",
            [],
        )  # Access the list of play dictionaries

        plays_data = list[
            dict[str, Any]
        ] = []  # Initialize an empty list to hold the data

        for play in plays_list:
            row: dict[str, Any] = {
                "gameId": play["gameId"],
                "playId": play["playId"],
                "playDescription": play["playDescription"],
                "possessionTeam": play["possessionTeam"],
                "down": play["down"],
                "yardsToGo": play["yardsToGo"],
                "quarter": play["quarter"],
            }

            plays_data.append(row)

        return pd.DataFrame(plays_data)
