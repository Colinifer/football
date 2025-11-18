# nflpro/film.py
"""Provide functionalities for downloading NFL play film footage.

It includes functions for:
- Downloading m3u8 playlists using the requests library.
- Parsing m3u8 playlists to extract the highest bandwidth stream URL.
- Downloading m3u8 streams using ffmpeg.
- Downloading play film footage based on identifiers like game ID, play ID,
  and possession team.

The module relies on the `requests` library for making HTTP requests,
the `pandas` library for data manipulation, and the `nflpro.nflapi` module
for interacting with the NFL Pro API. It also uses `ffmpeg` to download m3u8
streams.

Functions:
- download_playlist: Downloads an m3u8 playlist file using requests.
- get_highest_bandwidth_stream_url: Parses an m3u8 playlist to find the
  highest bandwidth stream URL.
- download_m3u8: Downloads an m3u8 stream using ffmpeg.
- download_play_film: Downloads play film footage based on provided
  identifiers.
"""

import logging
import re
import subprocess
from dataclasses import dataclass
from pathlib import Path

import pandas as pd
import requests

from .nflapi import NFLProAPI


def download_playlist(  # noqa: PLR0915
    m3u8_url: str,
    output_filename: str = "output.m3u8",
    print_response: bool | None = None,
) -> None:
    """Download an m3u8 playlist file using requests library.

    Check if the output directory exists and create one if it doesn't.

    Args:
        m3u8_url (str): The URL of the m3u8 playlist.
        output_filename (str): The desired filename for the downloaded playlist
            Defaults to 'output.m3u8'.
        print_response (Optional[bool]): Whether to print the response content.
            Defaults to None.

    Returns:
        None

    """
    try:
        # Extract the directory from the output filename
        path = Path(output_filename)
        output_dir = path.output_filename

        # Check if the directory exists, and create it if it doesn't
        if output_dir and not Path.exists(output_dir):
            Path.mkdir(
                output_dir,
                exist_ok=True,
            )  # exist_ok to avoid errors if it exists

        response = requests.get(m3u8_url, stream=True, timeout=10)
        response.raise_for_status()

        if print_response is not None:
            logging.info(response.content)

        # Additional checks on the response
        if not response.content:
            logging.warning("WARNING: Response content is empty.")
        else:
            msg = f"Downloaded {len(response.content)} bytes from {m3u8_url}"
            logging.debug(msg)

        with Path(output_filename).open("wb") as f:
            # Removed encoding since we're writing binary
            for chunk in response.iter_content(chunk_size=8192):
                f.write(chunk)  # Write the chunk directly

        msg = f"Successfully downloaded playlist from \
            {m3u8_url} to {output_filename}"
        logging.info(msg)

    except requests.exceptions.RequestException as e:
        msg = f"Error downloading m3u8 file: {e}"
        logging.exception(msg)
    except OSError as e:
        msg = f"OS error occurred: {e}"
        logging.exception(msg)
    except Exception as e:
        msg = f"An unexpected error occurred: {e}"
        logging.exception(msg)


def get_highest_bandwidth_stream_url(m3u8_content: str) -> str | None:  # noqa: PLR0915
    """Parse the m3u8 playlist content.

    Find the URL of the highest bandwidth stream.

    Args:
        m3u8_content (str): The content of the m3u8 master playlist \
            as a string.

    Returns:
        Optional[str]: The URL of the highest bandwidth stream, \
            or None if not found.

    """
    highest_bandwidth = 0
    best_stream_url = None

    #  Convert bytes to string if needed
    if isinstance(m3u8_content, bytes):
        m3u8_content = m3u8_content.decode("utf-8")

    # Split content into lines for easier parsing
    lines = m3u8_content.strip().split("\n")

    logging.info(lines)

    i = 0
    while i < len(lines):
        line = lines[i].strip()

        # Look for EXT-X-STREAM-INF tags
        if line.startswith("#EXT-X-STREAM-INF:"):
            # Extract bandwidth from the current line
            bandwidth_match = re.search(r"BANDWIDTH=(\d+)", line)

            if bandwidth_match:
                current_bandwidth = int(bandwidth_match.group(1))

                # The next non-empty, non-comment line should be the URL
                i += 1
                while i < len(lines):
                    next_line = lines[i].strip()
                    if next_line and not next_line.startswith("#"):
                        # Found the URL
                        if current_bandwidth > highest_bandwidth:
                            highest_bandwidth = current_bandwidth
                            best_stream_url = next_line
                        break
                    i += 1

        i += 1

    return best_stream_url


@dataclass
class DownloadOptions:
    """Encapsulate options for downloading the m3u8 file.

    Attributes:
        output_filename: The desired filename for the downloaded video.
            Defaults to "output.mp4".
        bufsize: Buffer size for ffmpeg.
            Defaults to "20M".
        overwrite: Overwrite flag for ffmpeg.
            Defaults to "-y".
        print_response: Whether to print the master playlist content.
            Defaults to None.
        print_stream: Whether to print the selected highest bandwidth \
            stream URL.
            Defaults to None.
        print_command: Whether to print the ffmpeg command (default: None).
        silent: Whether to suppress ffmpeg output.
            Defaults to True.

    """

    output_filename: str = "output.mp4"
    bufsize: str = "20M"
    overwrite: str = "-y"
    print_response: bool | None = None
    print_stream: bool | None = None
    print_command: bool | None = None
    silent: bool = True


def _run_ffmpeg(
    command: list[str],
    *,
    silent: bool,
) -> None:
    """Run the ffmpeg command with specified options."""
    try:
        subprocess.run(  # noqa: S603
            command,
            check=True,
            stdout=subprocess.DEVNULL if silent else None,
            stderr=subprocess.DEVNULL if silent else None,
        )
    except subprocess.CalledProcessError as e:
        msg = f"FFmpeg error: {e}"
        logging.exception(msg)
        raise


def download_m3u8(  # noqa: PLR0911, PLR0915
    m3u8_url: str,
    options: DownloadOptions,
) -> str | None:
    """Download an m3u8 playlist file using ffmpeg.

    Check if the output directory exists and creates it if it doesn't.

    Args:
        m3u8_url (str): The URL or filename of the m3u8 playlist.
        options: A DownloadOptions object containing download configurations.

    Returns:
        Optional[str]: The stream URL that was downloaded, \
            or None if an error occurred.

    """
    try:
        path = Path(options.output_filename)
        output_dir = path.parent
        if not Path.exists(output_dir):
            Path.mkdir(output_dir, exist_ok=True)

        response = requests.get(m3u8_url, timeout=10)
        response.raise_for_status()

        if options.print_response:
            msg = f"Master playlist content:\n{response.content.decode()}"
            logging.info(msg)

        stream_url = get_highest_bandwidth_stream_url(response.content)
        if not stream_url:
            logging.error("Could not find a stream in the master playlist.")
            return None

        stream_url = (
            stream_url
            if stream_url.startswith("http")
            else m3u8_url.rsplit("/", 1)[0] + "/" + stream_url
        )

        if options.print_stream:
            msg = f"Selected stream: {stream_url}"
            logging.info(msg)

        command = [
            "ffmpeg",
            options.overwrite,
            "-i",
            stream_url,
            "-c",
            "copy",
            "-bufsize",
            options.bufsize,
            options.output_filename,
        ]
        if options.print_command:
            msg = f"Running command: {' '.join(command)}"
            logging.info(msg)

        _run_ffmpeg(command, silent=options.silent)
        msg = f"Successfully downloaded \
            {m3u8_url} to {options.output_filename}"
        logging.info(msg)

    except requests.exceptions.RequestException as e:
        msg = f"Request error: {e}"
        logging.exception(msg)
    except FileNotFoundError:
        logging.exception(
            "ffmpeg not found. Is it installed and in your PATH?",
        )
        return None
    except Exception as e:
        msg = f"An unexpected error occurred: {e}"
        logging.exception(msg)
        return None

    else:
        return stream_url


def download_play_film(
    nfl_pro_api: NFLProAPI,
    coaches_df: pd.DataFrame,
    game_id_elias: str,
    game_id: str,
    posteam: str,
    play_id: str,
    save_playlist: bool | None = None,
) -> None:
    """Download play film footage.

    Args:
        nfl_pro_api (NFLProAPI): An instance of the NFLProAPI class.
        coaches_df (pd.DataFrame): DataFrame containing camera source and \
            playback IDs.
        game_id_elias (str): Elias game ID.
        game_id (str): nflfastR game ID.
        posteam (str): Posession team name.
        play_id (str): Play ID.
        save_playlist (Optional[bool]): Whether to save the m3u8 playlist file.
            Defaults to None.

    Returns:
        None

    """
    cameras = coaches_df["camera_source"]
    playback_ids = coaches_df["mcp_playback_id"]

    for camera, playback_id in zip(
        cameras,
        playback_ids,
        strict=False,
    ):
        msg = f"{camera}: {playback_id}"
        logging.info(msg)

        play_film_data = nfl_pro_api.get_play_film(
            mcp_playback_id=playback_id,
            print_url=True,
            playlist_filename=f"../playlists/{game_id_elias}-{play_id}.m3u8",
        )

        if play_film_data and "accessUrl" in play_film_data:
            m3u8_url = play_film_data["accessUrl"]

            output_playlist_file = (
                # f"../film_playlists/{game_id_elias}-{play_id}-{playback_id}-{camera}.m3u8"  # noqa: E501, ERA001
                f"../film_playlists/{game_id}-{posteam}-{play_id}-{camera}.m3u8"
            )
            output_video_file = (
                # f"../film/{game_id_elias}-{play_id}-{playback_id}-{camera}.mp4"  # noqa: E501, ERA001
                f"../film/{game_id}-{posteam}-{play_id}-{camera}.mp4"
            )

            if save_playlist:
                download_playlist(
                    m3u8_url,
                    output_playlist_file,
                )

            options = DownloadOptions(
                output_filename=output_video_file,
            )

            download_m3u8(
                m3u8_url,
                options=options,
            )


def download_film(
    nfl_pro_api: NFLProAPI,
    row: pd.Series,
    column_names: dict[str, str],
) -> None:
    """Download play film footage for a single play from a Pandas Series.

    Args:
        nfl_pro_api (NFLProAPI): An instance of the NFLProAPI class.
        row: A Pandas Series representing a single play.
        column_names: A dictionary mapping descriptive names to actual \
            column names.
            Required keys: "game_id", "old_game_id", "nfl_api_id", \
                "posteam", "play_id".

    Returns:
        None
    """
    game_id = row[column_names["game_id"]]
    game_elias = row[column_names["old_game_id"]]
    game_nflpro = row[column_names["nfl_api_id"]]
    posteam = row[column_names["posteam"]]
    play = row[column_names["play_id"]]

    logging.info(
        "Processing game_id: %s, play_id: %s",
        game_id,
        play,
    )

    coaches_data = nfl_pro_api.get_coaches(
        gameId=game_nflpro,
        playId=play,
    )

    if coaches_data is None:
        logging.warning(
            "No coaches data found for game_id: \
                %s, play_id: %s",
            game_nflpro,
            play,
        )
        return  # Skip to the next play

    coaches_df = nfl_pro_api.clean_coaches_data(coaches_data)

    download_play_film(
        nfl_pro_api=nfl_pro_api,
        coaches_df=coaches_df,
        game_id_elias=game_elias,
        game_id=game_id,
        posteam=posteam,
        play_id=play,
    )


def download_film_from_dataframe(
    nfl_pro_api: NFLProAPI,
    plays_df: pd.DataFrame,
    column_names: dict[str, str],
) -> None:
    """Download play film footage for each play in the given DataFrame.

    Args:
        nfl_pro_api (NFLProAPI): An instance of the NFLProAPI class.
        plays_df (pd.DataFrame): DataFrame containing play information.
        column_names (Dict[str, str]): A dictionary mapping descriptive \
            names to actual column names in plays_df.
            Required keys: "game_id", "old_game_id", "nfl_api_id", \
                "posteam", "play_id".
    """
    for _, row in plays_df.iterrows():
        download_film(
            nfl_pro_api,
            row,
            column_names,
        )
