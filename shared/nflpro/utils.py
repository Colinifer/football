# nflpro/utils.py
"""Utility functions for the nflpro package.

It currently includes the following functions:

- create_week_slug: Creates a week slug based on the week number \
    and season type.
"""

import logging


def create_week_slug(  # noqa: PLR0911
    week: dict[int, str],
    game_type: str,
) -> str | None:
    """Create a week slug based on the week number and season type.

    Args:
    ----
        week (int): The week number.
        game_type (str): The season type ("REG", "PRE", "POST", etc.).

    Returns:
    -------
        str: A string representing the week slug. If game_type is "REG",
             the format is "WEEK_{week}". Otherwise, returns the \
                "WEEK_{game_type}".

    Example:
    -------
        To create a new 'week_slug' column in a DataFrame:

        >>> df['week_slug'] = df.apply(
        ...     lambda row: create_week_slug(
        ...         row['week'],
        ...         row['game_type']
        ...     ),
        ...     axis=1
        ...)

    """
    try:
        if game_type == "REG":
            return f"WEEK_{week!s}"
        if game_type == "CON":
            return "CONF"
    except Exception as e:
        msg = f"Error processing week {week}, game_type {game_type}: {e}"
        logging.exception(msg)
        return None
    else:
        return f"{game_type}"
