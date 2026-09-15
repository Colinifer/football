import requests
from urllib.parse import urlencode, urljoin
from typing import Dict, Any

def fp_rankings(page: str, sport: str = None, include_metadata: bool = None, **kwargs) -> Dict[str, Any]:
    """
    Retrieves fantasy sports rankings from FantasyPros.

    Args:
    - page (str): The type of rankings page to retrieve (e.g., 'consensus', 'expert', etc.)
    - sport (str, optional): The sport to retrieve rankings for. Defaults to None, which uses the 'ffpros.sport' option.
    - include_metadata (bool, optional): Whether to include metadata in the response. Defaults to None, which uses the 'ffpros.include_metadata' option.
    - **kwargs: Additional query parameters to include in the request.

    Returns:
    - Dict[str, Any]: The parsed rankings data.
    """

    # Set default sport and include_metadata if not provided
    if sport is None:
        sport = get_option('ffpros.sport')
    if include_metadata is None:
        include_metadata = get_option('ffpros.include_metadata')

    # Validate sport and include_metadata
    valid_sports = ['nfl', 'mlb', 'nba', 'nhl']
    if sport not in valid_sports:
        raise ValueError(f"Invalid sport: {sport}. Must be one of: {', '.join(valid_sports)}")
    if not isinstance(include_metadata, bool):
        raise ValueError(f"Invalid include_metadata: {include_metadata}. Must be a boolean.")

    # Construct the URL
    base_url = f"https://www.fantasypros.com/{sport}/rankings/{page}.php"
    url = urljoin(base_url, '?' + urlencode(kwargs))

    # Send the request
    response = requests.get(url)

    # Parse the response
    parsed_rankings = fp_rankings_parse(response.json())

    # Return the parsed rankings, excluding metadata if requested
    if not include_metadata:
        return parsed_rankings['ecr']
    return parsed_rankings


def get_option(option_name: str) -> Any:
    # Replace this with your actual option storage and retrieval mechanism
    options = {
        'ffpros.sport': 'nfl',
        'ffpros.include_metadata': True
    }
    return options.get(option_name)


def fp_rankings_parse(response_data: Dict[str, Any]) -> Dict[str, Any]:
    # Replace this with your actual parsing logic
    return response_data