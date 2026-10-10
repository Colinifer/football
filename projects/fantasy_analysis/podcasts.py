import os
from datetime import UTC, datetime

import feedparser
import requests
from dateutil import parser as date_parser


def fetch_podcast_episodes(rss_feeds, start_date, end_date, output_dir="downloads"):
    os.makedirs(output_dir, exist_ok=True)

    # Ensure our filter dates are timezone-aware (UTC)
    start_date = start_date.replace(tzinfo=UTC)
    end_date = end_date.replace(tzinfo=UTC)

    extracted_episodes = []

    for feed_url in rss_feeds:
        print(f"Parsing feed: {feed_url}")
        feed = feedparser.parse(feed_url)
        podcast_title = feed.feed.get("title", "Unknown_Podcast")

        for entry in feed.entries:
            # 1. Extract and parse the publication date
            if not hasattr(entry, "published"):
                continue

            pub_date = date_parser.parse(entry.published)

            # Normalize to UTC
            if pub_date.tzinfo is None:
                pub_date = pub_date.replace(tzinfo=UTC)
            else:
                pub_date = pub_date.astimezone(UTC)

            # 2. Filter by date range
            if start_date <= pub_date <= end_date:
                # 3. Locate the MP3 enclosure URL
                mp3_url = None
                if hasattr(entry, "links"):
                    for link in entry.links:
                        # Podcast audio is typically stored in 'enclosure' links
                        if link.get("rel") == "enclosure" and (
                            "audio" in link.get("type", "")
                            or link.get("href", "").endswith(".mp3")
                        ):
                            mp3_url = link.href
                            break

                if mp3_url:
                    episode_data = {
                        "podcast": podcast_title,
                        "title": entry.get("title", "Untitled"),
                        "date": pub_date.strftime("%Y-%m-%d"),
                        "mp3_url": mp3_url,
                    }
                    extracted_episodes.append(episode_data)
                    print(
                        f" -> Found: {episode_data['title']} ({episode_data['date']})"
                    )

    return extracted_episodes


# --- Usage Example ---
feeds = [
    "https://feeds.npr.org/500005/podcast.xml",  # NPR Planet Money
]

# Define your target window
start = datetime(2023, 9, 1)
end = datetime(2023, 9, 30)

episodes = fetch_podcast_episodes(feeds, start, end)

# Download the matching MP3s
for ep in episodes:
    # Clean the title for safe file saving
    safe_title = "".join(
        [c for c in ep["title"] if c.isalpha() or c.isdigit() or c == " "]
    ).rstrip()
    filename = f"downloads/{ep['date']}_{safe_title[:30].replace(' ', '_')}.mp3"

    print(f"\nDownloading to {filename}...")
    response = requests.get(ep["mp3_url"], stream=True, timeout=(5, 30))
    with open(filename, "wb") as f:
        for chunk in response.iter_content(chunk_size=8192):
            f.write(chunk)
