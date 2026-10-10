import re
from pathlib import Path

import feedparser
import requests
import yaml

# Base RSS directory
base_rss_dir = Path(__file__).parent / "rss"
if not base_rss_dir.exists():
    base_rss_dir = Path("rss")

config_path = base_rss_dir / "podcast_feeds.yaml"
output_dir = base_rss_dir
output_dir.mkdir(parents=True, exist_ok=True)
feeds_yaml_path = output_dir / "podcast_feeds.yaml"


def sanitize_filename(name: str) -> str:
    """Strip or replace characters not allowed in filenames."""
    return re.sub(r'[\\/*?:"<>|]', "", name).strip().replace(" ", "_")


def fetch_and_save_feed_urls() -> Path:
    """Resolves Apple Podcast IDs to RSS feed URLs and saves to YAML."""
    with open(config_path, encoding="utf-8") as f:
        config = yaml.safe_load(f)

    feed_records = []
    for podcast in config.get("podcasts", []):
        podcast_id = podcast["id"]
        podcast_name = podcast.get("name", "Unknown Podcast")
        print(f"Resolving: {podcast_name} (ID: {podcast_id})")

        lookup_url = f"https://itunes.apple.com/lookup?id={podcast_id}"
        try:
            response = requests.get(lookup_url, timeout=10)
            response.raise_for_status()
            data = response.json()
            results = data.get("results", [])
            if results:
                feed_url = results[0].get("feedUrl")
                feed_records.append(
                    {
                        "name": podcast_name,
                        "id": str(podcast_id),
                        "feed_url": feed_url,
                    }
                )
            else:
                print(f"  No results found for ID: {podcast_id}")
        except requests.RequestException as exc:
            print(f"  Error fetching lookup URL: {exc}")

    with open(feeds_yaml_path, "w", encoding="utf-8") as f:
        yaml.safe_dump(
            {"podcasts": feed_records}, f, sort_keys=False, allow_unicode=True
        )

    print(f"\nSaved {len(feed_records)} podcast links to {feeds_yaml_path}")
    return feeds_yaml_path


def download_latest_episodes(
    yaml_file: Path = feeds_yaml_path,
    download_dir: Path = Path(__file__).parent / "downloads",
) -> None:
    """Reads podcast feeds YAML and downloads the latest MP3 for each show."""
    download_dir.mkdir(parents=True, exist_ok=True)

    with open(yaml_file, encoding="utf-8") as f:
        data = yaml.safe_load(f)

    podcasts = data.get("podcasts", [])
    print(
        f"\nDownloading latest episodes for {len(podcasts)} podcasts into '{download_dir}'..."
    )

    for podcast in podcasts:
        name = podcast.get("name", "Unknown")
        feed_url = podcast.get("feed_url")
        if not feed_url:
            continue

        print(f"\nFetching feed: {name}")
        feed = feedparser.parse(feed_url)

        if not feed.entries:
            print("  No entries found in feed.")
            continue

        # Most recent episode is the first item in the feed
        latest_entry = feed.entries[0]
        episode_title = latest_entry.get("title", "latest_episode")

        # Find MP3 enclosure URL
        mp3_url = None
        for link in latest_entry.get("links", []):
            if link.get("rel") == "enclosure" and (
                "audio" in link.get("type", "") or link.get("href", "").endswith(".mp3")
            ):
                mp3_url = link.get("href")
                break

        # Fallback to enclosures property if links wasn't populated
        if not mp3_url and hasattr(latest_entry, "enclosures"):
            for enclosure in latest_entry.enclosures:
                if "audio" in enclosure.get("type", "") or enclosure.get(
                    "href", ""
                ).endswith(".mp3"):
                    mp3_url = enclosure.get("href")
                    break

        if not mp3_url:
            print(f"  Could not find audio enclosure for '{episode_title}'.")
            continue

        # Prepare clean filename
        clean_podcast = sanitize_filename(name)[:30]
        clean_title = sanitize_filename(episode_title)[:50]
        file_path = download_dir / f"{clean_podcast}__{clean_title}.mp3"

        if file_path.exists():
            print(f"  Already downloaded: {file_path.name}")
            continue

        print(f"  Downloading: {file_path.name}")
        try:
            with requests.get(mp3_url, stream=True, timeout=30) as r:
                r.raise_for_status()
                with open(file_path, "wb") as f_out:
                    for chunk in r.iter_content(chunk_size=1024 * 1024):  # 1MB chunks
                        if chunk:
                            f_out.write(chunk)
            print(f"  Saved: {file_path.name}")
        except requests.RequestException as exc:
            print(f"  Failed to download {mp3_url}: {exc}")


if __name__ == "__main__":
    # If feeds haven't been resolved yet, resolve them first
    if not feeds_yaml_path.exists():
        fetch_and_save_feed_urls()

    download_latest_episodes()
