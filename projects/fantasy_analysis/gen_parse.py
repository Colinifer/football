import argparse
import os
import sys
import time
from pathlib import Path

from dotenv import find_dotenv, load_dotenv

# Load environment variables from .env (searches current and parent directories)
load_dotenv(find_dotenv())

# Verify the key is present before initializing clients
if not os.getenv("GEMINI_API_KEY") and not os.getenv("GOOGLE_API_KEY"):
    raise ValueError(
        "Missing API key. Set GEMINI_API_KEY or GOOGLE_API_KEY in your .env file."
    )

from google import genai
from langchain_core.messages import HumanMessage
from langchain_google_genai import ChatGoogleGenerativeAI

# Initialize clients (they will automatically read the loaded environment variable)
client = genai.Client()
llm = ChatGoogleGenerativeAI(model="gemini-2.5-flash")


def analyze_podcast(mp3_path: Path | str) -> str:
    path = Path(mp3_path)
    if not path.is_file():
        raise FileNotFoundError(f"Audio file not found: {path.resolve()}")

    print(f"Uploading {path.name} to Google servers...")
    # The File API supports up to 2GB per file
    audio_file = client.files.upload(file=str(path))

    print("Processing audio...")
    # Wait for the backend to extract audio features
    while audio_file.state == "PROCESSING":
        time.sleep(2)
        audio_file = client.files.get(name=audio_file.name)

    if audio_file.state == "FAILED":
        raise RuntimeError(f"Audio processing failed on Google servers: {audio_file}")

    print("Audio ready! Invoking LangChain...")

    message = HumanMessage(
        content=[
            {
                "type": "text",
                "text": (
                    "You are a strategic podcast analyst. Listen to this episode and:\n"
                    "1. Provide a structured transcript with speaker labels and timestamps.\n"
                    "2. Extract the top 3 core arguments or takeaways.\n"
                    "3. Summarize the overall sentiment."
                ),
            },
            {
                "type": "file",
                "file_id": audio_file.uri,
                "mime_type": "audio/mpeg",
            },
        ]
    )

    response = llm.invoke([message])
    return response.content


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Analyze a podcast MP3 file using Gemini and LangChain."
    )
    parser.add_argument(
        "mp3_path",
        type=Path,
        help="Path to the MP3 file to analyze (e.g., downloads/episode.mp3)",
    )
    parser.add_argument(
        "-o",
        "--output",
        type=Path,
        default=None,
        help="Optional path to save the analysis output (e.g., output.md)",
    )

    args = parser.parse_args()

    if not args.mp3_path.exists():
        print(f"Error: File not found at '{args.mp3_path}'", file=sys.stderr)
        sys.exit(1)

    result = analyze_podcast(args.mp3_path)

    if args.output:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text(result, encoding="utf-8")
        print(f"\nAnalysis saved to {args.output}")
    else:
        print("\n--- Analysis Result ---\n")
        print(result)


if __name__ == "__main__":
    main()
