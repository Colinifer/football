# Fantasy Football Draft Board

A Streamlit-based interactive draft board application for managing fantasy football drafts with support for snake drafts, keeper leagues, and real-time draft tracking.

## Features

- **Snake Draft Support**: Automatically calculates draft order for snake-style drafts (16 rounds)
- **Keeper League Integration**: Pre-populate keeper picks and skip them during the draft
- **Real-time Draft Status**: Live metrics showing current round, pick, team on clock, and picks until your turn
- **Player Database**: Upload your own player rankings with customizable columns
- **Export Functionality**: Download complete draft results as CSV
- **Position-based Player Highlighting**: Color-coded player table by position (QB, RB, WR, TE)
- **Undo Functionality**: Reverse picks if mistakes are made
- **Team Roster Tracking**: Individual team views with position summaries

## Requirements

```
streamlit
pandas
```

## Installation

1. Clone or download the repository
2. Install required packages:
   ```bash
   pip install streamlit pandas
   ```
3. Run the application:
   ```bash
   streamlit run app.py
   ```

## Setup Files

The application requires specific file formats for setup:

### 1. Draft Order (JSON) - Required
```json
{
  "teams": [
    {"team_name": "Team 1"},
    {"team_name": "Team 2"},
    {"team_name": "Team 3"}
  ]
}
```

### 2. Player Database (CSV) - Required
The CSV should contain at minimum:
- `player` or `name`: Player name
- `position`: Player position (QB, RB, WR, TE, etc.)

Optional columns that enhance functionality:
- `team`: NFL team
- `rank` or `average`: For sorting players
- `headshot_url`: Player photos
- `bye`: Bye week information

### 3. Keepers (JSON) - Optional
```json
{
  "keepers": [
    {
      "team_name": "Team 1",
      "player_name": "Patrick Mahomes",
      "round": 3
    }
  ]
}
```

## How to Use

### Initial Setup
1. **Upload Draft Order**: JSON file with team names
2. **Select Your Team**: Choose your team from the dropdown
3. **Upload Player Database**: CSV file with player data
4. **Upload Keepers** (Optional): JSON file with keeper assignments
5. **Start Draft**: Click the "Start Draft" button

### During the Draft
1. **Monitor Status**: View real-time metrics at the top showing current pick, your next turn, etc.
2. **Select Player**: Use the dropdown to choose from available players
3. **Draft Player**: Click the "Draft to [Team Name]" button
4. **Undo Mistakes**: Use the "Undo" button to reverse the last pick
5. **Track Progress**: View your team roster and all picks in the side panel

### Export Results
- Click "Download Draft Results CSV" above the draft selections table
- File includes all picks, keepers, and pending selections
- Timestamped filename prevents overwriting

## Interface Layout

- **Left Panel (Main)**: Player selection, available players table, draft controls
- **Right Panel**: Your team roster, team summary, draft selections history
- **Top Metrics**: Real-time draft status with 6 key metrics
- **Sidebar**: File uploads, team selection, draft controls

## Key Features Explained

### Snake Draft Pattern
The app automatically calculates snake draft order:
- Round 1: Teams 1, 2, 3, 4...
- Round 2: Teams ..., 4, 3, 2, 1
- Round 3: Teams 1, 2, 3, 4...
- And so on...

### Keeper Integration
- Keeper picks are pre-populated in designated rounds
- Draft automatically skips keeper slots
- Keepers are highlighted with 📌 emoji and special background colors
- Cannot undo keeper picks

### Position Color Coding
- **QB**: Light red background
- **RB**: Light blue background  
- **WR**: Light green background
- **TE**: Light yellow background

## File Structure

```
app.py                 # Main application file
README.md             # This file
draft_order.json      # Example draft order file
players.csv           # Example player database
keepers.json          # Example keepers file
```

## Troubleshooting

**"Invalid draft order file"**: Ensure JSON has a "teams" array with "team_name" fields

**"Missing required column"**: Player CSV must have "player" and "position" columns

**Players not sorting properly**: Add "rank", "average", or "avg" column to CSV

**Keepers not appearing**: Verify team names in keepers.json match draft_order.json exactly

## Contributing

This is a single-file Streamlit application. To modify:
1. Edit `app.py` directly
2. Test changes with `streamlit run app.py`
3. The app uses session state to maintain draft progress

## License

Open source - feel free to modify and distribute.