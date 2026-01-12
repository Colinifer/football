# Fantasy Football Drafting App - Technical Specification

## 1. Application Overview

### Purpose
A Streamlit-based fantasy football drafting application that simulates a 16-round snake draft with customizable team rosters, keeper functionality, and real-time draft tracking.

### Key Features
- Snake draft order simulation (16 rounds)
- File upload system for draft configuration
- Interactive player selection and drafting
- Real-time availability tracking
- Keeper player management
- User team roster display

## 2. Data Architecture

### 2.1 Input Data Structures

#### Draft Order JSON Format
```json
{
  "teams": [
    {"team_name": "Team Alpha", "draft_position": 1},
    {"team_name": "Team Beta", "draft_position": 2},
    // ... up to 12 teams
  ],
  "total_rounds": 16
}
```

#### Player Data CSV Format
```csv
Rank,Player,Team,Bye,POS,Yahoo,Sleeper,RTSports,AVG,gsis_id,fantasypros_id,espn_id,position,headshot_url
1,Ja'Marr Chase,CIN,10,WR1,1,1,1,1.0,00-0036900,19788,4362628,WR,"https://static.www.nfl.com/image/upload/f_auto,q_auto/league/qkcb3qt2fhasfgtfuprd"
2,Saquon Barkley,PHI,9,RB1,2,2,4,2.7,00-0034844,17240,3929630,RB,"https://static.www.nfl.com/image/upload/f_auto,q_auto/league/ugiuanl8bf6uoya5mgid"
```

#### Keepers JSON Format
```json
{
  "keepers": [
    {
      "team_name": "Team Alpha",
      "player_name": "Christian McCaffrey",
      "round": 1,
    },
    {
      "team_name": "Team Beta",
      "player_name": "Josh Allen",
      "round": 2,
    }
  ]
}
```

### 2.2 Internal Data Structures

#### Draft Board State
```python
draft_state = {
    "current_round": int,
    "current_pick": int,
    "current_team": str,
    "total_picks_made": int,
    "available_players": pd.DataFrame,
    "drafted_players": list,
    "team_rosters": dict,
    "draft_order": list,
    "snake_pattern": list  # Pre-calculated draft order for all 16 rounds
}
```

## 3. User Interface Layout

### 3.1 Sidebar (File Upload & Controls)
- **Draft Order Upload**: File uploader for JSON draft configuration
- **Team Selection**: Allow the user to select from which team they will be drafting as. Should be a dropdown of the teams uploaded in the Draft Order json.
- **Player Data Upload**: File uploader for CSV player database
- **Keepers Upload**: Optional file uploader for keeper configuration
- **Draft Controls**:
  - Reset draft button
  - Export draft results button
  - Current pick indicator

### 3.2 Main Content Area
- **Draft Status Display**:
  - Current round/pick
  - Whose turn it is
  - Total picks remaining
- **Player Selection Dropdown**: 
  - Searchable dropdown containing all available players
  - Sorted by ADP (ascending)
  - Filtered to show only undrafted players
- **Draft Action Buttons**:
  - "Draft to [Current Team]" button
  - Undo button to delete the previous pick
  - Auto-advance to next pick option
- **Available Players Table**:
  - Sortable/filterable DataFrame display
  - Columns: Player Name, Position, Team, ADP, Bye Week
  - Click-to-draft functionality
  - Visual indicators for drafted players (grayed out/removed)

### 3.3 Right Panel (User Team Display)
- **My Team Roster**:
  - List of drafted players for user's team
  - Organized by position (QB, RB1, RB2, WR1, WR2, TE, FLEX (RB/WR/TE), DEF, K, Bench(7x) )
  - Round drafted indicator
  - Remove player option (undo functionality)
- **Team Summary**:
  - Position counts
  - Remaining picks for user
  - Next user pick indicator
- **Draft Selections**:
  - DataFrame of selected picks
  - Columns: 
    - Round, 
    - Pick #, 
    - Team, 
    - Player

## 4. Core Functionality

### 4.1 Draft Order Logic
- **Snake Pattern Calculation**:
  ```python
  def calculate_snake_order(teams, rounds=16):
      draft_order = []
      for round_num in range(1, rounds + 1):
          if round_num % 2 == 1:  # Odd rounds: ascending
              round_order = teams.copy()
          else:  # Even rounds: descending
              round_order = teams[::-1]
          draft_order.extend(round_order)
      return draft_order
  ```

### 4.2 Keeper Integration
- Pre-populate draft slots with keeper players
- Calculate the pick number given the team name and round number
- Remove keepler players from the avalable players
- Display keeper information in draft board

### 4.3 Player Management
- **Drafting Process**:
  1. Validate player availability
  2. Add player to team roster
  3. Remove player from available pool
  4. Advance to next pick
  5. Update UI state

### 4.4 Data Validation
- Ensure uploaded files match expected format
- Validate keeper players exist in player database
- Check for duplicate keeper assignments
- Verify draft order completeness

## 5. State Management

### 5.1 Session State Variables
```python
# Core draft data
st.session_state.draft_order = []
st.session_state.players_df = pd.DataFrame()
st.session_state.keepers_data = {}
st.session_state.team_rosters = {}
st.session_state.current_pick_index = 0
st.session_state.drafted_players = set()

# UI state
st.session_state.selected_player = None
st.session_state.draft_started = False
st.session_state.user_team_name = ""
```

### 5.2 State Persistence
- Save draft progress to browser session
- Export functionality for draft results
- Import capability for resuming drafts

## 6. Technical Implementation

### 6.1 Required Libraries
```python
import streamlit as st
import pandas as pd
import json
import io
from datetime import datetime
import plotly.express as px  # For optional visualizations
```

### 6.2 File Processing Functions
```python
def load_draft_order(uploaded_file):
    """Process uploaded draft order JSON"""
    
def load_players_data(uploaded_file):
    """Process uploaded player CSV"""
    
def load_keepers_data(uploaded_file):
    """Process uploaded keepers JSON"""
    
def validate_data_integrity():
    """Validate all uploaded data for consistency"""
```

### 6.3 Draft Logic Functions
```python
def initialize_draft():
    """Set up initial draft state"""
    
def draft_player(player_name, team_name):
    """Execute a player draft action"""
    
def get_current_team():
    """Return the team whose turn it is"""
    
def advance_pick():
    """Move to the next draft pick"""
    
def calculate_remaining_picks(team_name):
    """Calculate remaining picks for a team"""
```

## 7. User Experience Features

### 7.1 Visual Enhancements
- Color coding for different positions
- Progress bar for draft completion
- Highlighting for user's upcoming picks
- Visual feedback for successful actions

### 7.2 Quality of Life Features
- Auto-refresh player list after each pick
- Keyboard shortcuts for common actions
- Undo last pick functionality
- Search and filter capabilities
- Position-based player grouping

### 7.3 Export Capabilities
- Draft results as CSV
- Team rosters as formatted report
- Draft recap with pick-by-pick breakdown

## 8. Error Handling

### 8.1 File Upload Validation
- Check file formats and structure
- Handle missing or malformed data
- Provide clear error messages
- Graceful fallback options

### 8.2 Draft Validation
- Prevent double-drafting players
- Validate team assignments
- Handle edge cases (empty selections, etc.)
- Maintain data consistency

## 9. Performance Considerations

### 9.1 Data Optimization
- Efficient DataFrame operations
- Minimal recomputation of static data
- Optimized player search functionality
- Lazy loading where appropriate

### 9.2 UI Responsiveness
- Avoid unnecessary re-renders
- Cache expensive computations
- Progressive loading for large player datasets
- Responsive design elements

## 10. Future Enhancements

### 10.1 Advanced Features
- Trade simulation during draft
- AI draft recommendations
- Historical draft analysis
- Mock draft scenarios
- Real-time multiplayer support

### 10.2 Analytics Dashboard
- Draft grade calculations
- Position strength analysis
- ADP vs. actual pick comparisons
- Team composition visualizations

## 11. Testing Strategy

### 11.1 Unit Tests
- Data processing functions
- Draft logic validation
- State management functions
- File upload/parsing

### 11.2 Integration Tests
- End-to-end draft simulation
- Multi-team draft scenarios
- Keeper integration testing
- Edge case handling

### 11.3 User Acceptance Testing
- Complete draft workflow
- File upload processes
- UI responsiveness
- Error recovery scenarios