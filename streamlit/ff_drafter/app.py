import streamlit as st
import pandas as pd
import json
from datetime import datetime

import io

# Set page configuration
st.set_page_config(
    page_title="Fantasy Football Draft Board", page_icon="🏈", layout="wide"
)

# Initialize session state variables if they don't exist
if "draft_order" not in st.session_state:
    st.session_state.draft_order = []
if "players_df" not in st.session_state:
    st.session_state.players_df = pd.DataFrame()
if "keepers_data" not in st.session_state:
    st.session_state.keepers_data = {}
if "team_rosters" not in st.session_state:
    st.session_state.team_rosters = {}
if "current_pick_index" not in st.session_state:
    st.session_state.current_pick_index = 0
if "drafted_players" not in st.session_state:
    st.session_state.drafted_players = set()
if "draft_started" not in st.session_state:
    st.session_state.draft_started = False
if "user_team_name" not in st.session_state:
    st.session_state.user_team_name = ""
if "snake_pattern" not in st.session_state:
    st.session_state.snake_pattern = []
if "keepers_processed" not in st.session_state:
    st.session_state.keepers_processed = False
if "keeper_map" not in st.session_state:
    st.session_state.keeper_map = {}


def load_draft_order(uploaded_file):
    """Process uploaded draft order JSON"""
    try:
        data = json.load(uploaded_file)
        if "teams" not in data:
            st.error("Invalid draft order file: 'teams' field is missing")
            return None
        return data
    except Exception as e:
        st.error(f"Error loading draft order file: {str(e)}")
        return None


def load_players_data(uploaded_file):
    """Process uploaded player CSV (flexible for different formats)"""
    try:
        df = pd.read_csv(uploaded_file)
        # Normalize column names to lowercase for easier mapping
        df.columns = [col.lower() for col in df.columns]
        # Map new/old columns to standard names used in the app
        col_map = {
            "player": "player",
            "Player": "player",
            "name": "player",
            "team": "team",
            "Team": "team",
            "position": "position",
            "pos": "position_rank",
            "position_rank": "position_rank",
            "avg": "average",
            "average": "average",
            "headshot_url": "headshot_url",
            "rank": "rank",
            "bye": "bye",
        }
        # Only keep columns that are mapped, but keep all for display
        mapped_cols = {col: col_map[col] for col in df.columns if col in col_map}
        df = df.rename(columns=mapped_cols)

        # Check for at least player, team, and position
        required_columns = [
            "player",
            "position",
        ]
        for col in required_columns:
            if col not in df.columns:
                st.error(f"Invalid players data: Missing required column '{col}'")
                return None
        return df
    except Exception as e:
        st.error(f"Error loading players data: {str(e)}")
        return None


def load_keepers_data(uploaded_file):
    """Process uploaded keepers JSON"""
    try:
        data = json.load(uploaded_file)
        if "keepers" not in data:
            st.error("Invalid keepers file: 'keepers' field is missing")
            return None
        return data
    except Exception as e:
        st.error(f"Error loading keepers file: {str(e)}")
        return None


def calculate_snake_order(teams, rounds=16):
    """Helper to calculate snake draft order"""
    draft_order = []
    for round_num in range(1, rounds + 1):
        if round_num % 2 == 1:
            round_order = teams.copy()
        else:
            round_order = teams[::-1]
        draft_order.extend(round_order)
    return draft_order


def process_keepers():
    """Process keeper assignments and update draft state"""
    if not st.session_state.keepers_data or st.session_state.keepers_processed:
        return
    
    keepers = st.session_state.keepers_data.get('keepers', [])
    if not keepers:
        return
    
    # Build a mapping: (round, team) -> player
    st.session_state.keeper_map = {(k['round'], k['team_name']): k['player_name'] for k in keepers}
    
    # Assign keepers to team rosters and drafted_players
    for k in keepers:
        team = k['team_name']
        player = k['player_name']
        round_num = k['round']
        
        # Try to get position from player DB
        pos = "?"
        if ('player' in st.session_state.players_df.columns and 
            'position' in st.session_state.players_df.columns and
            not st.session_state.players_df.empty):
            row = st.session_state.players_df[st.session_state.players_df['player'] == player]
            if not row.empty:
                pos = row.iloc[0]['position']
        
        # Initialize team roster if needed
        if team not in st.session_state.team_rosters:
            st.session_state.team_rosters[team] = []
        
        # Only add if not already present for this round
        already_kept = any(p == player and r == round_num for p, _, r in st.session_state.team_rosters[team])
        if not already_kept:
            st.session_state.team_rosters[team].append((player, pos, round_num))
            st.session_state.drafted_players.add(player)
    
    st.session_state.keepers_processed = True


def find_next_available_pick():
    """Find the next pick that isn't a keeper pick"""
    if not st.session_state.snake_pattern:
        return st.session_state.current_pick_index
    
    while st.session_state.current_pick_index < len(st.session_state.snake_pattern):
        pick_idx = st.session_state.current_pick_index
        round_num = (pick_idx // len(st.session_state.draft_order)) + 1
        team = st.session_state.snake_pattern[pick_idx]['team_name']
        
        # Check if this is a keeper pick
        if (round_num, team) in st.session_state.keeper_map:
            st.session_state.current_pick_index += 1
        else:
            break
    
    return st.session_state.current_pick_index


# Sidebar implementation
with st.sidebar:
    st.title("Draft Controls")

    # File uploaders section
    st.header("Setup Files")

    # Draft Order Upload
    draft_order_file = st.file_uploader(
        "Upload Draft Order (JSON)",
        type=["json"],
        help="Upload a JSON file containing draft order configuration",
    )
    if draft_order_file:
        draft_data = load_draft_order(draft_order_file)
        if draft_data:
            st.session_state.draft_order = draft_data["teams"]
            # Reset keepers processed flag when new draft order is loaded
            st.session_state.keepers_processed = False
            st.success("Draft order loaded successfully!")

    # Team Selection Dropdown (after draft order upload)
    team_names = (
        [team["team_name"] for team in st.session_state.draft_order]
        if st.session_state.draft_order
        else []
    )
    if team_names:
        st.session_state.user_team_name = st.selectbox(
            "Select Your Team",
            options=team_names,
            index=(
                team_names.index(st.session_state.user_team_name)
                if st.session_state.user_team_name in team_names
                else 0
            ),
            key="user_team_selectbox",
        )

    # Player Data Upload
    players_file = st.file_uploader(
        "Upload Player Database (CSV)",
        type=["csv"],
        help="Upload a CSV file containing player data",
    )
    if players_file:
        players_data = load_players_data(players_file)
        if players_data is not None:
            st.session_state.players_df = players_data
            # Reset keepers processed flag when new player data is loaded
            st.session_state.keepers_processed = False
            st.success("Player database loaded successfully!")

    # Keepers Upload (Optional)
    keepers_file = st.file_uploader(
        "Upload Keepers (JSON, Optional)",
        type=["json"],
        help="Upload a JSON file containing keeper player assignments",
    )
    if keepers_file:
        keepers_data = load_keepers_data(keepers_file)
        if keepers_data:
            st.session_state.keepers_data = keepers_data
            # Reset keepers processed flag when new keepers data is loaded
            st.session_state.keepers_processed = False
            st.success("Keepers loaded successfully!")

    # Draft Controls section
    st.header("Draft Status")

    if st.session_state.draft_started:
        # Calculate snake pattern if not set
        if (not st.session_state.snake_pattern or 
            len(st.session_state.snake_pattern) != len(st.session_state.draft_order) * 16):
            st.session_state.snake_pattern = calculate_snake_order(
                st.session_state.draft_order, rounds=16
            )

        # Process keepers when draft starts
        process_keepers()
        
        # Find next available pick (skip keeper picks)
        find_next_available_pick()

        current_round = (st.session_state.current_pick_index // len(st.session_state.draft_order)) + 1
        current_pick = (st.session_state.current_pick_index % len(st.session_state.draft_order)) + 1
        total_picks = len(st.session_state.draft_order) * 16
        remaining_picks = total_picks - st.session_state.current_pick_index

        # Show keeper info
        if st.session_state.keepers_data:
            keepers_count = len(st.session_state.keepers_data.get('keepers', []))
            st.info(f"📌 {keepers_count} keeper(s) loaded and processed")

        # Reset draft button
        if st.button("Reset Draft", type="secondary"):
            st.session_state.draft_started = False
            st.session_state.current_pick_index = 0
            st.session_state.drafted_players = set()
            st.session_state.team_rosters = {}
            st.session_state.keepers_processed = False
            st.session_state.keeper_map = {}

    else:
        # Check if we can start the draft
        can_start = (
            len(st.session_state.draft_order) > 0
            and not st.session_state.players_df.empty
        )

        if not can_start:
            st.warning("Please upload required files to start the draft")
        else:
            if st.button("Start Draft", type="primary"):
                st.session_state.draft_started = True

# Main content area
st.title("Fantasy Football Draft Board")

if not st.session_state.draft_started:
    st.info("Please upload required files and start the draft using the sidebar.")
else:
    # Process keepers if not already done
    process_keepers()
    
    # Filter available players (not drafted)
    drafted = st.session_state.drafted_players
    players_df = st.session_state.players_df
    if drafted:
        available_players = players_df[~players_df["player"].isin(drafted)]
    else:
        available_players = players_df.copy()

    # Sort by ADP (average column if available, else by rank)
    sort_col = None
    for col in ["average", "avg", "rank"]:
        if col in available_players.columns:
            sort_col = col
            break
    if sort_col:
        available_players = available_players.sort_values(by=sort_col, ascending=True)

    if st.session_state.draft_started:
        current_pick_index = st.session_state.current_pick_index
        total_picks = len(st.session_state.snake_pattern)
        current_round = (current_pick_index // len(st.session_state.draft_order)) + 1
        current_pick = (current_pick_index % len(st.session_state.draft_order)) + 1
        
        if current_pick_index < total_picks:
            current_team = st.session_state.snake_pattern[current_pick_index]["team_name"]
        else:
            current_team = "Draft Complete"
        
        picks_remaining = total_picks - current_pick_index
        
        # Calculate picks until user's next turn
        user_team = st.session_state.user_team_name
        picks_until_my_turn = None
        if user_team and current_pick_index < total_picks:
            for i, slot in enumerate(st.session_state.snake_pattern[current_pick_index:]):
                if slot["team_name"] == user_team:
                    picks_until_my_turn = i
                    break
        

        # Display metrics in columns
        col1, col2, col3, col4, col5 = st.columns([1, 1, 1, 1, 1, ])
        
        with col1:
            st.metric(
                label="Current Team",
                value=current_team[:12] + "..." if len(current_team) > 15 else current_team,
            )
        
        with col2:
            st.metric(
                label="Pick",
                value=f"{current_pick} of {len(st.session_state.draft_order)}",
                delta=f"Round: {current_round} of 16",
            )
        
        with col3:
            st.metric(
                label="Pick",
                value=f"{current_pick_index+1} of {total_picks}",
                delta=f"of {len(st.session_state.draft_order)}",
            )
        
        with col4:
            st.metric(
                label="Picks Remaining",
                value=picks_remaining,
                delta=f"{(picks_remaining/total_picks*100):.1f}% left" if picks_remaining > 0 else "Complete",
            )
        
        with col5:
            if picks_until_my_turn is not None:
                if picks_until_my_turn == 0:
                    st.metric(
                        label="My Turn",
                        value="NOW!",
                        delta="🔥",
                    )
                else:
                    st.metric(
                        label="Picks Until My Turn",
                        value=f"{picks_until_my_turn} picks away",
                        # delta=f"picks away",
                    )
            else:
                st.metric(
                    label="My Picks",
                    value="Done",
                    delta="No more picks",
                )
    
    
    # Layout: main content (left), right panel (user team display)
    main_col, right_col = st.columns([6, 4], gap="large")

    with main_col:

        st.subheader("Select a Player to Draft")
        
        # Player selection dropdown
        if "player" in available_players.columns:
            player_options = available_players["player"].tolist()
        else:
            str_cols = available_players.select_dtypes(include="object").columns
            player_options = available_players[str_cols[0]].tolist() if len(str_cols) > 0 else []
        
        def draft_player(player_name, team_name):
            """Execute a player draft action"""
            # Validate player availability
            if player_name in st.session_state.drafted_players:
                return f"{player_name} has already been drafted."
            
            # Get player info
            if "player" in st.session_state.players_df.columns:
                player_row = st.session_state.players_df[
                    st.session_state.players_df["player"] == player_name
                ]
            else:
                str_cols = st.session_state.players_df.select_dtypes(include="object").columns
                player_row = st.session_state.players_df[
                    st.session_state.players_df[str_cols[0]] == player_name
                ]
            
            if player_row.empty:
                return f"Player {player_name} not found in database."
            
            pos = player_row.iloc[0]["position"] if "position" in player_row.columns else "?"
            
            # Determine round
            round_num = (st.session_state.current_pick_index // len(st.session_state.draft_order)) + 1
            
            # Add to team roster
            if team_name not in st.session_state.team_rosters:
                st.session_state.team_rosters[team_name] = []
            st.session_state.team_rosters[team_name].append((player_name, pos, round_num))
            
            # Remove from available pool
            st.session_state.drafted_players.add(player_name)
            st.session_state.current_pick_index += 1
            
            # Skip any subsequent keeper picks
            find_next_available_pick()
            
            return f"Drafted {player_name} to {team_name} (Round {round_num})"

        def undo_last_pick():
            """Undo the last pick made"""
            if st.session_state.current_pick_index == 0:
                return "No picks to undo."
            
            # Move back one pick
            st.session_state.current_pick_index -= 1
            
            # Find the team that made the last pick
            last_team = st.session_state.snake_pattern[st.session_state.current_pick_index]["team_name"]
            
            if (last_team in st.session_state.team_rosters and 
                st.session_state.team_rosters[last_team]):
                
                # Find the most recent non-keeper pick for this team
                team_roster = st.session_state.team_rosters[last_team]
                for i in range(len(team_roster) - 1, -1, -1):
                    player, pos, round_num = team_roster[i]
                    # Check if this was a keeper pick
                    if (round_num, last_team) not in st.session_state.keeper_map:
                        # This was a regular pick, remove it
                        team_roster.pop(i)
                        st.session_state.drafted_players.discard(player)
                        return f"Undid pick: {player} from {last_team}"
                
                return "Last pick was a keeper and cannot be undone."
            
            return "No pick to undo for last team."
        

        selected_player = st.selectbox(
            "Available Players (sorted by ADP)",
            options=player_options,
            index=0 if player_options else None,
            key="player_select_box",
        )

        if st.session_state.current_pick_index < len(st.session_state.snake_pattern):
            current_team = st.session_state.snake_pattern[st.session_state.current_pick_index]["team_name"]
            
            col1, col2, col3, col4 = st.columns([2, 2, 2, 2])
            
            with col1:
                if st.button(f"Draft to {current_team}", key="draft_current_team", use_container_width=True):
                    draft_feedback = draft_player(selected_player, current_team)
                    st.success(draft_feedback)
                    st.rerun()
            
            with col2:
                if st.button("Undo", key="undo_last_pick", use_container_width=True):
                    undo_feedback = undo_last_pick()
                    st.info(undo_feedback)
                    st.rerun()


        st.subheader("Available Players")
        st.caption("Click a row below to select a player for drafting.")
        
        # Prepare display columns
        preferred_cols = ["rank", "headshot_url", "player", "position", "position_rank", "team", sort_col, "bye"]
        display_cols = [col for col in preferred_cols if col and col in available_players.columns]
        display_cols += [col for col in available_players.columns if col not in display_cols]
        
        def highlight_position(row):
            if "position" not in row:
                return [""] * len(row)
            if row["position"] == "QB":
                return ["background-color: #f2a19dff"] * len(row)
            elif row["position"] == "RB":
                return ["background-color: #b8caeaff"] * len(row)
            elif row["position"] == "WR":
                return ["background-color: #9df2a7ff"] * len(row)
            elif row["position"] == "TE":
                return ["background-color: #f2e09dff"] * len(row)
            else:
                return [""] * len(row)

        if not available_players.empty:
            styled_df = available_players.style.apply(highlight_position, axis=1)

            st.dataframe(
                styled_df,
                column_config={
                    "headshot_url": st.column_config.ImageColumn("", help="Player headshot"),
                    "team_url": st.column_config.ImageColumn("", help="Team logo"),
                },
                height=1000,
                use_container_width=True,
                hide_index=True,
                row_height=40,
            )
        else:
            st.info("All players have been drafted!")

        st.caption("Click a player above and use the draft button to make a pick. Use Undo to remove the previous pick.")

    with right_col:
        tab1, tab2 = st.tabs(["Draft Selections", "My Team",])

        with tab1:
            if st.session_state.snake_pattern:  # Only show if draft order is loaded
                # Generate the same picks_df as in the main display
                all_picks = []
                total_picks = len(st.session_state.snake_pattern)
                
                for pick_num in range(total_picks):
                    team = st.session_state.snake_pattern[pick_num]["team_name"]
                    round_num = (pick_num // len(st.session_state.draft_order)) + 1
                    overall_pick = pick_num + 1
                    
                    # Check if this was/is a keeper pick
                    if (round_num, team) in st.session_state.keeper_map:
                        player = st.session_state.keeper_map[(round_num, team)]
                        player_display = f"🔌 {player}"  # Mark keepers with pin emoji
                        is_keeper = True
                    elif pick_num < st.session_state.current_pick_index:
                        # This pick has been made (non-keeper)
                        team_roster = st.session_state.team_rosters.get(team, [])
                        player_display = "(undone)"
                        is_keeper = False
                        for player, pos, rnd in reversed(team_roster):
                            if rnd == round_num and (round_num, team) not in st.session_state.keeper_map:
                                player_display = player
                                break
                    else:
                        # Future pick (not yet made)
                        player_display = "-"
                        is_keeper = False
                    
                    all_picks.append({
                        "Round": round_num,
                        "Pick_Number": overall_pick,
                        "Team": team,
                        "Player": player_display,
                        "Is_Keeper": is_keeper,
                        "Draft_Status": "Completed" if pick_num < st.session_state.current_pick_index or (round_num, team) in st.session_state.keeper_map else "Pending"
                    })

            # Draft Selections Table
            st.subheader("Draft Selections")
            all_picks = []

            # Generate picks for all 16 rounds
            total_picks = len(st.session_state.snake_pattern)
            for pick_num in range(total_picks):
                team = st.session_state.snake_pattern[pick_num]["team_name"]
                round_num = (pick_num // len(st.session_state.draft_order)) + 1
                overall_pick = pick_num + 1
                
                # Check if this was/is a keeper pick
                if (round_num, team) in st.session_state.keeper_map:
                    player = st.session_state.keeper_map[(round_num, team)]
                    player_display = f"🔌 {player}"  # Mark keepers with pin emoji
                elif pick_num < st.session_state.current_pick_index:
                    # This pick has been made (non-keeper)
                    team_roster = st.session_state.team_rosters.get(team, [])
                    player_display = "(undone)"
                    for player, pos, rnd in reversed(team_roster):
                        if rnd == round_num and (round_num, team) not in st.session_state.keeper_map:
                            player_display = player
                            break
                else:
                    # Future pick (not yet made)
                    player_display = "-"
                
                all_picks.append({
                    "Round": round_num,
                    "Pick #": overall_pick,
                    "Team": team,
                    "Player": player_display,
                })

            if all_picks:
                export_df = pd.DataFrame(all_picks)
                
                # Create CSV buffer
                csv_buffer = io.StringIO()
                export_df.to_csv(csv_buffer, index=False)
                csv_data = csv_buffer.getvalue()
                
                # Generate filename with timestamp
                timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
                filename = f"draft_results_{timestamp}.csv"
                
                # Create download button - positioned above Draft Selections table
                st.download_button(
                    label="Download Draft Results",
                    data=csv_data,
                    file_name=filename,
                    mime="text/csv",
                    help="Download complete draft results including keepers and pending picks",
                    key="export_draft_csv_main",
                    icon=":material/download:",
                )
                
                # # Show preview of what will be exported
                # with st.expander("Preview Export Data"):
                #     st.dataframe(export_df.head(20), use_container_width=True)
                #     if len(export_df) > 20:
                #         st.caption(f"Showing first 20 rows of {len(export_df)} total picks")

                picks_df = pd.DataFrame(all_picks)
                
                # Highlight current pick
                def highlight_current_pick(row):
                    if row.name == st.session_state.current_pick_index:
                        return ["background-color: #e8f5e8; font-weight: bold"] * len(row)  # Light green highlight for current pick
                    elif row["Player"].startswith("🔌"):
                        return ["background-color: #ffffcc"] * len(row)  # Yellow for keepers
                    elif row["Player"] == "-":
                        return ["color: #888888"] * len(row)  # Gray for future picks
                    return [""] * len(row)
                
                styled_picks_df = picks_df.style.apply(highlight_current_pick, axis=1)
                st.dataframe(
                    styled_picks_df, 
                    hide_index=True, 
                    use_container_width=True, 
                    height=800,
                )
                st.caption("🔌 indicates keeper picks | Yellow highlight shows current pick | Gray text shows future picks")
            else:
                st.info("No draft order available.")
        

        with tab2:
            st.header("My Team Roster")
            user_team = st.session_state.user_team_name or "(Not Set)"
            roster = st.session_state.team_rosters.get(user_team, [])
            
            if roster:
                # Sort roster by round
                sorted_roster = sorted(roster, key=lambda x: x[2])  # Sort by round
                roster_df = pd.DataFrame(sorted_roster, columns=["Player", "Position", "Round"])
                
                # Highlight keeper picks
                def highlight_keepers(row):
                    player_name = row["Player"]
                    round_num = row["Round"]
                    if (round_num, user_team) in st.session_state.keeper_map:
                        return ["background-color: #ffffcc"] * len(row)  # Light yellow for keepers
                    return [""] * len(row)
                
                styled_roster = roster_df.style.apply(highlight_keepers, axis=1)
                st.dataframe(styled_roster, hide_index=True, use_container_width=True)
                
                # Show keeper legend
                keeper_count = sum(1 for p, pos, r in roster if (r, user_team) in st.session_state.keeper_map)
                if keeper_count > 0:
                    st.caption(f"📌 {keeper_count} keeper(s) highlighted in yellow")
                
                # Undo button for user's team
                if st.button("Undo Last Pick (My Team)", key="undo_my_team"):
                    if roster:
                        # Find the most recent non-keeper pick
                        for i in range(len(roster) - 1, -1, -1):
                            player, pos, round_num = roster[i]
                            if (round_num, user_team) not in st.session_state.keeper_map:
                                roster.pop(i)
                                st.session_state.drafted_players.discard(player)
                                st.info(f"Undid pick: {player} from {user_team}")
                                st.rerun()
                                break
                        else:
                            st.warning("No non-keeper picks to undo.")
            else:
                st.info("No players drafted to your team yet.")
            
            st.markdown("---")
            st.subheader("Team Summary")

            if roster:
                pos_counts = pd.Series([pos for _, pos, _ in roster]).value_counts()
                st.write("**Position Counts:**")
                st.write(pos_counts)
                st.write(f"**Total Picks for User:** {len(roster)}")
                
                # Remaining picks for user
                picks_left = 16 - len(roster)
                st.write(f"**Remaining Picks for User:** {picks_left}")
                
                # Next user pick indicator
                next_picks = [
                    i for i, slot in enumerate(st.session_state.snake_pattern[st.session_state.current_pick_index:])
                    if slot["team_name"] == user_team
                ]
                if next_picks:
                    next_pick_num = st.session_state.current_pick_index + next_picks[0] + 1
                    st.write(f"**Next User Pick #:** {next_pick_num}")
                else:
                    st.write("No more picks for your team.")
            else:
                st.write("No picks yet.")
            
            st.markdown("---")

