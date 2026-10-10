import argparse
import os
import re
import unicodedata
from difflib import SequenceMatcher

import pandas as pd
from dotenv import load_dotenv
from sqlalchemy import create_engine, text

# Load database config
dotenv_path = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", ".env"))
load_dotenv(dotenv_path=dotenv_path)

# Team abbreviation normalization map
TEAM_MAP = {
    # FantasyPros / NFL aliases
    "GBP": "GB",
    "KCC": "KC",
    "NEP": "NE",
    "NOS": "NO",
    "OAK": "LV",
    "SFO": "SF",
    "TBB": "TB",
    "JAX": "JAC",
    "WSH": "WAS",
    "LA": "LAR",
    "SD": "LAC",-
    "STL": "LAR",
    "KAN": "KC",
    "GNB": "GB",
    "NWE": "NE",
    "NOR": "NO",
    "TAM": "TB",
}

SUFFIXES_PATTERN = re.compile(r"\s+(jr\.?|sr\.?|iii|iv|ii|v)$", re.IGNORECASE)
PUNCT_PATTERN = re.compile(r"[^a-z0-9]")


def strip_accents(s: str) -> str:
    return "".join(c for c in unicodedata.normalize("NFD", s) if unicodedata.category(c) != "Mn")


def normalize_team(team_str: str | None) -> str:
    if not team_str or pd.isna(team_str):
        return ""
    cleaned = str(team_str).strip().upper()
    return TEAM_MAP.get(cleaned, cleaned)


def normalize_name(name_str: str | None) -> str:
    if not name_str or pd.isna(name_str):
        return ""
    name = strip_accents(str(name_str)).strip().lower()
    name = SUFFIXES_PATTERN.sub("", name)
    name = PUNCT_PATTERN.sub("", name)
    return name


def normalize_birthdate(b_str: str | None) -> str:
    if not b_str or pd.isna(b_str):
        return ""
    # Standardize to YYYY-MM-DD
    s = str(b_str).strip()
    if not s:
        return ""
    try:
        dt = pd.to_datetime(s)
        return dt.strftime("%Y-%m-%d")
    except Exception:
        return s[:10]


POS_MAP = {
    "QB": {"QB"},
    "RB": {"RB", "FB", "HB"},
    "FB": {"RB", "FB", "HB"},
    "WR": {"WR"},
    "TE": {"TE", "H-B"},
    "K": {"K", "PK"},
    "PK": {"K", "PK"},
    "DST": {"DST", "DEF"},
    "DEF": {"DST", "DEF"},
}


def positions_compatible(pos1: str | None, pos2: str | None) -> bool:
    if not pos1 or not pos2:
        return True  # Don't strictly rule out if missing
    p1 = str(pos1).strip().upper()
    p2 = str(pos2).strip().upper()
    if p1 == p2:
        return True
    valid1 = POS_MAP.get(p1, {p1})
    valid2 = POS_MAP.get(p2, {p2})
    return bool(valid1.intersection(valid2))


NICKNAMES = {
    "gabe": "gabriel",
    "gabriel": "gabe",
    "mitch": "mitchell",
    "mitchell": "mitch",
    "ken": "kenneth",
    "kenneth": "ken",
    "cam": "cameron",
    "cameron": "cam",
    "hollywood": "marquise",
    "marquise": "hollywood",
    "chark": "dj",
    "dj": "chark",
    "jeff": "jeffrey",
    "jeffrey": "jeff",
    "josh": "joshua",
    "joshua": "josh",
    "mike": "michael",
    "michael": "mike",
    "chris": "christopher",
    "christopher": "chris",
    "matt": "matthew",
    "matthew": "matt",
    "drew": "andrew",
    "andrew": "drew",
    "nate": "nathaniel",
    "nathaniel": "nate",
    "sam": "samuel",
    "samuel": "sam",
    "will": "william",
    "william": "will",
    "dan": "daniel",
    "daniel": "dan",
    "alex": "alexander",
    "alexander": "alex",
    "tony": "anthony",
    "anthony": "tony",
}


def parse_name_parts(name_str: str | None) -> tuple[str, str]:
    if not name_str or pd.isna(name_str):
        return "", ""
    cleaned = strip_accents(SUFFIXES_PATTERN.sub("", str(name_str).strip()))
    parts = cleaned.split()
    if len(parts) == 0:
        return "", ""
    if len(parts) == 1:
        return PUNCT_PATTERN.sub("", parts[0].lower()), ""
    first = PUNCT_PATTERN.sub("", parts[0].lower())
    last = PUNCT_PATTERN.sub("", " ".join(parts[1:]).lower())
    return first, last


def is_name_match(first1: str, last1: str, first_candidates: list[str], last2: str, same_team: bool) -> tuple[bool, float]:
    if not last1 or not last2:
        return False, 0.0

    last_sim = SequenceMatcher(None, last1, last2).ratio()
    if last_sim < 0.85:
        return False, 0.0
        
    best_match = False
    best_score = 0.0

    # Filter out empty strings from candidates
    valid_candidates = [c for c in first_candidates if c]
    if not valid_candidates:
        return False, 0.0

    for first2 in valid_candidates:
        # Exact full match
        if first1 == first2 and last1 == last2:
            return True, 1.0

        # Nickname check
        if NICKNAMES.get(first1) == first2 or NICKNAMES.get(first2) == first1:
            best_match = True
            best_score = max(best_score, 0.95)

        # Prefix match if one is a true prefix of the other with length >= 3
        elif len(first1) >= 3 and len(first2) >= 3:
            if first1.startswith(first2) or first2.startswith(first1):
                first_sim = SequenceMatcher(None, first1, first2).ratio()
                best_match = True
                best_score = max(best_score, round((first_sim + last_sim) / 2.0, 3))

        # Initial matching on same team
        elif same_team and (len(first1) <= 2 or len(first2) <= 2):
            if first1.startswith(first2) or first2.startswith(first1):
                best_match = True
                best_score = max(best_score, 0.85)

    return best_match, best_score


def get_db_engine():
    host = os.getenv("DB_HOST_LOCAL", "localhost")
    port = os.getenv("DB_PORT_LOCAL", "5432")
    database = os.getenv("DB_NAME", "football")
    user = os.getenv("DB_USER", "postgres")
    password = os.getenv("DB_PASS", "")

    conn_str = f"postgresql://{user}:{password}@{host}:{port}/{database}"
    return create_engine(conn_str)


def load_data(engine, week: int | None = 0, match_all_players: bool = False):
    # 1. FantasyPros Projections (if matching against weekly projections)
    if not match_all_players:
        if week is not None:
            proj_query = text("""
                SELECT DISTINCT ON (fpid)
                    fpid,
                    name,
                    season,
                    week,
                    _fetched_at
                FROM "ffpros_projections"
                WHERE week = :week
                ORDER BY fpid, _fetched_at DESC
            """)
            params = {"week": week}
        else:
            proj_query = text("""
                SELECT DISTINCT ON (fpid)
                    fpid,
                    name,
                    season,
                    week,
                    _fetched_at
                FROM "ffpros_projections"
                ORDER BY fpid, _fetched_at DESC
            """)
            params = {}
    else:
        proj_query = None
        params = {}

    # 2. FantasyPros Players (metadata: player_id, team, position, birthdate, etc.)
    players_query = text("""
        SELECT 
            player_id,
            player_name,
            position_id,
            team_id as team,
            birthdate,
            sportsdata_player_id
        FROM "ffpros_players"
    """)

    # 3. nflfastR players (canonical player registry & IDs)
    nfl_query = text("""
        SELECT 
            gsis_id,
            espn_id,
            display_name as nfl_name,
            common_first_name as nfl_common_first,
            first_name as nfl_first,
            last_name as nfl_last,
            football_name as nfl_football_name,
            position as nfl_pos,
            position_group as nfl_pos_group,
            latest_team as nfl_team,
            status as nfl_status,
            pfr_id,
            pff_id,
            esb_id,
            nfl_id,
            smart_id,
            otc_id,
            birth_date as nfl_birth_date,
            rookie_season,
            last_season
        FROM "nflfastR_players"
    """)

    with engine.connect() as conn:
        df_proj = pd.read_sql(proj_query, conn, params=params) if proj_query is not None else None
        df_fp = pd.read_sql(players_query, conn)
        df_nfl = pd.read_sql(nfl_query, conn)

    return df_proj, df_fp, df_nfl


def build_nfl_candidates(df_nfl: pd.DataFrame) -> list[dict]:
    candidates = []
    for idx, row in df_nfl.iterrows():
        disp_name = str(row["nfl_name"]) if pd.notna(row["nfl_name"]) else ""
        norm_disp = normalize_name(disp_name)
        p_first, p_last = parse_name_parts(disp_name)

        firsts = {p_first} if p_first else set()
        if pd.notna(row["nfl_common_first"]) and str(row["nfl_common_first"]).strip():
            firsts.add(normalize_name(str(row["nfl_common_first"])))
        if pd.notna(row["nfl_football_name"]) and str(row["nfl_football_name"]).strip():
            firsts.add(normalize_name(str(row["nfl_football_name"])))
        if pd.notna(row["nfl_first"]) and str(row["nfl_first"]).strip():
            firsts.add(normalize_name(str(row["nfl_first"])))
        firsts.discard("")

        lasts = {p_last} if p_last else set()
        if pd.notna(row["nfl_last"]) and str(row["nfl_last"]).strip():
            norm_l = normalize_name(str(row["nfl_last"]))
            lasts.add(norm_l)
            # Add unhyphenated variant
            lasts.add(norm_l.replace("-", ""))
        lasts.discard("")

        full_names = {norm_disp} if norm_disp else set()
        for f in firsts:
            for l in lasts:
                full_names.add(f"{f}{l}")
                full_names.add(f"{f} {l}")

        cand = {
            "index": idx,
            "row": row,
            "gsis_id": row["gsis_id"],
            "espn_id": row["espn_id"],
            "pfr_id": row["pfr_id"],
            "pff_id": row["pff_id"],
            "esb_id": row["esb_id"],
            "nfl_id": row.get("nfl_id"),
            "smart_id": row.get("smart_id"),
            "otc_id": row.get("otc_id"),
            "nfl_name": row["nfl_name"],
            "nfl_pos": str(row["nfl_pos"]).upper() if pd.notna(row["nfl_pos"]) else "",
            "nfl_team": normalize_team(row["nfl_team"]),
            "nfl_status": str(row["nfl_status"]) if pd.notna(row["nfl_status"]) else "",
            "birth_date": normalize_birthdate(row["nfl_birth_date"]),
            "rookie_season": int(row["rookie_season"]) if pd.notna(row["rookie_season"]) and str(row["rookie_season"]).isdigit() else 0,
            "last_season": int(row["last_season"]) if pd.notna(row["last_season"]) and str(row["last_season"]).isdigit() else 0,
            "first_candidates": firsts,
            "last_candidates": lasts,
            "full_name_candidates": full_names,
            "primary_first": p_first or (next(iter(firsts)) if firsts else ""),
            "primary_last": p_last or (next(iter(lasts)) if lasts else ""),
        }
        candidates.append(cand)
    return candidates


def match_players(df_proj, df_fp, df_nfl, match_all_players: bool = False):
    # 1. Clean & index nflfastR dataset
    nfl_candidates = build_nfl_candidates(df_nfl)
    
    # 2. Clean FantasyPros dataset
    df_fp_clean = df_fp.copy()
    df_fp_clean["norm_name"] = df_fp_clean["player_name"].apply(normalize_name)
    df_fp_clean["norm_team"] = df_fp_clean["team"].apply(normalize_team)
    df_fp_clean["norm_birthdate"] = df_fp_clean["birthdate"].apply(normalize_birthdate)
    parsed_fp = df_fp_clean["player_name"].apply(lambda x: pd.Series(parse_name_parts(x)))
    df_fp_clean["first_clean"] = parsed_fp[0]
    df_fp_clean["last_clean"] = parsed_fp[1]

    if df_proj is not None and not match_all_players:
        df_target = pd.merge(df_proj, df_fp_clean, left_on="fpid", right_on="player_id", how="inner")
    else:
        df_target = df_fp_clean.copy()
        if "fpid" not in df_target.columns:
            df_target["fpid"] = df_target["player_id"]
        if "name" not in df_target.columns:
            df_target["name"] = df_target["player_name"]

    results = []
    matched_nfl_indices = set()
    current_max_season = max([c["last_season"] for c in nfl_candidates], default=2026)

    for _, row in df_target.iterrows():
        p_name = row["name"] if pd.notna(row.get("name")) else row.get("player_name", "")
        p_norm_name = normalize_name(p_name)
        p_first = row["first_clean"]
        p_last = row["last_clean"]
        p_pos = str(row["position_id"]).upper() if pd.notna(row.get("position_id")) else ""
        p_team = row["norm_team"]
        p_birthdate = row["norm_birthdate"]

        matched_cand = None
        match_type = "Unmatched"
        score = 0.0

        # Special case for DST
        if p_pos in ["DST", "DEF"]:
            match_type = "DST Team Record"
            score = 1.0
        else:
            # -------------------------------------------------------------
            # STEP 1: Direct / Exact Full Name Candidates
            # -------------------------------------------------------------
            exact_cands = []
            for cand in nfl_candidates:
                # Check if exact normalized full name matches or first in first_candidates & last in last_candidates
                is_exact_full = (p_norm_name in cand["full_name_candidates"]) or (
                    p_first in cand["first_candidates"] and p_last in cand["last_candidates"]
                )
                if is_exact_full:
                    exact_cands.append(cand)

            if exact_cands:
                # Rank candidates by:
                # - Birth date match (+1000)
                # - Birth date conflict (-1000 if both non-empty and differ)
                # - Same Team (+500)
                # - Compatible Position (+300)
                # - Most recent last_season (+10 * season)
                # - Status == 'ACT' (+50)
                def rank_exact(cand):
                    points = 0
                    if p_birthdate and cand["birth_date"]:
                        if p_birthdate == cand["birth_date"]:
                            points += 1000
                        else:
                            points -= 1000
                    if p_team and cand["nfl_team"] and p_team == cand["nfl_team"]:
                        points += 500
                    if positions_compatible(p_pos, cand["nfl_pos"]):
                        points += 300
                    points += (cand["last_season"] - 2000) * 10
                    if cand["nfl_status"] == "ACT":
                        points += 50
                    return points

                exact_cands.sort(key=rank_exact, reverse=True)
                top_cand = exact_cands[0]
                
                # Verify top candidate is plausible (not filtered by birthdate conflict)
                if not (p_birthdate and top_cand["birth_date"] and p_birthdate != top_cand["birth_date"] and len(exact_cands) > 1):
                    matched_cand = top_cand
                    score = 1.0
                    
                    has_birth_match = bool(p_birthdate and top_cand["birth_date"] and p_birthdate == top_cand["birth_date"])
                    has_team_match = bool(p_team and top_cand["nfl_team"] and p_team == top_cand["nfl_team"])
                    has_pos_match = positions_compatible(p_pos, top_cand["nfl_pos"])

                    if has_team_match and has_pos_match:
                        match_type = "Exact (Name + Team + Pos)"
                    elif has_birth_match:
                        match_type = "Exact (Name + Birth Date)"
                    elif has_pos_match:
                        match_type = "Exact (Name + Pos)"
                    else:
                        match_type = "Exact (Name Only)"

            # -------------------------------------------------------------
            # STEP 2: Nickname / Alias Matching
            # -------------------------------------------------------------
            if matched_cand is None:
                nickname_cands = []
                p_nick = NICKNAMES.get(p_first)
                for cand in nfl_candidates:
                    if p_last in cand["last_candidates"]:
                        has_nick = False
                        if p_nick and p_nick in cand["first_candidates"]:
                            has_nick = True
                        else:
                            for c_first in cand["first_candidates"]:
                                if NICKNAMES.get(c_first) == p_first:
                                    has_nick = True
                                    break
                        if has_nick:
                            nickname_cands.append(cand)

                if nickname_cands:
                    def rank_nick(cand):
                        points = 0
                        if p_birthdate and cand["birth_date"]:
                            if p_birthdate == cand["birth_date"]:
                                points += 1000
                            else:
                                points -= 1000
                        if p_team and cand["nfl_team"] and p_team == cand["nfl_team"]:
                            points += 500
                        if positions_compatible(p_pos, cand["nfl_pos"]):
                            points += 300
                        points += (cand["last_season"] - 2000) * 10
                        return points

                    nickname_cands.sort(key=rank_nick, reverse=True)
                    top_nick = nickname_cands[0]
                    matched_cand = top_nick
                    score = 0.95
                    if p_birthdate and top_nick["birth_date"] and p_birthdate == top_nick["birth_date"]:
                        match_type = "Fuzzy (Nickname + Birth Date)"
                    elif p_team and top_nick["nfl_team"] and p_team == top_nick["nfl_team"]:
                        match_type = "Fuzzy (Nickname + Team)"
                    else:
                        match_type = "Fuzzy (Nickname)"

            # -------------------------------------------------------------
            # STEP 3: Birth Date Disambiguation & Verification
            # -------------------------------------------------------------
            if matched_cand is None and p_birthdate:
                birth_matches = [
                    cand for cand in nfl_candidates
                    if cand["birth_date"] == p_birthdate and positions_compatible(p_pos, cand["nfl_pos"])
                ]
                if birth_matches:
                    for cand in birth_matches:
                        # Check last name similarity
                        last_sim = max(
                            [SequenceMatcher(None, p_last, c_last).ratio() for c_last in cand["last_candidates"]],
                            default=0.0
                        )
                        if last_sim >= 0.70 or p_last in cand["last_candidates"]:
                            matched_cand = cand
                            score = round(0.90 + 0.10 * last_sim, 3)
                            match_type = "Fuzzy (Birth Date Verified)"
                            break

            # -------------------------------------------------------------
            # STEP 4: Fuzzy Matching (Same Team, Prefix, or High Similarity)
            # -------------------------------------------------------------
            if matched_cand is None:
                best_cand = None
                best_score = 0.0
                best_label = ""

                for cand in nfl_candidates:
                    # Don't match if birth dates exist and conflict
                    if p_birthdate and cand["birth_date"] and p_birthdate != cand["birth_date"]:
                        continue

                    # Position compatibility check
                    is_pos_compat = positions_compatible(p_pos, cand["nfl_pos"])
                    if not is_pos_compat:
                        continue

                    is_same_team = bool(p_team and cand["nfl_team"] and p_team == cand["nfl_team"])

                    # Check max similarity over all last name candidates
                    best_last_sim = max(
                        [SequenceMatcher(None, p_last, c_last).ratio() for c_last in cand["last_candidates"]],
                        default=0.0
                    )
                    if best_last_sim < 0.75:
                        continue

                    matched, sim = is_name_match(
                        p_first, p_last,
                        list(cand["first_candidates"]), cand["primary_last"],
                        same_team=is_same_team
                    )

                    if matched:
                        # Recency and active bonuses
                        total_score = sim
                        if is_same_team:
                            total_score += 0.05
                        if cand["last_season"] >= current_max_season - 1:
                            total_score += 0.02

                        if total_score > best_score:
                            best_score = total_score
                            best_cand = cand
                            if is_same_team:
                                best_label = "Fuzzy (Same Team)"
                            elif p_birthdate and cand["birth_date"] and p_birthdate == cand["birth_date"]:
                                best_label = "Fuzzy (Birth Date Verified)"
                            else:
                                best_label = "Fuzzy (High Similarity)"

                if best_cand is not None and best_score >= 0.80:
                    matched_cand = best_cand
                    score = round(min(best_score, 1.0), 3)
                    match_type = best_label

        if matched_cand is not None:
            matched_nfl_indices.add(matched_cand["index"])

        is_matched = (matched_cand is not None) or (p_pos in ["DST", "DEF"])
        match_status = "Matched" if is_matched else "Unmatched FP Player"

        res = {
            "match_status": match_status,
            "fpid": row["fpid"],
            "name": p_name,
            "position": p_pos,
            "team": row.get("team", ""),
            "birthdate": p_birthdate,
            "sportsdata_player_id": row.get("sportsdata_player_id"),
            "match_type": match_type,
            "similarity": score,
            "gsis_id": matched_cand["gsis_id"] if matched_cand is not None else None,
            "espn_id": matched_cand["espn_id"] if matched_cand is not None else None,
            "pfr_id": matched_cand["pfr_id"] if matched_cand is not None else None,
            "pff_id": matched_cand["pff_id"] if matched_cand is not None else None,
            "esb_id": matched_cand["esb_id"] if matched_cand is not None else None,
            "nfl_id": matched_cand["nfl_id"] if matched_cand is not None else None,
            "smart_id": matched_cand["smart_id"] if matched_cand is not None else None,
            "otc_id": matched_cand["otc_id"] if matched_cand is not None else None,
            "nfl_name": matched_cand["nfl_name"] if matched_cand is not None else None,
            "nfl_pos": matched_cand["nfl_pos"] if matched_cand is not None else None,
            "nfl_team": matched_cand["nfl_team"] if matched_cand is not None else None,
            "nfl_status": matched_cand["nfl_status"] if matched_cand is not None else None,
            "nfl_birth_date": matched_cand["birth_date"] if matched_cand is not None else None,
            "is_matched": is_matched,
        }
        results.append(res)

    df_master = pd.DataFrame(results)

    # Find active/current nflfastR players not in FantasyPros projections
    df_unmatched_nfl_active = df_nfl.loc[
        ~df_nfl.index.isin(matched_nfl_indices)
        & (df_nfl["last_season"] >= current_max_season)
        & (df_nfl["nfl_pos"].isin(["QB", "RB", "FB", "WR", "TE", "K", "PK"]))
    ].copy()

    return df_master, df_unmatched_nfl_active


def main():
    parser = argparse.ArgumentParser(description="Exhaustive crosswalk matching between FantasyPros and nflfastR_players.")
    parser.add_argument("--week", type=int, default=0, help="Projection week filter (default: 0). Pass -1 for all weeks.")
    parser.add_argument("--all-players", action="store_true", help="Match ALL players in ffpros_players registry instead of projection subset")
    parser.add_argument("--pos", type=str, default=None, help="Filter output by position (e.g. QB, RB, WR, TE, K, DST)")
    parser.add_argument(
        "--status",
        type=str,
        choices=["all", "unmatched", "unmatched-fp", "unmatched-nfl", "matched"],
        default="all",
        help="Filter output by status (default: all)",
    )
    parser.add_argument("--csv", action="store_true", help="Export matched crosswalk and unmatched datasets to CSV")
    parser.add_argument("--save-table", type=str, default=None, help="Save the crosswalk directly to a database table name (e.g. player_crosswalk)")
    args = parser.parse_args()

    week_filter = None if args.week == -1 else args.week

    engine = get_db_engine()
    df_proj, df_fp, df_nfl = load_data(engine, week=week_filter, match_all_players=args.all_players)
    df_master, df_unmatched_nfl = match_players(df_proj, df_fp, df_nfl, match_all_players=args.all_players)

    matched_df = df_master[df_master["match_status"] == "Matched"]
    unmatched_fp = df_master[df_master["match_status"] == "Unmatched FP Player"]

    total_records = len(df_master)
    matched_count = len(matched_df)
    unmatched_fp_count = len(unmatched_fp)
    unmatched_nfl_count = len(df_unmatched_nfl)

    pd.set_option("display.max_rows", None)
    pd.set_option("display.max_columns", None)
    pd.set_option("display.width", 1000)

    print("================================================================================")
    print("                 FANTASYPROS <-> NFLFASTR CROSSWALK AUDIT                       ")
    print("================================================================================")
    if args.all_players:
        print("Dataset Scope:                      All FantasyPros Players (ffpros_players)")
    else:
        print(f"Projection Week:                    {week_filter if week_filter is not None else 'All Weeks'}")
    print(f"Total FantasyPros Records:          {total_records}")
    print(f"Total nflfastR Player Registry:     {len(df_nfl)}")
    print(f"Successfully Matched to nflfastR:   {matched_count} / {total_records} ({matched_count/total_records*100:.1f}%)")
    print(f"Unmatched FantasyPros Records:      {unmatched_fp_count}")
    print(f"Unmatched Active nflfastR Players:  {unmatched_nfl_count}")
    print("--------------------------------------------------------------------------------")
    print("Match Type Breakdown:")
    print(df_master["match_type"].value_counts().to_string())

    # 1. EXHAUSTIVE LIST: UNMATCHED FANTASYPROS PLAYERS
    if args.status in ["all", "unmatched", "unmatched-fp"]:
        print("\n================================================================================")
        print(f"   EXHAUSTIVE LIST: UNMATCHED FANTASYPROS PLAYERS ({unmatched_fp_count} TOTAL)")
        print("================================================================================")
        if unmatched_fp.empty:
            print("✓ All FantasyPros players were successfully matched to nflfastR!")
        else:
            unmatched_cols = ["fpid", "name", "position", "team", "birthdate", "match_type"]
            unmatched_subset = unmatched_fp
            if args.pos:
                unmatched_subset = unmatched_subset[unmatched_subset["position"].str.upper() == args.pos.upper()]
            print(unmatched_subset[unmatched_cols].to_string(index=False))

    # 2. EXHAUSTIVE LIST: UNMATCHED ACTIVE NFLFASTR PLAYERS
    if args.status in ["all", "unmatched", "unmatched-nfl"]:
        print("\n================================================================================")
        print(f"   EXHAUSTIVE LIST: UNMATCHED ACTIVE NFLFASTR PLAYERS ({unmatched_nfl_count} TOTAL)")
        print("================================================================================")
        if df_unmatched_nfl.empty:
            print("✓ No unmatched active nflfastR players.")
        else:
            nfl_unmatched_cols = ["gsis_id", "espn_id", "nfl_name", "nfl_pos", "nfl_team", "nfl_birth_date", "pfr_id", "pff_id"]
            nfl_subset = df_unmatched_nfl
            if args.pos:
                nfl_subset = nfl_subset[nfl_subset["nfl_pos"].str.upper() == args.pos.upper()]
            print(nfl_subset[nfl_unmatched_cols].head(100).to_string(index=False))
            if len(nfl_subset) > 100:
                print(f"\n... and {len(nfl_subset) - 100} more active players.")

    # 3. MATCHED PLAYERS
    if args.status in ["matched"]:
        matched_display = matched_df[
            ["fpid", "name", "position", "team", "birthdate", "gsis_id", "espn_id", "pfr_id", "pff_id", "match_type"]
        ]
        if args.pos:
            matched_display = matched_display[matched_display["position"].str.upper() == args.pos.upper()]
        print("\n================================================================================")
        print(f"   MATCHED PLAYERS ({len(matched_display)} TOTAL)")
        print("================================================================================")
        print(matched_display.to_string(index=False))

    # Optional CSV export
    if args.csv:
        df_master.to_csv("master_ffpros_nflfastr_crosswalk.csv", index=False)
        unmatched_fp.to_csv("unmatched_ffpros_players.csv", index=False)
        df_unmatched_nfl.to_csv("unmatched_nflfastr_active.csv", index=False)
        print("\n[+] Exported CSVs:")
        print("  - master_ffpros_nflfastr_crosswalk.csv")
        print("  - unmatched_ffpros_players.csv")
        print("  - unmatched_nflfastr_active.csv")

    # Optional DB table save
    if args.save_table:
        table_name = args.save_table.strip()
        print(f"\n[+] Saving crosswalk table to PostgreSQL: '{table_name}'...")
        df_master.to_sql(table_name, engine, if_exists="replace", index=False)
        print(f"✓ Saved {len(df_master)} rows to table '{table_name}'.")


if __name__ == "__main__":
    main()