"""Dynamic PostgreSQL schema inspector, validator, and Pydantic model generator."""

from __future__ import annotations

import argparse
import keyword
import os
from typing import Any

from dotenv import find_dotenv, load_dotenv
from sqlalchemy import create_engine, inspect
from sqlalchemy.engine import Engine

from shared.models.base import BaseFootballModel

load_dotenv(find_dotenv())

# SQL to Python Type Mapping
TYPE_MAP = {
    "BIGINT": "int",
    "INTEGER": "int",
    "SMALLINT": "int",
    "DOUBLE PRECISION": "float",
    "FLOAT": "float",
    "REAL": "float",
    "NUMERIC": "float",
    "DECIMAL": "float",
    "BOOLEAN": "bool",
    "DATE": "date",
    "TIMESTAMP": "datetime",
    "TIMESTAMP WITHOUT TIME ZONE": "datetime",
    "TIMESTAMP WITH TIME ZONE": "datetime",
    "JSON": "dict[str, Any] | list[Any]",
    "JSONB": "dict[str, Any] | list[Any]",
    "VARCHAR": "str",
    "TEXT": "str",
    "CHAR": "str",
}


def get_db_engine() -> Engine:
    """Build SQLAlchemy engine from environment variables."""
    host = os.getenv("DB_HOST_LOCAL", os.getenv("DB_HOST", "localhost"))
    port = os.getenv("DB_PORT_LOCAL", os.getenv("DB_PORT", "5432"))
    database = os.getenv("DB_NAME", "football")
    user = os.getenv("DB_USER", "postgres")
    password = os.getenv("DB_PASS", "")

    connection_string = f"postgresql+psycopg2://{user}:{password}@{host}:{port}/{database}"
    return create_engine(connection_string)


def get_py_type(sql_type: str) -> str:
    """Map SQL column type to Python type annotation."""
    sql_upper = str(sql_type).upper()
    for key, val in TYPE_MAP.items():
        if key in sql_upper:
            return val
    return "str"


def inspect_table(engine: Engine, table_name: str) -> dict[str, Any]:
    """Inspect columns, types, nullabilities, and primary keys of a table."""
    insp = inspect(engine)
    cols = insp.get_columns(table_name)
    pk_constraint = insp.get_pk_constraint(table_name)
    pks = pk_constraint.get("constrained_columns", []) if pk_constraint else []

    col_definitions = []
    for c in cols:
        name = c["name"]
        sql_type = str(c["type"])
        nullable = c["nullable"]
        py_type = get_py_type(sql_type)
        col_definitions.append({
            "name": name,
            "sql_type": sql_type,
            "py_type": py_type,
            "nullable": nullable,
            "is_pk": name in pks,
        })

    return {
        "table_name": table_name,
        "pks": pks,
        "columns": col_definitions,
    }


def generate_pydantic_code(table_info: dict[str, Any], class_name: str | None = None) -> str:
    """Generate Pydantic model source code for a table."""
    table_name = table_info["table_name"]
    if not class_name:
        class_name = "".join(word.capitalize() for word in table_name.replace("-", "_").split("_"))

    lines = [
        f"class {class_name}(BaseFootballModel):",
        f'    """PostgreSQL table: `{table_name}`."""',
        "",
    ]

    for col in table_info["columns"]:
        c_name = col["name"]
        c_type = col["py_type"]
        c_null = col["nullable"]

        field_name = c_name
        alias_arg = ""

        # Handle reserved words or special prefixes
        if keyword.iskeyword(c_name) or c_name in ["int", "float", "str", "bool", "type", "list", "dict", "id"]:
            field_name = f"{c_name}_"
            alias_arg = f'alias="{c_name}"'
        elif c_name.startswith("_"):
            field_name = c_name.lstrip("_")
            alias_arg = f'alias="{c_name}"'

        if c_null:
            if alias_arg:
                lines.append(f"    {field_name}: {c_type} | None = Field(default=None, {alias_arg})")
            else:
                lines.append(f"    {field_name}: {c_type} | None = None")
        else:
            if alias_arg:
                lines.append(f"    {field_name}: {c_type} = Field(..., {alias_arg})")
            else:
                lines.append(f"    {field_name}: {c_type}")

    return "\n".join(lines)


def validate_table_with_model(
    engine: Engine,
    table_name: str,
    model_cls: type[BaseFootballModel],
    sample_size: int = 100,
) -> tuple[bool, int]:
    """Validate sample rows from PostgreSQL table against its Pydantic model."""
    # Table name from schema inspection is verified safe
    query = f'SELECT * FROM "{table_name}" LIMIT {sample_size}'  # noqa: S608
    try:
        models = model_cls.from_sql(query, engine)
        print(f"✓ Table `{table_name}`: Successfully validated {len(models)} rows against {model_cls.__name__}")
        return True, len(models)
    except Exception as e:
        print(f"✗ Table `{table_name}` validation error with {model_cls.__name__}: {e}")
        return False, 0


def main() -> None:
    parser = argparse.ArgumentParser(description="Football PostgreSQL Schema & Pydantic Tool")
    parser.add_argument("--table", type=str, help="Specific table to inspect or generate")
    parser.add_argument("--generate", action="store_true", help="Generate Pydantic model code")
    parser.add_argument("--validate-all", action="store_true", help="Validate all existing models against live DB")
    args = parser.parse_args()

    engine = get_db_engine()

    if args.validate_all:
        from shared.models import (
            NFLFTN,
            ADPHistory,
            ADPPlayer,
            DraftSelection,
            FFDraftResult,
            FFProsComparePlayer,
            FFProsConsensusRanking,
            FFProsECRRanking,
            FFProsPlayer,
            FFProsProjection,
            FFProsRanking,
            FFScrapRRoster,
            FFScrapRTransaction,
            Keeper,
            League,
            LeagueTeam,
            NFLCombine,
            NFLContract,
            NFLDepthChart,
            NFLDraftPick,
            NFLInjury,
            NFLOfficial,
            NFLParticipation,
            NFLPlayer,
            NFLRoster,
            NFLSchedule,
            NFLSeasonPlayerStats,
            NFLSeasonTeamStats,
            NFLTeamLogo,
            NFLTrade,
            NFLWeeklyPlayerStats,
            NFLWeeklyTeamStats,
            NGSDefenseStats,
            NGSHighlight,
            NGSPassingStats,
            NGSReceivingStats,
            NGSRushingStats,
            PlayerCrosswalk,
        )

        mapping = {
            "leagues": League,
            "teams": LeagueTeam,
            "draft_selections": DraftSelection,
            "keepers": Keeper,
            "ff_draft_results": FFDraftResult,
            "ffscrapR_rosters": FFScrapRRoster,
            "ffscrapR_transactions": FFScrapRTransaction,
            "adp_players": ADPPlayer,
            "adp_history": ADPHistory,
            "player_crosswalk": PlayerCrosswalk,
            "ffpros_players": FFProsPlayer,
            "ffpros_projections": FFProsProjection,
            "ffpros_rankings": FFProsRanking,
            "ffpros_ecr_rankings": FFProsECRRanking,
            "ffpros_consensus_rankings": FFProsConsensusRanking,
            "ffpros_compare_players": FFProsComparePlayer,
            "nflfastR_players": NFLPlayer,
            "nflfastR_player_stats_week": NFLWeeklyPlayerStats,
            "nflfastR_player_stats_season": NFLSeasonPlayerStats,
            "nflfastR_schedule": NFLSchedule,
            "nflfastR_rosters": NFLRoster,
            "nflfastR_depthcharts": NFLDepthChart,
            "nflfastR_injuries": NFLInjury,
            "nflfastR_contracts": NFLContract,
            "nflfastR_draft": NFLDraftPick,
            "nflfastR_combine": NFLCombine,
            "nflfastR_trades": NFLTrade,
            "nflfastR_officials": NFLOfficial,
            "nflfastR_logos": NFLTeamLogo,
            "nflfastR_team_stats_week": NFLWeeklyTeamStats,
            "nflfastR_team_stats_season": NFLSeasonTeamStats,
            "nflfastR_participation": NFLParticipation,
            "nflfastR_ftn": NFLFTN,
            "ngs_passing_stats": NGSPassingStats,
            "ngs_rushing_stats": NGSRushingStats,
            "ngs_receiving_stats": NGSReceivingStats,
            "ngs_defense_stats": NGSDefenseStats,
            "ngs_highlights": NGSHighlight,
        }

        print("Validating models against live PostgreSQL database...")
        passed = 0
        failed = 0
        for table_name, model_cls in mapping.items():
            success, _ = validate_table_with_model(engine, table_name, model_cls)
            if success:
                passed += 1
            else:
                failed += 1

        print(f"\nValidation Summary: {passed} passed, {failed} failed out of {len(mapping)} tables tested.")
        return

    if args.table:
        info = inspect_table(engine, args.table)
        if args.generate:
            code = generate_pydantic_code(info)
            print(code)
        else:
            print(f"Table: {args.table} ({len(info['columns'])} columns, PKs: {info['pks']})")
            for c in info["columns"]:
                print(f"  {c['name']}: {c['sql_type']} -> {c['py_type']} (nullable={c['nullable']})")


if __name__ == "__main__":
    main()
