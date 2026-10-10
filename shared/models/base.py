"""Base Pydantic model for football PostgreSQL tables."""

from __future__ import annotations

from collections.abc import Sequence
from typing import Any, TypeVar

import pandas as pd
from pydantic import BaseModel, ConfigDict
from sqlalchemy import Engine, text

T = TypeVar("T", bound="BaseFootballModel")


class BaseFootballModel(BaseModel):
    """Base class for all football domain models.

    Configured for Pydantic v2 with:
    - `from_attributes=True` for seamless ORM / SQLAlchemy / record mapping
    - `populate_by_name=True` so fields with aliases can be initialized by name or alias
    - `extra="ignore"` to gracefully handle unexpected columns
    - `str_strip_whitespace=True` for automatic text cleanup
    """

    model_config = ConfigDict(
        from_attributes=True,
        populate_by_name=True,
        extra="ignore",
        arbitrary_types_allowed=True,
        str_strip_whitespace=True,
        validate_assignment=True,
    )

    @classmethod
    def from_df(cls: type[T], df: pd.DataFrame) -> list[T]:
        """Convert a pandas DataFrame to a list of validated Pydantic models.

        Handles NaN/NaT values by replacing them with None.
        """
        if df.empty:
            return []
        
        # Replace NaN/NaT with None for clean Pydantic parsing
        cleaned_df = df.astype(object).where(pd.notna(df), None)
        records = cleaned_df.to_dict(orient="records")
        return [cls.model_validate(rec) for rec in records]

    @classmethod
    def to_df(cls, models: Sequence[BaseFootballModel]) -> pd.DataFrame:
        """Convert a sequence of Pydantic models to a pandas DataFrame."""
        if not models:
            return pd.DataFrame()
        records = [m.model_dump(by_alias=False) for m in models]
        return pd.DataFrame(records)

    @classmethod
    def from_sql(
        cls: type[T],
        query: str,
        engine: Engine,
        params: dict[str, Any] | None = None,
    ) -> list[T]:
        """Execute a SQL query and parse rows into validated Pydantic models."""
        with engine.connect() as conn:
            df = pd.read_sql(text(query), conn, params=params or {})
        return cls.from_df(df)

    @classmethod
    def from_sql_one(
        cls: type[T],
        query: str,
        engine: Engine,
        params: dict[str, Any] | None = None,
    ) -> T | None:
        """Execute a SQL query and parse single row into model, or None if empty."""
        models = cls.from_sql(query, engine, params=params)
        return models[0] if models else None
